#' Local package paths and dependencies
#'
#' Find locally installed packages and their dependencies
#'
#' These functions replace/extend similar functions in base and utils,
#' but provide slightly different interface and/or functionality.
#'
#' `find_packages()` explores `.libPaths()` to find locally installed packages
#' and returns their path, similar to [base::find.package()].
#' It is less chatty than the base function and returns `NA` when the package was not found,
#' instead of omitting the value altogether.
#' `find_packages()` returns a character vector of the same length as input with paths
#' towards the packages, and `NA` if the package wasn't found.
#'
#' `installed_packages()` is simple wrapper around `find_packages()` to check
#' whether the specified packages are installed. Compared to [utils::installed.packages()],
#' it doesn't return a list of all installed packages, making it much faster than
#' `pkg %in% rownames(utils::installed.packages())`.
#' It returns a logical vecor of the same length as input with `TRUE` if the package is found
#' in `.libPaths()` and `FALSE` otherwise.
#'
#' `package_dependencies()` will return dependencies of a locally installed package,
#' a package in a local directory, or a tarball (as obtained from `R CMD build`).
#' This is done by parsing the DESCRIPTION file and extracting the `Depends`, `Imports`,
#' `LinkingTo`, and `Enhances` fields.
#' Unlike [tools::package_dependencies()], it works only with locally available packages
#' and it doesn't explore CRAN or require internet access, but works on packages
#' that are not available on CRAN..
#' `package_dependencies()` returns a list with the same name as input, with each element
#' being a character vector of package dependencies.
#'
#' The `which` argument for `package_dependencies()` can be one or more of the dependency types
#' (`Depends`, `Imports`, `LinkingTo`, `Enhances`), or `strong` for the first three types,
#' `most` for the first four types, or `all` for all dependency types.
#'
#' @param x for `find_packages()`, `installed_packages()` a character vector of package names,
#'          for `package_dependencies()` a character vector of package names, local directories,
#'          or a path to tarballs.
#' @param which for `package_dependencies()` which dependencies to return, see details
#' @return a named logical vector
#'
#' @seealso
#' [utils::installed.packages()] for a database of all installed packages,
#' [tools::package_dependencies()] for remote the 
#' [base::system.file()] and [base::find.package()] to get file/folder within package directory
#' [base::.libPaths()] for a vector of all library directories
#'
#' @examples
#' find_packages(c("base", "mpd", "not even valid name"))
#' installed_packages(c("base", "mpd", "not even valid name"))
#'
#' package_dependencies(c("base", "mpd"), which = "all")
#'
#' @name packages
NULL

# From: https://stackoverflow.com/a/62809204/4868692
#' @rdname packages
#' @export
find_packages = function(x){
    .find_packages = function(y){
        paths = file.path(.libPaths(), y, "DESCRIPTION")
        # Take the first valid path
        # NA when path doesn't exists (out of bounds)
        dirname(paths)[file.exists(paths)][1] # need to remove DESCRIPTION
        }

    vapply(x, .find_packages, character(1))
    }


#' @rdname packages
#' @export
installed_packages = function(x){
    !is.na(find_packages(x))
    }


#' @rdname packages
#' @export
package_dependencies = function(x, which = "strong"){
    which = .package_dependencies_types(which)

    deps = vector(mode = "list", length = length(x))
    names(deps) = x

    for(i in seq_along(x)){
        pkg = x[i]
        # is archive
        if(endsWith(pkg, ".tar.gz")){
            deps[[i]] = .parse_dependencies_archive(pkg, which = which)
            next
            }

        # is locally installed
        path = find_packages(pkg)
        if(!is.na(path)){
            deps[[i]] = .parse_dependencies(file.path(path, "DESCRIPTION"), which = which)
            next
            }

        # is local directory
        path = file.path(pkg, "DESCRIPTION")
        if(file.exists(path)){
            deps[[i]] = .parse_dependencies(path, which = which)
            next
            }
        }

    not_found = names(deps)[vapply(deps, is.null, logical(1))]
    if(length(not_found) > 0)
        stop("Some packages were not found locally: ", toString(not_found))

    deps
    }


.parse_dependencies_archive = function(x, which){
    files = utils::untar(x, list = TRUE)
    descr = grep("^[^/]*/DESCRIPTION", files, value = TRUE)[1]
    if(is.na(descr))
        stop("There is no DESCRIPTION in the archive: ", basename(x))

    tmpdir = tempdir()
    path = file.path(tmpdir, descr)
    utils::untar(x, files = descr, exdir = tmpdir)
    on.exit(file.remove(path))

    .parse_dependencies(path, which = which)
    }


.parse_dependencies = function(x, which){
    deps = read.dcf(x, fields = which)
    deps = deps[!is.na(deps)]

    # no dependencies were found
    if(length(deps) == 0)
        return(character())

    deps = unlist(strsplit(deps, ",", fixed = TRUE))
    deps = gsub("\\s.*", "", trimws(deps))
    deps = deps[deps != "R"]
    deps
    }


.package_dependencies_types = function(x){
    types = c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
    if(identical(x, "strong"))
        x = types[1:3]
    if(identical(x, "most"))
        x = types[1:4]
    if(identical(x, "all"))
        x = types
    if(!all(x %in% types))
        stop("Unknown dependency types: ", toString(x[!x %in% types]))

    x
    }
