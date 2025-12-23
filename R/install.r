#' Install package from github
#'
#' A simple wrapper around [utils::install.packages()] to install packages from github.
#'
#' This functions wraps `utils::install.packages(url, repos = NULL)` which allows
#' installing packages directly from the `url`, provided it has correct format
#' (tarball).
#'
#' The limitation is that the package must be at the very root of the repository.
#' For instance, if the package is inside the `pkg` folder of the repository (e.g., `tinytest`),
#' the package cannot be installed.
#'
#' If `dependencies` is TRUE, `strong`, `most`, or `all`, the package dependencies will
#' also be installed from CRAN, see [package_dependencies()].
#'
#' The `branch` parameter specified from which git branch is the package installed.
#' If `branch = NULL` (the default), an extra request is performed through the github API
#' to read the metadata on the default branch (typically `master` or `main`).
#' If this fails for some reason, or you wish to not make an extra request, specify the branch
#' manually.
#'
#' @param x input in the form of owner/repo, such as `J-Moravec/mpd`.
#' @param branch **optional** a git branch to download the repo from, if not specified,
#' @param dependencies **optional** install dependencies, see details
#' additional http request is performed to retrieve the default branch from metadata.
#' @return Invisible `NULL`
#'
#' @seealso
#' [utils::install_packages()] for the underlying function,
#' [remotes::install_github()] for a more complex and feature-full version
#' [pak::pak()] for a modern way to install packages
#'
#' @examples
#' \dontrun{
#'   install_github("J-Moravec/mpd") # install the latest version of mpd
#' }
#'
#' @export
install_github = function(x, dependencies = FALSE, branch = NULL){
    x = strsplit(x, split = "/", fixed = TRUE)[[1]]
    if(length(x) < 2)
        stop("x is malformed, must be in the form of \"owner/repo\"")

    owner = x[1]
    repo = x[2]

    if(is.null(branch)){
        branch = github_detect_main_branch(owner, repo)
        }

    github_url = "https://github.com"
    url = paste(github_url, owner, repo, "archive/refs/heads", branch, sep = "/")
    url = paste0(url, ".tar.gz")

    # no dependencies, early exit
    if(isFALSE(dependencies)){
        res = utils::install.packages(url, repos = NULL, type = "source")
        return(invisible(res))
        }

    if(isTRUE(dependencies)) dependencies = "strong"
    which = .package_dependencies_types(dependencies)
    path = .download_package(url)
    deps = unique(unlist(package_dependencies(path, which)))
    deps = deps[!installed_packages(deps)]
    utils::install.packages(deps)

    res = utils::install.packages(path, repos = NULL, type = "source")
    invisible(res)
    }


.download_package = function(url, destdir = NULL){
    if(is.null(destdir))
        destdir = file.path(tempdir(), "downloaded_packages")

    if(!dir.exists(destdir) && !dir.create(destdir))
        stop("Unable to create directory \"%s\"", destdir)

    dest  = file.path(destdir, basename(url))
    err = utils::download.file(url, dest, quiet = TRUE, mode = "wb")

    if(0 != err)
        stop("Download from url failed: %s", url)

    dest
    }


github_detect_main_branch = function(owner, repo){
    res = github_api_request(owner, repo)

    branch = grep("default_branch", res, fixed = TRUE, value = TRUE)
    branch = sub(".*: \"(.*)\",", "\\1", branch)

    if(!length(branch))
        stop("No default branch found.")

    branch
    }


github_api_request = function(owner, repo, endpoint = NULL){
    github_url = "https://api.github.com/repos"


    url = paste(github_url, owner, repo, sep = "/")
    if(!is.null(endpoint))
        url = paste(url, endpoint, "/")

    readLines(url)
    }
