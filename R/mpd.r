#' Minimal package development functions
#'
#' These functions provide a dependency-light alternative to `devtools`.
#'
#' `test()` install the package and runs tests in the `test` folder, these tests must be created by
#' some unit-testing framework in that they need to be able to be run outside of `R CMD check`
#' in the sense that they must report an error if the test fail, as the `Rout.save` snapshot
#' testing isn't supported when you just run the files in the `test` folder.
#'
#' The `check()` maps to the `R CMD check` and `document()` runs the `roxygen2::roxygenize`
#' function to generate documentation.
#'
#' @param pkg a path to the package directory
#' @param as_cran run the `R CMD check` with the CRAN preset (internet connection required)
#' @return nothing, these functions are run for their side-effect
#'
#' @export
test = function(pkg = "."){
    pkg_name = pkg_name(pkg)

    # set local library
    tmp_lib = file.path(tempdir(), "r-lib")
    if(!dir.exists(tmp_lib)) dir.create(tmp_lib)
    .libPaths(c(tmp_lib, .libPaths()))

    # install pkg
    pkg_install(pkg, tmp_lib)

    # remove package from search path if its already there
    if(paste0("package:", pkg_name) %in% search())
        detach(paste0("package:", pkg_name), unload = TRUE, force = TRUE, character.only = TRUE)

    test_files = list.files(file.path(pkg, "tests"), full.names = TRUE)
    test_files = Filter(function(x) utils::file_test("-f", x), test_files)

    # run the code inside pkg environment
    env = new.env(parent = getNamespace(pkg_name))
    for(test_file in test_files){
        rm(list = ls(env, all.names = TRUE), envir = env)
        sys.source(test_file, chdir = TRUE, envir = env, toplevel.env = getNamespace(pkg_name))
        }
    }


#' @rdname test
#' @export
check = function(pkg = ".", as_cran = FALSE){
    tmp = tempdir()

    pkg_file = pkg_build(pkg)
    pkg_check(pkg_file, as_cran = as_cran)
    }


#' @rdname test
#' @export
document = function(pkg = "."){
    if(!requireNamespace("roxygen2"))
        stop("package roxygen2 required")

    roxygen2::roxygenize(pkg)
    }
