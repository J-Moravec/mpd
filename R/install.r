#' Install package from github
#'
#' A simple wrapper around [utils::install.packages()] to install packages from github.
#'
#' This functions wraps `utils::install.packages(url, repos = NULL)` which allows
#' installing packages directly from the `url`, provided it has correct format
#' (tarball). This means that the package dependencies
#' are not installed, see the `repos` option in `?install_packages` for more details.
#'
#' Another limitation is that the package must be at the very root of the repository.
#' For instance, if the package is inside the `pkg` folder of the repository (e.g., `tinytest`),
#' the package cannot be installed.
#'
#' The `branch` parameter specified from which git branch is the package installed.
#' If `branch = NULL` (the default), an extra request is performed through the github API
#' to read the metadata on the default branch (typically `master` or `main`).
#' If this fails for some reason, or you wish to not make an extra request, specify the branch
#' manually.
#'
#' @param x input in the form of owner/repo, such as `J-Moravec/mpd`.
#' @param branch **optional** a git branch to download the repo from, if not specified,
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
install_github = function(x, branch = NULL){
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
    invisible(utils::install.packages(paste0(url, ".tar.gz"), repos = NULL))
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
