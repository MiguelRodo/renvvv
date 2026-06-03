#' @title Restore and Update renv Lockfile Packages
#'
#' @description
#' First restores packages from the lockfile, then updates them to the latest
#' versions. Combines `renvvv_restore()` and `renvvv_update()` in sequence.
#'
#' If an renv project is detected (via `renv.lock` file) but not currently
#' active, the function will activate it. In interactive sessions, the user
#' will be prompted for confirmation before activation. In non-interactive
#' sessions, activation occurs automatically.
#'
#' @param github Logical. Whether to process GitHub packages. Default is `TRUE`.
#' @param non_github Logical. Whether to process non-GitHub packages
#'   (CRAN and Bioconductor). Default is `TRUE`.
#' @param biocmanager_install Logical.
#'   If `TRUE`, Bioconductor packages will be installed using
#'   `BiocManager::install`; otherwise,
#'   `renv::install("bioc::<package_name>")` will be used.
#'   Default is `FALSE`.
#' @param skip Character vector. Package names to skip during restore and update.
#'   Default is `character(0)` (no packages skipped).
#' @param skip_if_dep_unavailable Logical. Passed to `renvvv_update()`. If
#'   `TRUE`, skip installing a package during the update step when one of its
#'   lockfile-listed dependencies previously failed to install and is not
#'   currently available. Default is `TRUE`.
#' @param prompt Logical. Whether to prompt for user confirmation during renv
#'   operations. Default is `FALSE`.
#' @param transactional Logical. Whether to use transactional package
#'   restoration. Default is `FALSE`.
#' @param args_restore List. Arbitrary arguments to pass down to `renv::restore()`.
#' @param args_update List. Arbitrary arguments to pass down for update operations.
#' @param args_install List. Arbitrary arguments to pass down to `renv::install()`.
#'
#' @return Invisibly returns `TRUE` upon successful completion.
#'
#' @export
#' @aliases renv3_restore_and_update
renvvv_restore_and_update <- function(github = TRUE,
                                      non_github = TRUE,
                                      biocmanager_install = FALSE,
                                      skip = character(0),
                                      skip_if_dep_unavailable = TRUE,
                                      prompt = FALSE,
                                      transactional = FALSE,
                                      args_restore = list(),
                                      args_update = list(),
                                      args_install = list()) {
  renvvv_restore(
    github = github, 
    non_github = non_github, 
    biocmanager_install = biocmanager_install, 
    skip = skip,
    prompt = prompt,
    transactional = transactional,
    args_restore = args_restore,
    args_install = args_install
  )
  renvvv_update(
    github = github,
    non_github = non_github,
    biocmanager_install = biocmanager_install,
    skip = skip,
    skip_if_dep_unavailable = skip_if_dep_unavailable,
    prompt = prompt,
    transactional = transactional,
    args_update = args_update,
    args_install = args_install
  )
  invisible(TRUE)
}

#' @export
renv3_restore_and_update <- renvvv_restore_and_update
