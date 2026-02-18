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
#'
#' @return Invisibly returns `TRUE` upon successful completion.
#'
#' @examples
#' \dontrun{
#' # Restore and then update all packages
#' renvvv_restore_and_update()
#' }
#'
#' @export
renvvv_restore_and_update <- function(github = TRUE,
                                          non_github = TRUE,
                                          biocmanager_install = FALSE) {
  renvvv_restore(github, non_github, biocmanager_install)
  renvvv_update(github, non_github, biocmanager_install)
}
