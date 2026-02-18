#' @title Restore and Update renv Lockfile Packages
#'
#' @description
#' First restores packages from the lockfile, then updates them to the latest
#' versions. Combines `renvvv_restore()` and `renvvv_update()` in sequence.
#'
#' @param github Logical. Whether to process GitHub packages. Default is `TRUE`.
#' @param non_github Logical. Whether to process non-GitHub packages
#'   (CRAN and Bioconductor). Default is `TRUE`.
#' @param biocmanager_install Logical.
#'   If `TRUE`, Bioconductor packages will be installed using
#'   `BiocManager::install`; otherwise,
#'   `renv::install("bioc::<package_name>")` will be used.
#'   Default is `FALSE`.
#' @param project The project directory. If `NULL`, defaults to the current
#'   working directory. When specified, packages will be installed to the
#'   project's renv library even if renv is not activated.
#'
#' @return Invisibly returns `TRUE` upon successful completion.
#'
#' @examples
#' \dontrun{
#' # Restore and then update all packages
#' renvvv_restore_and_update()
#'
#' # Restore and update in a specific project without activating renv
#' renvvv_restore_and_update(project = "/path/to/project")
#' }
#'
#' @export
renvvv_restore_and_update <- function(github = TRUE,
                                          non_github = TRUE,
                                          biocmanager_install = FALSE,
                                          project = NULL) {
  renvvv_restore(github, non_github, biocmanager_install, project)
  renvvv_update(github, non_github, biocmanager_install, project)
}
