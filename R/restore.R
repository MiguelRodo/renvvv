#' @title Restore renv Lockfile Packages
#'
#' @description
#' Restores packages from the lockfile, attempting to install the lockfile
#' versions. When individual packages fail, continues with the remaining
#' packages and retries failures individually.
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
#' # Restore all packages
#' renvvv_restore()
#'
#' # Only restore non-GitHub packages
#' renvvv_restore(github = FALSE)
#'
#' # Restore to a specific project without activating renv
#' renvvv_restore(project = "/path/to/project")
#' }
#'
#' @export
renvvv_restore <- function(github = TRUE,
                               non_github = TRUE,
                               biocmanager_install = FALSE,
                               project = NULL) {
  .check_renv()
  .ensure_cli()

  cli::cli_h1("Starting renv environment restoration")

  package_list <- .renv_lockfile_pkg_get()
  .renv_restore_or_update_impl(
    package_list = package_list,
    non_github = non_github,
    github = github,
    restore = TRUE,
    biocmanager_install = biocmanager_install,
    project = project
  )
  cli::cli_h1("renv environment restoration completed")
  invisible(TRUE)
}
