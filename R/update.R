#' @title Update renv Lockfile Packages
#'
#' @description
#' Updates packages to their latest available versions, ignoring the lockfile
#' versions. When individual packages fail, continues with the remaining
#' packages and retries failures individually.
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
#' @param skip Character vector. Package names to skip during update.
#'   Default is `character(0)` (no packages skipped).
#' @param skip_if_dep_unavailable Logical. If `TRUE`, skip installing a
#'   package when one of its lockfile-listed dependencies previously failed
#'   to install and is not currently available. Default is `TRUE`.
#'
#' @return Invisibly returns `TRUE` upon successful completion.
#'
#' @examples
#' \dontrun{
#' # Update all packages
#' renvvv_update()
#'
#' # Only update GitHub packages
#' renvvv_update(non_github = FALSE)
#'
#' # Skip specific packages
#' renvvv_update(skip = c("dplyr", "ggplot2"))
#'
#' # Attempt every package even if a dependency failed
#' renvvv_update(skip_if_dep_unavailable = FALSE)
#' }
#'
#' @export
renvvv_update <- function(github = TRUE,
                              non_github = TRUE,
                              biocmanager_install = FALSE,
                              skip = character(0),
                              skip_if_dep_unavailable = TRUE) {
  .check_renv()
  .ensure_cli()
  .check_renv_activation()

  cli::cli_h1("Starting renv environment update")

  package_list <- .renv_lockfile_pkg_get()
  .renv_restore_or_update_impl(
    package_list = package_list,
    non_github = non_github,
    github = github,
    restore = FALSE,
    biocmanager_install = biocmanager_install,
    skip = skip,
    skip_if_dep_unavailable = skip_if_dep_unavailable
  )
  cli::cli_h1("renv environment update completed")
  invisible(TRUE)
}
