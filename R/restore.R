#' @title Restore renv Lockfile Packages
#'
#' @description
#' Restores packages from the lockfile, attempting to install the lockfile
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
#' @param skip Character vector. Package names to skip during restore.
#'   Default is `character(0)` (no packages skipped).
#' @param prompt Logical. Whether to prompt for user confirmation during renv
#'   operations. Default is `FALSE`.
#' @param transactional Logical. Whether to use transactional package
#'   restoration. Default is `FALSE`.
#' @param args_restore List. Arbitrary arguments to pass down to `renv::restore()`.
#' @param args_install List. Arbitrary arguments to pass down to `renv::install()`
#'   when fallback installations occur.
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
#' # Pass clean = FALSE to renv::restore
#' renvvv_restore(args_restore = list(clean = FALSE))
#' }
#'
#' @export
#' @aliases renv3_restore
renvvv_restore <- function(github = TRUE,
                           non_github = TRUE,
                           biocmanager_install = FALSE,
                           skip = character(0),
                           prompt = FALSE,
                           transactional = FALSE,
                           args_restore = list(),
                           args_install = list()) {
  # Sanitize to prevent overriding core arguments
  args_restore[c("packages", "prompt", "transactional")] <- NULL
  args_install[c("packages", "prompt")] <- NULL

  .check_renv()
  .ensure_cli()
  .check_renv_activation()

  cli::cli_h1("Starting renv environment restoration")

  lockfile_list_pkg <- .renv_lockfile_read_pkgs()
  package_list <- .renv_lockfile_pkg_get(lockfile_list_pkg)
  .renv_restore_or_update_impl(
    package_list = package_list,
    non_github = non_github,
    github = github,
    restore = TRUE,
    biocmanager_install = biocmanager_install,
    skip = skip,
    skip_if_dep_unavailable = TRUE,
    lockfile_list_pkg = lockfile_list_pkg,
    prompt = prompt,
    transactional = transactional,
    args_restore = args_restore,
    args_install = args_install,
    args_update = list()
  )
  cli::cli_h1("renv environment restoration completed")
  invisible(TRUE)
}

#' @export
renv3_restore <- renvvv_restore
