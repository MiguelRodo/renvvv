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
#' @param prompt Logical. Whether to prompt for user confirmation during renv
#'   operations. Default is `FALSE`.
#' @param transactional Logical. Whether to use transactional package
#'   restoration. Default is `FALSE`.
#' @param args_update List. Arbitrary arguments to pass down for update operations.
#' @param args_install List. Arbitrary arguments to pass down to `renv::install()`.
#'
#' @return Invisibly returns `TRUE` upon successful completion.
#'
#' @export
#' @aliases renv3_update
renvvv_update <- function(github = TRUE,
                          non_github = TRUE,
                          biocmanager_install = FALSE,
                          skip = character(0),
                          skip_if_dep_unavailable = TRUE,
                          prompt = FALSE,
                          transactional = FALSE,
                          args_update = list(),
                          args_install = list()) {
  # Sanitize to prevent overriding core arguments
  args_update[c("packages", "prompt", "transactional")] <- NULL
  args_install[c("packages", "prompt")] <- NULL

  .check_renv()
  .ensure_cli()
  .check_renv_activation()

  cli::cli_h1("Starting renv environment update")

  lockfile_list_pkg <- .renv_lockfile_read_pkgs()
  package_list <- .renv_lockfile_pkg_get(lockfile_list_pkg)
  .renv_restore_or_update_impl(
    package_list = package_list,
    non_github = non_github,
    github = github,
    restore = FALSE,
    biocmanager_install = biocmanager_install,
    skip = skip,
    skip_if_dep_unavailable = skip_if_dep_unavailable,
    lockfile_list_pkg = lockfile_list_pkg,
    prompt = prompt,
    transactional = transactional,
    args_restore = list(),
    args_install = args_install,
    args_update = args_update
  )
  cli::cli_h1("renv environment update completed")
  invisible(TRUE)
}

#' @export
renv3_update <- renvvv_update
