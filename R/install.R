#' @title Install Packages Robustly
#'
#' @description
#' A "tryhard" version of `renv::install()`. It first attempts to install all
#' specified packages in a single vectorized call. If that fails, it compares
#' the current installed packages and their versions against the state prior
#' to the installation attempt. Any package that is still missing or whose
#' version did not change will then be installed individually, ensuring that
#' one failing package does not prevent the installation of others.
#'
#' @param packages A character vector of packages to install. Can contain CRAN,
#'   Bioconductor, or GitHub package references (e.g., `"dplyr"`, `"user/repo@v1"`).
#' @param prompt Logical. Whether to prompt for user confirmation during
#'   installation. Default is `FALSE`.
#' @param args_install List. Arbitrary arguments to pass down to `renv::install()`.
#'
#' @return Invisibly returns `TRUE` upon completion.
#'
#' @examples
#' \dontrun{
#' # Install multiple packages robustly
#' renvvv_install(c("dplyr", "ggplot2", "SATVILab/projr@dev"))
#'
#' # Pass arbitrary parameters down to renv::install
#' renvvv_install(c("dplyr"), args_install = list(rebuild = TRUE))
#' }
#'
#' @export
#' @aliases renv3_install
renvvv_install <- function(packages, prompt = FALSE, args_install = list()) {
  # Sanitize to prevent overriding core arguments
  args_install[c("packages", "prompt")] <- NULL

  .check_renv()
  .ensure_cli()

  if (length(packages) == 0L) {
    cli::cli_alert_info("No packages specified for installation.")
    return(invisible(TRUE))
  }

  cli::cli_h1("Starting robust package installation")

  # Extract pure package names from references (e.g. "user/repo@v1" -> "repo")
  pkg_names <- vapply(packages, .extract_pkg_name, character(1))

  # Record library state before installation
  # installed.packages() returns a matrix where rownames are package names
  old_inst <- utils::installed.packages()
  old_versions <- if (nrow(old_inst) > 0) old_inst[, "Version"] else character(0)

  cli::cli_alert_info("Attempting vectorized installation of {length(packages)} package(s).")

  call_args <- c(list(packages = packages, prompt = prompt), args_install)
  call_args <- call_args[!duplicated(names(call_args))]

  install_failed <- FALSE
  tryCatch(
    do.call(renv::install, call_args),
    error = function(e) {
      cli::cli_alert_danger("Vectorized installation encountered an error.")
      message("Error: ", e$message)
      install_failed <<- TRUE
    }
  )

  if (!install_failed) {
    cli::cli_alert_success("All packages successfully installed in vectorized mode.")
    cli::cli_h1("Robust package installation completed")
    return(invisible(TRUE))
  }

  cli::cli_alert_info("Checking package installation states for fallback...")

  new_inst <- utils::installed.packages()
  new_versions <- if (nrow(new_inst) > 0) new_inst[, "Version"] else character(0)

  pkgs_to_retry <- character(0)
  names_to_retry <- character(0)

  for (i in seq_along(packages)) {
    rem <- packages[i]
    nm <- pkg_names[i]

    if (!nzchar(nm) || is.na(nm)) {
      next # Skip malformed entries
    }

    # Identify failures: either completely missing, or version hasn't changed
    if (!nm %in% names(new_versions)) {
      pkgs_to_retry <- c(pkgs_to_retry, rem)
      names_to_retry <- c(names_to_retry, nm)
    } else if (nm %in% names(old_versions)) {
      if (new_versions[[nm]] == old_versions[[nm]]) {
        # Note: If it was already up-to-date, it will trigger here too.
        # This is safe because individual renv::install handles up-to-date packages gracefully.
        pkgs_to_retry <- c(pkgs_to_retry, rem)
        names_to_retry <- c(names_to_retry, nm)
      }
    }
  }

  if (length(pkgs_to_retry) == 0L) {
    cli::cli_alert_success("Despite the error, all requested packages appear to be installed and updated.")
    cli::cli_h1("Robust package installation completed")
    return(invisible(TRUE))
  }

  cli::cli_alert_warning(
    "The following package(s) failed or did not update: {.pkg {names_to_retry}}"
  )
  cli::cli_alert_info("Attempting to install them individually.")

  failed_final <- character(0)

  for (i in seq_along(pkgs_to_retry)) {
    rem <- pkgs_to_retry[i]
    nm <- names_to_retry[i]

    cli::cli_alert_info("Installing {.pkg {nm}} ({rem})...")

    single_call_args <- c(list(packages = rem, prompt = prompt), args_install)
    single_call_args <- single_call_args[!duplicated(names(single_call_args))]

    tryCatch(
      {
        do.call(renv::install, single_call_args)
        
        # Validate successful installation natively
        if (!requireNamespace(nm, quietly = TRUE)) {
          failed_final <- c(failed_final, nm)
          cli::cli_alert_danger("Failed to install {.pkg {nm}}.")
        } else {
          cli::cli_alert_success("Successfully installed {.pkg {nm}}.")
        }
      },
      error = function(e) {
        # Trust but verify on JSON errors
        if (!requireNamespace(nm, quietly = TRUE)) {
          cli::cli_alert_danger("Error installing {.pkg {nm}}.")
          message("Error: ", e$message)
          failed_final <<- c(failed_final, nm)
        } else {
          cli::cli_alert_warning("renv reported an error, but {.pkg {nm}} is loadable.")
        }
      }
    )
  }

  if (length(failed_final) > 0L) {
    cli::cli_alert_danger("The following package(s) ultimately failed to install: {.pkg {failed_final}}")
  } else {
    cli::cli_alert_success("All packages installed successfully after individual attempts.")
  }

  cli::cli_h1("Robust package installation completed")
  invisible(TRUE)
}

#' @export
renv3_install <- renvvv_install