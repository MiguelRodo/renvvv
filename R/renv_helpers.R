# Internal function to find the renv lockfile path
# Mimics renv::renv_paths_lockfile() logic without activating the project
.renv_paths_lockfile <- function(project = NULL) {
  # Check for environment variable override
  override <- Sys.getenv("RENV_PATHS_LOCKFILE", unset = NA)
  if (!is.na(override)) {
    last <- substr(override, nchar(override), nchar(override))
    if (last %in% c("/", "\\")) {
      override <- paste0(override, "renv.lock")
    }
    return(override)
  }

  # Use project directory to construct lockfile path
  if (is.null(project)) {
    project <- getwd()
  }

  # Check for profile
  profile <- Sys.getenv("RENV_PROFILE", unset = NA)
  if (!is.na(profile) && profile != "") {
    # With profile: project/renv/profiles/<name>/renv.lock
    lockfile_path <- file.path(
      project, "renv", "profiles", profile, "renv.lock"
    )
  } else {
    # Standard location: project/renv.lock
    lockfile_path <- file.path(project, "renv.lock")
  }

  return(lockfile_path)
}

# Internal function to get package lists from the renv lockfile
.renv_lockfile_pkg_get <- function() {
  # Find lockfile path without activating the project
  lockfile_path <- .renv_paths_lockfile()

  # Read lockfile directly
  lockfile_list_pkg <- renv::lockfile_read(file = lockfile_path)$Packages

  pkg_vec_regular <- character()
  pkg_vec_bioc <- character()
  pkg_vec_gh <- character()

  for (package_name in names(lockfile_list_pkg)) {
    package_info <- lockfile_list_pkg[[package_name]]
    remote_username <- package_info$RemoteUsername
    source <- tolower(package_info$Source)

    if (is.null(remote_username)) {
      is_bioc <- grepl("bioc", source)
      if (is_bioc) {
        pkg_vec_bioc <- c(pkg_vec_bioc, package_name)
      } else {
        pkg_vec_regular <- c(pkg_vec_regular, package_name)
      }
    } else {
      pkg_vec_gh <- c(pkg_vec_gh, paste0(remote_username, "/", package_name))
    }
  }

  list(
    regular = pkg_vec_regular,
    bioc = pkg_vec_bioc,
    gh = pkg_vec_gh
  )
}

# Internal function to manage the restoration or updating process
.renv_restore_or_update_impl <- function(package_list,
                                               github,
                                               non_github,
                                               restore,
                                               biocmanager_install) {
  # CRAN Packages
  .renv_restore_or_update_actual_wrapper(
    pkg = package_list[["regular"]],
    act = non_github,
    restore = restore,
    source = "CRAN",
    biocmanager_install = biocmanager_install
  )

  # Bioconductor Packages
  .renv_restore_or_update_actual_wrapper(
    pkg = package_list[["bioc"]],
    act = non_github,
    restore = restore,
    source = "Bioconductor",
    biocmanager_install = biocmanager_install
  )

  # GitHub Packages
  .renv_restore_or_update_actual_wrapper(
    pkg = package_list[["gh"]],
    act = github,
    restore = restore,
    source = "GitHub",
    biocmanager_install = biocmanager_install
  )
  invisible(TRUE)
}

# Wrapper function for processing package groups
.renv_restore_or_update_actual_wrapper <- function(pkg,
                                                         act,
                                                         restore,
                                                         source,
                                                         biocmanager_install) {
  if (length(pkg) == 0L) {
    cli::cli_alert_info("No {source} packages to process.")
    return(invisible(FALSE))
  }

  if (act) {
    action <- if (restore) "Restoring" else "Installing latest"
    cli::cli_alert_info("{action} {source} packages.")
    .renv_restore_update_actual(
      pkg,
      restore,
      biocmanager_install,
      is_bioc = (source == "Bioconductor")
    )
  } else {
    action <- if (restore) "restoring" else "installing"
    cli::cli_alert_info("Skipping {action} {source} packages.")
  }
}

# Internal function to restore or update packages
.renv_restore_update_actual <- function(pkg, restore, biocmanager_install, is_bioc) {
  if (length(pkg) == 0L) {
    return(invisible(FALSE))
  }

  .ensure_cli()

  pkg_type <- if (is_bioc) {
    "Bioconductor"
  } else if (all(grepl("/", pkg))) {
    "GitHub"
  } else {
    "CRAN"
  }

  # Extract package names from possible remotes
  pkg_names <- sapply(pkg, function(x) sub("^.*/", "", x))

  if (restore) {
    cli::cli_alert_info(
      "Attempting to restore {pkg_type} packages: {.pkg {pkg_names}}"
    )
    # Attempt to restore packages
    tryCatch(
      renv::restore(packages = pkg_names, transactional = FALSE),
      error = function(e) {
        cli::cli_alert_danger(
          "Failed to restore {pkg_type} packages: {.pkg {pkg_names}}. Error: {e$message}"
        )
      }
    )
    cli::cli_alert_info("Checking for packages that failed to restore.")
    .renv_restore_remaining(pkg_names)
  } else {
    cli::cli_alert_info(
      "Installing latest {pkg_type} packages: {.pkg {pkg_names}}"
    )
    # Install the latest versions
    .renv_install(pkg, biocmanager_install, is_bioc)
  }

  cli::cli_alert_info("Checking for packages that are still not installed.")
  # Install any remaining packages that were not installed
  .renv_install_remaining(pkg, biocmanager_install, is_bioc)
  invisible(TRUE)
}

# Internal function to restore remaining packages individually
.renv_restore_remaining <- function(pkg) {
  .ensure_cli()

  installed_pkgs <- rownames(installed.packages())
  pkg_remaining <- pkg[!pkg %in% installed_pkgs]

  if (length(pkg_remaining) == 0L) {
    cli::cli_alert_success("All packages restored successfully.")
    return(invisible(FALSE))
  }

  cli::cli_alert_warning(
    "Packages that failed to restore: {.pkg {pkg_remaining}}"
  )
  cli::cli_alert_info("Attempting to restore packages individually.")

  for (x in pkg_remaining) {
    if (!requireNamespace(x, quietly = TRUE)) {
      tryCatch(
        renv::restore(packages = x, transactional = FALSE),
        error = function(e) {
          cli::cli_alert_danger(
            "Failed to restore package: {.pkg {x}}. Error: {e$message}"
          )
        }
      )
    }
  }
}

# Internal function to install packages
.renv_install <- function(pkg, biocmanager_install, is_bioc) {
  .ensure_cli()

  if (is_bioc) {
    if (biocmanager_install) {
      if (!requireNamespace("BiocManager", quietly = TRUE)) {
        cli::cli_alert_warning(
          "BiocManager not installed. Installing Bioconductor packages using renv instead."
        )
        cli::cli_alert_info(
          "Installing Bioconductor packages using renv: {.pkg {pkg}}"
        )
        tryCatch(
          renv::install(paste0("bioc::", pkg), prompt = FALSE),
          error = function(e) {
            cli::cli_alert_danger(
              "Failed to install Bioconductor packages via renv: {.pkg {pkg}}. Error: {e$message}"
            )
          }
        )
      } else {
        cli::cli_alert_info(
          "Installing Bioconductor packages using BiocManager: {.pkg {pkg}}"
        )
        tryCatch(
          BiocManager::install(pkg, update = TRUE, ask = FALSE),
          error = function(e) {
            cli::cli_alert_danger(
              "Failed to install Bioconductor packages using BiocManager: {.pkg {pkg}}. Error: {e$message}"
            )
          }
        )
      }
    } else {
      cli::cli_alert_info(
        "Installing Bioconductor packages using renv: {.pkg {pkg}}"
      )
      tryCatch(
        renv::install(paste0("bioc::", pkg), prompt = FALSE),
        error = function(e) {
          cli::cli_alert_danger(
            "Failed to install Bioconductor packages via renv: {.pkg {pkg}}. Error: {e$message}"
          )
        }
      )
    }
  } else {
    cli::cli_alert_info("Installing packages: {.pkg {pkg}}")
    tryCatch(
      renv::install(pkg, prompt = FALSE),
      error = function(e) {
        cli::cli_alert_danger(
          "Failed to install packages: {.pkg {pkg}}. Error: {e$message}"
        )
      }
    )
  }
}

# Internal function to install any remaining packages
.renv_install_remaining <- function(pkg, biocmanager_install, is_bioc) {
  .ensure_cli()

  installed_pkgs <- rownames(installed.packages())
  pkg_remaining <- pkg[
    !sapply(pkg, function(x) sub("^.*/", "", x)) %in% installed_pkgs
  ]

  if (length(pkg_remaining) == 0L) {
    cli::cli_alert_success("All packages are installed.")
    return(invisible(FALSE))
  }

  cli::cli_alert_warning(
    "Packages that are still missing: {.pkg {pkg_remaining}}"
  )
  cli::cli_alert_info("Attempting to install remaining packages.")

  # Attempt to install remaining packages
  .renv_install(pkg_remaining, biocmanager_install, is_bioc)

  # Check again for any packages that failed to install
  pkg_still_missing <- pkg_remaining[
    !sapply(pkg_remaining, function(x) {
      requireNamespace(sub("^.*/", "", x), quietly = TRUE)
    })
  ]

  if (length(pkg_still_missing) == 0L) {
    cli::cli_alert_success("All remaining packages installed successfully.")
    return(invisible(TRUE))
  }

  cli::cli_alert_warning(
    "Packages that failed to install: {.pkg {pkg_still_missing}}"
  )
  cli::cli_alert_info("Attempting to install missing packages individually.")

  # Try installing missing packages individually
  for (x in pkg_still_missing) {
    if (!requireNamespace(sub("^.*/", "", x), quietly = TRUE)) {
      .renv_install(x, biocmanager_install, is_bioc)
    }
  }

  # Final check
  pkg_final_missing <- pkg_still_missing[
    !sapply(pkg_still_missing, function(x) {
      requireNamespace(sub("^.*/", "", x), quietly = TRUE)
    })
  ]

  if (length(pkg_final_missing) == 0L) {
    cli::cli_alert_success(
      "All packages installed successfully after individual attempts."
    )
  } else {
    cli::cli_alert_danger(
      "Some packages failed to install: {.pkg {pkg_final_missing}}"
    )
  }
}
