# Helper function to ensure the cli package is available
.ensure_cli <- function() {
  if (!requireNamespace("cli", quietly = TRUE)) {
    try(renv::install("cli", prompt = FALSE))
  }
}

# Helper function to check if renv is available
.check_renv <- function() {
  if (!requireNamespace("renv", quietly = TRUE)) {
    stop("The 'renv' package is required but not installed.")
  }
}

# Helper function to check and activate renv project if needed
.check_renv_activation <- function() {
  # Check if we're in a directory with a renv.lock file
  lockfile_path <- .renv_paths_lockfile()
  if (!file.exists(lockfile_path)) {
    # Not in an renv project - no activation needed
    return(invisible(NULL))
  }

  # Check if the project is already active
  current_project <- renv::project(default = NULL)
  project_dir <- getwd()

  # Normalize paths for comparison
  if (!is.null(current_project)) {
    current_project <- normalizePath(current_project, mustWork = FALSE)
  }
  project_dir <- normalizePath(project_dir, mustWork = FALSE)

  # If already active in the correct project, return
  if (!is.null(current_project) && current_project == project_dir) {
    return(invisible(NULL))
  }

  # Project is not active - need to activate
  .ensure_cli()

  if (interactive()) {
    # Interactive mode - prompt user
    cli::cli_alert_warning(
      "The renv project at {.path {project_dir}} is not currently active."
    )
    response <- readline(
      prompt = "Would you like to activate it now? (y/n): "
    )
    if (tolower(trimws(response)) %in% c("y", "yes")) {
      cli::cli_alert_info("Activating renv project...")
      renv::activate(project = project_dir)
      cli::cli_alert_success("renv project activated.")
    } else {
      cli::cli_alert_info("Proceeding without activation.")
      cli::cli_alert_warning(
        "Note: Operations may not work as expected without activation."
      )
    }
  } else {
    # Non-interactive mode - activate silently
    cli::cli_alert_info(
      "Activating renv project at {.path {project_dir}}..."
    )
    renv::activate(project = project_dir)
    cli::cli_alert_success("renv project activated.")
  }

  invisible(NULL)
}

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

# Extract package names from a DESCRIPTION-style dependency field value.
# Handles entries like "curl (>= 5.1.0)" or plain "mime", returning
# only the package name without version constraints.
.parse_dep_field <- function(field_val) {
  if (is.null(field_val) || length(field_val) == 0) return(character(0))
  pkgs <- trimws(gsub("\\s*\\(.*?\\)", "", as.character(field_val)))
  pkgs[nzchar(pkgs)]
}

# Strategy 1 (renv 0.15.0 - 1.0.x): use the pre-computed Requirements field.
# Returns NULL if no package has a Requirements field (wrong lockfile format).
.deps_from_requirements <- function(lockfile_list_pkg) {
  has_requirements <- any(
    vapply(lockfile_list_pkg, function(x) !is.null(x$Requirements), logical(1))
  )
  if (!has_requirements) return(NULL)
  lapply(lockfile_list_pkg, function(pkg_info) {
    reqs <- pkg_info$Requirements
    if (is.null(reqs)) character(0) else as.character(reqs)
  })
}

# Strategy 2 (renv 1.1.0+): parse Imports / Depends / LinkingTo fields.
# Returns NULL if no package has any of these fields (wrong lockfile format).
.deps_from_description_fields <- function(lockfile_list_pkg) {
  dep_fields <- c("Depends", "Imports", "LinkingTo")
  has_fields <- any(vapply(lockfile_list_pkg, function(x) {
    any(dep_fields %in% names(x))
  }, logical(1)))
  if (!has_fields) return(NULL)
  lapply(lockfile_list_pkg, function(pkg_info) {
    raw <- unlist(pkg_info[intersect(dep_fields, names(pkg_info))],
                  use.names = FALSE)
    unique(.parse_dep_field(raw))
  })
}

# Internal function to get package dependencies from the renv lockfile.
# Returns a named list mapping each package name to its dependency names.
# Returns an empty list and emits a warning if dependencies cannot be
# extracted (so callers can proceed without skipping).
#
# Two strategies are tried in order:
#   1. Requirements field (renv 0.15.0 - 1.0.x lockfile format)
#   2. Imports/Depends/LinkingTo fields (renv 1.1.0+ lockfile format)
.renv_lockfile_deps_get <- function() {
  tryCatch({
    lockfile_path <- renv::paths$lockfile()
    if (!file.exists(lockfile_path)) {
      return(list())
    }
    lockfile_list_pkg <- renv::lockfile_read(file = lockfile_path)$Packages
    if (length(lockfile_list_pkg) == 0) {
      return(list())
    }

    deps <- .deps_from_requirements(lockfile_list_pkg)
    if (!is.null(deps)) return(deps)

    deps <- .deps_from_description_fields(lockfile_list_pkg)
    if (!is.null(deps)) return(deps)

    cli::cli_alert_warning(
      "Could not extract package dependencies from lockfile; \\
skip_if_dep_unavailable will be ignored."
    )
    list()
  }, error = function(e) {
    cli::cli_alert_warning(
      paste0(
        "Could not extract package dependencies from lockfile ",
        "(skip_if_dep_unavailable ignored): {e$message}"
      )
    )
    list()
  })
}

#' @importFrom utils installed.packages
# Internal function to get package lists from the renv lockfile
.renv_lockfile_pkg_get <- function() {
  lockfile_path <- renv::paths$lockfile()

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
                                               biocmanager_install,
                                               skip = character(0),
                                               skip_if_dep_unavailable = TRUE) {
  lockfile_deps <- .renv_lockfile_deps_get()

  # CRAN Packages
  .renv_restore_or_update_actual_wrapper(
    pkg = package_list[["regular"]],
    act = non_github,
    restore = restore,
    source = "CRAN",
    biocmanager_install = biocmanager_install,
    skip = skip,
    skip_if_dep_unavailable = skip_if_dep_unavailable,
    lockfile_deps = lockfile_deps
  )

  # Bioconductor Packages
  .renv_restore_or_update_actual_wrapper(
    pkg = package_list[["bioc"]],
    act = non_github,
    restore = restore,
    source = "Bioconductor",
    biocmanager_install = biocmanager_install,
    skip = skip,
    skip_if_dep_unavailable = skip_if_dep_unavailable,
    lockfile_deps = lockfile_deps
  )

  # GitHub Packages
  .renv_restore_or_update_actual_wrapper(
    pkg = package_list[["gh"]],
    act = github,
    restore = restore,
    source = "GitHub",
    biocmanager_install = biocmanager_install,
    skip = skip,
    skip_if_dep_unavailable = skip_if_dep_unavailable,
    lockfile_deps = lockfile_deps
  )
  invisible(TRUE)
}

# Wrapper function for processing package groups
.renv_restore_or_update_actual_wrapper <- function(pkg,
                                                         act,
                                                         restore,
                                                         source,
                                                         biocmanager_install,
                                                         skip = character(0),
                                                         skip_if_dep_unavailable = TRUE,
                                                         lockfile_deps = list()) {
  # Filter out packages in the skip list
  # For GitHub packages, extract package name from "user/package" format
  pkg_names <- sapply(pkg, function(x) sub("^.*/", "", x))
  pkg_to_process <- pkg[!pkg_names %in% skip]
  pkg_skipped <- pkg[pkg_names %in% skip]

  # Report skipped packages
  if (length(pkg_skipped) > 0L) {
    skipped_names <- sapply(pkg_skipped, function(x) sub("^.*/", "", x))
    action <- if (restore) "restoring" else "updating"
    cli::cli_alert_info(
      "Skipping {action} {source} packages: {.pkg {skipped_names}}"
    )
  }

  if (length(pkg_to_process) == 0L) {
    cli::cli_alert_info("No {source} packages to process.")
    return(invisible(FALSE))
  }

  if (act) {
    action <- if (restore) "Restoring" else "Installing latest"
    cli::cli_alert_info("{action} {source} packages.")
    .renv_restore_update_actual(
      pkg_to_process,
      restore,
      biocmanager_install,
      is_bioc = (source == "Bioconductor"),
      skip_if_dep_unavailable = skip_if_dep_unavailable,
      lockfile_deps = lockfile_deps
    )
  } else {
    action <- if (restore) "restoring" else "installing"
    cli::cli_alert_info("Skipping {action} {source} packages.")
  }
}

# Internal function to restore or update packages
.renv_restore_update_actual <- function(pkg, restore, biocmanager_install,
                                         is_bioc,
                                         skip_if_dep_unavailable = TRUE,
                                         lockfile_deps = list()) {
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
    .renv_restore_remaining(
      pkg_names,
      skip_if_dep_unavailable = skip_if_dep_unavailable,
      lockfile_deps = lockfile_deps
    )
  } else {
    cli::cli_alert_info(
      "Installing latest {pkg_type} packages: {.pkg {pkg_names}}"
    )
    # Install the latest versions
    .renv_install(pkg, biocmanager_install, is_bioc)
  }

  cli::cli_alert_info("Checking for packages that are still not installed.")
  # Install any remaining packages that were not installed
  .renv_install_remaining(
    pkg,
    biocmanager_install,
    is_bioc,
    skip_if_dep_unavailable = skip_if_dep_unavailable,
    lockfile_deps = lockfile_deps
  )
  invisible(TRUE)
}

# Internal function to restore remaining packages individually
.renv_restore_remaining <- function(pkg,
                                     skip_if_dep_unavailable = TRUE,
                                     lockfile_deps = list()) {
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

  failed_pkgs <- character(0)
  installed_now <- rownames(installed.packages())

  for (x in pkg_remaining) {
    if (!requireNamespace(x, quietly = TRUE)) {
      if (skip_if_dep_unavailable && length(failed_pkgs) > 0L) {
        x_deps <- lockfile_deps[[x]]
        if (!is.null(x_deps) && length(x_deps) > 0L) {
          blocking <- failed_pkgs[
            failed_pkgs %in% x_deps & !failed_pkgs %in% installed_now
          ]
          if (length(blocking) > 0L) {
            cli::cli_alert_warning(
              paste0(
                "Skipping {.pkg {x}}: dep ",
                "{.pkg {blocking}} failed and is not installed."
              )
            )
            failed_pkgs <- c(failed_pkgs, x)
            next
          }
        }
      }
      tryCatch(
        renv::restore(packages = x, transactional = FALSE),
        error = function(e) {
          cli::cli_alert_danger(
            "Failed to restore package: {.pkg {x}}. Error: {e$message}"
          )
        }
      )
      if (!requireNamespace(x, quietly = TRUE)) {
        failed_pkgs <- c(failed_pkgs, x)
      } else {
        installed_now <- rownames(installed.packages())
      }
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
.renv_install_remaining <- function(pkg, biocmanager_install, is_bioc,
                                     skip_if_dep_unavailable = TRUE,
                                     lockfile_deps = list()) {
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

  failed_pkgs <- character(0)
  installed_now <- rownames(installed.packages())

  # Try installing missing packages individually
  for (x in pkg_still_missing) {
    pkg_name <- sub("^.*/", "", x)
    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      if (skip_if_dep_unavailable && length(failed_pkgs) > 0L) {
        x_deps <- lockfile_deps[[pkg_name]]
        if (!is.null(x_deps) && length(x_deps) > 0L) {
          blocking <- failed_pkgs[
            failed_pkgs %in% x_deps & !failed_pkgs %in% installed_now
          ]
          if (length(blocking) > 0L) {
            cli::cli_alert_warning(
              paste0(
                "Skipping {.pkg {pkg_name}}: dep ",
                "{.pkg {blocking}} failed and is not installed."
              )
            )
            failed_pkgs <- c(failed_pkgs, pkg_name)
            next
          }
        }
      }
      .renv_install(x, biocmanager_install, is_bioc)
      if (!requireNamespace(pkg_name, quietly = TRUE)) {
        failed_pkgs <- c(failed_pkgs, pkg_name)
      } else {
        installed_now <- rownames(installed.packages())
      }
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
