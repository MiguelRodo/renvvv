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
  lockfile_path <- .renv_paths_lockfile()
  if (!file.exists(lockfile_path)) {
    return(invisible(NULL))
  }

  current_project <- renv::project(default = NULL)
  project_dir <- getwd()

  if (!is.null(current_project)) {
    current_project <- normalizePath(current_project, mustWork = FALSE)
  }
  project_dir <- normalizePath(project_dir, mustWork = FALSE)

  if (!is.null(current_project) && current_project == project_dir) {
    return(invisible(NULL))
  }

  .ensure_cli()

  if (interactive()) {
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
    cli::cli_alert_info(
      "Activating renv project at {.path {project_dir}}..."
    )
    renv::activate(project = project_dir)
    cli::cli_alert_success("renv project activated.")
  }

  invisible(NULL)
}

.renv_paths_lockfile <- function(project = NULL) {
  override <- Sys.getenv("RENV_PATHS_LOCKFILE", unset = NA)
  if (!is.na(override)) {
    last <- substr(override, nchar(override), nchar(override))
    if (last %in% c("/", "\\")) {
      override <- paste0(override, "renv.lock")
    }
    return(override)
  }

  if (is.null(project)) {
    project <- getwd()
  }

  profile <- Sys.getenv("RENV_PROFILE", unset = NA)
  if (!is.na(profile) && profile != "") {
    lockfile_path <- file.path(
      project, "renv", "profiles", profile, "renv.lock"
    )
  } else {
    lockfile_path <- file.path(project, "renv.lock")
  }

  return(lockfile_path)
}

# Robust package name extraction
.extract_pkg_name <- function(x) {
  if (!is.character(x) || length(x) == 0 || is.na(x)) return("")
  # Remove trailing slashes and branches
  x <- sub("/+$", "", x)
  x <- sub("[@#].*$", "", x)
  pkg <- sub("^.*/", "", x)
  if (!nzchar(pkg)) return(x)
  pkg
}

# Bulletproof check for missing packages
.get_missing_pkgs <- function(pkgs) {
  if (length(pkgs) == 0L) return(character(0))
  pkg_names <- vapply(pkgs, .extract_pkg_name, character(1))
  
  is_missing <- vapply(pkg_names, function(p) {
    if (!is.character(p) || length(p) == 0 || is.na(p) || p == "") return(TRUE)
    !requireNamespace(p, quietly = TRUE)
  }, logical(1))
  
  pkgs[is_missing]
}

.parse_dep_field <- function(field_val) {
  if (is.null(field_val) || length(field_val) == 0) return(character(0))
  pkgs <- trimws(gsub("\\s*\\(.*?\\)", "", as.character(field_val)))
  pkgs[nzchar(pkgs)]
}

.deps_from_requirements_pkg <- function(pkg_info) {
  reqs <- pkg_info$Requirements
  if (is.null(reqs)) return(character(0))

  if (is.character(reqs)) {
    return(as.character(reqs))
  } else if (is.list(reqs)) {
    return(names(reqs))
  }
  character(0)
}

.deps_from_requirements <- function(lockfile_list_pkg) {
  has_requirements <- any(
    vapply(lockfile_list_pkg, function(x) !is.null(x$Requirements), logical(1))
  )
  if (!has_requirements) return(NULL)
  lapply(lockfile_list_pkg, .deps_from_requirements_pkg)
}

.deps_from_description_fields <- function(lockfile_list_pkg) {
  dep_fields <- c("Depends", "Imports", "LinkingTo")
  has_fields <- any(vapply(lockfile_list_pkg, function(x) {
    any(dep_fields %in% names(x))
  }, logical(1)))
  if (!has_fields) return(NULL)
  lapply(lockfile_list_pkg, .extract_pkg_deps)
}

.extract_pkg_deps <- function(pkg_info) {
  deps <- character(0)
  fields <- c("Depends", "Imports", "LinkingTo")

  for (field in fields) {
    val <- pkg_info[[field]]
    if (!is.null(val) && length(val) > 0) {
      if (is.list(val)) {
        val <- if (!is.null(names(val))) names(val) else unlist(val)
      }
      if (length(val) > 0) {
        field_deps <- unlist(strsplit(as.character(val), ",\\s*"))
        parsed <- unlist(lapply(field_deps, .parse_dep_field))
        deps <- c(deps, parsed)
      }
    }
  }

  unique(deps[deps != "R"])
}

.renv_lockfile_read_pkgs <- function() {
  tryCatch({
    lockfile_path <- renv::paths$lockfile()
    if (!file.exists(lockfile_path)) {
      return(list())
    }
    lockfile_list_pkg <- renv::lockfile_read(file = lockfile_path)$Packages
    if (is.null(lockfile_list_pkg)) {
      return(list())
    }
    lockfile_list_pkg
  }, error = function(e) {
    list()
  })
}

.renv_lockfile_deps_get <- function(lockfile_list_pkg = NULL) {
  if (is.null(lockfile_list_pkg)) {
    lockfile_list_pkg <- tryCatch({
      .renv_lockfile_read_pkgs()
    }, error = function(e) {
      cli::cli_alert_warning(
        paste0("Could not read lockfile Packages ",
               "(skip_if_dep_unavailable ignored): {e$message}")
      )
      NULL
    })
  }

  if (is.null(lockfile_list_pkg) || length(lockfile_list_pkg) == 0) {
    return(list())
  }

  deps <- .deps_from_requirements(lockfile_list_pkg)
  if (!is.null(deps)) return(deps)

  deps <- .deps_from_description_fields(lockfile_list_pkg)
  if (!is.null(deps)) return(deps)

  cli::cli_alert_warning(
    paste0("Could not extract package dependencies from lockfile; ",
           "skip_if_dep_unavailable will be ignored.")
  )
  list()
}

.renv_lockfile_pkg_get <- function(lockfile_list_pkg = NULL) {
  if (is.null(lockfile_list_pkg)) {
    lockfile_list_pkg <- .renv_lockfile_read_pkgs()
  }

  if (length(lockfile_list_pkg) == 0L) {
    return(list(
      regular = character(),
      bioc = character(),
      gh = character()
    ))
  }

  pkg_names <- names(lockfile_list_pkg)

  remote_usernames <- vapply(
    lockfile_list_pkg,
    function(x) if (is.null(x$RemoteUsername)) "" else x$RemoteUsername,
    character(1),
    USE.NAMES = FALSE
  )

  sources <- vapply(
    lockfile_list_pkg,
    function(x) if (is.null(x$Source)) "" else tolower(x$Source),
    character(1),
    USE.NAMES = FALSE
  )

  is_gh <- remote_usernames != ""
  is_bioc <- !is_gh & grepl("bioc", sources)
  is_regular <- !is_gh & !is_bioc

  list(
    regular = pkg_names[is_regular],
    bioc = pkg_names[is_bioc],
    gh = paste0(remote_usernames[is_gh], "/", pkg_names[is_gh])
  )
}

.renv_restore_or_update_impl <- function(package_list,
                                               github,
                                               non_github,
                                               restore,
                                               biocmanager_install,
                                               skip = character(0),
                                               skip_if_dep_unavailable = TRUE,
                                               lockfile_list_pkg = NULL) {
  lockfile_deps <- .renv_lockfile_deps_get(lockfile_list_pkg)

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

.renv_restore_or_update_actual_wrapper <- function(pkg,
                                                   act,
                                                   restore,
                                                   source,
                                                   biocmanager_install,
                                                   skip = character(0),
                                                   skip_if_dep_unavailable = TRUE,
                                                   lockfile_deps = list()) {
  pkg_names <- vapply(pkg, .extract_pkg_name, character(1))
  
  idx_keep <- !pkg_names %in% skip
  pkg_to_process <- pkg[idx_keep]
  pkg_skipped <- pkg[!idx_keep]

  if (length(pkg_skipped) > 0L) {
    skipped_names <- pkg_names[!idx_keep]
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

  pkg_names <- vapply(pkg, .extract_pkg_name, character(1))

  # CRITICAL FIX: Filter out malformed empty names right away
  valid_idx <- nzchar(pkg_names) & !is.na(pkg_names)
  if (!all(valid_idx)) {
    invalid_pkgs_clean <- gsub("[{}]", "", pkg[!valid_idx])
    cli::cli_alert_warning("Skipping malformed package entries: {.pkg {invalid_pkgs_clean}}")
    pkg <- pkg[valid_idx]
    pkg_names <- pkg_names[valid_idx]
    if (length(pkg) == 0L) return(invisible(FALSE))
  }

  if (restore) {
    cli::cli_alert_info(
      "Attempting to restore {pkg_type} packages: {.pkg {pkg_names}}"
    )
    tryCatch(
      renv::restore(packages = pkg_names, transactional = FALSE),
      error = function(e) {
        cli::cli_alert_danger(
          "Failed to restore {pkg_type} packages: {.pkg {pkg_names}}."
        )
        message("Error: ", e$message)
      }
    )
    cli::cli_alert_info("Checking for packages that failed to restore.")
    
    # CRITICAL FIX: We must pass 'pkg' (the remotes) here, not 'pkg_names'
    .renv_restore_remaining(
      pkg,
      skip_if_dep_unavailable = skip_if_dep_unavailable,
      lockfile_deps = lockfile_deps
    )
  } else {
    cli::cli_alert_info(
      "Installing latest {pkg_type} packages: {.pkg {pkg_names}}"
    )
    .renv_install(pkg, biocmanager_install, is_bioc)
  }

  cli::cli_alert_info("Checking for packages that are still not installed.")
  .renv_install_remaining(
    pkg,
    biocmanager_install,
    is_bioc,
    skip_if_dep_unavailable = skip_if_dep_unavailable,
    lockfile_deps = lockfile_deps
  )
  invisible(TRUE)
}

.renv_restore_remaining <- function(pkg,
                                     skip_if_dep_unavailable = TRUE,
                                     lockfile_deps = list()) {
  .ensure_cli()

  pkg_names <- vapply(pkg, .extract_pkg_name, character(1))
  missing_pkgs <- .get_missing_pkgs(pkg)
  
  idx_missing <- pkg %in% missing_pkgs
  pkg_remaining <- pkg[idx_missing]
  pkg_names_remaining <- pkg_names[idx_missing]

  if (length(pkg_remaining) == 0L) {
    cli::cli_alert_success("All packages restored successfully.")
    return(invisible(FALSE))
  }

  cli::cli_alert_warning(
    "Packages that failed to restore: {.pkg {pkg_names_remaining}}"
  )
  cli::cli_alert_info("Attempting to restore packages individually.")

  failed_pkgs <- character(0)
  installed_now <- rownames(utils::installed.packages())

  for (i in seq_along(pkg_remaining)) {
    x <- pkg_remaining[i]
    pname <- pkg_names_remaining[i]
    
    if (!is.character(pname) || is.na(pname) || pname == "") {
      failed_pkgs <- c(failed_pkgs, pname)
      next
    }

    # CRITICAL FIX: Check requireNamespace using pname, but restore using x
    if (!requireNamespace(pname, quietly = TRUE)) {
      if (.is_blocked_by_failed_deps(
        pkg_name = pname,
        failed_pkgs = failed_pkgs,
        installed_now = installed_now,
        skip_if_dep_unavailable = skip_if_dep_unavailable,
        lockfile_deps = lockfile_deps
      )) {
        failed_pkgs <- c(failed_pkgs, pname)
        next
      }
      tryCatch(
        renv::restore(packages = x, transactional = FALSE),
        error = function(e) {
          safe_x <- gsub("[{}]", "", x)
          cli::cli_alert_danger("Failed to restore package: {.pkg {safe_x}}.")
          message("Error: ", e$message)
        }
      )
      if (!requireNamespace(pname, quietly = TRUE)) {
        failed_pkgs <- c(failed_pkgs, pname)
      } else {
        installed_now <- c(installed_now, pname)
      }
    }
  }
}

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
            cli::cli_alert_danger("Failed to install Bioconductor packages using BiocManager: {.pkg {pkg}}.")
            message("Error: ", e$message)
          }
        )
      } else {
        cli::cli_alert_info(
          "Installing Bioconductor packages using BiocManager: {.pkg {pkg}}"
        )
        tryCatch(
          BiocManager::install(pkg, update = TRUE, ask = FALSE),
          error = function(e) {
            cli::cli_alert_danger("Failed to install Bioconductor packages using BiocManager: {.pkg {pkg}}.")
            message("Error: ", e$message)
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
          cli::cli_alert_danger("Failed to install Bioconductor packages via renv: {.pkg {pkg}}.")
          message("Error: ", e$message)
        }
      )
    }
  } else {
    cli::cli_alert_info("Installing packages: {.pkg {pkg}}")
    tryCatch(
      renv::install(pkg, prompt = FALSE),
      error = function(e) {
        safe_pkg <- gsub("[{}]", "", pkg)
        cli::cli_alert_danger("Failed to install packages: {.pkg {safe_pkg}}.")
        message("Error: ", e$message)
      }
    )
  }
}

.is_blocked_by_failed_deps <- function(pkg_name,
                                        failed_pkgs,
                                        installed_now,
                                        skip_if_dep_unavailable,
                                        lockfile_deps) {
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
        return(TRUE)
      }
    }
  }
  FALSE
}

.renv_install_remaining <- function(pkg, biocmanager_install, is_bioc,
                                     skip_if_dep_unavailable = TRUE,
                                     lockfile_deps = list()) {
  .ensure_cli()

  pkg_names <- vapply(pkg, .extract_pkg_name, character(1))
  installed_pkgs <- rownames(utils::installed.packages())
  
  idx_missing <- !pkg_names %in% installed_pkgs
  pkg_remaining <- pkg[idx_missing]
  pkg_names_remaining <- pkg_names[idx_missing]

  if (length(pkg_remaining) == 0L) {
    cli::cli_alert_success("All packages are installed.")
    return(invisible(FALSE))
  }

  cli::cli_alert_warning(
    "Packages that are still missing: {.pkg {pkg_names_remaining}}"
  )
  cli::cli_alert_info("Attempting to install remaining packages.")

  .renv_install(pkg_remaining, biocmanager_install, is_bioc)

  pkg_still_missing <- .get_missing_pkgs(pkg_remaining)
  
  idx_still_missing <- pkg_remaining %in% pkg_still_missing
  pkg_still_missing_final <- pkg_remaining[idx_still_missing]
  pkg_names_still_missing <- pkg_names_remaining[idx_still_missing]

  if (length(pkg_still_missing_final) == 0L) {
    cli::cli_alert_success("All remaining packages installed successfully.")
    return(invisible(TRUE))
  }

  cli::cli_alert_warning(
    "Packages that failed to install: {.pkg {pkg_names_still_missing}}"
  )
  cli::cli_alert_info("Attempting to install missing packages individually.")

  failed_pkgs <- character(0)
  installed_now <- rownames(utils::installed.packages())

  for (i in seq_along(pkg_still_missing_final)) {
    x <- pkg_still_missing_final[i]
    pname <- pkg_names_still_missing[i]
    
    if (!is.character(pname) || is.na(pname) || pname == "") {
      failed_pkgs <- c(failed_pkgs, pname)
      next
    }

    if (!requireNamespace(pname, quietly = TRUE)) {
      if (.is_blocked_by_failed_deps(
        pkg_name = pname,
        failed_pkgs = failed_pkgs,
        installed_now = installed_now,
        skip_if_dep_unavailable = skip_if_dep_unavailable,
        lockfile_deps = lockfile_deps
      )) {
        failed_pkgs <- c(failed_pkgs, pname)
        next
      }
      .renv_install(x, biocmanager_install, is_bioc)
      if (!requireNamespace(pname, quietly = TRUE)) {
        failed_pkgs <- c(failed_pkgs, pname)
      } else {
        installed_now <- c(installed_now, pname)
      }
    }
  }

  pkg_final_missing <- .get_missing_pkgs(pkg_still_missing_final)

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
