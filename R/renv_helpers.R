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

# Extract package name from a remote spec (e.g. "user/repo" -> "repo")
.extract_pkg_name <- function(x) {
  sub("^.*/", "", x)
}

# Internal helper to check which packages are missing
.get_missing_pkgs <- function(pkgs) {
  if (length(pkgs) == 0L) return(character(0))
  pkg_names <- sub("^.*/", "", pkgs)
  pkgs[!vapply(pkg_names, requireNamespace, logical(1), quietly = TRUE)]
}

# Extract package names from a DESCRIPTION-style dependency field value.
# Handles entries like "curl (>= 5.1.0)" or plain "mime", returning
# only the package name without version constraints.
.parse_dep_field <- function(field_val) {
  if (is.null(field_val) || length(field_val) == 0) return(character(0))
  pkgs <- trimws(gsub("\\s*\\(.*?\\)", "", as.character(field_val)))
  pkgs[nzchar(pkgs)]
}

# Extract dependencies from a single package's Requirements field.
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

# Strategy 1 (renv 0.15.0 - 1.0.x): use the pre-computed Requirements field.
# Returns NULL if no package has a Requirements field (wrong lockfile format).
.deps_from_requirements <- function(lockfile_list_pkg) {
  has_requirements <- any(
    vapply(lockfile_list_pkg, function(x) !is.null(x$Requirements), logical(1))
  )
  if (!has_requirements) return(NULL)
  lapply(lockfile_list_pkg, .deps_from_requirements_pkg)
}

# Strategy 2 (renv 1.1.0+): parse Imports / Depends / LinkingTo fields.
# Returns NULL if no package has any of these fields (wrong lockfile format).
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
    if (!is.null(pkg_info[[field]])) {
      field_deps <- unlist(strsplit(pkg_info[[field]], ",\\s*"))
      parsed <- unlist(lapply(field_deps, .parse_dep_field))
      deps <- c(deps, parsed)
    }
  }

  unique(deps[deps != "R"])
}

# Internal function to read packages from the renv lockfile
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

# Internal function to get package dependencies from the renv lockfile.
# Returns a named list mapping each package name to its dependency names.
# Returns an empty list and emits a warning if dependencies cannot be
# extracted (so callers can proceed without skipping).
#
# Two strategies are tried in order:
#   1. Requirements field (renv 0.15.0 - 1.0.x lockfile format)
#   2. Imports/Depends/LinkingTo fields (renv 1.1.0+ lockfile format)
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

# Internal function to get package lists from the renv lockfile
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

# Internal function to manage the restoration or updating process
.renv_restore_or_update_impl <- function(package_list,
                                               github,
                                               non_github,
                                               restore,
                                               biocmanager_install,
                                               skip = character(0),
                                               skip_if_dep_unavailable = TRUE,
                                               lockfile_list_pkg = NULL) {
  lockfile_deps <- .renv_lockfile_deps_get(lockfile_list_pkg)

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
  pkg_names <- sub("^.*/", "", pkg)
  pkg_to_process <- pkg[!pkg_names %in% skip]
  pkg_skipped <- pkg[pkg_names %in% skip]

  # Report skipped packages
  if (length(pkg_skipped) > 0L) {
    skipped_names <- sub("^.*/", "", pkg_skipped)
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