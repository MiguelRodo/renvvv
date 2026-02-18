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

# Helper function to check for user permission to write files
.check_write_permission <- function(force, action_description) {
  if (!force) {
    if (interactive()) {
      msg <- paste0(
        action_description,
        "\nDo you want to proceed? (y/n): "
      )
      response <- readline(prompt = msg)
      if (!tolower(trimws(response)) %in% c("y", "yes")) {
        stop("Operation cancelled by user.", call. = FALSE)
      }
    } else {
      stop(
        paste0(
          action_description,
          "\nThis function requires permission to write files. ",
          "Use force = TRUE to proceed non-interactively."
        ),
        call. = FALSE
      )
    }
  }
}

#' @title Add Packages to `_dependencies.R` for renv
#'
#' @description
#' Adds `library(<pkg>)` calls for each package listed to the
#' `_dependencies.R` file.
#' Installs packages if they are not already installed.
#'
#' If an renv project is detected (via `renv.lock` file) but not currently
#' active, the function will activate it. In interactive sessions, the user
#' will be prompted for confirmation before activation. In non-interactive
#' sessions, activation occurs automatically.
#'
#' @param pkg Character vector of package names.
#'   Packages to add to `_dependencies.R` and install if not already installed.
#'   Can use the "`<remote>/<repo>`" syntax for installing from remotes
#'   (e.g., GitHub), provided that `<repo>` is also the name of the package.
#' @param force Logical. If `FALSE` (default), prompts for confirmation in
#'   interactive sessions or errors in non-interactive sessions. Set to `TRUE`
#'   to proceed without prompting.
#'
#' @return Invisibly returns `TRUE` upon successful completion.
#'
#' @examples
#' \dontrun{
#' # Add and install CRAN packages (interactive mode will prompt)
#' renvvv_dep_add(c("dplyr", "ggplot2"))
#'
#' # Add and install a GitHub package
#' renvvv_dep_add("hadley/httr")
#'
#' # Non-interactive mode requires force = TRUE
#' renvvv_dep_add(c("dplyr", "ggplot2"), force = TRUE)
#' }
#'
#' @importFrom utils installed.packages
#' @export
renvvv_dep_add <- function(pkg, force = FALSE) {
  # Ensure the cli package is available
  .ensure_cli()

  # Check renv activation
  .check_renv_activation()

  # Check for permission to write files
  .check_write_permission(
    force,
    "This will create/modify _dependencies.R and may install packages."
  )

  # Extract package names from possible remotes
  pkg_names <- sapply(pkg, function(x) sub("^.*/", "", x))

  # Identify packages that are not installed
  pkg_to_install <- pkg[
    !sapply(pkg_names, requireNamespace, quietly = TRUE)
  ]

  # Install packages that are not installed
  if (length(pkg_to_install) > 0) {
    cli::cli_alert_info("Installing packages: {.pkg {pkg_to_install}}")
    tryCatch(
      renv::install(pkg_to_install),
      error = function(e) {
        cli::cli_alert_danger(
          "Failed to install packages: {.pkg {pkg_to_install}}. Error: {e$message}"
        )
      }
    )
  } else {
    cli::cli_alert_info("All specified packages are already installed.")
  }

  # Read or create _dependencies.R file
  if (file.exists("_dependencies.R")) {
    txt_dep <- readLines("_dependencies.R")
    cli::cli_alert_info("Reading existing '_dependencies.R' file.")
  } else {
    txt_dep <- character()
    cli::cli_alert_info("Creating new '_dependencies.R' file.")
  }

  # Add library calls for each package
  new_lib_calls <- character()
  for (x in pkg) {
    pkg_name <- sub("^.*/", "", x)
    txt_add <- paste0("library(", pkg_name, ")")
    if (!txt_add %in% txt_dep) {
      new_lib_calls <- c(new_lib_calls, txt_add)
    }
  }

  if (length(new_lib_calls) > 0) {
    txt_dep <- c(txt_dep, new_lib_calls)
    cli::cli_alert_success(
      "Adding library calls to '_dependencies.R' for packages: {.pkg {pkg_names}}"
    )
  } else {
    cli::cli_alert_info("No new library calls to add to '_dependencies.R'.")
  }

  # Write updated dependencies to _dependencies.R
  writeLines(txt_dep, "_dependencies.R")
  cli::cli_alert_success("'_dependencies.R' has been updated.")
  invisible(TRUE)
}
