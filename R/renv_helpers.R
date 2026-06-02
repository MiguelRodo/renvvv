.renv_restore_or_update_impl <- function(package_list,
                                         github,
                                         non_github,
                                         restore,
                                         biocmanager_install,
                                         skip = character(0),
                                         skip_if_dep_unavailable = TRUE,
                                         lockfile_list_pkg = NULL,
                                         prompt = FALSE,
                                         transactional = FALSE,
                                         args_restore = list(),
                                         args_install = list(),
                                         args_update = list()) {
  lockfile_deps <- .renv_lockfile_deps_get(lockfile_list_pkg)

  .renv_restore_or_update_actual_wrapper(
    pkg = package_list[["regular"]],
    act = non_github,
    restore = restore,
    source = "CRAN",
    biocmanager_install = biocmanager_install,
    skip = skip,
    skip_if_dep_unavailable = skip_if_dep_unavailable,
    lockfile_deps = lockfile_deps,
    prompt = prompt,
    transactional = transactional,
    args_restore = args_restore,
    args_install = args_install,
    args_update = args_update
  )

  .renv_restore_or_update_actual_wrapper(
    pkg = package_list[["bioc"]],
    act = non_github,
    restore = restore,
    source = "Bioconductor",
    biocmanager_install = biocmanager_install,
    skip = skip,
    skip_if_dep_unavailable = skip_if_dep_unavailable,
    lockfile_deps = lockfile_deps,
    prompt = prompt,
    transactional = transactional,
    args_restore = args_restore,
    args_install = args_install,
    args_update = args_update
  )

  .renv_restore_or_update_actual_wrapper(
    pkg = package_list[["gh"]],
    act = github,
    restore = restore,
    source = "GitHub",
    biocmanager_install = biocmanager_install,
    skip = skip,
    skip_if_dep_unavailable = skip_if_dep_unavailable,
    lockfile_deps = lockfile_deps,
    prompt = prompt,
    transactional = transactional,
    args_restore = args_restore,
    args_install = args_install,
    args_update = args_update
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
                                                   lockfile_deps = list(),
                                                   prompt = FALSE,
                                                   transactional = FALSE,
                                                   args_restore = list(),
                                                   args_install = list(),
                                                   args_update = list()) {
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
      lockfile_deps = lockfile_deps,
      prompt = prompt,
      transactional = transactional,
      args_restore = args_restore,
      args_install = args_install,
      args_update = args_update
    )
  } else {
    action <- if (restore) "restoring" else "installing"
    cli::cli_alert_info("Skipping {action} {source} packages.")
  }
}

.renv_restore_update_actual <- function(pkg, restore, biocmanager_install,
                                        is_bioc,
                                        skip_if_dep_unavailable = TRUE,
                                        lockfile_deps = list(),
                                        prompt = FALSE,
                                        transactional = FALSE,
                                        args_restore = list(),
                                        args_install = list(),
                                        args_update = list()) {
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
    
    # Safe combination of defaults + user arguments (defaults take precedence)
    call_args <- c(list(packages = pkg_names, prompt = prompt, transactional = transactional), args_restore)
    call_args <- call_args[!duplicated(names(call_args))]

    tryCatch(
      do.call(renv::restore, call_args),
      error = function(e) {
        cli::cli_alert_danger(
          "Failed to restore {pkg_type} packages: {.pkg {pkg_names}}."
        )
        message("Error: ", e$message)
      }
    )
    cli::cli_alert_info("Checking for packages that failed to restore.")
    
    .renv_restore_remaining(
      pkg,
      skip_if_dep_unavailable = skip_if_dep_unavailable,
      lockfile_deps = lockfile_deps,
      prompt = prompt,
      transactional = transactional,
      args_restore = args_restore
    )
  } else {
    cli::cli_alert_info(
      "Installing latest {pkg_type} packages: {.pkg {pkg_names}}"
    )
    .renv_install(pkg, biocmanager_install, is_bioc, prompt, args_install, args_update)
  }

  cli::cli_alert_info("Checking for packages that are still not installed.")
  .renv_install_remaining(
    pkg,
    biocmanager_install,
    is_bioc,
    skip_if_dep_unavailable = skip_if_dep_unavailable,
    lockfile_deps = lockfile_deps,
    prompt = prompt,
    args_install = args_install,
    args_update = args_update
  )
  invisible(TRUE)
}

.renv_restore_remaining <- function(pkg,
                                    skip_if_dep_unavailable = TRUE,
                                    lockfile_deps = list(),
                                    prompt = FALSE,
                                    transactional = FALSE,
                                    args_restore = list()) {
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

      call_args <- c(list(packages = x, prompt = prompt, transactional = transactional), args_restore)
      call_args <- call_args[!duplicated(names(call_args))]

      tryCatch(
        do.call(renv::restore, call_args),
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

.renv_install <- function(pkg, biocmanager_install, is_bioc, prompt = FALSE, args_install = list(), args_update = list()) {
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
        
        call_args <- c(list(packages = paste0("bioc::", pkg), prompt = prompt), args_update, args_install)
        call_args <- call_args[!duplicated(names(call_args))]

        tryCatch(
          do.call(renv::install, call_args),
          error = function(e) {
            cli::cli_alert_danger("Failed to install Bioconductor packages using BiocManager fallback: {.pkg {pkg}}.")
            message("Error: ", e$message)
          }
        )
      } else {
        cli::cli_alert_info(
          "Installing Bioconductor packages using BiocManager: {.pkg {pkg}}"
        )
        tryCatch(
          BiocManager::install(pkg, update = TRUE, ask = prompt),
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
      
      call_args <- c(list(packages = paste0("bioc::", pkg), prompt = prompt), args_update, args_install)
      call_args <- call_args[!duplicated(names(call_args))]

      tryCatch(
        do.call(renv::install, call_args),
        error = function(e) {
          cli::cli_alert_danger("Failed to install Bioconductor packages via renv: {.pkg {pkg}}.")
          message("Error: ", e$message)
        }
      )
    }
  } else {
    cli::cli_alert_info("Installing packages: {.pkg {pkg}}")
    
    call_args <- c(list(packages = pkg, prompt = prompt), args_update, args_install)
    call_args <- call_args[!duplicated(names(call_args))]

    tryCatch(
      do.call(renv::install, call_args),
      error = function(e) {
        safe_pkg <- gsub("[{}]", "", pkg)
        cli::cli_alert_danger("Failed to install packages: {.pkg {safe_pkg}}.")
        message("Error: ", e$message)
      }
    )
  }
}

.renv_install_remaining <- function(pkg, biocmanager_install, is_bioc,
                                    skip_if_dep_unavailable = TRUE,
                                    lockfile_deps = list(),
                                    prompt = FALSE,
                                    args_install = list(),
                                    args_update = list()) {
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

  .renv_install(pkg_remaining, biocmanager_install, is_bioc, prompt, args_install, args_update)

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
      .renv_install(x, biocmanager_install, is_bioc, prompt, args_install, args_update)
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
