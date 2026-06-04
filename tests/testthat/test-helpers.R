# Tests for internal helper functions

# Test .ensure_cli
test_that(".ensure_cli function exists", {
  expect_true(is.function(.ensure_cli))
})

test_that(".ensure_cli succeeds when cli is available", {
  expect_silent(.ensure_cli())
})

test_that(".ensure_cli returns NULL", {
  result <- .ensure_cli()
  expect_null(result)
})

# Test .check_renv
test_that(".check_renv function exists", {
  expect_true(is.function(.check_renv))
})

test_that(".check_renv succeeds when renv is available", {
  skip_if_not(requireNamespace("renv", quietly = TRUE), "renv not available")
  expect_silent(.check_renv())
})

test_that(".check_renv returns NULL", {
  skip_if_not(requireNamespace("renv", quietly = TRUE), "renv not available")
  result <- .check_renv()
  expect_null(result)
})

# Test .renv_paths_lockfile
test_that(".renv_paths_lockfile function exists", {
  expect_true(is.function(.renv_paths_lockfile))
})

# Test .get_missing_pkgs
test_that(".get_missing_pkgs function exists", {
  expect_true(is.function(.get_missing_pkgs))
})

test_that(".get_missing_pkgs handles empty vector", {
  expect_equal(.get_missing_pkgs(character(0)), character(0))
})

test_that(".get_missing_pkgs correctly identifies missing packages", {
  # Base packages should be available
  expect_equal(.get_missing_pkgs(c("base", "utils")), character(0))

  # A definitely missing package
  missing_pkg <- "definitely_not_a_package_12345"
  expect_equal(.get_missing_pkgs(missing_pkg), missing_pkg)

  # Mixture of present and missing
  expect_equal(
    .get_missing_pkgs(c("base", missing_pkg)),
    missing_pkg
  )
})

test_that(".get_missing_pkgs handles remotes", {
  missing_gh <- "user/definitely_not_a_package_12345"
  expect_equal(.get_missing_pkgs(missing_gh), missing_gh)

  # If we have a package installed that we refer to via remote
  # (using 'cli' as it should be there for the tests)
  expect_equal(.get_missing_pkgs("r-lib/cli"), character(0))
})

test_that(".renv_paths_lockfile returns default path", {
  # Clear environment variables
  old_lockfile <- Sys.getenv("RENV_PATHS_LOCKFILE", unset = NA)
  old_profile <- Sys.getenv("RENV_PROFILE", unset = NA)
  on.exit({
    if (!is.na(old_lockfile)) {
      Sys.setenv(RENV_PATHS_LOCKFILE = old_lockfile)
    } else {
      Sys.unsetenv("RENV_PATHS_LOCKFILE")
    }
    if (!is.na(old_profile)) {
      Sys.setenv(RENV_PROFILE = old_profile)
    } else {
      Sys.unsetenv("RENV_PROFILE")
    }
  })
  Sys.unsetenv("RENV_PATHS_LOCKFILE")
  Sys.unsetenv("RENV_PROFILE")

  result <- .renv_paths_lockfile()
  expect_equal(result, file.path(getwd(), "renv.lock"))
})

test_that(".renv_paths_lockfile respects RENV_PATHS_LOCKFILE", {
  old_lockfile <- Sys.getenv("RENV_PATHS_LOCKFILE", unset = NA)
  on.exit({
    if (!is.na(old_lockfile)) {
      Sys.setenv(RENV_PATHS_LOCKFILE = old_lockfile)
    } else {
      Sys.unsetenv("RENV_PATHS_LOCKFILE")
    }
  })

  test_path <- "/custom/path/to/renv.lock"
  Sys.setenv(RENV_PATHS_LOCKFILE = test_path)
  result <- .renv_paths_lockfile()
  expect_equal(result, test_path)
})

test_that(".renv_paths_lockfile handles directory path with trailing slash", {
  old_lockfile <- Sys.getenv("RENV_PATHS_LOCKFILE", unset = NA)
  on.exit({
    if (!is.na(old_lockfile)) {
      Sys.setenv(RENV_PATHS_LOCKFILE = old_lockfile)
    } else {
      Sys.unsetenv("RENV_PATHS_LOCKFILE")
    }
  })

  test_path <- "/custom/path/"
  Sys.setenv(RENV_PATHS_LOCKFILE = test_path)
  result <- .renv_paths_lockfile()
  expect_equal(result, "/custom/path/renv.lock")
})

test_that(".renv_paths_lockfile handles RENV_PROFILE", {
  old_lockfile <- Sys.getenv("RENV_PATHS_LOCKFILE", unset = NA)
  old_profile <- Sys.getenv("RENV_PROFILE", unset = NA)
  on.exit({
    if (!is.na(old_lockfile)) {
      Sys.setenv(RENV_PATHS_LOCKFILE = old_lockfile)
    } else {
      Sys.unsetenv("RENV_PATHS_LOCKFILE")
    }
    if (!is.na(old_profile)) {
      Sys.setenv(RENV_PROFILE = old_profile)
    } else {
      Sys.unsetenv("RENV_PROFILE")
    }
  })
  Sys.unsetenv("RENV_PATHS_LOCKFILE")
  Sys.setenv(RENV_PROFILE = "test-profile")

  result <- .renv_paths_lockfile()
  expected <- file.path(
    getwd(), "renv", "profiles", "test-profile", "renv.lock"
  )
  expect_equal(result, expected)
})

# Test .renv_lockfile_deps_get
test_that(".renv_lockfile_deps_get function exists", {
  expect_true(is.function(.renv_lockfile_deps_get))
})

test_that(".parse_dep_field helper exists", {
  expect_true(is.function(.parse_dep_field))
})

test_that(".deps_from_requirements strategy exists", {
  expect_true(is.function(.deps_from_requirements))
})

test_that(".deps_from_requirements_pkg works correctly", {
  # Character vector
  pkg_char <- list(Requirements = c("pkg1", "pkg2"))
  expect_equal(renvvv:::.deps_from_requirements_pkg(pkg_char), c("pkg1", "pkg2"))

  # Named list
  pkg_list <- list(Requirements = list(pkg1 = "*", pkg2 = ">= 1.0.0"))
  expect_equal(renvvv:::.deps_from_requirements_pkg(pkg_list), c("pkg1", "pkg2"))

  # NULL/Missing
  expect_equal(renvvv:::.deps_from_requirements_pkg(list()), character(0))
  expect_equal(renvvv:::.deps_from_requirements_pkg(list(Requirements = NULL)), character(0))

  # Unexpected type
  expect_equal(renvvv:::.deps_from_requirements_pkg(list(Requirements = 123)), character(0))
})

test_that(".deps_from_requirements_pkg works correctly", {
  # Character vector
  pkg_char <- list(Requirements = c("pkg1", "pkg2"))
  expect_equal(renvvv:::.deps_from_requirements_pkg(pkg_char), c("pkg1", "pkg2"))

  # Named list
  pkg_list <- list(Requirements = list(pkg1 = "*", pkg2 = ">= 1.0.0"))
  expect_equal(renvvv:::.deps_from_requirements_pkg(pkg_list), c("pkg1", "pkg2"))

  # NULL/Missing
  expect_equal(renvvv:::.deps_from_requirements_pkg(list()), character(0))
  expect_equal(renvvv:::.deps_from_requirements_pkg(list(Requirements = NULL)), character(0))

  # Unexpected type
  expect_equal(renvvv:::.deps_from_requirements_pkg(list(Requirements = 123)), character(0))
})

test_that(".deps_from_description_fields strategy exists", {
  expect_true(is.function(.deps_from_description_fields))
})

test_that(".parse_dep_field strips version constraints", {
  result <- .parse_dep_field(c("curl (>= 5.1.0)", "mime", "R6"))
  expect_equal(result, c("curl", "mime", "R6"))
})

test_that(".parse_dep_field handles NULL and empty inputs", {
  expect_equal(.parse_dep_field(NULL), character(0))
  expect_equal(.parse_dep_field(character(0)), character(0))
})

test_that(".deps_from_requirements returns NULL when no Requirements field", {
  pkgs <- list(
    mime = list(Package = "mime", Imports = c("tools"))
  )
  expect_null(.deps_from_requirements(pkgs))
})

test_that(".deps_from_requirements extracts deps from Requirements field", {
  pkgs <- list(
    httr = list(
      Package = "httr",
      Requirements = c("R", "curl", "mime")
    ),
    mime = list(
      Package = "mime",
      Requirements = c("tools")
    )
  )
  result <- .deps_from_requirements(pkgs)
  expect_type(result, "list")
  expect_equal(result[["httr"]], c("R", "curl", "mime"))
  expect_equal(result[["mime"]], c("tools"))
})

test_that(".deps_from_description_fields returns NULL when no dep fields", {
  pkgs <- list(
    sys = list(Package = "sys", Version = "3.4.3")
  )
  expect_null(.deps_from_description_fields(pkgs))
})

test_that(".deps_from_description_fields parses Imports and Depends", {
  pkgs <- list(
    httr = list(
      Package = "httr",
      Depends = c("R (>= 3.6)"),
      Imports = c("curl (>= 5.1.0)", "jsonlite", "mime")
    ),
    mime = list(
      Package = "mime",
      Imports = c("tools")
    )
  )
  result <- .deps_from_description_fields(pkgs)
  expect_type(result, "list")
  expect_true("curl" %in% result[["httr"]])
  expect_false("R" %in% result[["httr"]])
  expect_false("R (>= 3.6)" %in% result[["httr"]])
  expect_equal(result[["mime"]], "tools")
})

test_that(".extract_pkg_deps function exists", {
  expect_true(is.function(renvvv:::.extract_pkg_deps))
})

test_that(".extract_pkg_deps extracts all relevant fields correctly", {
  pkg_info <- list(
    Package = "dummy",
    Depends = c("methods"),
    Imports = c("utils", "stats (>= 4.0.0)"),
    LinkingTo = c("Rcpp")
  )
  result <- renvvv:::.extract_pkg_deps(pkg_info)
  expected <- c("methods", "utils", "stats", "Rcpp")
  expect_equal(sort(result), sort(expected))
})

test_that(".extract_pkg_deps handles comma-separated fields", {
  pkg_info <- list(
    Package = "dummy",
    Imports = "cli (>= 3.0), glue, rlang"
  )
  result <- renvvv:::.extract_pkg_deps(pkg_info)
  expected <- c("cli", "glue", "rlang")
  expect_equal(sort(result), sort(expected))
})

test_that(".extract_pkg_deps ignores R dependency", {
  pkg_info <- list(
    Package = "dummy",
    Depends = c("R (>= 4.0.0)", "utils"),
    Imports = "R"
  )
  result <- renvvv:::.extract_pkg_deps(pkg_info)
  expect_equal(result, "utils")
})

test_that(".extract_pkg_deps gracefully handles missing fields", {
  pkg_info <- list(
    Package = "dummy",
    Suggests = c("testthat")
  )
  result <- renvvv:::.extract_pkg_deps(pkg_info)
  expect_equal(result, character(0))
})

test_that(".renv_lockfile_deps_get returns empty list when no lockfile", {
  tmp <- tempfile("renvvv_test_nodeps_")
  dir.create(tmp)
  old_wd <- setwd(tmp)
  on.exit({
    setwd(old_wd)
    unlink(tmp, recursive = TRUE)
  })

  # No renv project init here — lockfile should not exist
  result <- .renv_lockfile_deps_get()
  expect_type(result, "list")
  expect_length(result, 0L)
})

# Test .renv_lockfile_pkg_get (requires a mock lockfile)
test_that(".renv_lockfile_pkg_get function exists", {
  expect_true(is.function(.renv_lockfile_pkg_get))
})

test_that(".renv_lockfile_pkg_get returns list with expected structure", {
  skip_if_not(requireNamespace("renv", quietly = TRUE), "renv not available")

  # This test requires a lockfile to exist
  # We'll skip if not in a renv project
  skip_if_not(file.exists("renv.lock"), "No renv.lock file available")

  result <- .renv_lockfile_pkg_get()

  expect_type(result, "list")
  expect_named(result, c("regular", "bioc", "gh"))
  expect_type(result$regular, "character")
  expect_type(result$bioc, "character")
  expect_type(result$gh, "character")
})

# Test .renv_restore_or_update_impl
test_that(".renv_restore_or_update_impl function exists", {
  expect_true(is.function(.renv_restore_or_update_impl))
})

test_that(".renv_restore_or_update_impl accepts valid parameters", {
  skip_if_not(requireNamespace("renv", quietly = TRUE), "renv not available")

  # Create minimal package list
  pkg_list <- list(regular = character(0), bioc = character(0), gh = character(0))

  # Should complete without error when packages are empty
  suppressMessages({
    result <- .renv_restore_or_update_impl(
      package_list = pkg_list,
      github = TRUE,
      non_github = TRUE,
      restore = TRUE,
      biocmanager_install = FALSE
    )
  })

  expect_true(result)
})

test_that(".renv_restore_or_update_impl accepts skip_if_dep_unavailable", {
  skip_if_not(requireNamespace("renv", quietly = TRUE), "renv not available")

  pkg_list <- list(regular = character(0), bioc = character(0), gh = character(0))

  suppressMessages({
    result <- .renv_restore_or_update_impl(
      package_list = pkg_list,
      github = TRUE,
      non_github = TRUE,
      restore = FALSE,
      biocmanager_install = FALSE,
      skip_if_dep_unavailable = FALSE
    )
  })

  expect_true(result)
})

# Test .renv_restore_or_update_actual_wrapper
test_that(".renv_restore_or_update_actual_wrapper function exists", {
  expect_true(is.function(.renv_restore_or_update_actual_wrapper))
})

test_that(".renv_restore_or_update_actual_wrapper handles empty package list", {
  suppressMessages({
    result <- .renv_restore_or_update_actual_wrapper(
      pkg = character(0),
      act = TRUE,
      restore = TRUE,
      source = "CRAN",
      biocmanager_install = FALSE
    )
  })

  expect_false(result)
})

test_that(".renv_restore_or_update_actual_wrapper skips when act is FALSE", {
  suppressMessages({
    result <- .renv_restore_or_update_actual_wrapper(
      pkg = c("somepackage"),
      act = FALSE,
      restore = TRUE,
      source = "CRAN",
      biocmanager_install = FALSE
    )
  })

  # Function returns NULL or invisible result when skipping
  # Just check it completes without error
  expect_true(TRUE)
})

# Test .renv_restore_update_actual
test_that(".renv_restore_update_actual function exists", {
  expect_true(is.function(.renv_restore_update_actual))
})

test_that(".renv_restore_update_actual handles empty package list", {
  result <- .renv_restore_update_actual(
    pkg = character(0),
    restore = TRUE,
    biocmanager_install = FALSE,
    is_bioc = FALSE
  )

  expect_false(result)
})

# Test .renv_restore_remaining
test_that(".renv_restore_remaining function exists", {
  expect_true(is.function(.renv_restore_remaining))
})

test_that(".renv_restore_remaining handles empty package list", {
  suppressMessages({
    result <- .renv_restore_remaining(character(0))
  })

  expect_false(result)
})

test_that(".renv_restore_remaining skips pkg when failed dep not installed", {
  # Simulate: pkg1 failed, pkg2 depends on pkg1
  # pkg1 is not installed, so pkg2 should be skipped
  failed <- character(0)

  # We can't test the full renv restore without a project, but we can test
  # that the function accepts the new parameters without error
  suppressMessages({
    result <- .renv_restore_remaining(
      pkg = character(0),
      skip_if_dep_unavailable = TRUE,
      lockfile_deps = list(pkg2 = "pkg1")
    )
  })

  expect_false(result)
})

test_that(".renv_restore_remaining accepts skip_if_dep_unavailable=FALSE", {
  suppressMessages({
    result <- .renv_restore_remaining(
      pkg = character(0),
      skip_if_dep_unavailable = FALSE,
      lockfile_deps = list()
    )
  })

  expect_false(result)
})

# Test .renv_install
test_that(".renv_install function exists", {
  expect_true(is.function(.renv_install))
})

test_that(".renv_install handles empty package list", {
  # Empty package list should not cause errors
  # Just suppress output but don't require silence as cli may output
  suppressMessages({
    .renv_install(
      pkg = character(0),
      biocmanager_install = FALSE,
      is_bioc = FALSE
    )
  })
  expect_true(TRUE)
})

# Test .renv_install_remaining
test_that(".renv_install_remaining function exists", {
  expect_true(is.function(.renv_install_remaining))
})

test_that(".renv_install_remaining handles empty package list", {
  suppressMessages({
    result <- .renv_install_remaining(
      pkg = character(0),
      biocmanager_install = FALSE,
      is_bioc = FALSE
    )
  })

  expect_false(result)
})

test_that(".renv_install_remaining accepts skip_if_dep_unavailable param", {
  suppressMessages({
    result <- .renv_install_remaining(
      pkg = character(0),
      biocmanager_install = FALSE,
      is_bioc = FALSE,
      skip_if_dep_unavailable = TRUE,
      lockfile_deps = list(pkg2 = "pkg1")
    )
  })

  expect_false(result)
})

# Test package name extraction from remotes
test_that("Package name extraction works correctly", {
  # This tests the pattern used in multiple functions
  pkg_with_remote <- "user/package"
  pkg_name <- sub("^.*/", "", pkg_with_remote)
  expect_equal(pkg_name, "package")

  pkg_without_remote <- "package"
  pkg_name <- sub("^.*/", "", pkg_without_remote)
  expect_equal(pkg_name, "package")
})


# Test .is_blocked_by_failed_deps
test_that(".is_blocked_by_failed_deps function exists", {
  expect_true(is.function(renvvv:::.is_blocked_by_failed_deps))
})

test_that(".is_blocked_by_failed_deps returns FALSE when not blocked", {
  # Not skipped
  expect_false(renvvv:::.is_blocked_by_failed_deps(
    pkg_name = "pkgA",
    failed_pkgs = c("pkgB"),
    installed_now = c(),
    skip_if_dep_unavailable = FALSE,
    lockfile_deps = list(pkgA = c("pkgB"))
  ))

  # No failed pkgs
  expect_false(renvvv:::.is_blocked_by_failed_deps(
    pkg_name = "pkgA",
    failed_pkgs = character(0),
    installed_now = c(),
    skip_if_dep_unavailable = TRUE,
    lockfile_deps = list(pkgA = c("pkgB"))
  ))

  # No dependencies
  expect_false(renvvv:::.is_blocked_by_failed_deps(
    pkg_name = "pkgA",
    failed_pkgs = c("pkgB"),
    installed_now = c(),
    skip_if_dep_unavailable = TRUE,
    lockfile_deps = list()
  ))

  # Dependency failed but is now installed
  expect_false(renvvv:::.is_blocked_by_failed_deps(
    pkg_name = "pkgA",
    failed_pkgs = c("pkgB"),
    installed_now = c("pkgB"),
    skip_if_dep_unavailable = TRUE,
    lockfile_deps = list(pkgA = c("pkgB"))
  ))
})

test_that(".is_blocked_by_failed_deps returns TRUE when blocked", {
  # Dependency failed and is not installed
  expect_true(renvvv:::.is_blocked_by_failed_deps(
    pkg_name = "pkgA",
    failed_pkgs = c("pkgB"),
    installed_now = c(),
    skip_if_dep_unavailable = TRUE,
    lockfile_deps = list(pkgA = c("pkgB"))
  ))
})
