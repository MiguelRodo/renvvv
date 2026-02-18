# Tests for internal helper functions

# Test .ensure_cli
test_that(".ensure_cli function exists", {
  expect_true(is.function(renvvv:::.ensure_cli))
})

test_that(".ensure_cli succeeds when cli is available", {
  expect_silent(renvvv:::.ensure_cli())
})

test_that(".ensure_cli returns NULL", {
  result <- renvvv:::.ensure_cli()
  expect_null(result)
})

# Test .check_renv
test_that(".check_renv function exists", {
  expect_true(is.function(renvvv:::.check_renv))
})

test_that(".check_renv succeeds when renv is available", {
  skip_if_not(requireNamespace("renv", quietly = TRUE), "renv not available")
  expect_silent(renvvv:::.check_renv())
})

test_that(".check_renv returns NULL", {
  skip_if_not(requireNamespace("renv", quietly = TRUE), "renv not available")
  result <- renvvv:::.check_renv()
  expect_null(result)
})

# Test .renv_paths_lockfile
test_that(".renv_paths_lockfile function exists", {
  expect_true(is.function(renvvv:::.renv_paths_lockfile))
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

  result <- renvvv:::.renv_paths_lockfile()
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
  result <- renvvv:::.renv_paths_lockfile()
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
  result <- renvvv:::.renv_paths_lockfile()
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

  result <- renvvv:::.renv_paths_lockfile()
  expected <- file.path(
    getwd(), "renv", "profiles", "test-profile", "renv.lock"
  )
  expect_equal(result, expected)
})

# Test .renv_lockfile_pkg_get (requires a mock lockfile)
test_that(".renv_lockfile_pkg_get function exists", {
  expect_true(is.function(renvvv:::.renv_lockfile_pkg_get))
})

test_that(".renv_lockfile_pkg_get returns list with expected structure", {
  skip_if_not(requireNamespace("renv", quietly = TRUE), "renv not available")

  # This test requires a lockfile to exist
  # We'll skip if not in a renv project
  skip_if_not(file.exists("renv.lock"), "No renv.lock file available")

  result <- renvvv:::.renv_lockfile_pkg_get()

  expect_type(result, "list")
  expect_named(result, c("regular", "bioc", "gh"))
  expect_type(result$regular, "character")
  expect_type(result$bioc, "character")
  expect_type(result$gh, "character")
})

# Test .renv_restore_or_update_impl
test_that(".renv_restore_or_update_impl function exists", {
  expect_true(is.function(renvvv:::.renv_restore_or_update_impl))
})

test_that(".renv_restore_or_update_impl accepts valid parameters", {
  skip_if_not(requireNamespace("renv", quietly = TRUE), "renv not available")

  # Create minimal package list
  pkg_list <- list(regular = character(0), bioc = character(0), gh = character(0))

  # Should complete without error when packages are empty
  suppressMessages({
    result <- renvvv:::.renv_restore_or_update_impl(
      package_list = pkg_list,
      github = TRUE,
      non_github = TRUE,
      restore = TRUE,
      biocmanager_install = FALSE
    )
  })

  expect_true(result)
})

# Test .renv_restore_or_update_actual_wrapper
test_that(".renv_restore_or_update_actual_wrapper function exists", {
  expect_true(is.function(renvvv:::.renv_restore_or_update_actual_wrapper))
})

test_that(".renv_restore_or_update_actual_wrapper handles empty package list", {
  suppressMessages({
    result <- renvvv:::.renv_restore_or_update_actual_wrapper(
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
    result <- renvvv:::.renv_restore_or_update_actual_wrapper(
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
  expect_true(is.function(renvvv:::.renv_restore_update_actual))
})

test_that(".renv_restore_update_actual handles empty package list", {
  result <- renvvv:::.renv_restore_update_actual(
    pkg = character(0),
    restore = TRUE,
    biocmanager_install = FALSE,
    is_bioc = FALSE
  )

  expect_false(result)
})

# Test .renv_restore_remaining
test_that(".renv_restore_remaining function exists", {
  expect_true(is.function(renvvv:::.renv_restore_remaining))
})

test_that(".renv_restore_remaining handles empty package list", {
  suppressMessages({
    result <- renvvv:::.renv_restore_remaining(character(0))
  })

  expect_false(result)
})

# Test .renv_install
test_that(".renv_install function exists", {
  expect_true(is.function(renvvv:::.renv_install))
})

test_that(".renv_install handles empty package list", {
  # Empty package list should not cause errors
  # Just suppress output but don't require silence as cli may output
  suppressMessages({
    renvvv:::.renv_install(
      pkg = character(0),
      biocmanager_install = FALSE,
      is_bioc = FALSE
    )
  })
  expect_true(TRUE)
})

# Test .renv_install_remaining
test_that(".renv_install_remaining function exists", {
  expect_true(is.function(renvvv:::.renv_install_remaining))
})

test_that(".renv_install_remaining handles empty package list", {
  suppressMessages({
    result <- renvvv:::.renv_install_remaining(
      pkg = character(0),
      biocmanager_install = FALSE,
      is_bioc = FALSE
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
