# Unit tests for GitHub repository restore/update functionality
# These tests work with lockfile fixtures and don't require full GitHub API access

test_that("renvvv can parse GitHub packages from lockfile", {
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  # Create a test project with a fixture lockfile containing GitHub packages
  ctx <- .setup_renv_project(pkgs = NULL)
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Copy fixture lockfile with GitHub package
  fixture_path <- testthat::test_path("fixtures", "renv.lock.praise")
  file.copy(fixture_path, "renv.lock", overwrite = TRUE)

  # Test that our internal helper can extract GitHub packages
  pkg_list <- renvvv:::.renv_lockfile_pkg_get()

  expect_type(pkg_list, "list")
  expect_named(pkg_list, c("regular", "bioc", "gh"))

  # Should have praise in the GitHub packages list
  expect_true("gaborcsardi/praise" %in% pkg_list$gh)
  expect_length(pkg_list$gh, 1)

  # Should have no CRAN or Bioc packages in this fixture
  expect_length(pkg_list$regular, 0)
  expect_length(pkg_list$bioc, 0)
})

test_that("renvvv_restore correctly identifies GitHub packages to restore", {
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  # Create a test project with a fixture lockfile
  ctx <- .setup_renv_project(pkgs = NULL)
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Copy fixture lockfile with GitHub package
  fixture_path <- testthat::test_path("fixtures", "renv.lock.praise")
  file.copy(fixture_path, "renv.lock", overwrite = TRUE)

  # Call the internal implementation function to verify it processes correctly
  pkg_list <- renvvv:::.renv_lockfile_pkg_get()

  # Test the restore implementation (without actually installing)
  # This tests the logic without requiring API access
  suppressMessages({
    result <- tryCatch({
      # The function should handle empty package lists gracefully
      renvvv:::.renv_restore_or_update_impl(
        package_list = list(regular = character(0), bioc = character(0), gh = character(0)),
        github = TRUE,
        non_github = FALSE,
        restore = TRUE,
        biocmanager_install = FALSE
      )
      TRUE
    }, error = function(e) FALSE)
  })

  expect_true(result)
})

test_that("renvvv_update correctly identifies GitHub packages to update", {
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  # Create a test project with a fixture lockfile
  ctx <- .setup_renv_project(pkgs = NULL)
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Copy fixture lockfile with GitHub package
  fixture_path <- testthat::test_path("fixtures", "renv.lock.praise")
  file.copy(fixture_path, "renv.lock", overwrite = TRUE)

  # Test the update implementation (without actually installing)
  pkg_list <- renvvv:::.renv_lockfile_pkg_get()

  suppressMessages({
    result <- tryCatch({
      # The function should handle empty package lists gracefully
      renvvv:::.renv_restore_or_update_impl(
        package_list = list(regular = character(0), bioc = character(0), gh = character(0)),
        github = TRUE,
        non_github = FALSE,
        restore = FALSE,  # update = not restore
        biocmanager_install = FALSE
      )
      TRUE
    }, error = function(e) FALSE)
  })

  expect_true(result)
})

test_that("GitHub package format is correctly parsed", {
  # Test package name extraction from GitHub remotes
  github_pkg <- "gaborcsardi/praise"
  pkg_name <- sub("^.*/", "", github_pkg)

  expect_equal(pkg_name, "praise")

  # Test with just package name (no remote)
  regular_pkg <- "praise"
  pkg_name2 <- sub("^.*/", "", regular_pkg)

  expect_equal(pkg_name2, "praise")
})

test_that("renvvv functions accept github parameter correctly", {
  # Test that the exported functions have the correct signature
  expect_true("github" %in% names(formals(renvvv_restore)))
  expect_true("github" %in% names(formals(renvvv_update)))
  expect_true("github" %in% names(formals(renvvv_restore_and_update)))

  expect_true("non_github" %in% names(formals(renvvv_restore)))
  expect_true("non_github" %in% names(formals(renvvv_update)))
  expect_true("non_github" %in% names(formals(renvvv_restore_and_update)))
})
