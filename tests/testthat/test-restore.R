# Integration tests for renvvv_restore with real renv lockfiles
# Shared helpers are loaded from helper-renv-project.R

test_that("renvvv_restore function exists", {
  expect_true(is.function(renvvv_restore))
})

test_that("renvvv_restore restores a CRAN package from lockfile", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  ctx <- .setup_renv_project(pkgs = "tinytest")
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Install tinytest and snapshot
  renv::install("tinytest", prompt = FALSE)
  renv::snapshot(packages = "tinytest", confirm = FALSE)

  # Verify it's in the lockfile
  lockfile <- renv::lockfile_read()
  expect_true("tinytest" %in% names(lockfile$Packages))

  # Remove the package from the project library
  .remove_pkg("tinytest")
  expect_false(
    nzchar(system.file(package = "tinytest", lib.loc = .libPaths()[1]))
  )

  # Run renvvv_restore
  suppressMessages(renvvv_restore(non_github = TRUE, github = FALSE))

  # Verify tinytest is restored
  expect_true(
    nzchar(system.file(package = "tinytest", lib.loc = .libPaths()[1]))
  )
})

test_that("renvvv_restore restores a GitHub package from lockfile", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )
  skip_if_not(.github_api_available(), "GitHub API not accessible")

  ctx <- .setup_renv_project(pkgs = "praise")
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Install praise from GitHub and snapshot
  renv::install("gaborcsardi/praise", prompt = FALSE)
  renv::snapshot(packages = "praise", confirm = FALSE)

  # Verify it's in the lockfile as a GitHub package
  lockfile <- renv::lockfile_read()
  expect_true("praise" %in% names(lockfile$Packages))
  expect_equal(lockfile$Packages$praise$Source, "GitHub")

  # Remove the package from the project library
  .remove_pkg("praise")
  expect_false(
    nzchar(system.file(package = "praise", lib.loc = .libPaths()[1]))
  )

  # Run renvvv_restore
  suppressMessages(renvvv_restore(non_github = FALSE, github = TRUE))

  # Verify praise is restored
  expect_true(
    nzchar(system.file(package = "praise", lib.loc = .libPaths()[1]))
  )
})

test_that("renvvv_restore handles corrupted lockfile with invalid package version", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  ctx <- .setup_renv_project(pkgs = "tinytest")
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Install tinytest and snapshot
  renv::install("tinytest", prompt = FALSE)
  renv::snapshot(packages = "tinytest", confirm = FALSE)

  # Read the lockfile
  lockfile <- renv::lockfile_read()

  # Add a corrupted package entry with non-existent version
  lockfile$Packages$corruptedpkg <- list(
    Package = "corruptedpkg",
    Version = "100.0.1",
    Source = "Repository",
    Repository = "CRAN",
    Requirements = character(0)
  )

  # Write the corrupted lockfile
  renv::lockfile_write(lockfile)

  # Remove tinytest from the project library
  .remove_pkg("tinytest")
  expect_false(
    nzchar(system.file(package = "tinytest", lib.loc = .libPaths()[1]))
  )

  # Run renvvv_restore - should handle the failure gracefully
  suppressMessages(renvvv_restore(non_github = TRUE, github = FALSE))

  # Verify tinytest is restored despite corruptedpkg failure
  expect_true(
    nzchar(system.file(package = "tinytest", lib.loc = .libPaths()[1]))
  )

  # Verify corruptedpkg is NOT installed (as expected)
  expect_false(
    nzchar(system.file(package = "corruptedpkg", lib.loc = .libPaths()[1]))
  )
})
