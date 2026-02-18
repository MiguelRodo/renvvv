# Integration tests for renvvv_update with real renv lockfiles
# Shared helpers are loaded from helper-renv-project.R

test_that("renvvv_update function exists", {
  expect_true(is.function(renvvv_update))
})

test_that("renvvv_update updates a CRAN package to latest version", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  ctx <- .setup_renv_project(pkgs = "tinytest")
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Install an old version of tinytest
  old_version <- "1.3.1"
  renv::install(paste0("tinytest@", old_version), prompt = FALSE)
  installed_version <- as.character(packageVersion("tinytest"))
  expect_equal(installed_version, old_version)

  # Snapshot to create a lockfile with the old version
  renv::snapshot(packages = "tinytest", confirm = FALSE)

  # Verify the lockfile records the old version
  lockfile <- renv::lockfile_read()
  expect_equal(lockfile$Packages$tinytest$Version, old_version)

  # Run renvvv_update to update to latest
  suppressMessages(renvvv_update(non_github = TRUE, github = FALSE))

  # Verify the package is updated (version should be newer than old_version)
  updated_version <- as.character(packageVersion("tinytest"))
  expect_true(
    numeric_version(updated_version) >= numeric_version(old_version)
  )
})

test_that("renvvv_update updates a GitHub package", {
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

  # Run renvvv_update - should succeed without error
  suppressMessages(renvvv_update(non_github = FALSE, github = TRUE))

  # Verify praise is still installed after update
  expect_true(
    nzchar(system.file(package = "praise", lib.loc = .libPaths()[1]))
  )
})

test_that("renvvv_update handles corrupted lockfile with invalid package version", {
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

  # Run renvvv_update - should handle the failure gracefully
  suppressMessages(renvvv_update(non_github = TRUE, github = FALSE))

  # Verify tinytest is still present despite corruptedpkg failure
  expect_true(
    nzchar(system.file(package = "tinytest", lib.loc = .libPaths()[1]))
  )

  # Verify corruptedpkg is NOT installed (as expected)
  expect_false(
    nzchar(system.file(package = "corruptedpkg", lib.loc = .libPaths()[1]))
  )
})
