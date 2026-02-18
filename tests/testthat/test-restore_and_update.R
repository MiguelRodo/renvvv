# Integration tests for renvvv_restore_and_update with real renv lockfiles

# Helper to set up a temporary renv project with cache isolation
.setup_renv_project <- function(pkgs = NULL) {
  tmp <- tempfile("renvvv_restore_update_test_")
  dir.create(tmp)
  old_wd <- setwd(tmp)
  old_libpaths <- .libPaths()
  old_env <- Sys.getenv("RENV_CONFIG_PAK_ENABLED", unset = NA)
  Sys.setenv(RENV_CONFIG_PAK_ENABLED = "FALSE")
  # Purge test packages from renv cache to avoid cross-test interference
  for (pkg in pkgs) {
    try(renv::purge(pkg, prompt = FALSE), silent = TRUE)
  }
  renv::init(bare = TRUE, restart = FALSE)
  list(
    dir = tmp,
    old_wd = old_wd,
    old_libpaths = old_libpaths,
    old_env = old_env,
    pkgs = pkgs
  )
}

# Helper to tear down a temporary renv project
.teardown_renv_project <- function(ctx) {
  for (pkg in ctx$pkgs) {
    try(unloadNamespace(pkg), silent = TRUE)
  }
  setwd(ctx$old_wd)
  .libPaths(ctx$old_libpaths)
  if (is.na(ctx$old_env)) {
    Sys.unsetenv("RENV_CONFIG_PAK_ENABLED")
  } else {
    Sys.setenv(RENV_CONFIG_PAK_ENABLED = ctx$old_env)
  }
  unlink(ctx$dir, recursive = TRUE)
}

# Helper to remove a package from the renv project library
.remove_pkg <- function(pkg) {
  try(unloadNamespace(pkg), silent = TRUE)
  lib <- .libPaths()[1]
  if (dir.exists(file.path(lib, pkg))) {
    remove.packages(pkg, lib = lib)
  }
}

# Helper to check if GitHub API is accessible
.github_api_available <- function() {
  tryCatch({
    con <- url("https://api.github.com", open = "r")
    on.exit(close(con))
    TRUE
  }, error = function(e) FALSE,
  warning = function(w) FALSE)
}

test_that("renvvv_restore_and_update function exists", {
  expect_true(is.function(renvvv_restore_and_update))
})

test_that("renvvv_restore_and_update restores then updates a CRAN package", {
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
  expect_equal(as.character(packageVersion("tinytest")), old_version)

  # Snapshot to create a lockfile with the old version
  renv::snapshot(packages = "tinytest", confirm = FALSE)

  # Verify the lockfile records the old version
  lockfile <- renv::lockfile_read()
  expect_equal(lockfile$Packages$tinytest$Version, old_version)

  # Remove the package
  .remove_pkg("tinytest")
  expect_false(
    nzchar(system.file(package = "tinytest", lib.loc = .libPaths()[1]))
  )

  # Run renvvv_restore_and_update
  # This should first restore (install old version), then update (to latest)
  suppressMessages(
    renvvv_restore_and_update(non_github = TRUE, github = FALSE)
  )

  # Verify the package is installed and updated to a newer version
  expect_true(
    nzchar(system.file(package = "tinytest", lib.loc = .libPaths()[1]))
  )
  updated_version <- as.character(packageVersion("tinytest"))
  expect_true(
    numeric_version(updated_version) >= numeric_version(old_version)
  )
})

test_that("renvvv_restore_and_update works with a GitHub package", {
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

  # Remove the package
  .remove_pkg("praise")
  expect_false(
    nzchar(system.file(package = "praise", lib.loc = .libPaths()[1]))
  )

  # Run renvvv_restore_and_update
  suppressMessages(
    renvvv_restore_and_update(non_github = FALSE, github = TRUE)
  )

  # Verify praise is installed
  expect_true(
    nzchar(system.file(package = "praise", lib.loc = .libPaths()[1]))
  )
})

test_that("renvvv_restore_and_update handles corrupted lockfile with invalid package version", {
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

  # Run renvvv_restore_and_update - should handle the failure gracefully
  suppressMessages(
    renvvv_restore_and_update(non_github = TRUE, github = FALSE)
  )

  # Verify tinytest is restored and updated despite corruptedpkg failure
  expect_true(
    nzchar(system.file(package = "tinytest", lib.loc = .libPaths()[1]))
  )

  # Verify corruptedpkg is NOT installed (as expected)
  expect_false(
    nzchar(system.file(package = "corruptedpkg", lib.loc = .libPaths()[1]))
  )
})
