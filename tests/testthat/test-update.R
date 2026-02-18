# Integration tests for renvvv_update with real renv lockfiles

# Helper to set up a temporary renv project
.setup_renv_project <- function() {
  tmp <- tempfile("renvvv_update_test_")
  dir.create(tmp)
  old_wd <- setwd(tmp)
  old_libpaths <- .libPaths()
  old_env <- Sys.getenv("RENV_CONFIG_PAK_ENABLED", unset = NA)
  Sys.setenv(RENV_CONFIG_PAK_ENABLED = "FALSE")
  renv::init(bare = TRUE, restart = FALSE)
  list(
    dir = tmp,
    old_wd = old_wd,
    old_libpaths = old_libpaths,
    old_env = old_env
  )
}

# Helper to tear down a temporary renv project
.teardown_renv_project <- function(ctx) {
  setwd(ctx$old_wd)
  .libPaths(ctx$old_libpaths)
  if (is.na(ctx$old_env)) {
    Sys.unsetenv("RENV_CONFIG_PAK_ENABLED")
  } else {
    Sys.setenv(RENV_CONFIG_PAK_ENABLED = ctx$old_env)
  }
  unlink(ctx$dir, recursive = TRUE)
}

test_that("renvvv_update function exists", {
  expect_true(is.function(renvvv_update))
})

test_that("renvvv_update updates a CRAN package to latest version", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  ctx <- .setup_renv_project()
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Determine an old version of tinytest dynamically
  # The archive always has older versions; use 1.3.1 which is known old
  old_version <- "1.3.1"
  renv::install(paste0("tinytest@", old_version), prompt = FALSE)
  installed_version <- as.character(packageVersion("tinytest"))
  expect_equal(installed_version, old_version)

  # Snapshot to create a lockfile with the old version
  renv::snapshot(
    packages = "tinytest",
    type = "packages",
    confirm = FALSE
  )

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

  ctx <- .setup_renv_project()
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Install praise from GitHub and snapshot
  renv::install("gaborcsardi/praise", prompt = FALSE)
  renv::snapshot(
    packages = "praise",
    type = "packages",
    confirm = FALSE
  )

  # Verify it's in the lockfile as a GitHub package
  lockfile <- renv::lockfile_read()
  expect_true("praise" %in% names(lockfile$Packages))
  expect_equal(lockfile$Packages$praise$Source, "GitHub")

  # Run renvvv_update - should succeed without error
  suppressMessages(renvvv_update(non_github = FALSE, github = TRUE))

  # Verify praise is still installed after update
  expect_true(requireNamespace("praise", quietly = TRUE))
})
