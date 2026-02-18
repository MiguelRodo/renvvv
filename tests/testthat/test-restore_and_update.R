# Integration tests for renvvv_restore_and_update with real renv lockfiles

# Helper to set up a temporary renv project
.setup_renv_project <- function() {
  tmp <- tempfile("renvvv_restore_update_test_")
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

test_that("renvvv_restore_and_update function exists", {
  expect_true(is.function(renvvv_restore_and_update))
})

test_that("renvvv_restore_and_update restores then updates a CRAN package", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  ctx <- .setup_renv_project()
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Install an old version of tinytest
  old_version <- "1.3.1"
  renv::install(paste0("tinytest@", old_version), prompt = FALSE)
  expect_equal(as.character(packageVersion("tinytest")), old_version)

  # Snapshot to create a lockfile with the old version
  renv::snapshot(
    packages = "tinytest",
    type = "packages",
    confirm = FALSE
  )

  # Verify the lockfile records the old version
  lockfile <- renv::lockfile_read()
  expect_equal(lockfile$Packages$tinytest$Version, old_version)

  # Remove the package
  remove.packages("tinytest")
  expect_false(requireNamespace("tinytest", quietly = TRUE))

  # Run renvvv_restore_and_update
  # This should first restore (install old version), then update (to latest)
  suppressMessages(
    renvvv_restore_and_update(non_github = TRUE, github = FALSE)
  )

  # Verify the package is installed and updated to a newer version
  expect_true(requireNamespace("tinytest", quietly = TRUE))
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

  # Remove the package
  remove.packages("praise")
  expect_false(requireNamespace("praise", quietly = TRUE))

  # Run renvvv_restore_and_update
  suppressMessages(
    renvvv_restore_and_update(non_github = FALSE, github = TRUE)
  )

  # Verify praise is installed
  expect_true(requireNamespace("praise", quietly = TRUE))
})
