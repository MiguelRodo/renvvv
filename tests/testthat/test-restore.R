# Integration tests for renvvv_restore with real renv lockfiles

# Helper to set up a temporary renv project
.setup_renv_project <- function() {
  tmp <- tempfile("renvvv_restore_test_")
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

test_that("renvvv_restore function exists", {
  expect_true(is.function(renvvv_restore))
})

test_that("renvvv_restore restores a CRAN package from lockfile", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  ctx <- .setup_renv_project()
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Install tinytest and snapshot
  renv::install("tinytest", prompt = FALSE)
  renv::snapshot(
    packages = "tinytest",
    type = "packages",
    confirm = FALSE
  )

  # Verify it's in the lockfile
  lockfile <- renv::lockfile_read()
  expect_true("tinytest" %in% names(lockfile$Packages))

  # Remove the package from the project library
  remove.packages("tinytest")
  expect_false(requireNamespace("tinytest", quietly = TRUE))

  # Run renvvv_restore
  suppressMessages(renvvv_restore(non_github = TRUE, github = FALSE))

  # Verify tinytest is restored
  expect_true(requireNamespace("tinytest", quietly = TRUE))
})

test_that("renvvv_restore restores a GitHub package from lockfile", {
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

  # Remove the package from the project library
  remove.packages("praise")
  expect_false(requireNamespace("praise", quietly = TRUE))

  # Run renvvv_restore
  suppressMessages(renvvv_restore(non_github = FALSE, github = TRUE))

  # Verify praise is restored
  expect_true(requireNamespace("praise", quietly = TRUE))
})
