# Integration tests for renvvv_restore with real renv lockfiles

# Helper to set up a temporary renv project with cache isolation
.setup_renv_project <- function(pkgs = NULL) {
  tmp <- tempfile("renvvv_restore_test_")
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
