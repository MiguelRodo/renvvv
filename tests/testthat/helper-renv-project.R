# Shared helpers for renv project testing
# This file is automatically loaded before tests run

# Helper to set up a temporary renv project with cache isolation
.setup_renv_project <- function(pkgs = NULL) {
  tmp <- tempfile("renvvv_test_")
  dir.create(tmp)
  old_wd <- setwd(tmp)
  old_libpaths <- .libPaths()
  old_env <- Sys.getenv("RENV_CONFIG_PAK_ENABLED", unset = NA)
  old_lockfile_env <- Sys.getenv("RENV_PATHS_LOCKFILE", unset = NA)
  Sys.setenv(RENV_CONFIG_PAK_ENABLED = "FALSE")
  # Override lockfile path so renv always reads from the correct project
  Sys.setenv(RENV_PATHS_LOCKFILE = file.path(tmp, "renv.lock"))
  # Purge test packages from renv cache to avoid cross-test interference
  for (pkg in pkgs) {
    try(renv::purge(pkg, prompt = FALSE), silent = TRUE)
  }
  renv::init(bare = TRUE, restart = FALSE)
  # Ensure test dependencies remain available by including original lib paths
  .libPaths(c(.libPaths(), old_libpaths))
  list(
    dir = tmp,
    old_wd = old_wd,
    old_libpaths = old_libpaths,
    old_env = old_env,
    old_lockfile_env = old_lockfile_env,
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
  if (is.na(ctx$old_lockfile_env)) {
    Sys.unsetenv("RENV_PATHS_LOCKFILE")
  } else {
    Sys.setenv(RENV_PATHS_LOCKFILE = ctx$old_lockfile_env)
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

# Helper to check if GitHub API is accessible for renv operations
# Tests access to the GitHub API endpoint that renv uses for package installation
.github_api_available <- function() {
  tryCatch({
    # Test if we can access the GitHub repos API which renv needs
    # for resolving and installing GitHub packages
    con <- url("https://api.github.com/repos/gaborcsardi/praise", open = "r")
    on.exit(close(con), add = TRUE)
    TRUE
  }, error = function(e) FALSE,
  warning = function(w) FALSE)
}
