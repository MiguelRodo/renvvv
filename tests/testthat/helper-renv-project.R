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

# Helper to set up a temporary renv project WITHOUT activation
# This creates a lockfile but does NOT call renv::init(),
# allowing tests to verify that renvvv functions work without activation
.setup_renv_project_no_activation <- function(pkgs = NULL, lockfile_content = NULL) {
  tmp <- tempfile("renvvv_test_no_activation_")
  dir.create(tmp)
  old_wd <- setwd(tmp)
  old_libpaths <- .libPaths()
  old_env <- Sys.getenv("RENV_CONFIG_PAK_ENABLED", unset = NA)
  old_lockfile_env <- Sys.getenv("RENV_PATHS_LOCKFILE", unset = NA)

  Sys.setenv(RENV_CONFIG_PAK_ENABLED = "FALSE")
  # Override lockfile path
  Sys.setenv(RENV_PATHS_LOCKFILE = file.path(tmp, "renv.lock"))

  # Purge test packages from renv cache
  for (pkg in pkgs) {
    try(renv::purge(pkg, prompt = FALSE), silent = TRUE)
  }

  # Create a lockfile without activating the project
  if (is.null(lockfile_content)) {
    # Get actual package info from CRAN for valid lockfile
    lockfile_content <- list(
      R = list(Version = R.version.string),
      Packages = list()
    )
    # Add packages to lockfile if specified
    for (pkg in pkgs) {
      # Get package info from available.packages to use real versions
      avail <- available.packages()
      if (pkg %in% rownames(avail)) {
        version <- avail[pkg, "Version"]
      } else {
        version <- "1.4.1"  # Default version for tinytest
      }
      lockfile_content$Packages[[pkg]] <- list(
        Package = pkg,
        Version = version,
        Source = "Repository",
        Repository = "CRAN"
      )
    }
  }

  renv::lockfile_write(lockfile_content, file = file.path(tmp, "renv.lock"))

  # Create the renv library directory structure
  renv_lib <- renv:::renv_paths_library(project = tmp)
  dir.create(renv_lib, recursive = TRUE, showWarnings = FALSE)

  list(
    dir = tmp,
    old_wd = old_wd,
    old_libpaths = old_libpaths,
    old_env = old_env,
    old_lockfile_env = old_lockfile_env,
    pkgs = pkgs,
    renv_lib = renv_lib
  )
}

# Helper to tear down a non-activated renv project
.teardown_renv_project_no_activation <- function(ctx) {
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
