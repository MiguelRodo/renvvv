# This script benchmarks the improvement achieved by caching the renv lockfile
# rather than reading it from disk repeatedly.
# Note: Requires microbenchmark and renv to be installed to run fully.

# To run: Rscript tests/benchmark_lockfile_caching.R

# If run in an environment with missing dependencies, it gracefully skips.
if (!requireNamespace("microbenchmark", quietly = TRUE)) {
  message("microbenchmark not installed, skipping benchmark.")
  quit(save = "no", status = 0)
}

library(microbenchmark)

# Mock definitions based on the old and new approach
mock_lockfile_path <- "mock.lock"

# Dummy data
lockfile_data <- list(
  Packages = list(
    pkg1 = list(Package = "pkg1", Source = "CRAN"),
    pkg2 = list(Package = "pkg2", Source = "CRAN"),
    pkg3 = list(Package = "pkg3", Source = "GitHub", RemoteUsername = "user")
  )
)

# Mocked I/O delay
mock_lockfile_read <- function(file) {
  Sys.sleep(0.005) # Simulate 5ms lockfile read parsing delay
  list(Packages = lockfile_data$Packages)
}

# --- Old Approach ---
old_lockfile_pkg_get <- function() {
  pkgs <- mock_lockfile_read(mock_lockfile_path)$Packages
  # Processing logic...
  length(pkgs)
}

old_lockfile_deps_get <- function() {
  pkgs <- mock_lockfile_read(mock_lockfile_path)$Packages
  # Processing logic...
  length(pkgs)
}

old_operation <- function() {
  pkg_list <- old_lockfile_pkg_get()
  deps_list <- old_lockfile_deps_get()
  list(pkg_list, deps_list)
}

# --- New Approach ---
new_lockfile_read_pkgs <- function() {
  mock_lockfile_read(mock_lockfile_path)$Packages
}

new_lockfile_pkg_get <- function(pkgs) {
  # Processing logic...
  length(pkgs)
}

new_lockfile_deps_get <- function(pkgs) {
  # Processing logic...
  length(pkgs)
}

new_operation <- function() {
  cached_pkgs <- new_lockfile_read_pkgs()
  pkg_list <- new_lockfile_pkg_get(cached_pkgs)
  deps_list <- new_lockfile_deps_get(cached_pkgs)
  list(pkg_list, deps_list)
}

# --- Benchmark ---
cat("Running benchmark to compare lockfile parsing efficiency...\n")
results <- microbenchmark(
  OldApproach = old_operation(),
  NewApproach = new_operation(),
  times = 50
)

print(results)
