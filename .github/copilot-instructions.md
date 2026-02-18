# Copilot Instructions for renvvv

## Overview

renvvv is an R package that wraps `renv` to provide more robust package restore and update functions. Standard `renv::restore()` stops on the first error; renvvv catches failures, retries them individually, and reports what couldn't be installed. It also provides helpers for HPC (SLURM) environment configuration.

## Package Structure

```
R/                        # Source code
  renv_dep_add.R          # renvvv_dep_add() + helpers .ensure_cli(), .check_renv()
  renv_helpers.R          # Internal engine: lockfile parsing, restore/update logic
  restore.R               # renvvv_restore()
  update.R                # renvvv_update()
  restore_and_update.R    # renvvv_restore_and_update()
  rprofile.R              # renvvv_hpc_renv_setup(), renvvv_renv_repos_setup()
tests/testthat/           # Tests (testthat v3, Config/testthat/edition: 3)
  test-<source_file>.R    # Unit tests matching source files
  test-helpers.R          # Tests for internal helper functions
  test-edge-cases.R       # Edge case and error condition tests
man/                      # Auto-generated roxygen2 docs — DO NOT edit directly
inst/scripts/             # Helper scripts copied by HPC setup functions
  hpc_renv_setup.R        # Sets RENV_PATHS_* for SLURM scratch directories
  renv_repos.R            # Configures RSPM/CRAN repos per OS
NAMESPACE                 # Auto-generated — DO NOT edit directly
```

## Exported Functions

| Function | Purpose |
|----------|---------|
| `renvvv_restore()` | Restore lockfile packages, continuing past individual failures |
| `renvvv_update()` | Update lockfile packages to latest versions |
| `renvvv_restore_and_update()` | Restore then update (calls both above in sequence) |
| `renvvv_dep_add()` | Add `library()` calls to `_dependencies.R` and install packages |
| `renvvv_hpc_renv_setup()` | Configure `.Rprofile` and scripts for SLURM HPC environments |
| `renvvv_renv_repos_setup()` | Configure `.Rprofile` and scripts for RSPM/CRAN repo setup |

## Naming Conventions

- Exported functions: `renvvv_` prefix, snake_case (e.g., `renvvv_restore`)
- Internal functions: `.` prefix (e.g., `.ensure_cli`, `.renv_lockfile_pkg_get`)
- Test files: `test-<source_file_name>.R`

## Code Patterns

- Use `<-` for assignment, 2-space indentation, max 80 chars per line
- Error handling: wrap operations in `tryCatch()` with `cli::cli_alert_danger()` for failures
- Dependency checks: use `.check_renv()` for renv, `.ensure_cli()` for cli
- Functions return `invisible(TRUE)` on success
- No trailing whitespace on any line (including blank lines)
- Always add a blank line between headings and bullet points

## How renv Finds the Lockfile

(Last updated: 18 Feb 2026)

The `renv` package resolves the lockfile path in the following order:

1. **Environment Variable**: Checks `RENV_PATHS_LOCKFILE`. If set, uses this path directly. If the value ends with `/` or `\`, appends `renv.lock`.

2. **Default Location**: If not set, constructs the path as:
   - **Without profile**: `project/renv.lock` (where project is `getwd()` by default)
   - **With profile** (via `RENV_PROFILE`): `project/renv/profiles/<name>/renv.lock`

The package's `.renv_paths_lockfile()` helper mimics this logic to read lockfiles without activating the project (avoiding global state modification).

## Dependencies

- **Imports** (always available): `renv`, `cli`
- **Suggests** (check before use): `BiocManager`, `rcmdcheck`, `testthat`

## Environment Setup

The `copilot-setup-steps.yaml` workflow installs these system dependencies:

```
libcurl4-openssl-dev libfontconfig1-dev libfreetype6-dev
libgit2-dev libjpeg-dev libpng-dev libx11-dev pandoc
libharfbuzz-dev libfribidi-dev libtiff-dev
```

R packages installed via the workflow: `rcmdcheck`, `devtools`, `roxygen2`.

## Build, Test, and Check Commands

```r
# Generate/update documentation (NAMESPACE, man/ pages)
devtools::document()

# Run all tests
devtools::test()

# Run a specific test file
testthat::test_file("tests/testthat/test-renv_dep_add.R")

# Full R CMD check (runs tests, builds docs, checks package)
devtools::check()
```

**Before committing any code changes:**

1. Run `devtools::document()` to regenerate `man/` and `NAMESPACE`
2. Run `devtools::test()` to verify all tests pass
3. Run `devtools::check()` to ensure R CMD check passes

## Test Patterns

- **Function existence tests**: Verify every exported and internal function exists
- **Behavior tests**: Test with built-in packages (`utils`, `stats`, `tools`) to avoid network dependencies
- **Integration tests** (in `test-restore.R`, `test-update.R`, `test-restore_and_update.R`): Use `.setup_renv_project()` / `.teardown_renv_project()` helpers for temporary renv projects
- **Edge case tests**: NULL input, empty vectors, read-only files, rapid successive calls
- Tests use `suppressMessages()` to suppress `cli` output
- Tests use `on.exit()` for cleanup (restore working directory, remove temp files)
- Integration tests with GitHub API use `skip_if_not(.github_api_available())` — these will be skipped in sandboxed environments
- One concept per `test_that()` block

## CI Workflows

| Workflow | Trigger | Purpose |
|----------|---------|---------|
| `R-CMD-check.yaml` | push/PR to main/master | R CMD check on macOS, Windows, Ubuntu (devel, release, oldrel-1) |
| `test-coverage.yaml` | push/PR to main/master | Code coverage via `covr` and Codecov |
| `copilot-setup-steps.yaml` | workflow_dispatch | Environment setup for Copilot coding agent |

## Known Issues and Workarounds

- **INLA repo errors during tests**: The warning `error downloading 'https://inla.r-inla-download.org/...'` appears when `renv_repos.R` configures INLA as a repository. This is expected in environments without INLA access and does not cause test failures.
- **GitHub API tests skipped**: Integration tests that install packages from GitHub (e.g., `gaborcsardi/praise`) are skipped when the GitHub API is not accessible. This is expected in sandboxed CI environments.
- **renv.lock not found**: The test `.renv_lockfile_pkg_get returns list with expected structure` is skipped unless a `renv.lock` file exists in the test working directory.
- **Read-only file test warning**: `test-edge-cases.R` test for read-only `_dependencies.R` produces a warning about "Permission denied" — this is expected behavior being tested, not a real failure.

## Topic-Specific Instructions

See `.github/instructions/` for detailed topic files:

- `r-coding-standards.instructions.md` — R code style, naming, and documentation (`**/*.R`)
- `testing.instructions.md` — Test patterns, organization, and helpers (`tests/**/*`)
