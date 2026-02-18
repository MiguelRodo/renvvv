
# renvvv

<!-- badges: start -->

[![R-CMD-check](https://github.com/MiguelRodo/renvvv/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MiguelRodo/renvvv/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/MiguelRodo/renvvv/graph/badge.svg)](https://app.codecov.io/gh/MiguelRodo/renvvv)
<!-- badges: end -->

renvvv provides more robust `renv` restore and update functions that
keep going even when individual packages fail. Standard
`renv::restore()` stops on the first error; renvvv catches failures,
retries them individually, and reports what couldn't be installed — so
you don't lose progress on the packages that *can* be restored.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("MiguelRodo/renvvv")
```

## Usage

``` r
library(renvvv)

# Restore packages from lockfile — continues past individual failures
renvvv_restore()

# Update all lockfile packages to latest versions
renvvv_update()

# Restore first, then update
renvvv_restore_and_update()
```

All three functions handle CRAN, Bioconductor, and GitHub packages. Use
the `github` and `non_github` arguments to control which sources to
process.

### Automatic renv Project Activation

renvvv functions automatically detect and activate renv projects to
ensure consistent behavior:

- **Interactive sessions**: You'll be prompted to activate the project
  if a `renv.lock` file is detected but the project isn't currently
  active.
- **Non-interactive sessions**: The project is activated automatically
  without prompting.

This ensures that package operations run in the correct environment,
especially important for new setups or when switching between projects.

### Other Utilities

``` r
# Add packages to _dependencies.R and install them
renvvv_dep_add(c("dplyr", "ggplot2"))

# HPC (SLURM) setup helpers
renvvv_hpc_renv_setup()
renvvv_renv_repos_setup()
```

## License

MIT
