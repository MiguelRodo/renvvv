---
applyTo: "**/*.R"
---

# R Coding Standards

## Purpose & Scope

Guidelines for R code style, naming conventions, and documentation in this package.

---

## Naming Conventions

### Exported Functions

- Use `renvvv_` prefix for all exported functions
- Use snake_case for function names
- Be descriptive but concise

```r
# ✅ Correct
renvvv_restore()
renvvv_dep_add()
renvvv_hpc_renv_setup()
renvvv_renv_repos_setup()

# ❌ Incorrect
renv_restore()
renvRestore()
```

### Internal Functions

- Use `.` prefix for internal/helper functions
- Keep internal to the package using no `@export` tag

```r
# ✅ Correct
.ensure_cli <- function() { ... }
.check_renv <- function() { ... }

# ❌ Incorrect
ensure_cli <- function() { ... }
helper_check_renv <- function() { ... }
```

---

## Documentation

### Roxygen Comments

- All exported functions must have roxygen documentation
- Include `@title`, `@description`, `@param`, `@return`, `@export`
- Use `@examples` with `\dontrun{}` for functions with side effects

```r
#' @title Restore renv packages
#'
#' @description Restores packages from the renv lockfile.
#'
#' @param github Logical. Process GitHub packages. Default TRUE.
#' @param non_github Logical. Process non-GitHub packages. Default TRUE.
#'
#' @return Invisibly returns TRUE upon completion.
#'
#' @examples
#' \dontrun{
#' renvvv_restore()
#' }
#'
#' @export
```

---

## Code Style

### General Rules

- Use 2-space indentation
- Maximum line length: 80 characters recommended
- Use `<-` for assignment, not `=`
- No trailing whitespace

### Function Structure

- Return values explicitly or use `invisible(TRUE)`
- Check for required packages with `requireNamespace()`
- Use `tryCatch()` for error handling

```r
# ✅ Correct pattern
renvvv_example <- function(pkg) {
  if (!requireNamespace("cli", quietly = TRUE)) {
    stop("The 'cli' package is required.")
  }

  tryCatch(
    some_operation(),
    error = function(e) {
      cli::cli_alert_danger("Failed: {e$message}")
    }
  )

  invisible(TRUE)
}
```

---

## Package Dependencies

- Use `Suggests` for optional dependencies
- Check availability before using with `requireNamespace()`
- Install missing packages via `renv::install()` when appropriate

```r
# ✅ Correct
if (!requireNamespace("here", quietly = TRUE)) {
  renv::install("here")
}

# ❌ Incorrect - don't assume package is installed
library(here)
```
