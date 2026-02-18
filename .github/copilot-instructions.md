# Copilot Instructions for renvvv

## Overview

renvvv is an R package providing utility functions for managing R projects using `renv` for dependency management, with specialized tools for HPC environments.

## Package Structure

- `R/` - Source code (use `.` prefix for internal, `projr_` for exported functions)
- `tests/testthat/` - Tests (follow existing patterns)
- `man/` - Auto-generated docs (DO NOT edit directly)

## Code Quality

- Make minimal, surgical changes to fix issues
- Maintain backward compatibility when possible
- Follow existing patterns in the codebase
- Add tests for new functionality or bug fixes
- Never leave trailing whitespace at the end of lines or on blank lines
- Always add a blank line between headings and bullet points

## Naming Conventions

- Exported functions: `renvvv_` prefix (e.g., `renvvv_restore`)
- Internal functions: `.` prefix (e.g., `.ensure_cli`)
- Test files: `test-<source_file_name>.R`

## Before Committing

- Run `devtools::document()` to update documentation
- Run `devtools::test()` to run tests
- Run `devtools::check()` to ensure package passes R CMD check

## Topic-Specific Instructions

See `.github/instructions/` for detailed topic files:

- `r-coding-standards.instructions.md` - R code style and documentation
- `testing.instructions.md` - Test patterns and helpers
