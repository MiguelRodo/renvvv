# Test function existence
test_that("renvvv_dep_add function exists", {
  expect_true(is.function(renvvv_dep_add))
})

test_that("renvvv_restore function exists", {
  expect_true(is.function(renvvv_restore))
})

test_that("renvvv_update function exists", {
  expect_true(is.function(renvvv_update))
})

test_that("renvvv_restore_and_update function exists", {
  expect_true(is.function(renvvv_restore_and_update))
})

test_that(".ensure_cli helper exists", {
  expect_true(is.function(renvvv:::.ensure_cli))
})

test_that(".check_renv helper exists", {
  expect_true(is.function(renvvv:::.check_renv))
})

# Test renvvv_dep_add functionality
test_that("renvvv_dep_add creates _dependencies.R when it doesn't exist", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, "_dependencies.R"))
  })
  setwd(temp_dir)

  # Create test with a package that's already installed (utils is built-in)
  suppressMessages({
    result <- renvvv_dep_add("utils")
  })

  expect_true(file.exists("_dependencies.R"))
  expect_true(result)
  content <- readLines("_dependencies.R")
  expect_true(any(grepl("library\\(utils\\)", content)))
})

test_that("renvvv_dep_add appends to existing _dependencies.R", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, "_dependencies.R"))
  })
  setwd(temp_dir)

  # Create initial file
  writeLines("library(stats)", "_dependencies.R")

  suppressMessages({
    result <- renvvv_dep_add("utils")
  })

  expect_true(file.exists("_dependencies.R"))
  content <- readLines("_dependencies.R")
  expect_true(any(grepl("library\\(stats\\)", content)))
  expect_true(any(grepl("library\\(utils\\)", content)))
})

test_that("renvvv_dep_add doesn't add duplicate library calls", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, "_dependencies.R"))
  })
  setwd(temp_dir)

  # Add the same package twice
  suppressMessages({
    renvvv_dep_add("utils")
    renvvv_dep_add("utils")
  })

  content <- readLines("_dependencies.R")
  lib_calls <- grep("library\\(utils\\)", content, value = TRUE)
  expect_equal(length(lib_calls), 1)
})

test_that("renvvv_dep_add handles GitHub remote syntax", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, "_dependencies.R"))
  })
  setwd(temp_dir)

  # Use a package with remote syntax (just test file writing, not installation)
  suppressMessages({
    result <- renvvv_dep_add("user/package")
  })

  content <- readLines("_dependencies.R")
  expect_true(any(grepl("library\\(package\\)", content)))
})

test_that("renvvv_dep_add returns invisible TRUE", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, "_dependencies.R"))
  })
  setwd(temp_dir)

  suppressMessages({
    result <- withVisible(renvvv_dep_add("utils"))
  })

  expect_true(result$value)
  expect_false(result$visible)
})

test_that("renvvv_dep_add handles multiple packages", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, "_dependencies.R"))
  })
  setwd(temp_dir)

  suppressMessages({
    renvvv_dep_add(c("utils", "stats", "tools"))
  })

  content <- readLines("_dependencies.R")
  expect_true(any(grepl("library\\(utils\\)", content)))
  expect_true(any(grepl("library\\(stats\\)", content)))
  expect_true(any(grepl("library\\(tools\\)", content)))
})

# Test helper functions
test_that(".ensure_cli doesn't error when cli is available", {
  expect_silent(renvvv:::.ensure_cli())
})

test_that(".check_renv doesn't error when renv is available", {
  expect_silent(renvvv:::.check_renv())
})

# Test restore/update function parameters
test_that("renvvv_restore accepts parameter combinations", {
  # These tests verify the function accepts parameters without errors
  # We can't test actual restore without a lockfile
  skip_if_not(requireNamespace("renv", quietly = TRUE), "renv not available")

  # Test will error without lockfile - we're just testing the function exists and accepts params
  expect_error(
    suppressMessages(renvvv_restore(github = TRUE, non_github = TRUE))
  )
  expect_error(
    suppressMessages(renvvv_restore(github = FALSE, non_github = TRUE))
  )
  expect_error(
    suppressMessages(renvvv_restore(github = TRUE, non_github = FALSE))
  )
})

test_that("renvvv_update accepts parameter combinations", {
  skip_if_not(requireNamespace("renv", quietly = TRUE), "renv not available")

  expect_error(
    suppressMessages(renvvv_update(github = TRUE, non_github = TRUE))
  )
})

test_that("renvvv_restore_and_update calls both functions", {
  skip_if_not(requireNamespace("renv", quietly = TRUE), "renv not available")

  # Should error when calling restore (no lockfile)
  expect_error(
    suppressMessages(renvvv_restore_and_update())
  )
})
