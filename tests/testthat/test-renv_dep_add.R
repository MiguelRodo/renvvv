# Test function existence
test_that("renvvv_dep_add function exists", {
  expect_true(is.function(renvvv_dep_add))
})

test_that(".ensure_cli helper exists", {
  expect_true(is.function(renvvv:::.ensure_cli))
})

test_that(".check_renv helper exists", {
  expect_true(is.function(renvvv:::.check_renv))
})

test_that(".check_write_permission helper exists", {
  expect_true(is.function(renvvv:::.check_write_permission))
})

# Test permission checks
test_that("renvvv_dep_add errors without force in non-interactive mode", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, "_dependencies.R"))
  })
  setwd(temp_dir)

  expect_error(
    renvvv_dep_add("utils", force = FALSE),
    "This function requires permission to write files"
  )
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
    result <- renvvv_dep_add("utils", force = TRUE)
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
    result <- renvvv_dep_add("utils", force = TRUE)
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
    renvvv_dep_add("utils", force = TRUE)
    renvvv_dep_add("utils", force = TRUE)
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
    result <- renvvv_dep_add("user/package", force = TRUE)
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
    result <- withVisible(renvvv_dep_add("utils", force = TRUE))
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
    renvvv_dep_add(c("utils", "stats", "tools"), force = TRUE)
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


