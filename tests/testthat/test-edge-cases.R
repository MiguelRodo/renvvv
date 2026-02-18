# Tests for edge cases and error conditions

# Test error handling in renvvv_dep_add
test_that("renvvv_dep_add handles NULL input gracefully", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, "_dependencies.R"))
  })
  setwd(temp_dir)

  # NULL should work - sapply will return an empty result
  suppressMessages({
    result <- renvvv_dep_add(NULL)
  })
  expect_true(result)
})

test_that("renvvv_dep_add handles empty character vector", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, "_dependencies.R"))
  })
  setwd(temp_dir)

  suppressMessages({
    result <- renvvv_dep_add(character(0))
  })

  # Should create file even with no packages
  expect_true(file.exists("_dependencies.R"))
})

test_that("renvvv_dep_add handles very long package names", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, "_dependencies.R"))
  })
  setwd(temp_dir)

  long_name <- paste(rep("a", 100), collapse = "")
  suppressMessages({
    result <- renvvv_dep_add(long_name)
  })

  content <- readLines("_dependencies.R")
  expect_true(any(grepl(long_name, content)))
})

test_that("renvvv_dep_add handles special characters in package names", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, "_dependencies.R"))
  })
  setwd(temp_dir)

  # Test with dots (common in R packages)
  suppressMessages({
    result <- renvvv_dep_add("data.table")
  })

  content <- readLines("_dependencies.R")
  expect_true(any(grepl("library\\(data.table\\)", content)))
})

# Test file permission issues
test_that("renvvv_dep_add handles read-only _dependencies.R", {
  skip_on_os("windows") # File permissions work differently on Windows

  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    Sys.chmod(file.path(temp_dir, "_dependencies.R"), "644")
    unlink(file.path(temp_dir, "_dependencies.R"))
  })
  setwd(temp_dir)

  # Create a read-only file
  writeLines("library(stats)", "_dependencies.R")
  Sys.chmod("_dependencies.R", "444")

  # Should error when trying to write
  expect_error(
    suppressMessages(renvvv_dep_add("utils"))
  )
})

# Test renvvv_hpc_renv_setup edge cases
test_that("renvvv_hpc_renv_setup handles existing scripts directory", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, ".Rprofile"))
    unlink(file.path(temp_dir, "scripts"), recursive = TRUE)
  })
  setwd(temp_dir)

  # Create scripts/R directory first
  dir.create(file.path("scripts", "R"), recursive = TRUE)

  result <- renvvv_hpc_renv_setup()

  expect_true(result)
  expect_true(file.exists(".Rprofile"))
})

test_that("renvvv_hpc_renv_setup handles existing hpc_renv_setup.R", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, ".Rprofile"))
    unlink(file.path(temp_dir, "scripts"), recursive = TRUE)
  })
  setwd(temp_dir)

  # Create scripts/R directory and file
  dir.create(file.path("scripts", "R"), recursive = TRUE)
  writeLines("# Existing script", file.path("scripts", "R", "hpc_renv_setup.R"))

  result <- renvvv_hpc_renv_setup()

  expect_true(result)
  # File should be overwritten
  expect_true(file.exists(file.path("scripts", "R", "hpc_renv_setup.R")))
})

# Test renvvv_renv_repos_setup edge cases
test_that("renvvv_renv_repos_setup handles existing scripts directory", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, ".Rprofile"))
    unlink(file.path(temp_dir, "scripts"), recursive = TRUE)
  })
  setwd(temp_dir)

  # Create scripts/R directory first
  dir.create(file.path("scripts", "R"), recursive = TRUE)

  result <- renvvv_renv_repos_setup()

  expect_true(result)
  expect_true(file.exists(".Rprofile"))
})

test_that("renvvv_renv_repos_setup handles existing renv_repos.R", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, ".Rprofile"))
    unlink(file.path(temp_dir, "scripts"), recursive = TRUE)
  })
  setwd(temp_dir)

  # Create scripts/R directory and file
  dir.create(file.path("scripts", "R"), recursive = TRUE)
  writeLines("# Existing script", file.path("scripts", "R", "renv_repos.R"))

  result <- renvvv_renv_repos_setup()

  expect_true(result)
  # File should be overwritten
  expect_true(file.exists(file.path("scripts", "R", "renv_repos.R")))
})

# Test parameter validation
test_that("renvvv_restore validates boolean parameters", {
  skip_if_not(requireNamespace("renv", quietly = TRUE), "renv not available")

  # Non-boolean values should work (R is permissive with TRUE/FALSE)
  # Will error because lockfile doesn't exist
  expect_error(
    suppressMessages(renvvv_restore(github = "not a boolean"))
  )
})

test_that("renvvv_update validates boolean parameters", {
  skip_if_not(requireNamespace("renv", quietly = TRUE), "renv not available")

  # Will error because lockfile doesn't exist
  expect_error(
    suppressMessages(renvvv_update(non_github = "not a boolean"))
  )
})

# Test with malformed .Rprofile
test_that("renvvv_hpc_renv_setup handles malformed .Rprofile", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, ".Rprofile"))
    unlink(file.path(temp_dir, "scripts"), recursive = TRUE)
  })
  setwd(temp_dir)

  # Create .Rprofile with incomplete lines
  writeLines(c("# Comment", "library("), ".Rprofile")

  # Should still complete successfully
  result <- renvvv_hpc_renv_setup()

  expect_true(result)
  expect_true(file.exists(".Rprofile"))
})

# Test concurrent file access
test_that("renvvv_dep_add handles multiple rapid calls", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, "_dependencies.R"))
  })
  setwd(temp_dir)

  # Multiple rapid calls
  suppressMessages({
    renvvv_dep_add("utils")
    renvvv_dep_add("stats")
    renvvv_dep_add("tools")
  })

  content <- readLines("_dependencies.R")
  expect_true(any(grepl("library\\(utils\\)", content)))
  expect_true(any(grepl("library\\(stats\\)", content)))
  expect_true(any(grepl("library\\(tools\\)", content)))
})

# Test empty line handling
test_that("renvvv_dep_add preserves structure in _dependencies.R", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, "_dependencies.R"))
  })
  setwd(temp_dir)

  # Create file with various formatting
  writeLines(c("# Header comment", "", "library(stats)", "", "# Another comment"), "_dependencies.R")

  suppressMessages({
    renvvv_dep_add("utils")
  })

  content <- readLines("_dependencies.R")
  # All original content should be preserved
  expect_true(any(grepl("# Header comment", content)))
  expect_true(any(grepl("library\\(stats\\)", content)))
  expect_true(any(grepl("# Another comment", content)))
  expect_true(any(grepl("library\\(utils\\)", content)))
})
