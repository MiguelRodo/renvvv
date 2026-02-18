# Test function existence
test_that("renvvv_hpc_renv_setup function exists", {
  expect_true(is.function(renvvv_hpc_renv_setup))
})

test_that("renvvv_renv_repos_setup function exists", {
  expect_true(is.function(renvvv_renv_repos_setup))
})

# Test renvvv_hpc_renv_setup functionality
test_that("renvvv_hpc_renv_setup creates .Rprofile when it doesn't exist", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, ".Rprofile"))
    unlink(file.path(temp_dir, "scripts"), recursive = TRUE)
  })
  setwd(temp_dir)

  result <- renvvv_hpc_renv_setup()

  expect_true(file.exists(".Rprofile"))
  expect_true(result)
})

test_that("renvvv_hpc_renv_setup appends to existing .Rprofile", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, ".Rprofile"))
    unlink(file.path(temp_dir, "scripts"), recursive = TRUE)
  })
  setwd(temp_dir)

  # Create initial .Rprofile
  writeLines("# Existing content", ".Rprofile")

  result <- renvvv_hpc_renv_setup()

  content <- readLines(".Rprofile")
  expect_true(any(grepl("# Existing content", content)))
  expect_true(any(grepl("# make renv use scratch directory", content)))
})

test_that("renvvv_hpc_renv_setup creates scripts directory", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, ".Rprofile"))
    unlink(file.path(temp_dir, "scripts"), recursive = TRUE)
  })
  setwd(temp_dir)

  renvvv_hpc_renv_setup()

  expect_true(dir.exists(file.path("scripts", "R")))
})

test_that("renvvv_hpc_renv_setup copies hpc_renv_setup.R script", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, ".Rprofile"))
    unlink(file.path(temp_dir, "scripts"), recursive = TRUE)
  })
  setwd(temp_dir)

  renvvv_hpc_renv_setup()

  expect_true(file.exists(file.path("scripts", "R", "hpc_renv_setup.R")))
})

test_that("renvvv_hpc_renv_setup adds SLURM detection code", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, ".Rprofile"))
    unlink(file.path(temp_dir, "scripts"), recursive = TRUE)
  })
  setwd(temp_dir)

  renvvv_hpc_renv_setup()

  content <- readLines(".Rprofile")
  expect_true(any(grepl("slurm_ind", content)))
  expect_true(any(grepl("SLURM_", content)))
})

test_that("renvvv_hpc_renv_setup returns invisible TRUE", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, ".Rprofile"))
    unlink(file.path(temp_dir, "scripts"), recursive = TRUE)
  })
  setwd(temp_dir)

  result <- withVisible(renvvv_hpc_renv_setup())

  expect_true(result$value)
  expect_false(result$visible)
})

# Test renvvv_renv_repos_setup functionality
test_that("renvvv_renv_repos_setup creates .Rprofile when it doesn't exist", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, ".Rprofile"))
    unlink(file.path(temp_dir, "scripts"), recursive = TRUE)
  })
  setwd(temp_dir)

  result <- renvvv_renv_repos_setup()

  expect_true(file.exists(".Rprofile"))
  expect_true(result)
})

test_that("renvvv_renv_repos_setup appends to existing .Rprofile", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, ".Rprofile"))
    unlink(file.path(temp_dir, "scripts"), recursive = TRUE)
  })
  setwd(temp_dir)

  # Create initial .Rprofile
  writeLines("# Existing content", ".Rprofile")

  result <- renvvv_renv_repos_setup()

  content <- readLines(".Rprofile")
  expect_true(any(grepl("# Existing content", content)))
  expect_true(any(grepl("# enable renv to use binaries across OSs", content)))
})

test_that("renvvv_renv_repos_setup creates scripts directory", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, ".Rprofile"))
    unlink(file.path(temp_dir, "scripts"), recursive = TRUE)
  })
  setwd(temp_dir)

  renvvv_renv_repos_setup()

  expect_true(dir.exists(file.path("scripts", "R")))
})

test_that("renvvv_renv_repos_setup copies renv_repos.R script", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, ".Rprofile"))
    unlink(file.path(temp_dir, "scripts"), recursive = TRUE)
  })
  setwd(temp_dir)

  renvvv_renv_repos_setup()

  expect_true(file.exists(file.path("scripts", "R", "renv_repos.R")))
})

test_that("renvvv_renv_repos_setup returns invisible TRUE", {
  temp_dir <- tempdir()
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, ".Rprofile"))
    unlink(file.path(temp_dir, "scripts"), recursive = TRUE)
  })
  setwd(temp_dir)

  result <- withVisible(renvvv_renv_repos_setup())

  expect_true(result$value)
  expect_false(result$visible)
})
