# Tests for renv project activation checking

test_that(".check_renv_activation function exists", {
  expect_true(is.function(renvvv:::.check_renv_activation))
})

test_that(".check_renv_activation returns NULL when not in renv project", {
  # Create a temporary directory without renv.lock
  tmp <- tempfile("no_renv_")
  dir.create(tmp)
  old_wd <- setwd(tmp)
  on.exit({
    setwd(old_wd)
    unlink(tmp, recursive = TRUE)
  })

  result <- renvvv:::.check_renv_activation()
  expect_null(result)
})

test_that(".check_renv_activation returns NULL when project is already active", {
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  ctx <- .setup_renv_project()
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Project should be active after setup
  result <- suppressMessages(renvvv:::.check_renv_activation())
  expect_null(result)
})

test_that(".check_renv_activation works in activated project for restore", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  ctx <- .setup_renv_project(pkgs = "tinytest")
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Install and snapshot a package
  renv::install("tinytest", prompt = FALSE)
  renv::snapshot(packages = "tinytest", confirm = FALSE)

  # Remove the package
  .remove_pkg("tinytest")

  # Run restore - should work without activation prompt
  suppressMessages(renvvv_restore(non_github = TRUE, github = FALSE))

  # Verify package was restored
  expect_true(
    nzchar(system.file(package = "tinytest", lib.loc = .libPaths()[1]))
  )
})

test_that(".check_renv_activation normalizes paths correctly", {
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  ctx <- .setup_renv_project()
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Test that activation check doesn't fail due to path normalization
  result <- suppressMessages(renvvv:::.check_renv_activation())
  expect_null(result)
})

test_that(".check_renv_activation handles non-interactive mode", {
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  # This test is primarily to ensure no errors occur
  # We can't easily test the actual activation in non-interactive mode
  # because our test environment is already managing renv contexts

  # Just verify the function completes without error when called
  # in a simple renv project context
  ctx <- .setup_renv_project()
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  expect_silent(suppressMessages(renvvv:::.check_renv_activation()))
})

test_that("renvvv_restore calls activation check", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  ctx <- .setup_renv_project(pkgs = "tinytest")
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Install and snapshot
  renv::install("tinytest", prompt = FALSE)
  renv::snapshot(packages = "tinytest", confirm = FALSE)

  # Remove package
  .remove_pkg("tinytest")

  # This should succeed with activation check
  suppressMessages(renvvv_restore(non_github = TRUE, github = FALSE))

  expect_true(
    nzchar(system.file(package = "tinytest", lib.loc = .libPaths()[1]))
  )
})

test_that("renvvv_update calls activation check", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  ctx <- .setup_renv_project(pkgs = "tinytest")
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Install and snapshot
  renv::install("tinytest", prompt = FALSE)
  renv::snapshot(packages = "tinytest", confirm = FALSE)

  # This should succeed with activation check
  suppressMessages(renvvv_update(non_github = TRUE, github = FALSE))

  # Package should still be available
  expect_true(
    nzchar(system.file(package = "tinytest", lib.loc = .libPaths()[1]))
  )
})

test_that("renvvv_dep_add calls activation check", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  ctx <- .setup_renv_project()
  on.exit({
    if (file.exists("_dependencies.R")) {
      unlink("_dependencies.R")
    }
    .teardown_renv_project(ctx)
  }, add = TRUE)

  # This should succeed with activation check
  suppressMessages(renvvv_dep_add("utils", force = TRUE))

  # Verify _dependencies.R was created
  expect_true(file.exists("_dependencies.R"))

  # Check content
  content <- readLines("_dependencies.R")
  expect_true("library(utils)" %in% content)
})
