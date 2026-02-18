# Tests for renvvv operations without renv activation
# These tests verify that restore and update functions work correctly
# even when renv is not activated

test_that("renvvv_restore works without renv activation", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  # Set up project without activation
  ctx <- .setup_renv_project_no_activation(pkgs = "tinytest")
  on.exit(.teardown_renv_project_no_activation(ctx), add = TRUE)

  # Verify renv is NOT activated (first libpath should not be renv library)
  expect_false(grepl("renv/library", .libPaths()[1], fixed = TRUE))

  # Run renvvv_restore with explicit project parameter
  suppressMessages(
    renvvv_restore(
      non_github = TRUE,
      github = FALSE,
      project = ctx$dir
    )
  )

  # Verify package was installed to renv library (not global library)
  expect_true(
    dir.exists(file.path(ctx$renv_lib, "tinytest"))
  )

  # Verify renv is STILL not activated
  expect_false(grepl("renv/library", .libPaths()[1], fixed = TRUE))
})

test_that("renvvv_update works without renv activation", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  # Set up project without activation
  ctx <- .setup_renv_project_no_activation(pkgs = "tinytest")
  on.exit(.teardown_renv_project_no_activation(ctx), add = TRUE)

  # Verify renv is NOT activated
  expect_false(grepl("renv/library", .libPaths()[1], fixed = TRUE))

  # Run renvvv_update with explicit project parameter
  suppressMessages(
    renvvv_update(
      non_github = TRUE,
      github = FALSE,
      project = ctx$dir
    )
  )

  # Verify package was installed to renv library
  expect_true(
    dir.exists(file.path(ctx$renv_lib, "tinytest"))
  )

  # Verify renv is STILL not activated
  expect_false(grepl("renv/library", .libPaths()[1], fixed = TRUE))
})

test_that("renvvv_restore_and_update works without renv activation", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  # Set up project without activation
  ctx <- .setup_renv_project_no_activation(pkgs = "tinytest")
  on.exit(.teardown_renv_project_no_activation(ctx), add = TRUE)

  # Verify renv is NOT activated
  expect_false(grepl("renv/library", .libPaths()[1], fixed = TRUE))

  # Run renvvv_restore_and_update with explicit project parameter
  suppressMessages(
    renvvv_restore_and_update(
      non_github = TRUE,
      github = FALSE,
      project = ctx$dir
    )
  )

  # Verify package was installed to renv library
  expect_true(
    dir.exists(file.path(ctx$renv_lib, "tinytest"))
  )

  # Verify renv is STILL not activated
  expect_false(grepl("renv/library", .libPaths()[1], fixed = TRUE))
})

test_that("renvvv_restore uses global cache when not activated", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  # Set up project without activation
  ctx <- .setup_renv_project_no_activation(pkgs = "tinytest")
  on.exit(.teardown_renv_project_no_activation(ctx), add = TRUE)

  # Run restore
  suppressMessages(
    renvvv_restore(
      non_github = TRUE,
      github = FALSE,
      project = ctx$dir
    )
  )

  # Check that the package is in the renv global cache
  # renv stores packages in its cache and links them to project libraries
  cache_path <- renv::paths$cache()
  expect_true(
    length(list.files(cache_path, pattern = "tinytest", recursive = TRUE)) > 0
  )
})

test_that("renvvv_restore with project=NULL defaults to current directory", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  # Set up project without activation and change to that directory
  ctx <- .setup_renv_project_no_activation(pkgs = "tinytest")
  on.exit(.teardown_renv_project_no_activation(ctx), add = TRUE)

  # Already in ctx$dir due to setup
  # Verify current directory is the test project
  expect_equal(getwd(), ctx$dir)

  # Run restore without project parameter (should default to current dir)
  suppressMessages(
    renvvv_restore(
      non_github = TRUE,
      github = FALSE
    )
  )

  # Verify package was installed to renv library
  expect_true(
    dir.exists(file.path(ctx$renv_lib, "tinytest"))
  )
})
