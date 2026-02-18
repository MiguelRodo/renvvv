test_that("setup_hpc_renv function exists", {
  expect_true(is.function(setup_hpc_renv))
})

test_that("setup_renv_repos function exists", {
  expect_true(is.function(setup_renv_repos))
})
