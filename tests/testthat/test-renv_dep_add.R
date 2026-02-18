test_that("renv_dep_add function exists", {
  expect_true(is.function(renv_dep_add))
})

test_that("renv_restore function exists", {
  expect_true(is.function(renv_restore))
})

test_that("renv_update function exists", {
  expect_true(is.function(renv_update))
})

test_that("renv_restore_and_update function exists", {
  expect_true(is.function(renv_restore_and_update))
})

test_that(".ensure_cli helper exists", {
  expect_true(is.function(renvvv:::.ensure_cli))
})

test_that(".check_renv helper exists", {
  expect_true(is.function(renvvv:::.check_renv))
})
