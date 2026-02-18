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
