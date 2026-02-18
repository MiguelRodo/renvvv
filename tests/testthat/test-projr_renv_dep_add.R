test_that("projr_renv_dep_add function exists", {
  expect_true(is.function(projr_renv_dep_add))
})

test_that("projr_renv_restore function exists", {
  expect_true(is.function(projr_renv_restore))
})

test_that("projr_renv_update function exists", {
  expect_true(is.function(projr_renv_update))
})

test_that("projr_renv_restore_and_update function exists", {
  expect_true(is.function(projr_renv_restore_and_update))
})

test_that(".ensure_cli helper exists", {
  expect_true(is.function(UtilsProjrMR:::.ensure_cli))
})

test_that(".check_renv helper exists", {
  expect_true(is.function(UtilsProjrMR:::.check_renv))
})

test_that(".projr_renv_lockfile_pkg_get helper exists", {
  expect_true(is.function(UtilsProjrMR:::.projr_renv_lockfile_pkg_get))
})

test_that(".merge_results helper exists", {
  expect_true(is.function(UtilsProjrMR:::.merge_results))
})

test_that(".merge_results combines results correctly", {
  result1 <- list(
    success = TRUE,
    failed_packages = c("pkg1"),
    successful_packages = c("pkg2"),
    skipped_packages = c("pkg3")
  )

  result2 <- list(
    success = TRUE,
    failed_packages = c("pkg4"),
    successful_packages = c("pkg5"),
    skipped_packages = c("pkg6")
  )

  merged <- UtilsProjrMR:::.merge_results(result1, result2)

  expect_true(merged$success)
  expect_equal(merged$failed_packages, c("pkg1", "pkg4"))
  expect_equal(merged$successful_packages, c("pkg2", "pkg5"))
  expect_equal(merged$skipped_packages, c("pkg3", "pkg6"))
})

test_that(".merge_results handles failure correctly", {
  result1 <- list(
    success = TRUE,
    failed_packages = character(),
    successful_packages = c("pkg1"),
    skipped_packages = character()
  )

  result2 <- list(
    success = FALSE,
    failed_packages = c("pkg2"),
    successful_packages = character(),
    skipped_packages = character()
  )

  merged <- UtilsProjrMR:::.merge_results(result1, result2)

  expect_false(merged$success)
  expect_equal(merged$failed_packages, c("pkg2"))
})

test_that(".projr_renv_lockfile_pkg_get returns correct structure", {
  skip_if_not(requireNamespace("renv", quietly = TRUE), "renv not available")
  skip_if_not(file.exists("renv.lock"), "No renv.lock file found")

  result <- UtilsProjrMR:::.projr_renv_lockfile_pkg_get()

  expect_type(result, "list")
  expect_true("regular" %in% names(result))
  expect_true("bioc" %in% names(result))
  expect_true("gh" %in% names(result))
  expect_type(result$regular, "character")
  expect_type(result$bioc, "character")
  expect_type(result$gh, "character")
})
