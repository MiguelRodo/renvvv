# Tests for edge cases and error conditions

# Test package name extraction from remotes
test_that("Package name extraction works correctly", {
  # This tests the pattern used in multiple functions
  pkg_with_remote <- "user/package"
  pkg_name <- sub("^.*/", "", pkg_with_remote)
  expect_equal(pkg_name, "package")

  pkg_without_remote <- "package"
  pkg_name <- sub("^.*/", "", pkg_without_remote)
  expect_equal(pkg_name, "package")
})
