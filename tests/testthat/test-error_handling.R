testthat::skip_if_not_installed("mockery")
test_that("error handlers are triggered correctly when packages fail", {
  # Mock renv::install and renv::restore to always throw errors
  mockery::stub(renvvv:::.renv_restore_remaining, "renv::restore", function(...) stop("Mocked restore error"))
  mockery::stub(renvvv:::.renv_install_remaining, "renv::install", function(...) stop("Mocked install error"))
  mockery::stub(renvvv:::.renv_install, "BiocManager::install", function(...) stop("Mocked BiocManager install error"))
  # Ensure BiocManager is mocked as available
  mockery::stub(renvvv:::.renv_install, "requireNamespace", function(pkg, ...) {
    if (pkg == "BiocManager") return(TRUE)
    return(FALSE)
  })

  # Execute a deliberately failing restore
  expect_error(
    renvvv:::.renv_restore_remaining("non_existent_pkg_1"),
    NA # we expect it to swallow the error internally and continue
  )

  # Execute deliberately failing installs
  expect_error(
    renvvv:::.renv_install_remaining("non_existent_pkg_2", biocmanager_install = FALSE, is_bioc = FALSE),
    NA
  )
  expect_error(
    renvvv:::.renv_install_remaining("non_existent_pkg_bioc", biocmanager_install = TRUE, is_bioc = TRUE),
    NA
  )
  expect_error(
    renvvv:::.renv_install_remaining("non_existent_pkg_bioc_renv", biocmanager_install = FALSE, is_bioc = TRUE),
    NA
  )

})
