test_that("error handlers are triggered correctly when packages fail", {
  # Mock renv::install and renv::restore to always throw errors
  mockery::stub(renvvv:::.renv_restore_remaining, "renv::restore", function(...) stop("Mocked restore error"))
  mockery::stub(renvvv:::.renv_install_remaining, "renv::install", function(...) stop("Mocked install error"))
  mockery::stub(renvvv:::.renv_install_remaining, "BiocManager::install", function(...) stop("Mocked BiocManager install error"))

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

test_that(".renv_install error handler for BiocManager works", {
  # Mock requireNamespace to pretend BiocManager is installed, and cli is available
  mockery::stub(.renv_install, "requireNamespace", function(pkg, ...) {
    if (pkg == "BiocManager" || pkg == "cli") return(TRUE)
    return(FALSE)
  })

  # Mock BiocManager::install to throw an error
  mock_install <- function(...) stop("Mocked BiocManager install error")
  mockery::stub(.renv_install, "BiocManager::install", mock_install)

  # Mock cli_alert_danger to capture its call
  mock_danger <- mockery::mock()
  mockery::stub(.renv_install, "cli::cli_alert_danger", mock_danger)

  # Execute deliberately failing install through .renv_install
  expect_error(
    .renv_install("fake_bioc_pkg", biocmanager_install = TRUE, is_bioc = TRUE),
    NA
  )

  # Verify that cli::cli_alert_danger was called correctly
  mockery::expect_called(mock_danger, 1)

  # Verify the arguments passed to cli_alert_danger contained our message
  args <- mockery::mock_args(mock_danger)[[1]]
  expect_match(args[[1]], "Failed to install Bioconductor packages using BiocManager")
})
