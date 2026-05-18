testthat::skip_if_not_installed("mockery")

test_that("error handlers are triggered correctly when packages fail", {
  skip_if_not_installed("mockery")
  
  # Mock renv::restore to throw an error
  mockery::stub(.renv_restore_remaining, "renv::restore", function(...) stop("Mocked restore error"))
  
  # Mock .renv_install inside .renv_install_remaining so it doesn't actually call it
  mockery::stub(.renv_install_remaining, ".renv_install", function(...) stop("Mocked install error"))

  # Execute a deliberately failing restore and verify the expected message
  expect_message(
    expect_error(
      .renv_restore_remaining("non_existent_pkg_1"),
      NA # we expect it to swallow the error internally and continue
    ),
    "Failed to restore package: .*non_existent_pkg_1.*Error: Mocked restore error"
  )

  # Execute deliberately failing installs
  expect_error(
    .renv_install_remaining("non_existent_pkg_2", biocmanager_install = FALSE, is_bioc = FALSE),
    NA
  )
  expect_error(
    .renv_install_remaining("non_existent_pkg_bioc", biocmanager_install = TRUE, is_bioc = TRUE),
    NA
  )
  expect_error(
    .renv_install_remaining("non_existent_pkg_bioc_renv", biocmanager_install = FALSE, is_bioc = TRUE),
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

test_that(".renv_lockfile_read_pkgs handles lockfile path error gracefully", {
  # Mock renv::paths$lockfile to throw an error
  mockery::stub(.renv_lockfile_read_pkgs, "renv::paths", list(lockfile = function(...) stop("Mocked lockfile path error")))

  # The error should be caught internally, silently returning an empty list.
  result <- .renv_lockfile_read_pkgs()
  
  expect_type(result, "list")
  expect_length(result, 0)
})

test_that(".renv_lockfile_read_pkgs handles lockfile read error gracefully", {
  # Mock lockfile path so file.exists passes
  tmp_lock <- tempfile("mock_renv_", fileext = ".lock")
  file.create(tmp_lock)
  on.exit(unlink(tmp_lock))

  mockery::stub(.renv_lockfile_read_pkgs, "renv::paths", list(lockfile = function(...) tmp_lock))

  # Mock renv::lockfile_read to throw an error
  mockery::stub(.renv_lockfile_read_pkgs, "renv::lockfile_read", function(...) stop("Mocked lockfile read error"))

  # The error should be caught internally, silently returning an empty list
  result <- .renv_lockfile_read_pkgs()
  
  expect_type(result, "list")
  expect_length(result, 0)
})