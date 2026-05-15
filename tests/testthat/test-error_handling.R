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

test_that(".renv_lockfile_deps_get handles lockfile path error gracefully", {
  # Mock renv::paths$lockfile to throw an error. mockery::stub can handle "renv::paths$lockfile".
  mockery::stub(.renv_lockfile_deps_get, "renv::paths$lockfile", function(...) stop("Mocked lockfile path error"))

  # The error should be caught internally, returning an empty list.
  # cli::cli_alert_warning issues messages, not warnings.
  expect_message(
    result <- .renv_lockfile_deps_get(),
    "Could not determine lockfile path"
  )
  expect_type(result, "list")
  expect_length(result, 0)
})

test_that(".renv_lockfile_deps_get handles lockfile read error gracefully", {
  # Mock lockfile path so file.exists passes
  tmp_lock <- tempfile("mock_renv_", fileext = ".lock")
  file.create(tmp_lock)
  on.exit(unlink(tmp_lock))

  mockery::stub(.renv_lockfile_deps_get, "renv::paths$lockfile", function(...) tmp_lock)

  # Mock renv::lockfile_read to throw an error
  mockery::stub(.renv_lockfile_deps_get, "renv::lockfile_read", function(...) stop("Mocked lockfile read error"))

  # The error should be caught internally, returning an empty list
  expect_message(
    result <- .renv_lockfile_deps_get(),
    "Could not read lockfile Packages"
  )
  expect_type(result, "list")
  expect_length(result, 0)
})
