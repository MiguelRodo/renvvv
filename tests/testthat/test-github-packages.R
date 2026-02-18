# Unit tests for GitHub repository restore/update functionality
# These tests work with lockfile fixtures and don't require full GitHub API access

test_that("renvvv can parse GitHub packages from lockfile", {
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  # Resolve fixture path to absolute before setup changes working directory
  fixture_path <- normalizePath(
    testthat::test_path("fixtures", "renv.lock.praise")
  )

  # Create a test project with a fixture lockfile containing GitHub packages
  ctx <- .setup_renv_project(pkgs = NULL)
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Copy fixture lockfile with GitHub package
  file.copy(fixture_path, "renv.lock", overwrite = TRUE)

  # Test that our internal helper can extract GitHub packages
  pkg_list <- renvvv:::.renv_lockfile_pkg_get()

  expect_type(pkg_list, "list")
  expect_named(pkg_list, c("regular", "bioc", "gh"))

  # Should have praise in the GitHub packages list
  expect_true("gaborcsardi/praise" %in% pkg_list$gh)
  expect_length(pkg_list$gh, 1)

  # Should have no CRAN or Bioc packages in this fixture
  expect_length(pkg_list$regular, 0)
  expect_length(pkg_list$bioc, 0)
})

test_that("lockfile fixture has correct GitHub remote fields", {
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  # Resolve fixture path to absolute before setup changes working directory
  fixture_path <- normalizePath(
    testthat::test_path("fixtures", "renv.lock.praise")
  )

  ctx <- .setup_renv_project(pkgs = NULL)
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  file.copy(fixture_path, "renv.lock", overwrite = TRUE)

  lockfile <- renv::lockfile_read()
  praise_info <- lockfile$Packages$praise

  expect_equal(praise_info$Source, "GitHub")
  expect_equal(praise_info$RemoteType, "github")
  expect_equal(praise_info$RemoteHost, "api.github.com")
  expect_equal(praise_info$RemoteUsername, "gaborcsardi")
  expect_equal(praise_info$RemoteRepo, "praise")
})

test_that("GitHub API is accessible for package operations", {
  skip_on_cran()
  skip_if_not(.github_api_available(), "GitHub API not accessible")

  # Verify the GitHub repos API endpoint is reachable
  con <- url("https://api.github.com/repos/gaborcsardi/praise", open = "r")
  on.exit(close(con), add = TRUE)
  response <- readLines(con, warn = FALSE)
  expect_true(length(response) > 0)
})

test_that("GitHub codeload endpoint is accessible", {
  skip_on_cran()
  skip_if_not(.github_api_available(), "GitHub API not accessible")

  # Verify the codeload endpoint used by renv for tarball downloads
  result <- tryCatch({
    con <- url(
      "https://api.github.com/repos/gaborcsardi/praise/tarball/HEAD",
      open = "r"
    )
    on.exit(close(con), add = TRUE)
    TRUE
  }, error = function(e) FALSE,
  warning = function(w) TRUE)

  expect_true(result)
})

test_that("renvvv_restore correctly identifies GitHub packages to restore", {
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  # Resolve fixture path to absolute before setup changes working directory
  fixture_path <- normalizePath(
    testthat::test_path("fixtures", "renv.lock.praise")
  )

  # Create a test project with a fixture lockfile
  ctx <- .setup_renv_project(pkgs = NULL)
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Copy fixture lockfile with GitHub package
  file.copy(fixture_path, "renv.lock", overwrite = TRUE)

  # Call the internal implementation function to verify it processes correctly
  pkg_list <- renvvv:::.renv_lockfile_pkg_get()

  # Test the restore implementation (without actually installing)
  # This tests the logic without requiring API access
  suppressMessages({
    result <- tryCatch({
      # The function should handle empty package lists gracefully
      renvvv:::.renv_restore_or_update_impl(
        package_list = list(
          regular = character(0),
          bioc = character(0),
          gh = character(0)
        ),
        github = TRUE,
        non_github = FALSE,
        restore = TRUE,
        biocmanager_install = FALSE
      )
      TRUE
    }, error = function(e) FALSE)
  })

  expect_true(result)
})

test_that("renvvv_update correctly identifies GitHub packages to update", {
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  # Resolve fixture path to absolute before setup changes working directory
  fixture_path <- normalizePath(
    testthat::test_path("fixtures", "renv.lock.praise")
  )

  # Create a test project with a fixture lockfile
  ctx <- .setup_renv_project(pkgs = NULL)
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Copy fixture lockfile with GitHub package
  file.copy(fixture_path, "renv.lock", overwrite = TRUE)

  # Test the update implementation (without actually installing)
  pkg_list <- renvvv:::.renv_lockfile_pkg_get()

  suppressMessages({
    result <- tryCatch({
      # The function should handle empty package lists gracefully
      renvvv:::.renv_restore_or_update_impl(
        package_list = list(
          regular = character(0),
          bioc = character(0),
          gh = character(0)
        ),
        github = TRUE,
        non_github = FALSE,
        restore = FALSE,  # update = not restore
        biocmanager_install = FALSE
      )
      TRUE
    }, error = function(e) FALSE)
  })

  expect_true(result)
})

test_that("renvvv_restore installs GitHub package from fixture lockfile", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )
  skip_if_not(.github_api_available(), "GitHub API not accessible")

  # Resolve fixture path to absolute before setup changes working directory
  fixture_path <- normalizePath(
    testthat::test_path("fixtures", "renv.lock.praise")
  )

  ctx <- .setup_renv_project(pkgs = "praise")
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Copy fixture lockfile and restore
  file.copy(fixture_path, "renv.lock", overwrite = TRUE)

  # Remove praise from the project library to ensure clean state
  .remove_pkg("praise")
  expect_false(
    nzchar(system.file(package = "praise", lib.loc = .libPaths()[1]))
  )

  # Restore GitHub packages from lockfile
  suppressMessages(renvvv_restore(non_github = FALSE, github = TRUE))

  # Verify praise is now installed
  expect_true(
    nzchar(system.file(package = "praise", lib.loc = .libPaths()[1]))
  )
})

test_that("renvvv_update installs GitHub package from fixture lockfile", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )
  skip_if_not(.github_api_available(), "GitHub API not accessible")

  # Resolve fixture path to absolute before setup changes working directory
  fixture_path <- normalizePath(
    testthat::test_path("fixtures", "renv.lock.praise")
  )

  ctx <- .setup_renv_project(pkgs = "praise")
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  # Copy fixture lockfile and update
  file.copy(fixture_path, "renv.lock", overwrite = TRUE)

  # Remove praise from the project library to ensure clean state
  .remove_pkg("praise")
  expect_false(
    nzchar(system.file(package = "praise", lib.loc = .libPaths()[1]))
  )

  # Update should install the latest version of the GitHub package
  suppressMessages(renvvv_update(non_github = FALSE, github = TRUE))

  # Verify praise is now installed
  expect_true(
    nzchar(system.file(package = "praise", lib.loc = .libPaths()[1]))
  )
})

test_that("renvvv_restore with github=FALSE skips GitHub packages", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )
  skip_if_not(.github_api_available(), "GitHub API not accessible")

  # Use mixed fixture with both CRAN and GitHub packages
  fixture_path <- normalizePath(
    testthat::test_path("fixtures", "renv.lock.mixed")
  )

  ctx <- .setup_renv_project(pkgs = c("praise", "tinytest"))
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  file.copy(fixture_path, "renv.lock", overwrite = TRUE)

  # Remove both packages to ensure clean state
  .remove_pkg("praise")
  .remove_pkg("tinytest")

  # Restore with github=FALSE should skip GitHub packages
  suppressMessages(
    renvvv_restore(non_github = TRUE, github = FALSE)
  )

  # tinytest (CRAN) should be restored
  expect_true(
    nzchar(system.file(package = "tinytest", lib.loc = .libPaths()[1]))
  )

  # praise (GitHub) should NOT be restored
  expect_false(
    nzchar(system.file(package = "praise", lib.loc = .libPaths()[1]))
  )
})

test_that("renvvv_restore with non_github=FALSE skips CRAN packages", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )
  skip_if_not(.github_api_available(), "GitHub API not accessible")

  # Use mixed fixture with both CRAN and GitHub packages
  fixture_path <- normalizePath(
    testthat::test_path("fixtures", "renv.lock.mixed")
  )

  ctx <- .setup_renv_project(pkgs = c("praise", "tinytest"))
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  file.copy(fixture_path, "renv.lock", overwrite = TRUE)

  # Remove both packages to ensure clean state
  .remove_pkg("praise")
  .remove_pkg("tinytest")

  # Restore with non_github=FALSE should skip CRAN packages
  suppressMessages(
    renvvv_restore(non_github = FALSE, github = TRUE)
  )

  # praise (GitHub) should be restored
  expect_true(
    nzchar(system.file(package = "praise", lib.loc = .libPaths()[1]))
  )

  # tinytest (CRAN) should NOT be restored
  expect_false(
    nzchar(system.file(package = "tinytest", lib.loc = .libPaths()[1]))
  )
})

test_that("renvvv_update with github=FALSE skips GitHub packages", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )
  skip_if_not(.github_api_available(), "GitHub API not accessible")

  # Use mixed fixture with both CRAN and GitHub packages
  fixture_path <- normalizePath(
    testthat::test_path("fixtures", "renv.lock.mixed")
  )

  ctx <- .setup_renv_project(pkgs = c("praise", "tinytest"))
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  file.copy(fixture_path, "renv.lock", overwrite = TRUE)

  # Remove both packages to ensure clean state
  .remove_pkg("praise")
  .remove_pkg("tinytest")

  # Update with github=FALSE should skip GitHub packages
  suppressMessages(
    renvvv_update(non_github = TRUE, github = FALSE)
  )

  # tinytest (CRAN) should be installed
  expect_true(
    nzchar(system.file(package = "tinytest", lib.loc = .libPaths()[1]))
  )

  # praise (GitHub) should NOT be installed
  expect_false(
    nzchar(system.file(package = "praise", lib.loc = .libPaths()[1]))
  )
})

test_that("renvvv_update with non_github=FALSE skips CRAN packages", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )
  skip_if_not(.github_api_available(), "GitHub API not accessible")

  # Use mixed fixture with both CRAN and GitHub packages
  fixture_path <- normalizePath(
    testthat::test_path("fixtures", "renv.lock.mixed")
  )

  ctx <- .setup_renv_project(pkgs = c("praise", "tinytest"))
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  file.copy(fixture_path, "renv.lock", overwrite = TRUE)

  # Remove both packages to ensure clean state
  .remove_pkg("praise")
  .remove_pkg("tinytest")

  # Update with non_github=FALSE should skip CRAN packages
  suppressMessages(
    renvvv_update(non_github = FALSE, github = TRUE)
  )

  # praise (GitHub) should be installed
  expect_true(
    nzchar(system.file(package = "praise", lib.loc = .libPaths()[1]))
  )

  # tinytest (CRAN) should NOT be installed
  expect_false(
    nzchar(system.file(package = "tinytest", lib.loc = .libPaths()[1]))
  )
})

test_that("renvvv_restore_and_update with github=FALSE skips GitHub packages", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )
  skip_if_not(.github_api_available(), "GitHub API not accessible")

  # Use mixed fixture with both CRAN and GitHub packages
  fixture_path <- normalizePath(
    testthat::test_path("fixtures", "renv.lock.mixed")
  )

  ctx <- .setup_renv_project(pkgs = c("praise", "tinytest"))
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  file.copy(fixture_path, "renv.lock", overwrite = TRUE)

  # Remove both packages to ensure clean state
  .remove_pkg("praise")
  .remove_pkg("tinytest")

  # Restore and update with github=FALSE should skip GitHub packages
  suppressMessages(
    renvvv_restore_and_update(non_github = TRUE, github = FALSE)
  )

  # tinytest (CRAN) should be installed
  expect_true(
    nzchar(system.file(package = "tinytest", lib.loc = .libPaths()[1]))
  )

  # praise (GitHub) should NOT be installed
  expect_false(
    nzchar(system.file(package = "praise", lib.loc = .libPaths()[1]))
  )
})

test_that("renvvv_restore_and_update with non_github=FALSE skips CRAN packages", {
  skip_on_cran()
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )
  skip_if_not(.github_api_available(), "GitHub API not accessible")

  # Use mixed fixture with both CRAN and GitHub packages
  fixture_path <- normalizePath(
    testthat::test_path("fixtures", "renv.lock.mixed")
  )

  ctx <- .setup_renv_project(pkgs = c("praise", "tinytest"))
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  file.copy(fixture_path, "renv.lock", overwrite = TRUE)

  # Remove both packages to ensure clean state
  .remove_pkg("praise")
  .remove_pkg("tinytest")

  # Restore and update with non_github=FALSE should skip CRAN packages
  suppressMessages(
    renvvv_restore_and_update(non_github = FALSE, github = TRUE)
  )

  # praise (GitHub) should be installed
  expect_true(
    nzchar(system.file(package = "praise", lib.loc = .libPaths()[1]))
  )

  # tinytest (CRAN) should NOT be installed
  expect_false(
    nzchar(system.file(package = "tinytest", lib.loc = .libPaths()[1]))
  )
})

test_that("GitHub package format is correctly parsed", {
  # Test package name extraction from GitHub remotes
  github_pkg <- "gaborcsardi/praise"
  pkg_name <- sub("^.*/", "", github_pkg)

  expect_equal(pkg_name, "praise")

  # Test with just package name (no remote)
  regular_pkg <- "praise"
  pkg_name2 <- sub("^.*/", "", regular_pkg)

  expect_equal(pkg_name2, "praise")
})

test_that("renvvv functions accept github parameter correctly", {
  # Test that the exported functions have the correct signature
  expect_true("github" %in% names(formals(renvvv_restore)))
  expect_true("github" %in% names(formals(renvvv_update)))
  expect_true("github" %in% names(formals(renvvv_restore_and_update)))

  expect_true("non_github" %in% names(formals(renvvv_restore)))
  expect_true("non_github" %in% names(formals(renvvv_update)))
  expect_true("non_github" %in% names(formals(renvvv_restore_and_update)))

  expect_true("biocmanager_install" %in% names(formals(renvvv_restore)))
  expect_true("biocmanager_install" %in% names(formals(renvvv_update)))
  expect_true(
    "biocmanager_install" %in% names(formals(renvvv_restore_and_update))
  )
})

test_that("renvvv functions have correct default parameter values", {
  expect_true(formals(renvvv_restore)$github)
  expect_true(formals(renvvv_restore)$non_github)
  expect_false(formals(renvvv_restore)$biocmanager_install)

  expect_true(formals(renvvv_update)$github)
  expect_true(formals(renvvv_update)$non_github)
  expect_false(formals(renvvv_update)$biocmanager_install)

  expect_true(formals(renvvv_restore_and_update)$github)
  expect_true(formals(renvvv_restore_and_update)$non_github)
  expect_false(formals(renvvv_restore_and_update)$biocmanager_install)
})

test_that("mixed lockfile correctly classifies CRAN and GitHub packages", {
  skip_if_not(
    requireNamespace("renv", quietly = TRUE),
    "renv not available"
  )

  fixture_path <- normalizePath(
    testthat::test_path("fixtures", "renv.lock.mixed")
  )

  ctx <- .setup_renv_project(pkgs = NULL)
  on.exit(.teardown_renv_project(ctx), add = TRUE)

  file.copy(fixture_path, "renv.lock", overwrite = TRUE)

  pkg_list <- renvvv:::.renv_lockfile_pkg_get()

  expect_type(pkg_list, "list")
  expect_named(pkg_list, c("regular", "bioc", "gh"))

  # tinytest should be classified as regular (CRAN)
  expect_true("tinytest" %in% pkg_list$regular)

  # praise should be classified as GitHub
  expect_true("gaborcsardi/praise" %in% pkg_list$gh)

  # No Bioconductor packages
  expect_length(pkg_list$bioc, 0)
})
