skip_if_not_installed("mockery")
test_that("success paths for installed_now are covered", {
  make_req_ns <- function() {
    called <- 0
    function(...) {
      called <<- called + 1
      if (called %% 2 == 1) return(FALSE)
      return(TRUE)
    }
  }

  mock_req_ns1 <- make_req_ns()
  mockery::stub(renvvv:::.renv_restore_remaining, "requireNamespace", mock_req_ns1)
  mockery::stub(renvvv:::.renv_restore_remaining, "renv::restore", function(...) NULL)

  expect_error(renvvv:::.renv_restore_remaining("dummy_pkg_1"), NA)

  mock_req_ns2 <- make_req_ns()
  mockery::stub(renvvv:::.renv_install_remaining, "requireNamespace", mock_req_ns2)
  mockery::stub(renvvv:::.renv_install_remaining, "renvvv:::.renv_install", function(...) NULL)
  mockery::stub(renvvv:::.renv_install_remaining, ".renv_install", function(...) NULL)
  mockery::stub(renvvv:::.renv_install_remaining, ".get_missing_pkgs", function(...) "dummy_pkg_2")

  expect_error(renvvv:::.renv_install_remaining("dummy_pkg_2", FALSE, FALSE), NA)
})
