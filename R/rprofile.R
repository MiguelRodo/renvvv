#' @title Make .Rprofile source script to make renv use scratch directory
#'
#' @export
setup_hpc_renv <- function() {
  if (!file.exists(".Rprofile")) {
    file.create(".Rprofile")
  }
  txt_rprofile <- readLines(".Rprofile")
  txt_rprofile <- c(
    txt_rprofile,
    "\n# make renv use scratch directory",
    'slurm_ind <- any(grepl("^SLURM_", names(Sys.getenv())))',
    'dir_exists_ind <- dir.exists(file.path("/scratch/", Sys.getenv("USER")))',
    "if (slurm_ind && dir_exists_ind) {",
    '  source("./scripts/R/hpc_renv_setup.R")',
    "}"
  )
  writeLines(txt_rprofile, ".Rprofile")
  if (!dir.exists(file.path("scripts", "R"))) {
    dir.create(file.path("scripts", "R"), recursive = TRUE)
  }
  file.copy(
    system.file("scripts", "hpc_renv_setup.R", package = "renvvv"),
    "scripts/R/hpc_renv_setup.R"
  )
  slurm_ind <- any(grepl("^SLURM_", names(Sys.getenv())))
  dir_exists_ind <- dir.exists(file.path("/scratch/", Sys.getenv("USER")))
  if (slurm_ind && dir_exists_ind) {
    source("./scripts/R/hpc_renv_setup.R")
  }
  invisible(TRUE)
}

#' @title Make .Rprofile source script to make renv use right repos
#'
#' @description Copied from
#' \url{https://github.com/rstudio/renv/issues/1052#issuecomment-1342567839}.
#'
#' @export
setup_renv_repos <- function() {
  if (!file.exists(".Rprofile")) {
    file.create(".Rprofile")
  }
  txt_rprofile <- readLines(".Rprofile")
  txt_rprofile <- c(
    txt_rprofile,
    "\n# enable renv to use binaries across OSs",
    'source("./scripts/R/renv_repos.R")'
  )
  writeLines(txt_rprofile, ".Rprofile")
  if (!dir.exists(file.path("scripts", "R"))) {
    dir.create(file.path("scripts", "R"), recursive = TRUE)
  }
  file.copy(
    system.file("scripts", "renv_repos.R", package = "renvvv"),
    "scripts/R/renv_repos.R"
  )
  source("scripts/R/renv_repos.R")
  invisible(TRUE)
}
