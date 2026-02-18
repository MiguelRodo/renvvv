# Helper function to check for user permission to write files
.check_write_permission <- function(force, action_description) {
  if (!force) {
    if (interactive()) {
      msg <- paste0(
        action_description,
        "\nDo you want to proceed? (y/n): "
      )
      response <- readline(prompt = msg)
      if (!tolower(trimws(response)) %in% c("y", "yes")) {
        stop("Operation cancelled by user.", call. = FALSE)
      }
    } else {
      stop(
        paste0(
          action_description,
          "\nThis function requires permission to write files. ",
          "Use force = TRUE to proceed non-interactively."
        ),
        call. = FALSE
      )
    }
  }
}

#' @title Make .Rprofile source script to make renv use right repos
#'
#' @description Copied from
#' \url{https://github.com/rstudio/renv/issues/1052#issuecomment-1342567839}.
#'
#' @param force Logical. If `FALSE` (default), prompts for confirmation in
#'   interactive sessions or errors in non-interactive sessions. Set to `TRUE`
#'   to proceed without prompting.
#'
#' @return Invisibly returns `TRUE` upon successful completion.
#'
#' @examples
#' \dontrun{
#' # Interactive mode will prompt for confirmation
#' renvvv_renv_repos_setup()
#'
#' # Non-interactive mode requires force = TRUE
#' renvvv_renv_repos_setup(force = TRUE)
#' }
#'
#' @export
renvvv_renv_repos_setup <- function(force = FALSE) {
  .check_write_permission(
    force,
    "This will create/modify .Rprofile and create scripts/R/renv_repos.R."
  )
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
