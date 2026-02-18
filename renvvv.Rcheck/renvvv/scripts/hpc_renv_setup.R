Sys.setenv(
  "RENV_PATHS_CACHE" = paste0(
    "/scratch/", Sys.getenv("USER"),
    "/.local/R/lib/renv"
  )
)
Sys.setenv(
  "RENV_PATHS_ROOT" = paste0(
    "/scratch/", Sys.getenv("USER"),
    "/.cache/R/renv"
  )
)
Sys.setenv(
  "RENV_PATHS_LIBRARY_ROOT" = paste0(
    "/scratch/", Sys.getenv("USER"),
    "/.cache/R/renv"
  )
)
Sys.setenv(
  "RENV_PATHS_LIBRARY" = paste0(
    "/scratch/", Sys.getenv("USER"),
    "/.cache/R/renv"
  )
)

Sys.setenv(
  "RENV_PATHS_PREFIX_AUTO" = "TRUE"
)
