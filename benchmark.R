library(microbenchmark)

# Mock lockfile_list_pkg
set.seed(42)
n_pkgs <- 1000
lockfile_list_pkg <- lapply(1:n_pkgs, function(i) {
  r <- runif(1)
  if (r < 0.1) {
    list(Source = "github", RemoteUsername = paste0("user", i))
  } else if (r < 0.3) {
    list(Source = "bioconductor")
  } else {
    list(Source = "cran")
  }
})
names(lockfile_list_pkg) <- paste0("pkg", 1:n_pkgs)

# Current method
method_current <- function() {
  pkg_vec_regular <- character()
  pkg_vec_bioc <- character()
  pkg_vec_gh <- character()

  for (package_name in names(lockfile_list_pkg)) {
    package_info <- lockfile_list_pkg[[package_name]]
    remote_username <- package_info$RemoteUsername
    source <- tolower(package_info$Source)

    if (is.null(remote_username)) {
      is_bioc <- grepl("bioc", source)
      if (is_bioc) {
        pkg_vec_bioc <- c(pkg_vec_bioc, package_name)
      } else {
        pkg_vec_regular <- c(pkg_vec_regular, package_name)
      }
    } else {
      pkg_vec_gh <- c(pkg_vec_gh, paste0(remote_username, "/", package_name))
    }
  }

  list(
    regular = pkg_vec_regular,
    bioc = pkg_vec_bioc,
    gh = pkg_vec_gh
  )
}

# New method
method_new <- function() {
  pkg_names <- names(lockfile_list_pkg)

  remote_usernames <- vapply(lockfile_list_pkg, function(x) {
    res <- x$RemoteUsername
    if (is.null(res)) "" else res
  }, character(1), USE.NAMES = FALSE)

  sources <- vapply(lockfile_list_pkg, function(x) {
    res <- x$Source
    if (is.null(res)) "" else tolower(res)
  }, character(1), USE.NAMES = FALSE)

  is_gh <- remote_usernames != ""
  is_bioc <- !is_gh & grepl("bioc", sources)
  is_regular <- !is_gh & !is_bioc

  list(
    regular = pkg_names[is_regular],
    bioc = pkg_names[is_bioc],
    gh = paste0(remote_usernames[is_gh], "/", pkg_names[is_gh])
  )
}

res_current <- method_current()
res_new <- method_new()

print(identical(res_current, res_new))

mb <- microbenchmark(
  current = method_current(),
  new = method_new(),
  times = 100
)
print(mb)
