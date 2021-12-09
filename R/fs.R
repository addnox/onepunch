#' Make folder under working directory.
#'
#' @export

dir_make <- function(path, wd = NULL) {
  if (is.null(wd)) {
    path_full <- path
  } else {
    path_full <- fs::path(wd, path)
  }

  if (fs::dir_exists(path_full)) {
    return(invisible(FALSE))
  }
  else if (fs::file_exists(path_full)) {
    cli::cli_abort(paste0(path_full, " exists but is not a directory."))
  }

  fs::dir_create(path_full, recurse = TRUE)
  cli::cli_alert_success(paste0("Created ", path))
  invisible(TRUE)
}

#' Source all the r codes in one folder
#'
#' @export

dir_source <- function(path, recurse = FALSE, verbose = TRUE) {
  if (fs::dir_exists(path)) {
    all_R_files <- fs::dir_ls(path, recurse = recurse, type = "file", regexp = "*.[rR]")
    suppressMessages(lapply(all_R_files, source, encoding = "UTF-8", verbose = FALSE))
  }

  if (verbose) {
    ## show sourced files
    usethis::ui_info("R Files sourced:")
    lapply(fs::path_file(all_R_files), function(x) usethis::ui_done(usethis::ui_field(x)))
  }

  invisible(TRUE)
}
