suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(rvest)
  library(RCurl)
  library(XML)
  library(googlesheets4)
  library(fuzzyjoin)
  library(crayon)
  library(httr)
})

credentials_path <- Sys.getenv(
  "DREAMLEAGUE_GOOGLE_CREDENTIALS",
  "credentials.json"
)
shared_drive_target <- Sys.getenv(
  "DREAMLEAGUE_SHARED_DRIVE_TARGET",
  ""
)
options(gargle_oauth_cache = ".secrets", gargle_oauth_email = TRUE)

try_drive_auth <- function(path = credentials_path) {
  if (!file.exists(path)) {
    message(
      "Drive auth unavailable; credentials file not found."
    )
    return(invisible(FALSE))
  }

  tryCatch(
    {
      googledrive::drive_auth(path = path)
      invisible(TRUE)
    },
    error = function(e) {
      message(
        "Drive auth unavailable: ",
        conditionMessage(e)
      )
      invisible(FALSE)
    }
  )
}

resolve_shared_drive_path <- function(target = shared_drive_target) {
  if (!nzchar(target)) {
    return(NULL)
  }

  if (grepl("^[A-Za-z0-9_-]{20,}$", target)) {
    googledrive::as_id(target)
  } else {
    googledrive::drive_get(target)
  }
}

remote_xlsx_pattern <- function(file) {
  stem <- tools::file_path_sans_ext(basename(file))
  escaped_stem <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", stem)
  paste0("^", escaped_stem, "(\\s*\\([0-9]+\\))?\\.[xX][lL][sS][xX]$")
}

get_remote_listing <- function(file) {
  pattern <- remote_xlsx_pattern(file)
  tryCatch(
    {
      shared_path <- resolve_shared_drive_path()
      if (is.null(shared_path)) {
        googledrive::drive_find(pattern = pattern) |>
          googledrive::drive_reveal("modified_time")
      } else {
        googledrive::drive_ls(shared_path) |>
          dplyr::filter(grepl(pattern, .data$name)) |>
          googledrive::drive_reveal("modified_time")
      }
    },
    error = function(e) {
      structure(
        list(error = conditionMessage(e)),
        class = "drive_listing_error"
      )
    }
  )
}

refresh_remote_xlsx <- function(
  local_path,
  remote_name = basename(local_path),
  listing_fn = get_remote_listing,
  download_fn = googledrive::drive_download,
  force = FALSE,
  local_test = FALSE
) {
  if (local_test) {
    if (force || identical(Sys.getenv("GITHUB_ACTIONS"), "true")) {
      stop("Local XLSX testing mode cannot be combined with --force or GitHub Actions.", call. = FALSE)
    }
    if (!file.exists(local_path)) {
      stop("Local test XLSX not found: ", local_path, call. = FALSE)
    }
    message("Using local XLSX for testing only: ", local_path)
    return(invisible(list(
      status = "local_test",
      modified_time = file.info(local_path)$mtime[[1]]
    )))
  }
  remote_listing <- listing_fn(remote_name)
  if (inherits(remote_listing, "drive_listing_error")) {
    stop("Drive listing unavailable for ", remote_name, ": ", remote_listing$error, call. = FALSE)
  }
  remote <- remote_listing |>
    dplyr::filter(grepl(remote_xlsx_pattern(remote_name), .data$name)) |>
    dplyr::slice_max(.data$modified_time, n = 1, with_ties = FALSE)
  if (nrow(remote) == 0) {
    stop("Google Drive XLSX not found: ", remote_name, call. = FALSE)
  }
  dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
  download_path <- tempfile(tmpdir = dirname(local_path), fileext = ".xlsx")
  on.exit(unlink(download_path), add = TRUE)
  download_fn(remote, path = download_path, overwrite = TRUE)
  if (!file.exists(download_path) || file.info(download_path)$size[[1]] == 0) {
    stop("Google Drive download produced an empty XLSX: ", remote_name, call. = FALSE)
  }
  if (!file.copy(download_path, local_path, overwrite = TRUE)) {
    stop("Could not save downloaded XLSX: ", local_path, call. = FALSE)
  }
  message("Downloaded Google Drive XLSX: ", remote$name[[1]])
  invisible(list(
    status = "downloaded",
    modified_time = remote$modified_time[[1]]
  ))
}
