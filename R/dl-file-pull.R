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
      "Drive auth unavailable; credentials file not found. Using bundled data.RDa only."
    )
    return(invisible(NULL))
  }

  tryCatch(
    googledrive::drive_auth(path = path),
    error = function(e) {
      message(
        "Drive auth unavailable; using bundled data.RDa only. ",
        conditionMessage(e)
      )
      invisible(NULL)
    }
  )
}

resolve_shared_drive_path <- function(target = shared_drive_target) {
  if (!nzchar(target)) {
    return(NULL)
  }

  tryCatch(
    if (grepl("^[A-Za-z0-9_-]{20,}$", target)) {
      googledrive::as_id(target)
    } else {
      googledrive::drive_get(target)
    },
    error = function(e) {
      message(
        "Shared Drive target could not be resolved; using bundled data.RDa only. ",
        conditionMessage(e)
      )
      NULL
    }
  )
}

get_remote_listing <- function(file) {
  tryCatch(
    if (is.null(resolve_shared_drive_path())) {
      googledrive::drive_find(pattern = paste0("^", file, "$")) |>
        googledrive::drive_reveal("modified_time")
    } else {
      googledrive::drive_ls(resolve_shared_drive_path()) |>
        dplyr::filter(.data$name == file) |>
        googledrive::drive_reveal("modified_time")
    },
    error = function(e) {
      structure(
        list(error = conditionMessage(e)),
        class = "drive_listing_error"
      )
    }
  )
}

read_drive_time <- function(path) {
  if (!file.exists(path)) {
    return(as.POSIXct(NA))
  }
  value <- suppressWarnings(as.numeric(readLines(path, n = 1, warn = FALSE)))
  if (length(value) == 0 || is.na(value)) {
    return(as.POSIXct(NA))
  }
  as.POSIXct(value, origin = "1970-01-01", tz = "UTC")
}

write_drive_time <- function(path, time) {
  writeLines(format(as.numeric(time), digits = 15), path)
}

refresh_remote_xlsx <- function(
  local_path,
  remote_name = basename(local_path),
  listing_fn = get_remote_listing,
  download_fn = googledrive::drive_download,
  force = FALSE
) {
  remote_listing <- listing_fn(remote_name)

  sidecar_path <- paste0(local_path, ".drivetime")
  stored_time <- read_drive_time(sidecar_path)
  local_time <- if (file.exists(local_path)) {
    file.info(local_path)$mtime[[1]]
  } else {
    as.POSIXct(NA)
  }
  # Prefer the last recorded Drive time over the local download mtime
  known_time <- if (is.na(stored_time)) local_time else stored_time
  remote_time <- NULL

  if (inherits(remote_listing, "drive_listing_error")) {
    message("Drive listing unavailable; keeping local file: ", local_path)
    status <- "unavailable"
  } else if (nrow(remote_listing) == 0) {
    message("No remote file found; keeping local file: ", local_path)
    status <- "not_found"
  } else {
    remote <- remote_listing |>
      dplyr::slice_max(.data$modified_time, n = 1, with_ties = FALSE)
    remote_time <- remote$modified_time[[1]]

    if (
      !force &&
        file.exists(local_path) &&
        !is.na(known_time) &&
        remote_time <= known_time
    ) {
      message("Local file is current; skipping download: ", local_path)
      status <- "current"
    } else {
      dir.create(dirname(local_path), recursive = TRUE, showWarnings = FALSE)
      download_fn(remote[1, ], path = local_path, overwrite = TRUE)
      message("Downloaded newer remote file: ", local_path)
      status <- "downloaded"
    }
    write_drive_time(sidecar_path, remote_time)
  }

  invisible(list(
    status = status,
    modified_time = if (!is.null(remote_time)) {
      remote_time
    } else {
      known_time
    }
  ))
}
