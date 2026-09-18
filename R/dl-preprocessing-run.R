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
a <- Sys.time()

source("R/dl-file-pull.R")
source("R/dl-preprocessing-fast.R")
source("R/validate-dreamleague-data.R")
source("R/supabase-storage.R")

try_drive_auth()
credentials_path <- Sys.getenv(
  "DREAMLEAGUE_GOOGLE_CREDENTIALS",
  "credentials.json"
)
shared_drive_target <- Sys.getenv("DREAMLEAGUE_SHARED_DRIVE_TARGET", "")
safe_gs4_auth <- function(path = credentials_path) {
  if (!file.exists(path)) {
    message("Google Sheets auth unavailable; credentials file not found.")
    return(invisible(NULL))
  }

  tryCatch(
    gs4_auth(path = path),
    error = function(e) {
      message(
        "Google Sheets auth unavailable; continuing without auth. ",
        conditionMessage(e)
      )
      invisible(NULL)
    }
  )
}

safe_gs4_auth()


file_d <- "data/DreamLeague26-27.xlsx"
refresh_remote_xlsx(file_d)
dl_d <- readxl::read_excel(
  file_d,
  na = c("SOLD"),
  sheet = "Stats",
  skip = 0,
  col_names = F
) |>
  suppressMessages() |>
  dplyr::select(2:8)
managers_d <- readxl::read_excel(file_d, na = c("SOLD"), sheet = "Stats") |>
  suppressMessages() |>
  dplyr::select(11:12) |>
  na.omit() |>
  rename(manager = 1, team = 2) |>
  filter(team != "TEAM")

mod_d <- file.info(file_d)$mtime
cat("Didsbury\n")

out_d <- dl_process(dl_d, managers_d, "Didsbury", season_id = 159)


file_o <- "data/DL26-27.xlsx"
refresh_remote_xlsx(file_o)
dl_o <- readxl::read_excel(
  file_o,
  na = c(""),
  sheet = "Stats",
  skip = 0,
  col_names = F
) |>
  suppressMessages() |>
  dplyr::select(1:7)
managers_o <- readxl::read_excel(
  file_o,
  na = c("SOLD"),
  sheet = "Table",
  skip = 4
) |>
  suppressMessages() |>
  dplyr::select(c(2, 4)) |>
  rename(manager = 1, team = 2) |>
  filter(team != "TEAM")

mod_o <- file.info(file_o)$mtime
cat("Original\n")
dl_o <- dl_o |>
  mutate(
    `...7` = case_when(
      `...2` == "JAMES TRAFFORD" ~ "45529",
      `...2` == "NONI MADUEKE" ~ "45530",
      T ~ `...7`
    )
  )


out_o <- dl_process(dl_o, managers_o, "Original", season_id = 159)


time <- list("update_time" = Sys.time(), "mod_d" = mod_d, "mod_o" = mod_o)

cupties <- read.csv("data/cupties.csv") |>
  mutate(date = as.Date(date, format = "%d/%m/%Y")) |>
  mutate(across(where(is.character), trimws))

managers <- rbind.data.frame(
  managers_d |> mutate(league = "didsbury"),
  managers_o |> mutate(league = "original")
)
save(managers_d, managers_o, file = "dreamleague/managers.RDa")

googledrive::drive_auth(
  # email = TRUE,
  path = credentials_path,
  subject = NULL,
  scopes = "drive",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)


shared_drive_path <- if (nzchar(shared_drive_target)) {
  if (grepl("^[A-Za-z0-9_-]{20,}$", shared_drive_target)) {
    googledrive::as_id(shared_drive_target)
  } else {
    googledrive::drive_get(shared_drive_target)
  }
} else {
  warning(
    "DREAMLEAGUE_SHARED_DRIVE_TARGET is not set; skipping Drive upload.",
    call. = FALSE
  )
  NULL
}

upload_to_drive <- function(local_file, remote_name) {
  if (is.null(shared_drive_path)) {
    return(invisible(NULL))
  }

  existing <- tryCatch(
    googledrive::drive_ls(shared_drive_path) |>
      dplyr::filter(.data$name == remote_name) |>
      dplyr::slice_head(n = 1),
    error = function(e) NULL
  )

  tryCatch(
    if (!is.null(existing) && nrow(existing) > 0) {
      googledrive::drive_update(existing[1, ], media = local_file)
    } else {
      googledrive::drive_upload(
        local_file,
        path = shared_drive_path,
        name = remote_name
      )
    },
    error = function(e) {
      if (
        grepl(
          "storageQuotaExceeded|Service Accounts do not have storage quota",
          conditionMessage(e)
        )
      ) {
        warning(
          paste0(
            "Skipping Drive upload for ",
            remote_name,
            ": ",
            conditionMessage(e),
            " Use a shared drive target or OAuth delegation instead."
          ),
          call. = FALSE
        )
        return(invisible(NULL))
      }
      stop(e)
    }
  )
}

normalize_daily_schema <- function(daily) {
  if ("App" %in% names(daily) && !"SBapp" %in% names(daily)) {
    daily$SBapp <- daily$App
  }
  if ("SBapp" %in% names(daily) && !"App" %in% names(daily)) {
    daily$App <- daily$SBapp
  }
  daily
}

run_data_shape_tests <- function(dl, daily, time, cupties, managers) {
  options(
    dreamleague.test_bundle = list(
      dl = dl,
      daily = daily,
      time = time,
      cupties = cupties,
      managers = managers
    )
  )
  on.exit(options(dreamleague.test_bundle = NULL), add = TRUE)

  testthat::test_file(
    "tests/testthat/test-validate-dreamleague-data.R",
    reporter = "summary",
    stop_on_failure = TRUE
  )

  invisible(TRUE)
}

if (out_d$cut_time == Sys.Date() & out_o$cut_time == Sys.Date()) {
  dl <- rbind.data.frame(
    out_d$scores |> mutate(league = "didsbury"),
    out_o$scores |> mutate(league = "original")
  )

  daily <- rbind.data.frame(
    out_d$daily |> mutate(league = "didsbury"),
    out_o$daily |> mutate(league = "original")
  )

  daily <- normalize_daily_schema(daily)

  validate_dreamleague_bundle(dl, daily, time, cupties, managers)
  run_data_shape_tests(dl, daily, time, cupties, managers)

  bundle_path <- "dreamleague/data.RDa"
  save(dl, daily, time, cupties, file = bundle_path)

  bundle <- new.env(parent = emptyenv())
  load(bundle_path, envir = bundle)
  if (!exists("cupties", envir = bundle, inherits = FALSE)) {
    stop("The published data bundle does not contain cupties", call. = FALSE)
  }

  for (i in names(out_d)) {
    write.csv(out_d[[i]], glue::glue("data/diagnostics/didsbury_{i}.csv"))
    write.csv(out_o[[i]], glue::glue("data/diagnostics/original_{i}.csv"))
  }

  drive_published <- tryCatch(
    {
      if (is.null(shared_drive_path)) {
        warning(
          "Google Drive publication was skipped: no shared drive is configured.",
          call. = FALSE
        )
        FALSE
      } else {
        upload_to_drive(bundle_path, "data.RDa")
        TRUE
      }
    },
    error = function(e) {
      warning(
        "Google Drive publication was not completed: ",
        conditionMessage(e),
        call. = FALSE
      )
      FALSE
    }
  )

  publish_to_supabase <- tryCatch(
    {
      supabase_config <- supabase_storage_config()
      supabase_upload_object(
        bundle_path,
        config = supabase_config
      )
      verify_supabase_bundle(config = supabase_config)
      message("Supabase publication completed successfully.")
      TRUE
    },
    error = function(e) {
      warning(
        "Supabase publication was not completed: ",
        conditionMessage(e),
        call. = FALSE
      )
      FALSE
    }
  )

  if (!drive_published && !publish_to_supabase) {
    stop(
      "Neither Google Drive nor Supabase publication completed.",
      call. = FALSE
    )
  }
}
b <- Sys.time()

difftime(b, a, units = "secs")
