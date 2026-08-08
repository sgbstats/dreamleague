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

source("R/dl-preprocessing-fast.R")
gs4_auth(
  path = "credentials.json"
)

file_d <- "data/DreamLeague25-26.xlsx"
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

out_d <- dl_process(dl_d, managers_d, "Didsbury", season_id = 158)


file_o <- "data/DL25-26.xlsx"
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


out_o <- dl_process(dl_o, managers_o, "Original", season_id = 158)


dl_d <- out_d$scores
dl_o <- out_o$scores
daily_o <- out_o$daily
daily_d <- out_d$daily
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
  path = "credentials.json",
  subject = NULL,
  scopes = "drive",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)

shared_drive_target <- Sys.getenv("DREAMLEAGUE_SHARED_DRIVE_TARGET", "")
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


if (out_d$cut_time == Sys.Date() & out_o$cut_time == Sys.Date()) {
  dl <- rbind.data.frame(
    out_d$scores |> mutate(league = "didsbury"),
    out_o$scores |> mutate(league = "original")
  )

  daily <- rbind.data.frame(
    out_d$daily |> mutate(league = "didsbury"),
    out_o$daily |> mutate(league = "original")
  )

  save(dl, daily, time, cupties, file = "dreamleague/data.RDa")
  source("R/export-dreamleague-json.R")

  save(dl = dl, file = "dreamleague/teams.RDa")
  save(daily = daily, time = time, file = "dreamleague/daily.RDa")
  save(cupties = cupties, file = "dreamleague/cupties.RDa")

  for (i in names(out_d)) {
    write.csv(out_d[[i]], glue::glue("data/diagnostics/didsbury_{i}.csv"))
    write.csv(out_d[[i]], glue::glue("data/diagnostics/original_{i}.csv"))
  }

  upload_to_drive("dreamleague/managers.RDa", "managers.RDa")
  upload_to_drive("dreamleague/teams.RDa", "teams.RDa")
  upload_to_drive("dreamleague/daily.RDa", "daily.RDa")
  upload_to_drive("dreamleague/cupties.RDa", "cupties.RDa")
}
b <- Sys.time()

difftime(b, a, units = "secs")
