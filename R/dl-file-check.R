suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
})

source("R/dl-preprocessing-fast.R")
source("R/dl-file-pull.R")

run_file_check <- function(local_test = FALSE) {
  file_d <- "data/DreamLeague26-27.xlsx"
  file_o <- "data/DL26-27.xlsx"
  source_d <- refresh_remote_xlsx(file_d, local_test = local_test)
  source_o <- refresh_remote_xlsx(file_o, local_test = local_test)
  didsbury_cut_time <- as.Date(source_d$modified_time) - 1

  dl_d <- readxl::read_excel(
    file_d,
    na = c("SOLD"),
    sheet = "Stats",
    skip = 0,
    col_names = FALSE
  ) |>
    suppressMessages() |>
    dplyr::select(2:8)

  managers_d <- readxl::read_excel(file_d, na = c("SOLD"), sheet = "Stats") |>
    suppressMessages() |>
    dplyr::select(11:12) |>
    tidyr::drop_na() |>
    dplyr::rename(manager = 1, team = 2) |>
    dplyr::filter(team != "TEAM")

  didsbury <- dl_process(
    dl_d,
    managers_d,
    "Didsbury",
    cut_time = didsbury_cut_time,
    season_id = 159
  )

  original_cut_time <- as.Date(source_o$modified_time) - 1

  dl_o <- readxl::read_excel(
    file_o,
    na = c(""),
    sheet = "Stats",
    skip = 0,
    col_names = FALSE
  ) |>
    suppressMessages() |>
    dplyr::select(1:7) |>
    dplyr::mutate(
      `...7` = dplyr::case_when(
        `...2` == "JAMES TRAFFORD" ~ "45529",
        `...2` == "NONI MADUEKE" ~ "45530",
        TRUE ~ `...7`
      )
    )

  managers_o <- readxl::read_excel(
    file_o,
    na = c("SOLD"),
    sheet = "Table",
    skip = 4
  ) |>
    suppressMessages() |>
    dplyr::select(c(2, 4)) |>
    dplyr::rename(manager = 1, team = 2) |>
    dplyr::filter(team != "TEAM")

  original <- dl_process(
    dl_o,
    managers_o,
    "Original",
    cut_time = original_cut_time,
    season_id = 159
  )

  list(
    didsbury = didsbury,
    original = original,
    cut_times = list(
      didsbury = didsbury_cut_time,
      original = original_cut_time
    )
  )
}

check <- run_file_check(
  local_test = "--local-test" %in% commandArgs(trailingOnly = TRUE)
)
save(
  check,
  file = glue::glue("data/diagnostics/dl-file-check_{Sys.Date()}.RData")
)
