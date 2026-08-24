suppressPackageStartupMessages({
  library(jsonlite)
  library(tidyverse)
})

load("dreamleague/data.RDa")
load("dreamleague/managers.RDa")
load("data/ids.RDa")

player_soccerbase_url <- function(player_id) {
  if (is.na(player_id) || player_id == "") {
    return(NA_character_)
  }
  glue::glue(
    "https://www.soccerbase.com/players/player.sd?player_id={player_id}&season_id=158"
  )
}

make_team_results_url <- function(team_id) {
  if (is.na(team_id) || team_id == "") {
    return(NA_character_)
  }
  glue::glue(
    "https://www.soccerbase.com/teams/team.sd?team_id={team_id}&teamTabs=results&season_id=158"
  )
}

output_dir <- "web/public/data"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

normalize_dates <- function(df) {
  out <- df
  for (nm in names(out)) {
    if (
      inherits(out[[nm]], "Date") ||
        inherits(out[[nm]], "POSIXct") ||
        inherits(out[[nm]], "POSIXt")
    ) {
      out[[nm]] <- as.character(out[[nm]])
    }
  }
  out
}

write_json_file <- function(x, path) {
  jsonlite::write_json(
    x,
    path = path,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null",
    dataframe = "rows"
  )
}

bundle <- list(
  dl = dl |> normalize_dates(),
  daily = daily |> normalize_dates(),
  time = lapply(time, as.character),
  cupties = cupties |> normalize_dates(),
  managers = bind_rows(
    managers_d |> mutate(league = "didsbury"),
    managers_o |> mutate(league = "original")
  ) |>
    normalize_dates()
)

write_json_file(bundle, file.path(output_dir, "bundle.json"))
write_json_file(bundle$dl, file.path(output_dir, "dl.json"))
write_json_file(bundle$daily, file.path(output_dir, "daily.json"))
write_json_file(bundle$time, file.path(output_dir, "time.json"))
write_json_file(bundle$cupties, file.path(output_dir, "cupties.json"))
write_json_file(bundle$managers, file.path(output_dir, "managers.json"))
