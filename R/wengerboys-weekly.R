source("R/wengerboys-lib.R")
source("R/supabase-storage.R")

wb_main <- function() {
  today <- as.Date(Sys.getenv("WENGERBOYS_TODAY", as.character(Sys.Date())))
  season_id <- Sys.getenv("WENGERBOYS_SEASON_ID", "159")
  team_name <- toupper(Sys.getenv("WENGERBOYS_TEAM", "WENGER BOYS"))
  dry_run <- identical(tolower(Sys.getenv("WENGERBOYS_DRY_RUN", "false")), "true")
  window <- wb_window(today)
  notices <- character()

  league92 <- read_league92()
  notice92 <- league92_warning(league92)
  if (!is.null(notice92)) notices <- c(notices, notice92)

  bundle_path <- Sys.getenv("WENGERBOYS_BUNDLE_PATH", "")
  if (!nzchar(bundle_path)) {
    bundle_path <- tempfile(fileext = ".RDa")
    on.exit(unlink(bundle_path), add = TRUE)
    supabase_download_object(destination = bundle_path)
  }
  bundle <- load_dreamleague_bundle(bundle_path)
  dl <- bundle$dl
  ids_env <- new.env()
  load("data/ids.RDa", envir = ids_env)
  stored <- as.data.frame(ids_env$player_id)

  roster <- dl |>
    filter(league == "didsbury", toupper(team) == team_name) |>
    mutate(
      player_id = as.numeric(str_match(as.character(url), "player_id=(\\d+)")[, 2]),
      owned = is.na(sold2) | sold2 >= today
    ) |>
    filter(owned, !is.na(player_id))
  if (nrow(roster) == 0) {
    stop("No current players found for team ", team_name, call. = FALSE)
  }

  team_cache <- new.env()
  lineup_cache <- new.env()
  get_team_games <- function(club_id) {
    key <- as.character(club_id)
    if (is.null(team_cache[[key]])) {
      html <- sb_get_html(sprintf(
        "/teams/team.sd?team_id=%s&teamTabs=results&season_id=%s",
        club_id, season_id
      ))
      team_cache[[key]] <- if (is.na(html)) NULL else parse_team_results(html)
    }
    team_cache[[key]]
  }
  get_lineup <- function(game_id) {
    if (is.null(lineup_cache[[game_id]])) {
      html <- sb_get_html(paste0("/teams/lineup_bubble.sd?id_game=", game_id))
      lineup_cache[[game_id]] <- if (is.na(html)) NULL else parse_lineup_bubble(html)
    }
    lineup_cache[[game_id]]
  }

  player_reports <- lapply(seq_len(nrow(roster)), function(i) {
    pid <- roster$player_id[i]
    cat("Report:", roster$player[i], "\n")
    html <- sb_get_html(sprintf(
      "/players/player.sd?player_id=%s&season_id=%s", pid, season_id
    ))
    if (is.na(html)) {
      return(list(
        player = roster$player[i], position = as.character(roster$position[i]),
        club = roster$club[i], games = data.frame(), goals = 0L,
        games_played = 0L, minutes = 0L, missed = 0L,
        warning = "Soccerbase page could not be retrieved.", note = NULL
      ))
    }
    page <- parse_player_page(html, pid)
    flags <- club_flags(page, as.character(roster$club[i]), league92)

    games <- data.frame()
    if (!is.na(page$club_id)) {
      team_games <- get_team_games(page$club_id)
      if (!is.null(team_games)) {
        wanted <- team_games |>
          filter(
            played, date >= window$start, date <= window$end,
            home_id == page$club_id | away_id == page$club_id,
            wb_comp_match(comp)
          )
        lineups <- setNames(lapply(wanted$game_id, get_lineup), wanted$game_id)
        games <- classify_player_games(
          page, team_games, lineups, window$start, window$end
        )
      }
    }

    played <- games$status %in% c("Started", "Substitute", "Played")
    list(
      player = roster$player[i],
      position = if (nzchar(page$position)) page$position else as.character(roster$position[i]),
      club = if (is.na(page$club)) roster$club[i] else page$club,
      games = games,
      goals = as.integer(sum(games$goals[played], na.rm = TRUE)),
      games_played = as.integer(sum(played)),
      minutes = as.integer(sum(games$minutes[played], na.rm = TRUE)),
      missed = as.integer(sum(games$status %in% c("Unused substitute", "Not in squad"))),
      warning = flags$warning,
      note = flags$note
    )
  })

  taken_ids <- wb_taken_ids(dl)
  taken_names <- toupper(dl$player)
  candidates <- stored |>
    filter(
      team_id %in% league92$team_id,
      !player_id %in% taken_ids,
      !toupper(player) %in% taken_names
    ) |>
    distinct(player_id, .keep_all = TRUE)
  cat("Scouting candidates:", nrow(candidates), "\n")
  scout_limit <- as.integer(Sys.getenv("WENGERBOYS_SCOUT_LIMIT", "0"))
  if (scout_limit > 0) candidates <- utils::head(candidates, scout_limit)

  future::plan(future::multisession, workers = 4)
  scouted <- furrr::future_map(
    seq_len(nrow(candidates)),
    function(i) {
      html <- sb_get_html(sprintf(
        "/players/player.sd?player_id=%s&season_id=%s",
        candidates$player_id[i], season_id
      ), delay = 0.2)
      if (is.na(html)) return(NULL)
      page <- tryCatch(parse_player_page(html, candidates$player_id[i]), error = function(e) NULL)
      if (is.null(page) || is.na(page$club_id) || !page$club_id %in% league92$team_id) {
        return(NULL)
      }
      games <- page$games[page$games$played & wb_comp_match(page$games$comp), , drop = FALSE]
      data.frame(
        player_id = candidates$player_id[i],
        player = candidates$player[i],
        club = page$club,
        position = page$position,
        goals = sum(games$goals, na.rm = TRUE),
        apps = nrow(games),
        stringsAsFactors = FALSE
      )
    },
    .options = furrr::furrr_options(
      packages = c("dplyr", "stringr", "rvest", "httr", "xml2"),
      seed = NULL
    )
  )
  scouting <- scout_best(dplyr::bind_rows(scouted))

  email <- build_report_email(player_reports, scouting, window, notices)
  subject <- sprintf("Wenger Boys weekly report - %s", format(today, "%d %b %Y"))

  snapshot <- tempfile(fileext = ".json")
  on.exit(unlink(snapshot), add = TRUE)
  jsonlite::write_json(
    list(
      generated = format(Sys.time(), tz = "UTC", usetz = TRUE),
      window = list(start = as.character(window$start), end = as.character(window$end)),
      players = player_reports,
      scouting = scouting,
      notices = notices
    ),
    snapshot, auto_unbox = TRUE, pretty = TRUE, null = "null"
  )

  if (dry_run) {
    out_dir <- Sys.getenv("WENGERBOYS_OUTPUT_DIR", "wengerboys-output")
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    writeLines(email$html, file.path(out_dir, "report.html"))
    writeLines(email$text, file.path(out_dir, "report.txt"))
    file.copy(snapshot, file.path(out_dir, "snapshot.json"), overwrite = TRUE)
    cat("Dry run: report written to", out_dir, "\n")
    return(invisible(email))
  }

  send_resend_email(subject, email$html, email$text)
  supabase_upload_object(snapshot, sprintf("wengerboys/%s.json", today))
  cat("Report emailed and snapshot saved.\n")
  invisible(email)
}

if (sys.nframe() == 0) wb_main()
