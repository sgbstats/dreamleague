library(dplyr)
library(stringr)
library(rvest)
library(httr)
library(jsonlite)

SB_BASE <- "https://www.soccerbase.com"
SB_UA <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
  "(KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36"
)

# Same competition list the DreamLeague scoring uses.
wb_comps <- c(
  "English premier",
  "English Premier",
  "Premier League",
  "EFL Cup",
  "English League Cup",
  "Europa League",
  "Community Shield",
  "Champions League",
  "European Cup",
  "FA Cup",
  "English FA Cup",
  "Europa Conference League",
  "Football League Championship",
  "Football League Championship Play-Off",
  "Championship Play-Off",
  "Football League One",
  "Football League One Play-Off",
  "League One Play-Off",
  "Football League Two",
  "Football League Two Play-Off",
  "League Two Play-Off",
  "Fairs Cup",
  "UEFA Cup",
  "Euro Cup Winners Cup",
  "European Super Cup",
  "Capital One Cup",
  "Carling Cup",
  "Premiership",
  "English Div 1 \\(old\\)",
  "Charity Shield"
)

wb_comp_match <- function(x) {
  grepl(paste(wb_comps, collapse = "|"), x)
}

sb_get_html <- function(path, delay = 0.4, tries = 3) {
  url <- if (grepl("^https?://", path)) path else paste0(SB_BASE, path)
  for (attempt in seq_len(tries)) {
    Sys.sleep(delay)
    response <- tryCatch(
      httr::GET(url, httr::user_agent(SB_UA), httr::timeout(30)),
      error = function(e) NULL
    )
    if (!is.null(response) && !httr::http_error(response)) {
      return(httr::content(response, as = "text", encoding = "UTF-8"))
    }
    Sys.sleep(attempt * 2)
  }
  NA_character_
}

# ---- Football League 92 -----------------------------------------------------

read_league92 <- function(path = "data/league92_clubs.csv") {
  clubs <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!all(c("team_id", "club", "division", "season") %in% names(clubs))) {
    stop(path, " must have team_id, club, division and season", call. = FALSE)
  }
  clubs
}

league92_warning <- function(clubs, expected = 92) {
  if (nrow(clubs) != expected) {
    return(sprintf(
      "data/league92_clubs.csv lists %d clubs, expected %d - update it for the new season.",
      nrow(clubs),
      expected
    ))
  }
  NULL
}

# ---- Soccerbase page parsers ------------------------------------------------

parse_player_page <- function(html, player_id) {
  page <- xml2::read_html(html)

  head_cells <- page |>
    html_elements("table.imageHead td") |>
    html_text2()
  head_info <- head_cells[grepl(
    "^(Goalkeeper|Defender|Midfielder|Forward)\\b",
    head_cells,
    ignore.case = TRUE
  )]
  head_info <- if (length(head_info) > 0) head_info[[1]] else ""
  position <- toupper(str_extract(head_info, "^\\S+"))

  club_link <- page |>
    html_elements("table.imageHead a[href*='team_id=']") |>
    html_attr("href")
  club_id <- if (length(club_link) > 0) {
    as.numeric(str_match(club_link[[1]], "team_id=(\\d+)")[, 2])
  } else {
    NA_real_
  }
  club <- str_trim(sub("^\\S+", "", head_info))
  club <- if (nzchar(club)) club else NA_character_

  joined <- as.Date(NA)
  career <- page |> html_element("table.career")
  if (!inherits(career, "xml_missing")) {
    career_tbl <- suppressWarnings(as.data.frame(html_table(career)))
    if (ncol(career_tbl) >= 3) {
      from <- as.Date(career_tbl[[2]], format = "%d %b, %y")
      from <- from[!is.na(from)]
      if (length(from) > 0) joined <- from[[1]]
    }
  }

  rows <- page |> html_elements("table#tpg tr.match")
  games <- if (length(rows) == 0) {
    data.frame(
      game_id = character(),
      date = as.Date(character()),
      comp = character(),
      home_id = numeric(),
      away_id = numeric(),
      home = character(),
      away = character(),
      played = logical(),
      goals = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    dplyr::bind_rows(lapply(rows, parse_match_row, prefix = "tpg"))
  }
  if (nrow(games) > 0 && "goals" %in% names(games)) {
    games$goals[is.na(games$goals)] <- 0
  }

  list(
    player_id = player_id,
    position = position,
    club = club,
    club_id = club_id,
    joined = joined,
    games = games
  )
}

parse_match_row <- function(row, prefix) {
  tds <- row |> html_elements("td")
  team_cells <- row |> html_elements("td.team")
  team_ids <- vapply(
    team_cells,
    function(td) {
      href <- td |> html_element("a") |> html_attr("href")
      as.numeric(str_match(href, "team_id=(\\d+)")[, 2])
    },
    numeric(1)
  )
  team_names <- vapply(
    team_cells,
    function(td) html_text2(html_element(td, "a")),
    character(1)
  )
  date_href <- row |>
    html_elements("td.dateTime a") |>
    html_attr("href")
  score <- row |> html_elements("td.score em") |> html_text2()
  goals <- if (length(tds) >= 7) {
    suppressWarnings(as.numeric(str_trim(html_text2(tds[[7]]))))
  } else {
    NA_real_
  }
  data.frame(
    game_id = sub(paste0("^", prefix), "", html_attr(row, "id")),
    date = as.Date(str_extract(date_href[[1]], "\\d{4}-\\d{2}-\\d{2}")),
    comp = html_text2(html_element(row, "td.tournament a")),
    home_id = team_ids[[1]],
    away_id = team_ids[[2]],
    home = team_names[[1]],
    away = team_names[[2]],
    played = length(score) == 2 && all(grepl("^\\d+$", score)),
    goals = goals,
    stringsAsFactors = FALSE
  )
}

parse_team_results <- function(html) {
  page <- xml2::read_html(html)
  rows <- page |> html_elements("table tr.match")
  if (length(rows) == 0) {
    return(data.frame(
      game_id = character(),
      date = as.Date(character()),
      comp = character(),
      home_id = numeric(),
      away_id = numeric(),
      home = character(),
      away = character(),
      played = logical(),
      stringsAsFactors = FALSE
    ))
  }
  out <- dplyr::bind_rows(lapply(rows, parse_match_row, prefix = "tgc"))
  out$goals <- NULL
  out
}

# ---- Line-ups ---------------------------------------------------------------

parse_lineup_side <- function(players_html, subs_html) {
  tokens_of <- function(html) {
    html <- gsub(
      "<a [^>]*player_id=(\\d+)[^>]*>[^<]*</a>",
      "@\\1@",
      html,
      perl = TRUE
    )
    html <- gsub("<[^>]+>", "", html)
    stringr::str_match_all(
      html,
      "@(\\d+)@|(\\()|(\\))|,\\s*(\\d+)(?:\\+\\d+)?"
    )[[1]]
  }

  tokens <- tokens_of(players_html)
  ids <- character()
  entry <- c()
  exit <- c()
  last <- character()
  depth <- 0L
  for (i in seq_len(nrow(tokens))) {
    if (!is.na(tokens[i, 2])) {
      pid <- tokens[i, 2]
      ids <- c(ids, pid)
      last[depth + 1L] <- pid
    } else if (!is.na(tokens[i, 3])) {
      depth <- depth + 1L
    } else if (!is.na(tokens[i, 4])) {
      depth <- max(depth - 1L, 0L)
    } else if (!is.na(tokens[i, 5])) {
      minute <- as.numeric(tokens[i, 5])
      entry[last[depth + 1L]] <- minute
      if (depth >= 1L) exit[last[depth]] <- minute
    }
  }

  played <- data.frame(
    player_id = ids,
    status = ifelse(is.na(entry[ids]), "Started", "Substitute"),
    minutes = pmax(
      ifelse(is.na(exit[ids]), 90, exit[ids]) -
        ifelse(is.na(entry[ids]), 0, entry[ids]),
      0
    ),
    stringsAsFactors = FALSE
  )
  rownames(played) <- NULL

  unused_ids <- character()
  if (nzchar(subs_html)) {
    unused_tokens <- tokens_of(subs_html)
    unused_ids <- unused_tokens[!is.na(unused_tokens[, 2]), 2]
  }
  unused <- data.frame(
    player_id = unused_ids,
    status = rep("Unused substitute", length(unused_ids)),
    minutes = rep(0, length(unused_ids)),
    stringsAsFactors = FALSE
  )

  dplyr::bind_rows(played, unused)
}

parse_lineup_bubble <- function(html) {
  page <- xml2::read_html(html)
  inner <- function(selector) {
    node <- page |> html_element(selector)
    if (inherits(node, "xml_missing")) "" else as.character(node)
  }
  home <- parse_lineup_side(
    inner("tr.players td.right"),
    inner("tr.subs td.right")
  )
  away <- parse_lineup_side(
    inner("tr.players td.left"),
    inner("tr.subs td.left")
  )
  if (nrow(home) > 0) home$side <- "home"
  if (nrow(away) > 0) away$side <- "away"
  dplyr::bind_rows(home, away)
}

# ---- Per-player weekly report ----------------------------------------------

classify_player_games <- function(
  player_page,
  team_games,
  lineups,
  window_start,
  window_end
) {
  club_id <- player_page$club_id
  pid <- as.character(player_page$player_id)
  start <- window_start
  if (!is.na(player_page$joined) && player_page$joined > start) {
    start <- player_page$joined
  }

  fixtures <- team_games |>
    filter(
      played,
      date >= start,
      date <= window_end,
      home_id == club_id | away_id == club_id,
      wb_comp_match(comp)
    ) |>
    arrange(date)

  if (nrow(fixtures) == 0) {
    return(data.frame(
      game_id = character(),
      date = as.Date(character()),
      comp = character(),
      opponent = character(),
      venue = character(),
      status = character(),
      minutes = numeric(),
      goals = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  own <- player_page$games
  rows <- lapply(seq_len(nrow(fixtures)), function(i) {
    fx <- fixtures[i, ]
    lineup <- lineups[[fx$game_id]]
    entry <- if (is.null(lineup)) {
      NULL
    } else {
      lineup[lineup$player_id == pid, , drop = FALSE]
    }
    has_entry <- !is.null(entry) && nrow(entry) > 0
    appeared <- fx$game_id %in% own$game_id
    status <- if (has_entry) {
      entry$status[[1]]
    } else if (appeared) {
      "Played"
    } else if (is.null(lineup)) {
      NA_character_
    } else {
      "Not in squad"
    }
    data.frame(
      game_id = fx$game_id,
      date = fx$date,
      comp = fx$comp,
      opponent = if (fx$home_id == club_id) fx$away else fx$home,
      venue = if (fx$home_id == club_id) "H" else "A",
      status = status,
      minutes = if (has_entry) entry$minutes[[1]] else NA_real_,
      goals = sum(own$goals[own$game_id == fx$game_id], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  out <- dplyr::bind_rows(rows)
  out[!is.na(out$status), , drop = FALSE]
}

club_norm <- function(x) {
  x <- tolower(iconv(as.character(x), to = "ASCII//TRANSLIT"))
  x <- gsub("[^a-z0-9 ]", " ", x)
  x <- gsub("\\b(fc|afc|a f c|f c|the)\\b", " ", x)
  trimws(gsub("\\s+", " ", x))
}

club_names_match <- function(a, b, threshold = 0.8) {
  a <- club_norm(a)
  b <- club_norm(b)
  if (is.na(a) || is.na(b) || !nzchar(a) || !nzchar(b)) return(NA)
  if (a == b) return(TRUE)
  ta <- strsplit(a, " ")[[1]]
  tb <- strsplit(b, " ")[[1]]
  if (all(ta %in% tb) || all(tb %in% ta)) return(TRUE)
  sim <- 1 - drop(adist(a, b)) / max(nchar(a), nchar(b))
  sim >= threshold
}

club_flags <- function(player_page, sheet_club, league92, window_start = NULL) {
  flags <- list(note = NULL, warning = NULL)
  in92 <- !is.na(player_page$club_id) && player_page$club_id %in% league92$team_id
  moved_vs_stored <- isFALSE(club_names_match(sheet_club, player_page$club))

  if (is.na(player_page$club_id)) {
    flags$warning <- "Could not determine current club from Soccerbase."
    return(flags)
  }
  if (moved_vs_stored) {
    flags$note <- sprintf("Now playing for %s (new club).", player_page$club)
  }
  if (!in92) {
    flags$warning <- sprintf(
      "Now at %s, which is outside the Football League 92.",
      player_page$club
    )
  }
  flags
}

wb_window <- function(today = Sys.Date(), days = 7) {
  list(start = today - days + 1, end = today)
}

# ---- Scouting ---------------------------------------------------------------

scout_best <- function(candidates, quota = c(DEFENDER = 2, MIDFIELDER = 3, FORWARD = 5)) {
  out <- lapply(names(quota), function(pos) {
    candidates |>
      filter(position == pos) |>
      arrange(desc(goals), apps, player) |>
      slice_head(n = quota[[pos]])
  })
  dplyr::bind_rows(out)
}

wb_taken_ids <- function(dl) {
  urls <- as.character(dl$url)
  as.numeric(str_match(urls, "player_id=(\\d+)")[, 2])
}

# ---- Email ------------------------------------------------------------------

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

build_report_email <- function(player_reports, scouting, window, notices = character()) {
  th <- "style='text-align:left;padding:4px 8px;background:#eee'"
  td <- "style='padding:4px 8px;border-bottom:1px solid #ddd'"

  player_blocks <- vapply(
    player_reports,
    function(p) {
      alerts <- c(
        if (!is.null(p$warning)) {
          sprintf("<p style='color:#b00020'><b>&#9888; %s</b></p>", html_escape(p$warning))
        },
        if (!is.null(p$note)) {
          sprintf("<p style='color:#0b6'><b>Note:</b> %s</p>", html_escape(p$note))
        }
      )
      games <- p$games
      rows <- if (nrow(games) == 0) {
        sprintf("<tr><td %s colspan='6'>No games for the club in this period.</td></tr>", td)
      } else {
        paste(
          sprintf(
            "<tr><td %s>%s</td><td %s>%s (%s)</td><td %s>%s</td><td %s>%s</td><td %s>%s</td><td %s>%s</td></tr>",
            td, format(games$date, "%a %d %b"),
            td, html_escape(games$opponent), games$venue,
            td, html_escape(games$comp),
            td, html_escape(games$status),
            td, ifelse(is.na(games$minutes), "-", games$minutes),
            td, games$goals
          ),
          collapse = ""
        )
      }
      sprintf(
        "<h3>%s <small>(%s, %s)</small></h3>%s<p>Goals: <b>%d</b> &middot; Games: <b>%d</b> &middot; Minutes: <b>%d</b> &middot; Missed with club playing: <b>%d</b></p><table cellspacing='0' style='border-collapse:collapse'><tr><th %s>Date</th><th %s>Opponent</th><th %s>Competition</th><th %s>Status</th><th %s>Mins</th><th %s>Goals</th></tr>%s</table>",
        html_escape(p$player), tolower(p$position), html_escape(p$club),
        paste(alerts, collapse = ""),
        p$goals, p$games_played, p$minutes, p$missed,
        th, th, th, th, th, th, rows
      )
    },
    character(1)
  )

  scouting_rows <- if (nrow(scouting) == 0) {
    "<tr><td colspan='5'>No candidates found.</td></tr>"
  } else {
    paste(
      sprintf(
        "<tr><td %s>%s</td><td %s>%s</td><td %s>%s</td><td %s>%d</td><td %s>%d</td></tr>",
        td, tolower(scouting$position),
        td, html_escape(scouting$player),
        td, html_escape(scouting$club),
        td, as.integer(scouting$goals),
        td, as.integer(scouting$apps)
      ),
      collapse = ""
    )
  }

  html <- paste0(
    "<div style='font-family:Arial,sans-serif;max-width:760px'>",
    sprintf(
      "<h2>Wenger Boys weekly report</h2><p>%s to %s</p>",
      format(window$start, "%d %b %Y"), format(window$end, "%d %b %Y")
    ),
    if (length(notices) > 0) {
      paste0("<p style='color:#b00020'><b>", html_escape(notices), "</b></p>", collapse = "")
    },
    paste(player_blocks, collapse = ""),
    "<h2>Scouting shortlist</h2><p>Best available players not owned by any DreamLeague team (season goals, fewer appearances breaks ties).</p>",
    sprintf(
      "<table cellspacing='0' style='border-collapse:collapse'><tr><th %s>Position</th><th %s>Player</th><th %s>Club</th><th %s>Goals</th><th %s>Apps</th></tr>%s</table>",
      th, th, th, th, th, scouting_rows
    ),
    "</div>"
  )

  text_players <- vapply(
    player_reports,
    function(p) {
      sprintf(
        "%s (%s, %s): %d goals, %d games, %d mins, %d missed%s%s",
        p$player, tolower(p$position), p$club, p$goals, p$games_played,
        p$minutes, p$missed,
        if (!is.null(p$warning)) paste0("\n  WARNING: ", p$warning) else "",
        if (!is.null(p$note)) paste0("\n  NOTE: ", p$note) else ""
      )
    },
    character(1)
  )
  text_scout <- if (nrow(scouting) == 0) {
    "No candidates found."
  } else {
    sprintf(
      "%s: %s (%s) - %d goals, %d apps",
      tolower(scouting$position), scouting$player, scouting$club,
      as.integer(scouting$goals), as.integer(scouting$apps)
    )
  }
  text <- paste(
    c(
      sprintf("Wenger Boys weekly report: %s to %s", window$start, window$end),
      notices,
      "",
      text_players,
      "",
      "Scouting shortlist:",
      text_scout
    ),
    collapse = "\n"
  )

  list(html = html, text = text)
}

send_resend_email <- function(
  subject,
  html,
  text,
  to = Sys.getenv("WENGERBOYS_EMAIL_TO"),
  from = Sys.getenv("WENGERBOYS_EMAIL_FROM", "Wenger Boys <onboarding@resend.dev>"),
  api_key = Sys.getenv("RESEND_API_KEY")
) {
  if (!nzchar(api_key)) stop("RESEND_API_KEY is not configured", call. = FALSE)
  if (!nzchar(to)) stop("WENGERBOYS_EMAIL_TO is not configured", call. = FALSE)
  response <- httr::POST(
    "https://api.resend.com/emails",
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    body = list(from = from, to = list(to), subject = subject, html = html, text = text),
    encode = "json",
    httr::timeout(30)
  )
  if (httr::http_error(response)) {
    stop(
      sprintf(
        "Resend failed (HTTP %s): %s",
        httr::status_code(response),
        httr::content(response, as = "text", encoding = "UTF-8")
      ),
      call. = FALSE
    )
  }
  invisible(httr::content(response, as = "parsed"))
}
