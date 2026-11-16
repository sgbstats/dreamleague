suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})

sys.source("R/wengerboys-lib.R", envir = environment())
sys.source("R/supabase-storage.R", envir = environment())

didsbury_auction_quotas <- c(
  GOALKEEPER = 1L,
  DEFENDER = 2L,
  MIDFIELDER = 3L,
  FORWARD = 5L
)

didsbury_auction_candidates <- function(dl, goalkeeper_conceded) {
  required <- c(
    "league", "position", "player", "club", "cost", "is_transfer",
    "SBgoals", "sold", "url"
  )
  missing <- setdiff(required, names(dl))
  if (length(missing) > 0) {
    stop(
      "Didsbury data is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!all(c("team_id", "conceded") %in% names(goalkeeper_conceded))) {
    stop(
      "goalkeeper_conceded must contain team_id and conceded columns",
      call. = FALSE
    )
  }

  candidates <- dl |>
    mutate(
      position = str_to_upper(str_trim(as.character(position))),
      league = str_to_lower(str_trim(as.character(league))),
      cost = suppressWarnings(as.numeric(as.character(cost))),
      is_transfer = as.logical(is_transfer),
      goals = suppressWarnings(as.numeric(as.character(SBgoals))),
      team_id = suppressWarnings(as.numeric(str_match(
        as.character(url),
        "team_id=(\\d+)"
      )[, 2]))
    ) |>
    filter(
      league == "didsbury",
      position %in% names(didsbury_auction_quotas),
      !is.na(cost),
      cost >= 0,
      cost == floor(cost),
      !is.na(is_transfer),
      !is_transfer
    )

  if (any(candidates$position != "GOALKEEPER" & is.na(candidates$goals))) {
    stop(
      "Didsbury outfield candidates have missing Soccerbase goals",
      call. = FALSE
    )
  }

  goalkeepers <- candidates$position == "GOALKEEPER"
  if (any(goalkeepers & is.na(candidates$team_id))) {
    stop(
      "A priced Didsbury goalkeeper has no Soccerbase team_id in its URL",
      call. = FALSE
    )
  }

  goalkeeper_conceded <- goalkeeper_conceded |>
    mutate(
      team_id = as.numeric(team_id),
      conceded = as.numeric(conceded)
    ) |>
    distinct(team_id, .keep_all = TRUE)
  gk_idx <- which(goalkeepers)
  if (length(gk_idx) > 0) {
    conceded_idx <- match(candidates$team_id[gk_idx], goalkeeper_conceded$team_id)
    if (anyNA(conceded_idx) || anyNA(goalkeeper_conceded$conceded[conceded_idx])) {
      stop(
        "Full-season conceded totals are missing for a priced Didsbury goalkeeper",
        call. = FALSE
      )
    }
    candidates$conceded <- NA_real_
    candidates$conceded[gk_idx] <- goalkeeper_conceded$conceded[conceded_idx]
    candidates$goals[gk_idx] <- -candidates$conceded[gk_idx]
  } else {
    candidates$conceded <- NA_real_
  }

  candidates |>
    arrange(position, player, club, cost) |>
    select(position, player, club, cost, goals, conceded, sold, team_id, url)
}

didsbury_auction_role_options <- function(players, quota, budget) {
  score <- matrix(-Inf, nrow = quota + 1L, ncol = budget + 1L)
  picks <- matrix(vector("list", (quota + 1L) * (budget + 1L)),
                  nrow = quota + 1L, ncol = budget + 1L)
  score[1, 1] <- 0
  picks[[1, 1]] <- integer()

  for (i in seq_len(nrow(players))) {
    player_cost <- players$cost[i]
    if (player_cost > budget) next
    max_count <- min(quota, i)
    for (count in seq.int(max_count, 1L)) {
      for (total_cost in seq.int(budget, player_cost, by = -1L)) {
        previous_cost <- total_cost - player_cost
        previous_score <- score[count, previous_cost + 1L]
        if (!is.finite(previous_score)) next
        new_score <- previous_score + players$goals[i]
        target <- count + 1L
        if (new_score > score[target, total_cost + 1L]) {
          score[target, total_cost + 1L] <- new_score
          picks[[target, total_cost + 1L]] <- c(
            picks[[count, previous_cost + 1L]],
            i
          )
        }
      }
    }
  }

  list(
    score = score[quota + 1L, ],
    picks = picks[quota + 1L, ]
  )
}

didsbury_auction_best_team <- function(dl, goalkeeper_conceded, budget = 100L) {
  if (length(budget) != 1L || is.na(budget) || budget < 0 ||
      budget != floor(budget)) {
    stop("budget must be a non-negative whole number", call. = FALSE)
  }
  budget <- as.integer(budget)
  players <- didsbury_auction_candidates(dl, goalkeeper_conceded)

  for (position in names(didsbury_auction_quotas)) {
    available <- sum(players$position == position)
    if (available < didsbury_auction_quotas[[position]]) {
      stop(
        sprintf(
          "Not enough eligible %s players: need %d, found %d",
          tolower(position),
          didsbury_auction_quotas[[position]],
          available
        ),
        call. = FALSE
      )
    }
  }

  total_score <- rep(-Inf, budget + 1L)
  total_picks <- vector("list", budget + 1L)
  total_score[1] <- 0
  total_picks[[1]] <- integer()

  for (position in names(didsbury_auction_quotas)) {
    role_players <- players |> filter(position == .env$position)
    options <- didsbury_auction_role_options(
      role_players,
      didsbury_auction_quotas[[position]],
      budget
    )
    option_costs <- which(is.finite(options$score)) - 1L
    next_score <- rep(-Inf, budget + 1L)
    next_picks <- vector("list", budget + 1L)
    for (current_cost in which(is.finite(total_score)) - 1L) {
      for (role_cost in option_costs) {
        combined_cost <- current_cost + role_cost
        if (combined_cost > budget) next
        combined_score <- total_score[current_cost + 1L] +
          options$score[role_cost + 1L]
        if (combined_score > next_score[combined_cost + 1L]) {
          next_score[combined_cost + 1L] <- combined_score
          role_indices <- options$picks[[role_cost + 1L]]
          next_picks[[combined_cost + 1L]] <- c(
            total_picks[[current_cost + 1L]],
            which(players$position == position)[role_indices]
          )
        }
      }
    }
    total_score <- next_score
    total_picks <- next_picks
  }

  feasible <- which(is.finite(total_score))
  if (length(feasible) == 0) {
    stop(
      sprintf("No complete Didsbury team fits the budget of %d", budget),
      call. = FALSE
    )
  }
  best_cost_idx <- feasible[which.max(total_score[feasible])]
  team <- players[total_picks[[best_cost_idx]], , drop = FALSE] |>
    arrange(factor(position, levels = names(didsbury_auction_quotas)), player)
  team$goals <- ifelse(team$position == "GOALKEEPER", NA_real_, team$goals)

  list(
    players = team,
    budget = budget,
    total_cost = sum(team$cost),
    outfield_goals = sum(team$goals, na.rm = TRUE),
    goalkeeper_conceded = sum(team$conceded, na.rm = TRUE),
    net_goals = total_score[best_cost_idx]
  )
}

didsbury_auction_goalkeeper_conceded <- function(
  dl,
  season_id = Sys.getenv("DIDSBURY_AUCTION_SEASON_ID", "159"),
  today = Sys.Date()
) {
  required <- c("league", "position", "cost", "is_transfer", "url")
  missing <- setdiff(required, names(dl))
  if (length(missing) > 0) {
    stop(
      "Didsbury data is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  team_ids <- dl |>
    mutate(
      league = str_to_lower(str_trim(as.character(league))),
      position = str_to_upper(str_trim(as.character(position))),
      cost = suppressWarnings(as.numeric(as.character(cost))),
      is_transfer = as.logical(is_transfer),
      team_id = suppressWarnings(as.numeric(str_match(
        as.character(url),
        "team_id=(\\d+)"
      )[, 2]))
    ) |>
    filter(
      league == "didsbury",
      position == "GOALKEEPER",
      !is.na(cost),
      cost >= 0,
      cost == floor(cost),
      !is.na(is_transfer),
      !is_transfer
    ) |>
    pull(team_id) |>
    unique()

  if (length(team_ids) == 0) {
    return(data.frame(team_id = numeric(), conceded = numeric()))
  }
  if (anyNA(team_ids)) {
    stop(
      "A priced Didsbury goalkeeper has no Soccerbase team_id in its URL",
      call. = FALSE
    )
  }

  dplyr::bind_rows(lapply(team_ids, function(team_id) {
    path <- sprintf(
      "/teams/team.sd?team_id=%s&teamTabs=results&season_id=%s",
      team_id,
      season_id
    )
    html <- sb_get_html(path)
    if (is.na(html)) {
      stop(
        "Soccerbase team results could not be retrieved for team_id ",
        team_id,
        call. = FALSE
      )
    }
    games <- parse_team_results(html) |>
      filter(
        played,
        date <= as.Date(today),
        wb_comp_match(comp),
        home_id == team_id | away_id == team_id
      )
    conceded <- ifelse(games$home_id == team_id, games$away_goals, games$home_goals)
    if (anyNA(conceded)) {
      stop(
        "Soccerbase has a completed match without a score for team_id ",
        team_id,
        call. = FALSE
      )
    }
    data.frame(
      team_id = team_id,
      conceded = sum(conceded),
      stringsAsFactors = FALSE
    )
  }))
}

didsbury_auction_html <- function(result) {
  escape <- function(x) {
    x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  rows <- paste(
    vapply(seq_len(nrow(result$players)), function(i) {
      player <- result$players[i, ]
      sprintf(
        paste0(
          "<tr><td>%s</td><td>%s</td><td>%s</td>",
          "<td>&#163;%s</td><td>%s</td><td>%s</td></tr>"
        ),
        escape(player$position),
        escape(player$player),
        escape(player$club),
        format(player$cost, trim = TRUE, scientific = FALSE),
        ifelse(
          is.na(player$goals),
          "-",
          format(player$goals, trim = TRUE, scientific = FALSE)
        ),
        ifelse(
          is.na(player$conceded),
          "-",
          format(player$conceded, trim = TRUE, scientific = FALSE)
        )
      )
    }, character(1)),
    collapse = "\n"
  )
  sprintf(
    paste0(
      "<!doctype html><html><head><meta charset='utf-8'>",
      "<title>Didsbury best auction team</title></head><body>",
      "<h1>Didsbury best auction team</h1>",
      "<p>Cost: &#163;%d / &#163;%d. Net goals: %s ",
      "(%s outfield goals minus %s goalkeeper goals conceded).</p>",
      "<table><thead><tr><th>Position</th><th>Player</th><th>Club</th>",
      "<th>Auction cost</th><th>Goals</th><th>Conceded</th></tr></thead>",
      "<tbody>%s</tbody></table>",
      "<p>Only priced Didsbury auction signings are considered; transfers are",
      " excluded. Outfield goals stop at the sale date (or the latest data",
      " update if unsold). Goalkeeper concessions cover the full season to",
      " date, regardless of sale.</p></body></html>"
    ),
    result$total_cost,
    result$budget,
    format(result$net_goals, trim = TRUE, scientific = FALSE),
    format(result$outfield_goals, trim = TRUE, scientific = FALSE),
    format(result$goalkeeper_conceded, trim = TRUE, scientific = FALSE),
    rows
  )
}

didsbury_auction_main <- function() {
  bundle_path <- Sys.getenv("DIDSBURY_AUCTION_BUNDLE_PATH", "")
  if (!nzchar(bundle_path)) {
    if (file.exists("dreamleague/data.RDa")) {
      bundle_path <- "dreamleague/data.RDa"
    } else {
      bundle_path <- tempfile(fileext = ".RDa")
      on.exit(unlink(bundle_path), add = TRUE)
      supabase_download_object(destination = bundle_path)
    }
  }
  bundle <- load_dreamleague_bundle(bundle_path)
  goalkeepers <- didsbury_auction_goalkeeper_conceded(bundle$dl)
  result <- didsbury_auction_best_team(bundle$dl, goalkeepers)

  out_dir <- Sys.getenv("DIDSBURY_AUCTION_OUTPUT_DIR", "auction-report")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    didsbury_auction_html(result),
    file.path(out_dir, "report.html"),
    useBytes = TRUE
  )
  utils::write.csv(
    result$players,
    file.path(out_dir, "team.csv"),
    row.names = FALSE,
    na = ""
  )
  cat(sprintf(
    "Best Didsbury auction team: GBP %d / %d, net goals %s. Report: %s\n",
    result$total_cost,
    result$budget,
    format(result$net_goals, trim = TRUE, scientific = FALSE),
    normalizePath(out_dir, winslash = "/", mustWork = TRUE)
  ))
  invisible(result)
}

if (sys.nframe() == 0) didsbury_auction_main()
