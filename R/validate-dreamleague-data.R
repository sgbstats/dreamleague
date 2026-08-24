validate_required_columns <- function(df, required, object_name) {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(
      sprintf(
        "%s is missing required columns: %s",
        object_name,
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(df)
}

validate_allowed_values <- function(x, allowed, field_name, allow_na = TRUE) {
  values <- unique(x)
  if (allow_na) {
    values <- values[!is.na(values)]
  }

  invalid <- setdiff(as.character(values), allowed)
  if (length(invalid) > 0) {
    stop(
      sprintf(
        "%s contains invalid values: %s",
        field_name,
        paste(sort(invalid), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(x)
}

validate_date_column <- function(x, field_name) {
  if (!inherits(x, "Date")) {
    stop(sprintf("%s must be a Date column", field_name), call. = FALSE)
  }

  invisible(x)
}

validate_numeric_column <- function(x, field_name) {
  if (!is.numeric(x)) {
    stop(sprintf("%s must be numeric", field_name), call. = FALSE)
  }

  invisible(x)
}

validate_time_bundle <- function(time) {
  required <- c("update_time", "mod_d", "mod_o")
  missing <- setdiff(required, names(time))
  if (length(missing) > 0) {
    stop(
      sprintf("time is missing required fields: %s", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }

  invalid <- vapply(
    time[required],
    function(x) inherits(x, c("POSIXct", "POSIXt")) || inherits(x, "Date"),
    logical(1)
  )

  if (!all(invalid)) {
    stop(
      sprintf(
        "time fields must be date/time values: %s",
        paste(required[!invalid], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(time)
}

validate_current_squads <- function(dl, managers) {
  current <- dl |>
    dplyr::filter(is.na(sold) | sold == "") |>
    dplyr::count(league, team, position, name = "n") |>
    tidyr::pivot_wider(names_from = position, values_from = n, values_fill = 0) |>
    dplyr::right_join(managers, by = c("league", "team")) |>
    dplyr::mutate(
      GOALKEEPER = tidyr::replace_na(.data$GOALKEEPER, 0L),
      DEFENDER = tidyr::replace_na(.data$DEFENDER, 0L),
      MIDFIELDER = tidyr::replace_na(.data$MIDFIELDER, 0L),
      FORWARD = tidyr::replace_na(.data$FORWARD, 0L)
    )

  invalid <- current |>
    dplyr::filter(
      GOALKEEPER != 1 |
        DEFENDER != 2 |
        MIDFIELDER != 3 |
        FORWARD != 5
    )

  if (nrow(invalid) > 0) {
    details <- invalid |>
      dplyr::transmute(
        label = sprintf(
          "%s/%s [GK=%s, DEF=%s, MID=%s, FWD=%s]",
          league,
          team,
          GOALKEEPER,
          DEFENDER,
          MIDFIELDER,
          FORWARD
        )
      ) |>
      dplyr::pull(label)

    stop(
      sprintf(
        "Current squad shape is invalid for: %s",
        paste(details, collapse = "; ")
      ),
      call. = FALSE
    )
  }

  invisible(current)
}

validate_manager_rows <- function(managers) {
  validate_required_columns(managers, c("manager", "team", "league"), "managers")
  validate_allowed_values(managers$league, c("didsbury", "original"), "managers$league")

  duplicates <- managers |>
    dplyr::count(league, team, name = "n") |>
    dplyr::filter(n > 1)

  if (nrow(duplicates) > 0) {
    details <- duplicates |>
      dplyr::transmute(label = sprintf("%s/%s", league, team)) |>
      dplyr::pull(label)

    stop(
      sprintf("Duplicate manager-team rows found: %s", paste(details, collapse = ", ")),
      call. = FALSE
    )
  }

  invisible(managers)
}

validate_dl_data <- function(dl) {
  validate_required_columns(
    dl,
    c(
      "team", "player", "club", "position", "cost", "goals", "sold",
      "bought", "SBgoals", "SBapp", "url", "bought2", "sold2", "league"
    ),
    "dl"
  )
  validate_numeric_column(dl$SBgoals, "dl$SBgoals")
  validate_numeric_column(dl$SBapp, "dl$SBapp")
  validate_date_column(dl$bought2, "dl$bought2")
  validate_date_column(dl$sold2, "dl$sold2")
  validate_allowed_values(dl$league, c("didsbury", "original"), "dl$league")
  validate_allowed_values(
    as.character(dl$position),
    c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD"),
    "dl$position"
  )

  invisible(dl)
}

validate_daily_data <- function(daily) {
  validate_required_columns(
    daily,
    c(
      "Date", "App", "team", "position", "player", "club", "cost", "goals",
      "sold", "bought", "SBapp", "url", "bought2", "sold2", "SBgoals", "league"
    ),
    "daily"
  )
  validate_date_column(daily$Date, "daily$Date")
  validate_date_column(daily$bought2, "daily$bought2")
  validate_date_column(daily$sold2, "daily$sold2")
  validate_numeric_column(daily$SBgoals, "daily$SBgoals")
  validate_allowed_values(daily$league, c("didsbury", "original"), "daily$league")
  validate_allowed_values(
    as.character(daily$position),
    c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD"),
    "daily$position"
  )

  invisible(daily)
}

validate_cupties_data <- function(cupties, managers) {
  validate_required_columns(cupties, c("comp", "round", "team1", "team2", "date"), "cupties")
  validate_date_column(cupties$date, "cupties$date")
  validate_allowed_values(cupties$comp, c("bfl", "didsbury", "original"), "cupties$comp")

  known_teams <- unique(managers$team)
  unknown_teams <- setdiff(unique(c(cupties$team1, cupties$team2)), known_teams)
  unknown_teams <- unknown_teams[!is.na(unknown_teams) & nzchar(unknown_teams)]

  if (length(unknown_teams) > 0) {
    stop(
      sprintf(
        "cupties reference unknown teams: %s",
        paste(sort(unknown_teams), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(cupties)
}

validate_dreamleague_bundle <- function(dl, daily, time, cupties, managers) {
  validate_manager_rows(managers)
  validate_dl_data(dl)
  validate_daily_data(daily)
  validate_time_bundle(time)
  validate_cupties_data(cupties, managers)
  validate_current_squads(dl, managers)

  invisible(TRUE)
}
