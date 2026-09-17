# Generates pre-match BFL Cup squad graphics from data/cupties.csv.

canonical_team_name <- function(team) {
  aliases <- c("ALTERNATIVE ULSTERMAN" = "ALTERNATIVE ULSTERMEN")
  unname(ifelse(team %in% names(aliases), aliases[team], team))
}

current_squad <- function(dl, managers, team) {
  manager_rows <- managers[
    managers$team == team,
    c("manager", "team", "league")
  ]
  if (nrow(manager_rows) != 1) {
    stop(
      sprintf(
        "Expected one manager record for '%s'; found %d.",
        team,
        nrow(manager_rows)
      ),
      call. = FALSE
    )
  }

  squad <- dl[
    dl$team == team &
      dl$league == manager_rows$league &
      (is.na(dl$sold) | dl$sold == ""),
    c("player", "club", "position")
  ]
  squad <- squad[
    order(
      match(
        squad$position,
        c("GOALKEEPER", "DEFENDER", "MIDFIELDER", "FORWARD")
      ),
      squad$player
    ),
    ,
    drop = FALSE
  ]

  expected <- c(GOALKEEPER = 1L, DEFENDER = 2L, MIDFIELDER = 3L, FORWARD = 5L)
  actual <- table(factor(squad$position, levels = names(expected)))
  if (nrow(squad) != 11 || !identical(as.integer(actual), unname(expected))) {
    stop(
      sprintf(
        "Squad for '%s' must contain 1 goalkeeper, 2 defenders, 3 midfielders, and 5 forwards.",
        team
      ),
      call. = FALSE
    )
  }

  cbind(
    manager_rows[rep(1, nrow(squad)), , drop = FALSE],
    squad,
    stringsAsFactors = FALSE
  )
}

logo_path <- function(team, logo_directory) {
  filename <- paste0(toupper(gsub("[^[:alnum:]]", "", team)), ".png")
  path <- file.path(logo_directory, filename)
  if (!file.exists(path)) {
    stop(sprintf("No logo found for '%s' at '%s'.", team, path), call. = FALSE)
  }
  path
}

club_styles <- function() {
  list(
    ARSENAL = c("#D71920", "#FFFFFF", "solid"),
    ASTON_VILLA = c("#670E36", "#7BB9E8", "solid"),
    BOURNEMOUTH = c("#D71920", "#111111", "vertical"),
    BRENTFORD = c("#D71920", "#FFFFFF", "vertical"),
    BRIGHTON = c("#0057B8", "#FFFFFF", "vertical"),
    CHELSEA = c("#034694", "#FFFFFF", "solid"),
    CRYSTAL_PALACE = c("#1B458F", "#C4122E", "vertical"),
    EVERTON = c("#003399", "#FFFFFF", "solid"),
    FULHAM = c("#FFFFFF", "#111111", "solid"),
    LEEDS = c("#FFFFFF", "#1D428A", "solid"),
    LIVERPOOL = c("#C8102E", "#FFFFFF", "solid"),
    MAN_CITY = c("#6CABDD", "#FFFFFF", "solid"),
    MAN_UTD = c("#DA291C", "#FBE122", "solid"),
    NEWCASTLE = c("#FFFFFF", "#111111", "vertical"),
    NEWPORT = c("#FFA500", "#111111", "solid"),
    NOTTINGHAM_FOREST = c("#DD0000", "#FFFFFF", "solid"),
    SUNDERLAND = c("#FFFFFF", "#E30613", "vertical"),
    TOTTENHAM = c("#FFFFFF", "#132257", "solid"),
    WEST_HAM = c("#7A263A", "#7EB8E7", "solid"),
    BARNET = c("#F2A900", "#111111", "solid"),
    BLACKPOOL = c("#F2A900", "#111111", "vertical"),
    BROMLEY = c("#FFFFFF", "#111111", "solid"),
    BURTON = c("#F2A900", "#111111", "solid"),
    CAMBRIDGE = c("#F2A900", "#111111", "solid"),
    CREWE = c("#E30613", "#FFFFFF", "solid"),
    BOLTON = c("#FFFFFF", "#1C3C6E", "solid"),
    BRADFORD = c("#7A1E48", "#F6C700", "solid"),
    BIRMINGHAM = c("#1D4F91", "#FFFFFF", "solid"),
    BRISTOL_CITY = c("#E2231A", "#FFFFFF", "solid"),
    CARDIFF = c("#0070B8", "#FFFFFF", "solid"),
    CHARLTON = c("#E31B23", "#FFFFFF", "solid"),
    CHELTENHAM = c("#E31B23", "#FFFFFF", "solid"),
    CHESTERFIELD = c("#1B75BB", "#FFFFFF", "solid"),
    COLCHESTER = c("#1C3E94", "#FFFFFF", "solid"),
    DERBY = c("#FFFFFF", "#111111", "solid"),
    DONCASTER = c("#D71920", "#FFFFFF", "solid"),
    GRIMSBY = c("#FFFFFF", "#111111", "vertical"),
    HUDDERSFIELD = c("#0B3B8C", "#FFFFFF", "vertical"),
    HULL = c("#F9A01B", "#111111", "solid"),
    IPSWICH = c("#0064A6", "#FFFFFF", "solid"),
    LEICESTER = c("#003090", "#FFFFFF", "solid"),
    LEYTON_ORIENT = c("#E30613", "#FFFFFF", "solid"),
    LINCOLN = c("#E30613", "#FFFFFF", "vertical"),
    MANCHESTEER_CITY = c("#6CABDD", "#FFFFFF", "solid"),
    MANCHESTER_CITY = c("#6CABDD", "#FFFFFF", "solid"),
    MANCHESTER_UNITED = c("#DA291C", "#FBE122", "solid"),
    MANSFIELD = c("#F2A900", "#1D4F91", "solid"),
    MIDDLESBROUGH = c("#E21A22", "#FFFFFF", "solid"),
    MILLWALL = c("#1D4F91", "#FFFFFF", "solid"),
    MILTON_KEYNES_DONS = c("#FFFFFF", "#D71920", "solid"),
    NORWICH = c("#00A650", "#FFDD00", "solid"),
    NOTTS_COUNTY = c("#FFFFFF", "#111111", "vertical"),
    OLDHAM = c("#1B75BB", "#FFFFFF", "solid"),
    PETERBOROUGH = c("#004B9B", "#FFFFFF", "solid"),
    PLYMOUTH = c("#006B3F", "#FFFFFF", "solid"),
    PORTSMOUTH = c("#1D4F91", "#FFFFFF", "solid"),
    READING = c("#1D4F91", "#FFFFFF", "solid"),
    ROCHDALE = c("#1D4F91", "#FFFFFF", "solid"),
    ROTHERHAM = c("#D71920", "#FFFFFF", "solid"),
    SALFORD = c("#D71920", "#FFFFFF", "solid"),
    SHEFFIELD_UNITED = c("#E30613", "#FFFFFF", "vertical"),
    SHEFFIELD_WEDNESDAY = c("#1D4F91", "#FFFFFF", "vertical"),
    SOUTHAMPTON = c("#D71920", "#FFFFFF", "vertical"),
    STEVENAGE = c("#D71920", "#FFFFFF", "solid"),
    STOCKPORT = c("#1D4F91", "#FFFFFF", "solid"),
    SWANSEA = c("#FFFFFF", "#111111", "solid"),
    SWINDON = c("#D71920", "#FFFFFF", "solid"),
    WALSALL = c("#E30613", "#FFFFFF", "solid"),
    WEST_BROMWICH = c("#1D4F91", "#FFFFFF", "vertical"),
    WOLVERHAMPTON = c("#FDB913", "#111111", "solid"),
    WREXHAM = c("#E30613", "#FFFFFF", "solid"),
    WYCOMBE = c("#1D4F91", "#FFFFFF", "solid"),
    YORK = c("#B51E2E", "#FFFFFF", "solid"),
    WOLVES = c("#FDB913", "#111111", "solid")
  )
}

club_style_keys <- function() names(club_styles())

shirt_style <- function(club) {
  key <- gsub("[^A-Z0-9]+", "_", toupper(trimws(club)))
  styles <- club_styles()
  if (key %in% names(styles)) {
    unname(styles[[key]])
  } else {
    c("#9AA3A8", "#FFFFFF", "solid")
  }
}

formation_coordinates <- function(side) {
  positions <- c(
    "GOALKEEPER",
    rep("DEFENDER", 2),
    rep("MIDFIELDER", 3),
    rep("FORWARD", 5)
  )
  x <- c(7, 18, 18, 30, 30, 30, 40, 40, 40, 40, 40)
  y <- c(50, 22, 78, 12, 50, 88, 6, 28, 50, 72, 94)
  if (side == "right") {
    x <- 100 - x
  }
  data.frame(position = positions, x = x, y = y)
}

add_shirts <- function(plot, players) {
  sleeves <- rbind(
    transform(
      players,
      xmin = x - 6.2,
      xmax = x - 2.8,
      ymin = y - 1.8,
      ymax = y + 2.6
    ),
    transform(
      players,
      xmin = x + 2.8,
      xmax = x + 6.2,
      ymin = y - 1.8,
      ymax = y + 2.6
    )
  )
  torso <- transform(
    players,
    xmin = x - 3.8,
    xmax = x + 3.8,
    ymin = y - 6,
    ymax = y + 4.2
  )
  neckline <- do.call(
    rbind,
    lapply(seq_len(nrow(players)), function(i) {
      angle <- seq(pi, 2 * pi, length.out = 30)
      data.frame(
        player_id = i,
        x = players$x[i] + 1.25 * cos(angle),
        y = players$y[i] + 4.2 + 1.1 * sin(angle)
      )
    })
  )

  plot <- plot +
    ggplot2::geom_rect(
      data = sleeves,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = fill
      ),
      inherit.aes = FALSE,
      colour = "#1A1A1A",
      linewidth = 0.45
    ) +
    ggplot2::geom_rect(
      data = torso,
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = fill
      ),
      inherit.aes = FALSE,
      colour = "#1A1A1A",
      linewidth = 0.45
    ) +
    ggplot2::geom_path(
      data = neckline,
      ggplot2::aes(x = x, y = y, group = player_id),
      inherit.aes = FALSE,
      colour = "#1A1A1A",
      linewidth = 0.55
    )

  striped <- players[players$pattern == "vertical", ]
  if (nrow(striped) > 0) {
    stripes <- do.call(
      rbind,
      lapply(seq_len(nrow(striped)), function(i) {
        data.frame(
          xmin = striped$x[i] + c(-2.25, -0.75, 0.75),
          xmax = striped$x[i] + c(-1.25, 0.25, 1.75),
          ymin = striped$y[i] - 5.9,
          ymax = striped$y[i] + 4.15,
          fill = striped$secondary[i]
        )
      })
    )
    plot <- plot +
      ggplot2::geom_rect(
        data = stripes,
        ggplot2::aes(
          xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          ymax = ymax,
          fill = fill
        ),
        inherit.aes = FALSE,
        colour = NA
      )
  }
  plot
}

create_cup_tie_graphics <- function(
  comp = "bfl",
  round = "R1",
  output_directory = "cupties"
) {
  required_packages <- c("ggplot2", "magick")
  unavailable <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(unavailable) > 0) {
    stop(
      sprintf(
        "Required packages unavailable: %s",
        paste(unavailable, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  load("dreamleague/data.RDa")
  load("dreamleague/managers.RDa")
  managers <- rbind(
    transform(managers_d, league = "didsbury"),
    transform(managers_o, league = "original")
  )
  ties <- utils::read.csv(
    "data/cupties.csv",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  ties <- ties[ties$comp == comp & ties$round == round, ]
  ties$team1_canonical <- canonical_team_name(ties$team1)
  ties$team2_canonical <- canonical_team_name(ties$team2)
  if (nrow(ties) == 0) {
    stop(
      sprintf("No ties found for comp '%s', round '%s'.", comp, round),
      call. = FALSE
    )
  }

  logo_directory <- "dreamleague/img"
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  written <- character(nrow(ties))
  fallback_clubs <- character()

  for (i in seq_len(nrow(ties))) {
    home <- current_squad(dl, managers, ties$team1_canonical[i])
    away <- current_squad(dl, managers, ties$team2_canonical[i])
    home <- cbind(
      home,
      formation_coordinates("left")[c("x", "y")],
      stringsAsFactors = FALSE
    )
    away <- cbind(
      away,
      formation_coordinates("right")[c("x", "y")],
      stringsAsFactors = FALSE
    )
    players <- rbind(home, away)
    styles <- t(vapply(players$club, shirt_style, character(3)))
    players$fill <- styles[, 1]
    players$secondary <- styles[, 2]
    players$pattern <- styles[, 3]
    style_keys <- gsub("[^A-Z0-9]+", "_", toupper(players$club))
    fallback_clubs <- union(
      fallback_clubs,
      players$club[!style_keys %in% club_style_keys()]
    )
    players$name_label <- ifelse(
      players$position == "GOALKEEPER",
      NA_character_,
      players$player
    )
    players$label_x <- ifelse(players$x < 50, players$x - 6.5, players$x + 6.5)
    players$label_hjust <- ifelse(players$x < 50, 1, 0)
    players$name_y <- ifelse(players$y > 76, players$y - 8.5, players$y + 8.5)
    players$club_y <- ifelse(players$y > 76, players$y - 11.5, players$y + 5.5)

    pitch <- ggplot2::ggplot() +
      ggplot2::annotate(
        "rect",
        xmin = -16,
        xmax = 116,
        ymin = 1,
        ymax = 99,
        fill = "#16883D",
        colour = "white",
        linewidth = 1
      ) +
      ggplot2::annotate(
        "segment",
        x = 50,
        xend = 50,
        y = 1,
        yend = 99,
        colour = "white",
        linewidth = 1
      ) +
      ggplot2::annotate(
        "path",
        x = 50 + 8 * cos(seq(0, 2 * pi, length.out = 100)),
        y = 50 + 8 * sin(seq(0, 2 * pi, length.out = 100)),
        colour = "white",
        linewidth = 1
      ) +
      ggplot2::annotate(
        "point",
        x = c(50, 50),
        y = c(50, 50),
        colour = "white",
        size = 2
      ) +
      ggplot2::annotate(
        "rect",
        xmin = c(1, 87),
        xmax = c(13, 99),
        ymin = 30,
        ymax = 70,
        colour = "white",
        fill = NA,
        linewidth = 1
      ) +
      ggplot2::annotate(
        "rect",
        xmin = c(1, 93),
        xmax = c(7, 99),
        ymin = 40,
        ymax = 60,
        colour = "white",
        fill = NA,
        linewidth = 1
      ) +
      ggplot2::scale_fill_identity() +
      ggplot2::scale_colour_identity() +
      ggplot2::coord_fixed(
        xlim = c(-17, 117),
        ylim = c(0, 100),
        ratio = 0.75,
        expand = FALSE,
        clip = "off"
      ) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.margin = ggplot2::margin(12, 12, 12, 12),
        plot.background = ggplot2::element_rect(fill = "#16883D", colour = NA)
      )
    pitch <- add_shirts(pitch, players)
    pitch <- pitch +
      ggplot2::geom_text(
        data = players[
          !is.na(players$name_label) & nzchar(players$name_label),
        ],
        ggplot2::aes(
          x = label_x,
          y = name_y,
          label = name_label,
          hjust = label_hjust
        ),
        colour = "white",
        fontface = "bold",
        size = 2.8,
        lineheight = 0.9
      ) +
      ggplot2::geom_text(
        data = players[!is.na(players$club) & nzchar(players$club), ],
        ggplot2::aes(
          x = label_x,
          y = club_y,
          label = club,
          hjust = label_hjust
        ),
        colour = "white",
        size = 2.2,
        lineheight = 0.9
      ) +
      ggplot2::annotate(
        "text",
        x = 18,
        y = 94,
        label = ties$team1[i],
        colour = "white",
        fontface = "bold",
        size = 6
      ) +
      ggplot2::annotate(
        "text",
        x = 18,
        y = 89,
        label = home$manager[1],
        colour = "white",
        size = 3.5
      ) +
      ggplot2::annotate(
        "text",
        x = 82,
        y = 94,
        label = ties$team2[i],
        colour = "white",
        fontface = "bold",
        size = 6
      ) +
      ggplot2::annotate(
        "text",
        x = 82,
        y = 89,
        label = away$manager[1],
        colour = "white",
        size = 3.5
      )

    base_image <- tempfile(fileext = ".png")
    ggplot2::ggsave(
      base_image,
      pitch,
      width = 18,
      height = 9,
      dpi = 150,
      bg = "#16883D"
    )
    canvas <- magick::image_read(base_image)
    home_logo <- magick::image_read(logo_path(
      ties$team1_canonical[i],
      logo_directory
    )) |>
      magick::image_scale("150x150")
    away_logo <- magick::image_read(logo_path(
      ties$team2_canonical[i],
      logo_directory
    )) |>
      magick::image_scale("150x150")
    canvas_width <- magick::image_info(canvas)$width
    canvas <- magick::image_composite(canvas, home_logo, offset = "+25+20") |>
      magick::image_composite(
        away_logo,
        offset = sprintf("+%d+20", canvas_width - 175)
      )

    safe_name <- function(value) {
      gsub("_+", "_", gsub("[^A-Za-z0-9]+", "_", tolower(value)))
    }
    written[i] <- file.path(
      output_directory,
      sprintf(
        "%s_%s_%s_%s.png",
        comp,
        round,
        safe_name(ties$team1[i]),
        safe_name(ties$team2[i])
      )
    )
    magick::image_write(canvas, written[i], format = "png")
  }

  fallback_clubs <- sort(unique(fallback_clubs))
  if (length(fallback_clubs) > 0) {
    warning(
      sprintf(
        "Using neutral shirt styles for unconfigured clubs: %s",
        paste(fallback_clubs, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(written)
}

if (identical(environment(), globalenv()) && !interactive()) {
  create_cup_tie_graphics()
}
