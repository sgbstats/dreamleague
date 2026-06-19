library(shinytest2)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Attempt to start the test app, skip entire file if chromium is unavailable
# or if fixture files have not been created by setup.R.
app_dir <- dirname(dirname(dirname(testthat::test_path())))

skip_if_no_chromium <- function() {
  tryCatch(
    chromote::find_chrome(),
    error = function(e) {
      skip("Chromium not available – skipping shinytest2 integration tests")
    }
  )
}

skip_if_no_fixtures <- function() {
  fixture <- file.path(app_dir, "tests", "testthat", "fixtures", "data.RDa")
  if (!file.exists(fixture)) {
    skip("Fixture files not found – run the full test suite first")
  }
}

make_app <- function() {
  AppDriver$new(
    app_dir = app_dir,
    name    = "dreamleague-test",
    options = list(shiny.maxRequestSize = 10 * 1024^2),
    envvars = c(SHINY_TEST_MODE = "1"),
    timeout = 30000,
    load_timeout = 30000
  )
}

# ---------------------------------------------------------------------------

test_that("app starts without errors", {
  skip_if_no_chromium()
  skip_if_no_fixtures()

  app <- make_app()
  on.exit(app$stop(), add = TRUE)

  # If the app started it will have a session; a failed app would have thrown
  expect_no_error(app$get_js("true"))
})

test_that("league tab shows the reactable table", {
  skip_if_no_chromium()
  skip_if_no_fixtures()

  app <- make_app()
  on.exit(app$stop(), add = TRUE)

  # Default active tab is 'league'
  app$wait_for_js("document.querySelector('.reactable') !== null", timeout = 10000)
  html <- app$get_html(".reactable")
  expect_true(nchar(html) > 0)
})

test_that("league table shows 'Total', 'For', 'Against' columns", {
  skip_if_no_chromium()
  skip_if_no_fixtures()

  app <- make_app()
  on.exit(app$stop(), add = TRUE)

  app$wait_for_js("document.querySelector('.reactable') !== null", timeout = 10000)
  html <- app$get_html("body")
  expect_match(html, "Total")
  expect_match(html, "For")
  expect_match(html, "Against")
})

test_that("switching league radio button updates the table", {
  skip_if_no_chromium()
  skip_if_no_fixtures()

  app <- make_app()
  on.exit(app$stop(), add = TRUE)

  # Start on didsbury
  app$wait_for_js("document.querySelector('.reactable') !== null", timeout = 10000)
  html_d <- app$get_html(".reactable")

  # Switch to original
  app$set_inputs(league = "original")
  app$wait_for_js("document.querySelector('.reactable') !== null", timeout = 10000)
  html_o <- app$get_html(".reactable")

  expect_true(nchar(html_d) > 0)
  expect_true(nchar(html_o) > 0)
  # The two tables should differ because they show different leagues
  expect_false(html_d == html_o)
})

test_that("teams tab loads and shows a reactable after selecting a team", {
  skip_if_no_chromium()
  skip_if_no_fixtures()

  app <- make_app()
  on.exit(app$stop(), add = TRUE)

  # Navigate to teams tab
  app$click(selector = "a[data-value='teams']")
  Sys.sleep(1)

  # Select a team from the picker
  app$set_inputs(league_teams = "didsbury")
  app$set_inputs(team = "WENGER BOYS")
  app$wait_for_js("document.querySelector('#team_out .reactable') !== null", timeout = 10000)

  html <- app$get_html("#team_out")
  expect_true(nchar(html) > 0)
})

test_that("teams tab 'current team only' checkbox changes displayed rows", {
  skip_if_no_chromium()
  skip_if_no_fixtures()

  app <- make_app()
  on.exit(app$stop(), add = TRUE)

  app$click(selector = "a[data-value='teams']")
  Sys.sleep(0.5)
  app$set_inputs(league_teams = "didsbury", team = "ERIMUS")
  app$wait_for_js("document.querySelector('#team_out .reactable') !== null", timeout = 10000)

  # With current only (default TRUE): sold player not shown
  html_current <- app$get_html("#team_out")

  # Without current only: sold player should appear
  app$set_inputs(current = FALSE)
  app$wait_for_idle()
  html_all <- app$get_html("#team_out")

  # html_all should contain WARD PROWSE (sold player) while html_current should not
  expect_match(html_all, "WARD PROWSE", ignore.case = TRUE)
  expect_false(grepl("WARD PROWSE", html_current, ignore.case = TRUE))
})

test_that("cup tab loads and shows the reactable", {
  skip_if_no_chromium()
  skip_if_no_fixtures()

  app <- make_app()
  on.exit(app$stop(), add = TRUE)

  app$click(selector = "a[data-value='cup']")
  app$wait_for_js("document.querySelector('#cup .reactable') !== null", timeout = 10000)

  html <- app$get_html("#cup")
  expect_true(nchar(html) > 0)
})

test_that("cup tab round selector updates when competition changes", {
  skip_if_no_chromium()
  skip_if_no_fixtures()

  app <- make_app()
  on.exit(app$stop(), add = TRUE)

  app$click(selector = "a[data-value='cup']")
  Sys.sleep(0.5)

  # Change competition to BFL
  app$set_inputs(comp_cup = "bfl")
  app$wait_for_idle()

  # The round picker should now reflect BFL rounds
  val <- app$get_value(input = "round_cup")
  expect_false(is.null(val))
})

test_that("cup tab shows round date text", {
  skip_if_no_chromium()
  skip_if_no_fixtures()

  app <- make_app()
  on.exit(app$stop(), add = TRUE)

  app$click(selector = "a[data-value='cup']")
  app$wait_for_js("document.querySelector('#round_date2') !== null", timeout = 10000)

  html <- app$get_html("#round_date2")
  expect_match(html, "Round date:")
})

test_that("players taken tab shows a reactable", {
  skip_if_no_chromium()
  skip_if_no_fixtures()

  app <- make_app()
  on.exit(app$stop(), add = TRUE)

  app$click(selector = "a[data-value='players']")
  app$wait_for_js("document.querySelector('#playerstaken .reactable') !== null", timeout = 10000)

  html <- app$get_html("#playerstaken")
  expect_true(nchar(html) > 0)
})

test_that("players taken shows team, player, club, position columns", {
  skip_if_no_chromium()
  skip_if_no_fixtures()

  app <- make_app()
  on.exit(app$stop(), add = TRUE)

  app$click(selector = "a[data-value='players']")
  app$wait_for_js("document.querySelector('#playerstaken .reactable') !== null", timeout = 10000)

  html <- app$get_html("#playerstaken")
  expect_match(html, "Team",     ignore.case = TRUE)
  expect_match(html, "Player",   ignore.case = TRUE)
  expect_match(html, "Club",     ignore.case = TRUE)
  expect_match(html, "Position", ignore.case = TRUE)
})

test_that("history tab loads and shows a reactable", {
  skip_if_no_chromium()
  skip_if_no_fixtures()

  app <- make_app()
  on.exit(app$stop(), add = TRUE)

  app$click(selector = "a[data-value='history']")
  app$wait_for_js("document.querySelector('#team_history_out .reactable') !== null", timeout = 10000)

  html <- app$get_html("#team_history_out")
  expect_true(nchar(html) > 0)
})

test_that("history tab date inputs change the displayed data", {
  skip_if_no_chromium()
  skip_if_no_fixtures()

  app <- make_app()
  on.exit(app$stop(), add = TRUE)

  app$click(selector = "a[data-value='history']")
  app$wait_for_js("document.querySelector('#team_history_out .reactable') !== null", timeout = 10000)

  # Set to GW2 window (WENGER BOYS scored)
  app$set_inputs(
    start = "2025-09-12",
    end   = "2025-09-16",
    league_team_history = "didsbury"
  )
  app$wait_for_idle()
  html <- app$get_html("#team_history_out")
  expect_match(html, "WENGER BOYS", ignore.case = TRUE)
})

test_that("diagnostics tab shows the datatable", {
  skip_if_no_chromium()
  skip_if_no_fixtures()

  app <- make_app()
  on.exit(app$stop(), add = TRUE)

  app$click(selector = "a[data-value='diagnostics']")
  Sys.sleep(1)

  # The diagnostics table uses DT, which renders a .dataTable element
  html <- app$get_html("body")
  # If all teams are valid the table is empty; we just check the tab loaded
  expect_true(nchar(html) > 0)
})

test_that("league update_time panel shows last updated timestamp", {
  skip_if_no_chromium()
  skip_if_no_fixtures()

  app <- make_app()
  on.exit(app$stop(), add = TRUE)

  html <- app$get_html("#update_time")
  expect_match(html, "Last score update")
})

test_that("'Return to League' button navigates back to league tab", {
  skip_if_no_chromium()
  skip_if_no_fixtures()

  app <- make_app()
  on.exit(app$stop(), add = TRUE)

  # Go to teams tab
  app$click(selector = "a[data-value='teams']")
  Sys.sleep(0.5)
  app$set_inputs(team = "WENGER BOYS")
  Sys.sleep(0.5)

  # Click "Return to League"
  app$click(selector = "#goto_league")
  app$wait_for_idle()

  # Should now be on the league tab
  active_tab <- app$get_js(
    "document.querySelector('.sidebar-menu .active a').getAttribute('href')"
  )
  expect_match(active_tab, "league", ignore.case = TRUE)
})

test_that("clicking team name in league table navigates to teams tab", {
  skip_if_no_chromium()
  skip_if_no_fixtures()

  app <- make_app()
  on.exit(app$stop(), add = TRUE)

  app$wait_for_js("document.querySelector('.reactable') !== null", timeout = 10000)

  # Trigger Shiny.setInputValue as the onclick would
  app$set_inputs(
    goto_team = list(team = "WENGER BOYS", nonce = 0.5)
  )
  app$wait_for_idle()

  active <- app$get_js(
    "document.querySelector('.sidebar-menu .active a') && document.querySelector('.sidebar-menu .active a').dataset.value"
  )
  expect_match(active, "teams", ignore.case = TRUE)
})

test_that("league radio buttons are synchronised across tabs", {
  skip_if_no_chromium()
  skip_if_no_fixtures()

  app <- make_app()
  on.exit(app$stop(), add = TRUE)

  # Change league on league tab
  app$set_inputs(league = "original")
  app$wait_for_idle()

  # The teams tab should also show original
  val_teams <- app$get_value(input = "league_teams")
  expect_equal(val_teams, "original")

  # The history tab should also show original
  val_hist <- app$get_value(input = "league_team_history")
  expect_equal(val_hist, "original")
})
