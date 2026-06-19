# Helper: replicate output$round_date2 logic from the server
format_round_date <- function(rd) {
  rd <- as.Date(rd)
  if (lubridate::month(rd) == lubridate::month(rd + 3)) {
    paste0("Round date: ", format(rd, format = "%d"), "-", format(rd + 3, format = "%d %b"))
  } else {
    paste0("Round date: ", format(rd, format = "%d %b"), "-", format(rd + 3, format = "%d %b"))
  }
}

# ---------------------------------------------------------------------------

test_that("same-month range shows only start day, not month, for the start", {
  result <- format_round_date("2025-09-12")
  # Sep 12 to Sep 15 – same month
  expect_match(result, "^Round date: 12-15 Sep$")
})

test_that("cross-month range shows full 'dd Mon' for both dates", {
  result <- format_round_date("2025-08-30")
  # Aug 30 to Sep 2 – different months
  expect_match(result, "^Round date: 30 Aug-02 Sep$")
})

test_that("end-of-January cross-February boundary is formatted correctly", {
  result <- format_round_date("2026-01-29")
  # Jan 29 to Feb 1 – different months
  expect_match(result, "^Round date: 29 Jan-01 Feb$")
})

test_that("mid-December to mid-December stays in same month", {
  result <- format_round_date("2025-12-12")
  # Dec 12 to Dec 15 – same month
  expect_match(result, "^Round date: 12-15 Dec$")
})

test_that("Dec 29 to Jan 1 spans year boundary (different months)", {
  result <- format_round_date("2025-12-29")
  # Dec 29 to Jan 1 – different months
  expect_match(result, "^Round date: 29 Dec-01 Jan$")
})

test_that("format_round_date always starts with 'Round date: '", {
  dates <- c("2025-08-16", "2025-10-01", "2025-12-28", "2026-01-30")
  for (d in dates) {
    result <- format_round_date(d)
    expect_match(result, "^Round date: ", info = paste("date:", d))
  }
})

test_that("same-month label does not duplicate month name", {
  result <- format_round_date("2025-11-14")
  # Nov 14 to Nov 17 – same month, so format should be "14-17 Nov" not "14 Nov-17 Nov"
  expect_false(grepl("Nov.*Nov", result))
  expect_match(result, "Nov")
})

test_that("round date is pulled from cupties for a real round", {
  # Validate that mock_cupties supplies a parseable date for round_date2
  rd <- mock_cupties |>
    filter(round == "R1", comp == "didsbury") |>
    slice_head(n = 1) |>
    pull(date)

  expect_false(is.na(rd))
  result <- format_round_date(rd)
  expect_match(result, "^Round date: ")
})

test_that("round date SF is computed correctly from cupties", {
  rd <- mock_cupties |>
    filter(round == "SF", comp == "original") |>
    slice_head(n = 1) |>
    pull(date)

  # SF date is 2026-01-16, so window is 2026-01-16 to 2026-01-19 (same month Jan)
  result <- format_round_date(rd)
  expect_match(result, "^Round date: 16-19 Jan$")
})
