source(file.path("..", "..", "R", "benchmark-drive-data-formats.R"))

make_benchmark_bundle <- function() {
  list(
    dl = data.frame(
      player = c("A", "B"),
      goals = c(1, NA_real_),
      bought2 = as.Date(c("2026-07-01", NA)),
      active = c(TRUE, FALSE),
      stringsAsFactors = FALSE
    ),
    daily = data.frame(
      Date = as.Date(c("2026-08-01", "2026-08-08")),
      score = c(2.5, NA_real_),
      stringsAsFactors = FALSE
    ),
    time = list(
      update_time = as.POSIXct("2026-09-03 10:00:00", tz = "UTC"),
      mod_d = as.POSIXct("2026-09-02 10:00:00", tz = "UTC")
    ),
    cupties = data.frame(
      date = as.Date("2026-08-10"),
      team1 = "A",
      stringsAsFactors = FALSE
    )
  )
}

test_that("benchmark artifacts round-trip through each format", {
  bundle <- make_benchmark_bundle()
  bundle$cupties <- bundle$cupties[0, , drop = FALSE]
  for (format in c("rda", "json_bundle", "json_separate", "csv")) {
    directory <- tempfile(pattern = paste0("benchmark-", format, "-"))
    write_benchmark_artifacts(bundle, directory, format)
    expect_true(compare_bundles(
      bundle,
      read_benchmark_bundle(directory, format)
    ))
    unlink(directory, recursive = TRUE)
  }
})

test_that("all local benchmark formats pass preflight validation", {
  bundle <- make_benchmark_bundle()
  bundle$cupties <- bundle$cupties[0, , drop = FALSE]
  expect_true(validate_local_formats(
    bundle,
    c("rda", "json_bundle", "json_separate", "csv")
  ))
})

test_that("local preflight identifies the failing format", {
  bundle <- make_benchmark_bundle()
  expect_error(
    validate_local_formats(bundle, "unknown"),
    "Unknown format: unknown"
  )
})

test_that("unknown formats fail explicitly", {
  expect_error(
    write_benchmark_artifacts(make_benchmark_bundle(), tempfile(), "xml"),
    "Unknown format"
  )
})

test_that("testing target must be configured", {
  expect_error(resolve_testing_target(""), "DREAMLEAGUE_TESTING_TARGET")
})

test_that("remote artifact lookup preserves requested order", {
  lookup <- function(target, names) {
    listing <- data.frame(name = c("b", "a"), id = c("2", "1"))
    listing[match(names, listing$name), , drop = FALSE]
  }
  expect_identical(lookup(NULL, c("a", "b"))$name, c("a", "b"))
})

test_that("remote artifact cleanup ignores missing Drive files", {
  expect_invisible(remove_remote_artifacts(
    NULL,
    "missing-file",
    listing_fn = function(...) {
      stop("Client error: (404) Not Found")
    }
  ))
})
