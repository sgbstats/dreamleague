source_functions_only <- function(path, envir = new.env(parent = globalenv())) {
  expressions <- parse(path)
  function_expression <- vapply(
    expressions,
    function(expression) {
      is.call(expression) &&
        (identical(as.character(expression[[1]]), "<-") ||
          identical(as.character(expression[[1]]), "=")) &&
        is.call(expression[[3]]) &&
        identical(as.character(expression[[3]][[1]]), "function")
    },
    logical(1)
  )
  lapply(expressions[function_expression], eval, envir = envir)
  envir
}

pull_env <- source_functions_only(file.path("..", "..", "R", "dl-file-pull.R"))
run_env <- source_functions_only(file.path(
  "..",
  "..",
  "R",
  "dl-preprocessing-run.R"
))
run_env$shared_drive_path <- NULL
file_check_env <- source_functions_only(file.path(
  "..",
  "..",
  "R",
  "dl-file-check.R"
))
fast_env <- source_functions_only(file.path(
  "..",
  "..",
  "R",
  "dl-preprocessing-fast.R"
))
slow_env <- source_functions_only(file.path(
  "..",
  "..",
  "R",
  "dl-preprocessing-slow.R"
))

make_remote_listing <- function(name, modified_time) {
  data.frame(
    name = name,
    modified_time = as.POSIXct(modified_time, tz = "UTC")
  )
}

test_that("remote xlsx refresh handles missing files and empty listings", {
  missing_path <- tempfile(fileext = ".xlsx")
  downloaded <- FALSE
  result <- pull_env$refresh_remote_xlsx(
    missing_path,
    listing_fn = function(name) {
      make_remote_listing(name, "2026-09-01 10:00:00")
    },
    download_fn = function(...) downloaded <<- TRUE
  )

  expect_identical(result, "downloaded")
  expect_true(downloaded)

  result <- pull_env$refresh_remote_xlsx(
    missing_path,
    listing_fn = function(name) {
      data.frame(
        name = character(),
        modified_time = as.POSIXct(character())
      )
    },
    download_fn = function(...) downloaded <<- TRUE
  )

  expect_identical(result, "not_found")
  expect_false(file.exists(missing_path))
})

test_that("remote xlsx refresh selects the newest matching file", {
  local_path <- tempfile(fileext = ".xlsx")
  file.create(local_path)
  Sys.setFileTime(local_path, as.POSIXct("2026-09-01 10:00:00", tz = "UTC"))
  downloaded_file <- NULL

  result <- pull_env$refresh_remote_xlsx(
    local_path,
    listing_fn = function(name) {
      data.frame(
        name = c(name, name),
        modified_time = as.POSIXct(
          c("2026-09-01 11:00:00", "2026-09-02 11:00:00"),
          tz = "UTC"
        )
      )
    },
    download_fn = function(file, path, overwrite) downloaded_file <<- file
  )

  expect_identical(result, "downloaded")
  expect_identical(
    downloaded_file$modified_time[[1]],
    as.POSIXct(
      "2026-09-02 11:00:00",
      tz = "UTC"
    )
  )
})

test_that("authentication helpers do not error without credentials", {
  expect_invisible(pull_env$try_drive_auth(tempfile()))
  expect_null(pull_env$resolve_shared_drive_path(""))
  expect_invisible(run_env$safe_gs4_auth(tempfile()))
})

test_that("daily schema normalization supplies either application column", {
  expect_identical(
    run_env$normalize_daily_schema(data.frame(App = 1:2))$SBapp,
    1:2
  )
  expect_identical(
    run_env$normalize_daily_schema(data.frame(SBapp = 3:4))$App,
    3:4
  )
  expect_identical(
    names(run_env$normalize_daily_schema(data.frame(App = 1, SBapp = 2))),
    c("App", "SBapp")
  )
})

test_that("drive upload is a no-op without a shared drive", {
  expect_invisible(run_env$upload_to_drive("missing.RDa", "data.RDa"))
})

test_that("scraping scripts expose the same core interface", {
  expect_true(is.function(fast_env$scraplinks2))
  expect_true(is.function(fast_env$dl_process))
  expect_true(is.function(slow_env$scraplinks2))
  expect_true(is.function(slow_env$dl_process))
  expect_true(all(
    c("dl", "managers", "league", "cut_time") %in%
      names(formals(slow_env$dl_process))
  ))
  expect_true("season_id" %in% names(formals(fast_env$dl_process)))
})

test_that("scraping functions fail predictably for malformed local HTML", {
  html_path <- tempfile(fileext = ".html")
  writeLines("<html><body><table></table></body></html>", html_path)

  fast_env$comps <- "Premier League"
  slow_env$comps <- "Premier League"
  expect_error(fast_env$scraplinks2(paste0("file://", html_path)))
  expect_error(slow_env$scraplinks2(paste0("file://", html_path)))
})

test_that("file check exposes a callable checker without running it on source", {
  expect_true(is.function(file_check_env$run_file_check))
  expect_identical(
    names(formals(file_check_env$run_file_check)),
    NULL
  )
})
