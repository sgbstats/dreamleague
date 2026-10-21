repo_root <- normalizePath(file.path("..", ".."))
upload_file <- file.path(repo_root, "R", "supabase-storage.R")
app_file <- file.path(repo_root, "dreamleague", "app.R")
app_storage_file <- file.path(repo_root, "dreamleague", "supabase-storage.R")
bundle_objects <- c("dl", "daily", "time", "cupties")

load_functions <- function(path) {
  env <- new.env(parent = globalenv())
  sys.source(path, envir = env)
  env
}

test_that("app sources the storage file and pulls with supabase_download_object", {
  app_lines <- readLines(app_file, warn = FALSE)
  expect_true(any(grepl(
    'source\\(resolve_app_path\\("supabase-storage.R"\\)\\)',
    app_lines
  )))

  exprs <- parse(app_file, keep.source = FALSE)
  loader <- NULL
  for (e in exprs) {
    if (
      is.call(e) &&
        identical(e[[1]], as.name("<-")) &&
        identical(e[[2]], as.name("load_supabase_bundle"))
    ) {
      loader <- e[[3]]
    }
  }
  expect_false(is.null(loader))
  loader_calls <- all.names(loader)
  expect_true("supabase_download_object" %in% loader_calls)
  expect_true("load_dreamleague_bundle" %in% loader_calls)
})

test_that("pull functions in the app and pipeline copies are identical", {
  app_env <- load_functions(app_storage_file)
  pipeline_env <- load_functions(upload_file)

  for (fn in c(
    "supabase_object_url",
    "supabase_storage_headers",
    "supabase_require_success",
    "supabase_download_object",
    "load_dreamleague_bundle"
  )) {
    expect_identical(
      deparse(get(fn, envir = app_env)),
      deparse(get(fn, envir = pipeline_env)),
      info = fn
    )
  }
})

test_that("data uploaded to Supabase matches data pulled by the app", {
  skip_if(!nzchar(Sys.getenv("SUPABASE_API_KEY")), "SUPABASE_API_KEY not set")
  skip_if_not_installed("httr")

  local_bundle <- file.path(repo_root, "dreamleague", "data.RDa")
  skip_if(!file.exists(local_bundle), "No local data.RDa to upload")

  # Upload with the pipeline's function; pull with the app's function
  upload_env <- load_functions(upload_file)
  app_env <- load_functions(app_storage_file)

  config <- upload_env$supabase_storage_config(
    url = Sys.getenv("SUPABASE_URL", app_env$DREAMLEAGUE_SUPABASE_URL),
    bucket = Sys.getenv("DREAMLEAGUE_SUPABASE_BUCKET", "dreamleague")
  )

  # Separate object so production data is never overwritten
  test_object <- sprintf(
    "tests/roundtrip-%s-%s.RDa",
    format(Sys.time(), "%Y%m%d%H%M%S"),
    sample.int(1e6, 1)
  )
  withr::defer(
    try(
      httr::DELETE(
        upload_env$supabase_object_url(config, test_object),
        httr::add_headers(
          .headers = upload_env$supabase_storage_headers(config)
        )
      ),
      silent = TRUE
    )
  )

  up <- upload_env$supabase_upload_object(
    local_bundle,
    object_path = test_object,
    config = config
  )

  pulled_path <- withr::local_tempfile(fileext = ".RDa")
  down <- app_env$supabase_download_object(
    object_path = test_object,
    destination = pulled_path,
    config = config
  )
  expect_equal(down$bytes, up$bytes)
  expect_identical(
    unname(tools::md5sum(pulled_path)),
    unname(tools::md5sum(local_bundle))
  )

  sent <- app_env$load_dreamleague_bundle(local_bundle)
  pulled <- app_env$load_dreamleague_bundle(pulled_path)
  for (name in bundle_objects) {
    expect_identical(
      get(name, envir = pulled, inherits = FALSE),
      get(name, envir = sent, inherits = FALSE),
      info = name
    )
  }
})
