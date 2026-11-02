library(testthat)

# DREAMLEAGUE_TEST_SET selects test files by prefix: app (test-app-*),
# pipeline (test-pipeline-*), or all (default). test-shared-* runs in both.
test_set <- Sys.getenv("DREAMLEAGUE_TEST_SET", "all")
filter <- switch(
  test_set,
  all = NULL,
  app = "^(app|shared)-",
  pipeline = "^(pipeline|shared)-",
  stop("DREAMLEAGUE_TEST_SET must be one of: all, app, pipeline", call. = FALSE)
)

test_dir("tests/testthat", filter = filter)
