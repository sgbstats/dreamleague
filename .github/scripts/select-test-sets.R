# The same area tags select changed paths and test files in the local runner.
test_area_filters <- c(
  app = "^(app-|shared-)",
  pipeline = "^pipeline-(dl-|validate-dreamleague-data)",
  wengerboys = "^pipeline-wengerboys",
  storage = "^(pipeline-supabase-storage|shared-supabase-roundtrip)",
  benchmarks = "^pipeline-benchmark-",
  ci = "^(pipeline-(select-test-sets|ci-cd-deploy)|shared-github-token)"
)

app_runtime_patterns <- c(
  "^dreamleague/",
  "^(Dockerfile|\\.dockerignore|render\\.yaml)$"
)

test_set_patterns <- list(
  app = app_runtime_patterns,
  pipeline = c(
    "^R/(dl-|validate-dreamleague-data|scoutingreport)",
    "^dreamleague/google-credentials\\.R$",
    "^\\.github/workflows/dl-preprocessing\\.yml$"
  ),
  wengerboys = c(
    "^R/wengerboys-",
    "^data/league92_clubs\\.csv$",
    "^\\.github/workflows/wengerboys-weekly\\.yml$",
    "^tests/testthat/fixtures/"
  ),
  storage = c(
    "^R/supabase-storage\\.R$",
    "^dreamleague/(supabase-storage|app)\\.R$"
  ),
  benchmarks = c(
    "^R/benchmark-",
    "^R/(dl-file-pull|supabase-storage)\\.R$"
  ),
  ci = c(
    "^\\.github/scripts/",
    "^\\.github/workflows/ci-cd\\.yml$",
    "^(Dockerfile|\\.dockerignore|render\\.yaml)$"
  ),
  all = c(
    "^\\*$",
    "^tests/testthat\\.R$",
    "^tests/testthat/(helper|setup)"
  ),
  supabase = c(
    "^dreamleague/(app|supabase-storage)\\.R$",
    "^R/supabase-storage\\.R$",
    "^tests/testthat/test-(pipeline-supabase-storage|shared-supabase-roundtrip)\\.R$"
  )
)

matches_any <- function(paths, patterns) {
  any(vapply(patterns, function(p) any(grepl(p, paths)), logical(1)))
}

select_test_sets <- function(paths) {
  paths <- gsub("\\\\", "/", paths[nzchar(paths)])
  run_all <- matches_any(paths, test_set_patterns$all)
  changed_tests <- sub("^tests/testthat/test-", "", paths[
    grepl("^tests/testthat/test-.*\\.R$", paths)
  ])
  if (length(changed_tests) && any(!vapply(changed_tests, function(test) {
    matches_any(test, test_area_filters)
  }, logical(1)))) {
    stop("Changed test file has no registered area tag.", call. = FALSE)
  }
  areas <- names(test_area_filters)[vapply(names(test_area_filters), function(area) {
    matches_any(paths, test_set_patterns[[area]]) ||
      any(grepl(test_area_filters[[area]], changed_tests))
  }, logical(1))]
  # Shared upload helpers are consumed by preprocessing and the weekly report.
  if ("R/supabase-storage.R" %in% paths) {
    areas <- union(areas, c("pipeline", "wengerboys"))
  }
  known <- unlist(test_set_patterns[names(test_area_filters)], use.names = FALSE)
  unclassified <- paths[
    grepl("^(R/|data/|\\.github/|tests/testthat/test-)", paths) &
      !vapply(paths, function(path) matches_any(path, known), logical(1)) &
      !grepl("^tests/testthat/test-", paths)
  ]
  if (length(unclassified)) areas <- union(areas, "pipeline")
  areas <- names(test_area_filters)[names(test_area_filters) %in% areas]
  tests <- if (run_all) "all" else if (length(areas)) paste(areas, collapse = ",") else "none"
  list(
    tests = tests,
    supabase = run_all || matches_any(paths, test_set_patterns$supabase),
    app = "*" %in% paths || matches_any(paths, app_runtime_patterns)
  )
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1L) {
    stop("Usage: Rscript select-test-sets.R <changed-files.txt>", call. = FALSE)
  }
  selected <- select_test_sets(readLines(args[[1]], warn = FALSE))
  lines <- c(
    paste0("tests=", selected$tests),
    paste0("supabase=", tolower(selected$supabase)),
    paste0("app=", tolower(selected$app))
  )
  writeLines(lines)
  output <- Sys.getenv("GITHUB_OUTPUT")
  if (nzchar(output)) {
    cat(lines, file = output, sep = "\n", append = TRUE)
  }
}
