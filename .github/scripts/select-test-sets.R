# Decides which test sets CI runs from the list of changed paths.
#   app      - the Shiny app in dreamleague/ (test-app-*.R)
#   pipeline - R/ data scripts, data/, and .github/ CI/CD (test-pipeline-*.R)
# test-shared-*.R files belong to both sets. A changed path of "*" (used when
# the diff cannot be computed) or a change to shared test infrastructure runs
# everything.

test_set_patterns <- list(
  app = c(
    "^dreamleague/",
    "^tests/testthat/test-app-",
    "^tests/testthat/test-shared-"
  ),
  pipeline = c(
    "^R/",
    "^data/",
    "^\\.github/",
    "^tests/testthat/test-pipeline-",
    "^tests/testthat/test-shared-",
    "^tests/testthat/fixtures/"
  ),
  all = c(
    "^\\*$",
    "^tests/testthat\\.R$",
    "^tests/testthat/(helper|setup)"
  ),
  supabase = c(
    "^dreamleague/",
    "supabase",
    "^\\.github/workflows/ci-cd\\.yml$",
    "^tests/testthat\\.R$"
  )
)

matches_any <- function(paths, patterns) {
  any(vapply(patterns, function(p) any(grepl(p, paths)), logical(1)))
}

select_test_sets <- function(paths) {
  paths <- paths[nzchar(paths)]
  run_all <- matches_any(paths, test_set_patterns$all)
  app <- run_all || matches_any(paths, test_set_patterns$app)
  pipeline <- run_all || matches_any(paths, test_set_patterns$pipeline)
  tests <- if (app && pipeline) "all" else if (app) "app" else if (pipeline) "pipeline" else "none"
  list(
    tests = tests,
    supabase = run_all || matches_any(paths, test_set_patterns$supabase)
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
    paste0("supabase=", tolower(selected$supabase))
  )
  writeLines(lines)
  output <- Sys.getenv("GITHUB_OUTPUT")
  if (nzchar(output)) {
    cat(lines, file = output, sep = "\n", append = TRUE)
  }
}
