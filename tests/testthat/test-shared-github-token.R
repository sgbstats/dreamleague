source(file.path("..", "..", "dreamleague", "github-actions.R"))

test_that("API errors stay fatal unless the probe explicitly accepts 422", {
  config <- dreamleague_github_actions_config(token = "test-token")
  for (status in c(401L, 403L, 404L, 422L)) {
    local({
      response <- structure(list(
        url = "https://api.github.com/test",
        status_code = status,
        headers = structure(
          list(`content-type` = "application/json"),
          class = c("insensitive", "list")
        ),
        content = charToRaw('{"message":"Rejected"}')
      ), class = "response")
      testthat::local_mocked_bindings(
        VERB = function(...) response,
        .package = "httr"
      )
      expect_error(
        github_actions_api_request("POST", "/test", config),
        paste0("HTTP ", status)
      )
      if (status == 422L) {
        expect_identical(
          github_actions_api_request("POST", "/test", config, allowed_status = 422L)$status_code,
          422L
        )
      } else {
        expect_error(
          github_actions_api_request("POST", "/test", config, allowed_status = 422L),
          paste0("HTTP ", status)
        )
      }
    })
  }
})

test_that("token probe checks reads and dispatch without a valid ref", {
  calls <- list()
  request <- function(method, path, config, body = NULL, allowed_status = integer()) {
    calls[[length(calls) + 1L]] <<- list(method, path, body)
    if (method == "POST") {
      expect_identical(allowed_status, 422L)
      expect_identical(body$ref, "~dreamleague-token-permission-check")
      return(list(
        status_code = 422L,
        content = '{"message":"No ref found for: ~dreamleague-token-permission-check"}'
      ))
    }
    list(status_code = 200L, content = '{"state":"active","workflow_runs":[]}')
  }
  expect_true(github_actions_check_token(
    dreamleague_github_actions_config(token = "test-token"), request
  ))
  expect_length(calls, 3L)
  expect_identical(calls[[1]][[2]],
                   "/repos/sgbstats/dreamleague/actions/workflows/dl-preprocessing.yml")
  expect_match(calls[[2]][[2]], "/runs\\?")
  expect_match(calls[[3]][[2]], "/dispatches$")
})

test_that("token probe rejects disabled workflows and unexpected errors", {
  config <- dreamleague_github_actions_config(token = "test-token")
  expect_error(github_actions_check_token(config, function(...) {
    list(status_code = 200L, content = '{"state":"disabled_manually"}')
  }), "not active")
  expect_error(github_actions_check_token(config, function(method, ...) {
    if (method == "POST") {
      return(list(status_code = 422L, content = '{"message":"Validation Failed"}'))
    }
    list(status_code = 200L, content = '{"state":"active","workflow_runs":[]}')
  }), "did not confirm")
  for (status in c(401L, 403L, 404L)) {
    expect_error(github_actions_check_token(config, function(...) {
      stop(sprintf("GitHub API request failed (HTTP %s)", status))
    }), paste0("HTTP ", status))
  }
})

test_that("CI checks the deployed environment PAT and gates every promotion", {
  workflow <- readLines(file.path("..", "..", ".github", "workflows", "ci-cd.yml"))
  job_text <- function(name) {
    start <- match(paste0("  ", name, ":"), workflow)
    ends <- which(grepl("^  [a-z-]+:$", workflow) & seq_along(workflow) > start)
    end <- if (length(ends)) min(ends) - 1L else length(workflow)
    paste(workflow[start:end], collapse = "\n")
  }
  job <- job_text("test-github-token")
  expect_match(job, "github.ref == 'refs/heads/main' && 'Production' || 'staging'", fixed = TRUE)
  expect_match(job, "refs/heads/staging", fixed = TRUE)
  expect_match(job, "ACTION_TOKEN: ${{ secrets.ACTION_TOKEN }}", fixed = TRUE)
  expect_match(job, 'DREAMLEAGUE_REQUIRE_GITHUB_TOKEN_TEST: "true"', fixed = TRUE)
  for (name in c("promote-staging", "deploy-staging", "deploy-production")) {
    expect_match(job_text(name), "needs: [test, test-supabase, test-github-token]", fixed = TRUE)
    expect_match(job_text(name), "needs.test-github-token.result", fixed = TRUE)
  }
})

test_that("configured app PAT can read and dispatch the data collection workflow", {
  required <- identical(Sys.getenv("DREAMLEAGUE_REQUIRE_GITHUB_TOKEN_TEST"), "true")
  if (!required) {
    skip("Live PAT probe only runs in the dedicated CI check")
  }
  token <- Sys.getenv("ACTION_TOKEN")
  if (!nzchar(token)) {
    stop(
      paste(
        "ACTION_TOKEN is missing. Add an ACTION_TOKEN repository or deployment environment secret with",
        "Actions: read and write access to sgbstats/dreamleague."
      ),
      call. = FALSE
    )
  }
  expect_true(github_actions_check_token(
    dreamleague_github_actions_config(token = token)
  ))
})
