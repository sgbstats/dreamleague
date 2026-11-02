source(file.path("..", "..", "dreamleague", "github-actions.R"))

test_that("GitHub Actions configuration requires a token and valid repository", {
  expect_error(
    dreamleague_github_actions_config(token = ""),
    "GITHUB_TOKEN"
  )
  expect_error(
    dreamleague_github_actions_config(token = "token", repository = "invalid"),
    "owner/repository"
  )
  expect_error(
    dreamleague_github_actions_config(
      token = "token",
      workflow = "another-workflow.yml"
    ),
    "dl-preprocessing"
  )
})

test_that("workflow dispatch targets main through the configured endpoint", {
  config <- dreamleague_github_actions_config(token = "token")
  captured <- NULL
  request <- function(method, path, config, body = NULL) {
    captured <<- list(method = method, path = path, body = body)
    list(status_code = 204L, content = "")
  }

  github_actions_dispatch(config, request = request)

  expect_identical(captured$method, "POST")
  expect_identical(
    captured$path,
    "/repos/sgbstats/dreamleague/actions/workflows/dl-preprocessing.yml/dispatches"
  )
  expect_identical(captured$body, list(ref = "main"))
})

test_that("dispatched run selection requires a matching event, branch, and time", {
  dispatch_time <- as.POSIXct("2026-09-23 14:00:00", tz = "UTC")
  runs <- list(
    list(
      id = 1,
      event = "schedule",
      head_branch = "main",
      created_at = "2026-09-23T14:01:00Z"
    ),
    list(
      id = 2,
      event = "workflow_dispatch",
      head_branch = "feature",
      created_at = "2026-09-23T14:02:00Z"
    ),
    list(
      id = 3,
      event = "workflow_dispatch",
      head_branch = "main",
      created_at = "2026-09-23T13:59:50Z"
    ),
    list(
      id = 4,
      event = "workflow_dispatch",
      head_branch = "main",
      created_at = "2026-09-23T14:03:00Z"
    )
  )

  expect_identical(
    github_actions_find_dispatched_run(runs, dispatch_time)$id,
    4
  )
})

test_that("workflow completion only recognizes completed runs", {
  expect_true(github_actions_run_is_complete(list(status = "completed")))
  expect_false(github_actions_run_is_complete(list(status = "in_progress")))
})
