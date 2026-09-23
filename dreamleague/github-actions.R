dreamleague_github_actions_config <- function(
  token = Sys.getenv("GITHUB_TOKEN", ""),
  repository = Sys.getenv("DREAMLEAGUE_GITHUB_REPOSITORY", "sgbstats/dreamleague"),
  workflow = "dl-preprocessing.yml"
) {
  if (!nzchar(token)) {
    stop(
      paste(
        "GitHub Actions is not configured.",
        "Set the GITHUB_TOKEN environment variable to a token with Actions",
        "workflow dispatch permission for ", repository, "."
      ),
      call. = FALSE
    )
  }
  if (!grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", repository)) {
    stop("DREAMLEAGUE_GITHUB_REPOSITORY must use owner/repository format", call. = FALSE)
  }
  if (!identical(workflow, "dl-preprocessing.yml")) {
    stop("Only the dl-preprocessing.yml workflow can be dispatched", call. = FALSE)
  }

  list(token = token, repository = repository, workflow = workflow)
}

github_actions_api_request <- function(method, path, config, body = NULL) {
  url <- paste0("https://api.github.com", path)
  response <- tryCatch(
    httr::VERB(
      method,
      url,
      httr::add_headers(
        Accept = "application/vnd.github+json",
        Authorization = paste("Bearer", config$token),
        `X-GitHub-Api-Version` = "2022-11-28"
      ),
      body = body,
      encode = "json",
      httr::timeout(20)
    ),
    error = function(error) {
      stop(
        sprintf(
          "GitHub API %s request failed: %s",
          method,
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )

  status_code <- httr::status_code(response)
  if (status_code < 200 || status_code >= 300) {
    headers <- httr::headers(response)
    detail <- trimws(httr::content(response, as = "text", encoding = "UTF-8"))
    if (!nzchar(detail)) {
      detail <- httr::http_status(response)$message
    }
    rate_limit_reset <- headers[["x-ratelimit-reset"]]
    rate_limit_message <- if (
      identical(status_code, 403L) && !is.null(rate_limit_reset)
    ) {
      paste0(
        " Rate limit resets at ",
        format(
          as.POSIXct(as.numeric(rate_limit_reset), origin = "1970-01-01", tz = "UTC"),
          tz = "UTC",
          usetz = TRUE
        ),
        "."
      )
    } else {
      ""
    }
    stop(
      sprintf(
        "GitHub API %s request failed (HTTP %s): %s%s",
        method,
        status_code,
        detail,
        rate_limit_message
      ),
      call. = FALSE
    )
  }

  list(
    status_code = status_code,
    content = httr::content(response, as = "text", encoding = "UTF-8")
  )
}

github_actions_parse_json <- function(content, action) {
  if (!nzchar(content)) {
    stop("GitHub API returned an empty response while ", action, call. = FALSE)
  }

  tryCatch(
    jsonlite::fromJSON(content, simplifyVector = FALSE),
    error = function(error) {
      stop(
        "GitHub API returned invalid JSON while ",
        action,
        ": ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )
}

github_actions_dispatch <- function(config, request = github_actions_api_request) {
  request(
    "POST",
    sprintf(
      "/repos/%s/actions/workflows/%s/dispatches",
      config$repository,
      config$workflow
    ),
    config,
    body = list(ref = "main")
  )
  invisible(TRUE)
}

github_actions_list_dispatched_runs <- function(
  config,
  request = github_actions_api_request
) {
  response <- request(
    "GET",
    sprintf(
      "/repos/%s/actions/workflows/%s/runs?event=workflow_dispatch&branch=main&per_page=20",
      config$repository,
      config$workflow
    ),
    config
  )
  payload <- github_actions_parse_json(response$content, "listing workflow runs")
  if (is.null(payload$workflow_runs)) {
    return(list())
  }
  payload$workflow_runs
}

github_actions_get_run <- function(
  config,
  run_id,
  request = github_actions_api_request
) {
  response <- request(
    "GET",
    sprintf("/repos/%s/actions/runs/%s", config$repository, run_id),
    config
  )
  github_actions_parse_json(response$content, "checking workflow run status")
}

github_actions_run_time <- function(run) {
  as.POSIXct(
    run$created_at,
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
}

github_actions_find_dispatched_run <- function(runs, dispatch_time) {
  if (length(runs) == 0) {
    return(NULL)
  }

  earliest_time <- as.POSIXct(dispatch_time, tz = "UTC") - 5
  eligible_runs <- Filter(
    function(run) {
      identical(run$event, "workflow_dispatch") &&
        identical(run$head_branch, "main") &&
        !is.na(github_actions_run_time(run)) &&
        github_actions_run_time(run) >= earliest_time
    },
    runs
  )
  if (length(eligible_runs) == 0) {
    return(NULL)
  }

  eligible_runs[[which.max(vapply(
    eligible_runs,
    function(run) as.numeric(github_actions_run_time(run)),
    numeric(1)
  ))]]
}

github_actions_run_is_complete <- function(run) {
  identical(run$status, "completed")
}
