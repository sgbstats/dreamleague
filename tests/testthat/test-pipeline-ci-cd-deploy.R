test_that("deployment requires credentials before contacting Connect Cloud", {
  withr::local_envvar(c(
    CONNECT_CLOUD_CLIENT_ID = NA_character_,
    CONNECT_CLOUD_CLIENT_SECRET = NA_character_,
    CONNECT_CLOUD_ACCOUNT = NA_character_,
    CONNECT_CLOUD_APP_ID = NA_character_
  ))
  expect_error(
    source(file.path("..", "..", ".github", "scripts", "deploy-connect-cloud.R")),
    "Missing required deployment settings: CONNECT_CLOUD_CLIENT_ID"
  )
})

test_that("deployment targets the existing content without opening a browser", {
  withr::local_envvar(c(
    CONNECT_CLOUD_CLIENT_ID = "test-client",
    CONNECT_CLOUD_CLIENT_SECRET = "test-secret",
    CONNECT_CLOUD_ACCOUNT = "test-account",
    CONNECT_CLOUD_APP_ID = "test-content",
    GOOGLE_PRIVATE_KEY = "test-google-key",
    SUPABASE_API_KEY = "test-supabase-key",
    ACTION_TOKEN = NA_character_,
    DREAMLEAGUE_SHARED_DRIVE_TARGET = "",
    SUPABASE_URL = NA_character_,
    DREAMLEAGUE_SUPABASE_BUCKET = NA_character_
  ))
  withr::local_options(repos = getOption("repos"))
  testthat::local_mocked_bindings(
    connectCloudClientCredentials = function(clientId, clientSecret, accountName, quiet) {
      expect_identical(clientId, "test-client")
      expect_identical(clientSecret, "test-secret")
      expect_identical(accountName, "test-account")
      expect_true(quiet)
    },
    deployApp = function(appDir, appId, account, server, launch.browser, forceUpdate, envVars) {
      expect_identical(appDir, "dreamleague")
      expect_identical(appId, "test-content")
      expect_identical(account, "test-account")
      expect_identical(server, "connect.posit.cloud")
      expect_false(launch.browser)
      expect_true(forceUpdate)
      expect_identical(envVars, c("GOOGLE_PRIVATE_KEY", "SUPABASE_API_KEY"))
      TRUE
    },
    .package = "rsconnect"
  )
  expect_error(
    source(file.path("..", "..", ".github", "scripts", "deploy-connect-cloud.R")),
    NA
  )
})

test_that("deployment maps CI repository names to resolvable URLs for the manifest", {
  withr::local_envvar(c(
    CONNECT_CLOUD_CLIENT_ID = "test-client",
    CONNECT_CLOUD_CLIENT_SECRET = "test-secret",
    CONNECT_CLOUD_ACCOUNT = "test-account",
    CONNECT_CLOUD_APP_ID = "test-content",
    GOOGLE_PRIVATE_KEY = "test-google-key",
    SUPABASE_API_KEY = "test-supabase-key",
    ACTION_TOKEN = NA_character_,
    DREAMLEAGUE_SHARED_DRIVE_TARGET = "",
    SUPABASE_URL = NA_character_,
    DREAMLEAGUE_SUPABASE_BUCKET = NA_character_
  ))
  withr::local_options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/noble/latest"))
  repos_at_deploy <- NULL
  testthat::local_mocked_bindings(
    connectCloudClientCredentials = function(...) invisible(NULL),
    deployApp = function(...) {
      repos_at_deploy <<- getOption("repos")
      TRUE
    },
    .package = "rsconnect"
  )
  source(file.path("..", "..", ".github", "scripts", "deploy-connect-cloud.R"))
  expect_true(all(c("CRAN", "RSPM") %in% names(repos_at_deploy)))
  expect_match(repos_at_deploy[c("CRAN", "RSPM")], "^https://")
  expect_false(any(grepl("__linux__", repos_at_deploy[c("CRAN", "RSPM")])))
})

test_that("deploy jobs install packages as standard refs so the manifest resolves repositories", {
  # `any::pkg` is recorded by renv with Source "any", which rsconnect writes to
  # manifest.json with the unresolvable repository name "RSPM".
  workflow <- readLines(file.path("..", "..", ".github", "workflows", "ci-cd.yml"))
  deploy_lines <- workflow[seq(grep("^  deploy-staging:", workflow), length(workflow))]
  expect_false(any(grepl("any::", deploy_lines, fixed = TRUE)))
})

test_that("deployment requires the app's runtime secrets", {
  withr::local_envvar(c(
    CONNECT_CLOUD_CLIENT_ID = "test-client",
    CONNECT_CLOUD_CLIENT_SECRET = "test-secret",
    CONNECT_CLOUD_ACCOUNT = "test-account",
    CONNECT_CLOUD_APP_ID = "test-content",
    GOOGLE_PRIVATE_KEY = "",
    SUPABASE_API_KEY = "test-supabase-key"
  ))
  testthat::local_mocked_bindings(
    connectCloudClientCredentials = function(...) stop("must not authenticate"),
    deployApp = function(...) stop("must not deploy"),
    .package = "rsconnect"
  )
  expect_error(
    source(file.path("..", "..", ".github", "scripts", "deploy-connect-cloud.R")),
    "Missing required app runtime secrets: GOOGLE_PRIVATE_KEY"
  )
})

test_that("deployment sends every configured runtime variable to the app", {
  # rsconnect replaces the content's variables on each Connect Cloud deploy,
  # so anything not sent here is removed from the app.
  withr::local_envvar(c(
    CONNECT_CLOUD_CLIENT_ID = "test-client",
    CONNECT_CLOUD_CLIENT_SECRET = "test-secret",
    CONNECT_CLOUD_ACCOUNT = "test-account",
    CONNECT_CLOUD_APP_ID = "test-content",
    GOOGLE_PRIVATE_KEY = "k",
    SUPABASE_API_KEY = "s",
    ACTION_TOKEN = "a",
    DREAMLEAGUE_SHARED_DRIVE_TARGET = "d",
    SUPABASE_URL = "https://example.supabase.co",
    DREAMLEAGUE_SUPABASE_BUCKET = "b"
  ))
  withr::local_options(repos = getOption("repos"))
  sent <- NULL
  testthat::local_mocked_bindings(
    connectCloudClientCredentials = function(...) invisible(NULL),
    deployApp = function(..., envVars) {
      sent <<- envVars
      TRUE
    },
    .package = "rsconnect"
  )
  source(file.path("..", "..", ".github", "scripts", "deploy-connect-cloud.R"))
  expect_setequal(sent, c(
    "GOOGLE_PRIVATE_KEY", "SUPABASE_API_KEY", "ACTION_TOKEN",
    "DREAMLEAGUE_SHARED_DRIVE_TARGET", "SUPABASE_URL", "DREAMLEAGUE_SUPABASE_BUCKET"
  ))
})

test_that("deploy jobs pass the app runtime secrets to the deploy script", {
  workflow <- readLines(file.path("..", "..", ".github", "workflows", "ci-cd.yml"))
  workflow <- paste(workflow[seq(grep("^  deploy-staging:", workflow), length(workflow))], collapse = "\n")
  for (name in c("GOOGLE_PRIVATE_KEY", "SUPABASE_API_KEY")) {
    hits <- gregexpr(paste0(name, ": \\$\\{\\{ secrets\\.", name, " \\}\\}"), workflow)[[1]]
    expect_identical(sum(hits > 0), 2L, info = name)
  }
})

test_that("an unsuccessful cloud deployment fails the job", {
  withr::local_envvar(c(
    CONNECT_CLOUD_CLIENT_ID = "test-client",
    CONNECT_CLOUD_CLIENT_SECRET = "test-secret",
    CONNECT_CLOUD_ACCOUNT = "test-account",
    CONNECT_CLOUD_APP_ID = "test-content",
    GOOGLE_PRIVATE_KEY = "test-google-key",
    SUPABASE_API_KEY = "test-supabase-key",
    ACTION_TOKEN = NA_character_,
    DREAMLEAGUE_SHARED_DRIVE_TARGET = "",
    SUPABASE_URL = NA_character_,
    DREAMLEAGUE_SUPABASE_BUCKET = NA_character_
  ))
  testthat::local_mocked_bindings(
    connectCloudClientCredentials = function(...) invisible(NULL),
    deployApp = function(...) FALSE,
    .package = "rsconnect"
  )
  withr::local_options(repos = getOption("repos"))
  expect_error(
    source(file.path("..", "..", ".github", "scripts", "deploy-connect-cloud.R")),
    "Posit Connect Cloud deployment failed"
  )
})
