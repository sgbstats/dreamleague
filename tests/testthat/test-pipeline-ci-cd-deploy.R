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

test_that("Render Blueprint adopts both services without syncing app secrets", {
  blueprint <- readLines(file.path("..", "..", "render.yaml"))
  blueprint <- paste(blueprint, collapse = "\n")
  expect_match(blueprint, "name: dreamleague-staging")
  expect_match(blueprint, "name: dreamleague")
  expect_match(blueprint, "branch: staging")
  expect_match(blueprint, "branch: main")
  expect_match(blueprint, "autoDeployTrigger: commit")
  expect_match(blueprint, "autoDeployTrigger: off")
  expect_match(blueprint, "dreamleague\\.uk")
  expect_false(grepl("plan:", blueprint, fixed = TRUE))
  expect_equal(lengths(gregexpr("sync: false", blueprint, fixed = TRUE)), 12L)
})

test_that("Docker image runs Shiny on Render's configured port", {
  dockerfile <- paste(
    readLines(file.path("..", "..", "Dockerfile")),
    collapse = "\n"
  )
  expect_true(grepl("FROM rocker/r-ver:4.6.2", dockerfile, fixed = TRUE))
  expect_true(grepl(
    "shiny::runApp('.', host = '0.0.0.0', port = as.integer(Sys.getenv('PORT', '10000')))",
    dockerfile,
    fixed = TRUE
  ))
})

test_that("Docker build includes the system libuv headers required by fs", {
  dockerfile <- readLines(file.path("..", "..", "Dockerfile"))
  expect_true(any(trimws(dockerfile) == "libuv1-dev \\"))
})

test_that("Docker package installation fails when a runtime dependency cannot load", {
  dockerfile <- readLines(file.path("..", "..", "Dockerfile"))
  install_line <- grep("^RUN R -e ", dockerfile, value = TRUE)
  expect_length(install_line, 1L)
  install_code <- sub('^RUN R -e "(.*)"$', "\\1", install_line)
  for (missing_package in c("shiny", "magick", "googledrive")) {
    checked <- character()
    build_env <- new.env(parent = baseenv())
    build_env$install.packages <- function(pkgs, repos) invisible(NULL)
    build_env$requireNamespace <- function(package, quietly) {
      checked <<- c(checked, package)
      package != missing_package
    }
    expect_error(
      eval(parse(text = install_code), envir = build_env),
      paste0("Required R packages could not be loaded: ", missing_package),
      fixed = TRUE
    )
    expect_contains(checked, missing_package)
  }
})

test_that("Docker package installation accepts a complete loadable runtime", {
  dockerfile <- readLines(file.path("..", "..", "Dockerfile"))
  install_line <- grep("^RUN R -e ", dockerfile, value = TRUE)
  install_code <- sub('^RUN R -e "(.*)"$', "\\1", install_line)
  installed <- character()
  checked <- character()
  build_env <- new.env(parent = baseenv())
  build_env$install.packages <- function(pkgs, repos) {
    installed <<- pkgs
    expect_identical(repos, "https://cloud.r-project.org")
  }
  build_env$requireNamespace <- function(package, quietly) {
    checked <<- c(checked, package)
    expect_true(quietly)
    TRUE
  }
  expect_error(eval(parse(text = install_code), envir = build_env), NA)
  expect_contains(installed, "shiny")
  expect_setequal(checked, installed)
})

test_that("Render deploy workflow waits for the correct commit and keeps approval", {
  workflow <- readLines(file.path("..", "..", ".github", "workflows", "ci-cd.yml"))
  workflow <- paste(workflow, collapse = "\n")
  expect_match(workflow, "RENDER_STAGING_SERVICE_ID")
  expect_match(workflow, "RENDER_PRODUCTION_SERVICE_ID")
  expect_match(workflow, "RENDER_PRODUCTION_DEPLOY_HOOK")
  expect_match(workflow, "select\\(.commit.id == \\$sha\\)")
  expect_true(grepl("environment:\n      name: Production", workflow, fixed = TRUE))
  expect_false(grepl("CONNECT_CLOUD_APP_URL", workflow, fixed = TRUE))
  expect_false(grepl("deploy-connect-cloud.R", workflow, fixed = TRUE))
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
