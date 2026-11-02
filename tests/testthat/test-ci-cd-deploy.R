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
    CONNECT_CLOUD_APP_ID = "test-content"
  ))
  withr::local_options(repos = getOption("repos"))
  testthat::local_mocked_bindings(
    connectCloudClientCredentials = function(clientId, clientSecret, accountName, quiet) {
      expect_identical(clientId, "test-client")
      expect_identical(clientSecret, "test-secret")
      expect_identical(accountName, "test-account")
      expect_true(quiet)
    },
    deployApp = function(appDir, appId, account, server, launch.browser, forceUpdate) {
      expect_identical(appDir, "dreamleague")
      expect_identical(appId, "test-content")
      expect_identical(account, "test-account")
      expect_identical(server, "connect.posit.cloud")
      expect_false(launch.browser)
      expect_true(forceUpdate)
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
    CONNECT_CLOUD_APP_ID = "test-content"
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

test_that("an unsuccessful cloud deployment fails the job", {
  withr::local_envvar(c(
    CONNECT_CLOUD_CLIENT_ID = "test-client",
    CONNECT_CLOUD_CLIENT_SECRET = "test-secret",
    CONNECT_CLOUD_ACCOUNT = "test-account",
    CONNECT_CLOUD_APP_ID = "test-content"
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
