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
  expect_error(
    source(file.path("..", "..", ".github", "scripts", "deploy-connect-cloud.R")),
    "Posit Connect Cloud deployment failed"
  )
})
