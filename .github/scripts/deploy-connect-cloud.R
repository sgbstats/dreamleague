required <- c(
  "CONNECT_CLOUD_CLIENT_ID",
  "CONNECT_CLOUD_CLIENT_SECRET",
  "CONNECT_CLOUD_ACCOUNT",
  "CONNECT_CLOUD_APP_ID"
)
missing <- required[!nzchar(Sys.getenv(required))]
if (length(missing) > 0) {
  stop(
    "Missing required deployment settings: ",
    paste(missing, collapse = ", "),
    call. = FALSE
  )
}

rsconnect::connectCloudClientCredentials(
  clientId = Sys.getenv("CONNECT_CLOUD_CLIENT_ID"),
  clientSecret = Sys.getenv("CONNECT_CLOUD_CLIENT_SECRET"),
  accountName = Sys.getenv("CONNECT_CLOUD_ACCOUNT"),
  quiet = TRUE
)

rsconnect::deployApp(
  appDir = "dreamleague",
  appId = Sys.getenv("CONNECT_CLOUD_APP_ID"),
  account = Sys.getenv("CONNECT_CLOUD_ACCOUNT"),
  server = "connect.posit.cloud",
  launch.browser = FALSE,
  forceUpdate = TRUE
)
