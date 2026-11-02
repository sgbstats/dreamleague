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

# rsconnect replaces the content's variables on every Connect Cloud deploy, so
# the app's runtime configuration must be sent each time from CI secrets.
required_runtime <- c("GOOGLE_PRIVATE_KEY", "SUPABASE_API_KEY")
optional_runtime <- c(
  "ACTION_TOKEN",
  "DREAMLEAGUE_SHARED_DRIVE_TARGET",
  "SUPABASE_URL",
  "DREAMLEAGUE_SUPABASE_BUCKET"
)
missing_runtime <- required_runtime[!nzchar(Sys.getenv(required_runtime))]
if (length(missing_runtime) > 0) {
  stop(
    "Missing required app runtime secrets: ",
    paste(missing_runtime, collapse = ", "),
    call. = FALSE
  )
}
runtime_vars <- c(required_runtime, optional_runtime)
runtime_vars <- runtime_vars[nzchar(Sys.getenv(runtime_vars))]

# pak records binaries as `Repository: RSPM`; rsconnect only writes a real URL
# into manifest.json when that name exists in `repos`. Use the source (not
# Linux-binary) URL so Connect Cloud can download packages it does not cache.
source_repo <- "https://packagemanager.posit.co/cran/latest"
repos <- getOption("repos")
repos[c("CRAN", "RSPM")] <- source_repo
options(repos = repos)

rsconnect::connectCloudClientCredentials(
  clientId = Sys.getenv("CONNECT_CLOUD_CLIENT_ID"),
  clientSecret = Sys.getenv("CONNECT_CLOUD_CLIENT_SECRET"),
  accountName = Sys.getenv("CONNECT_CLOUD_ACCOUNT"),
  quiet = TRUE
)

deployed <- rsconnect::deployApp(
  appDir = "dreamleague",
  appId = Sys.getenv("CONNECT_CLOUD_APP_ID"),
  account = Sys.getenv("CONNECT_CLOUD_ACCOUNT"),
  server = "connect.posit.cloud",
  launch.browser = FALSE,
  forceUpdate = TRUE,
  envVars = runtime_vars
)
if (!isTRUE(deployed)) {
  stop("Posit Connect Cloud deployment failed.", call. = FALSE)
}
