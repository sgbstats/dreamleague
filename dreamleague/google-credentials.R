initialize_google_credentials <- function(default_path = "credentials.json") {
  private_key <- Sys.getenv("GOOGLE_PRIVATE_KEY", "")
  if (!nzchar(private_key)) {
    stop("Set the GOOGLE_PRIVATE_KEY environment variable", call. = FALSE)
  }

  credentials_path <- Sys.getenv("DREAMLEAGUE_GOOGLE_CREDENTIALS", "")
  if (!nzchar(credentials_path)) {
    credentials_path <- default_path
  }
  if (!file.exists(credentials_path)) {
    stop(
      "Google credentials metadata file not found: ",
      credentials_path,
      call. = FALSE
    )
  }

  credentials <- jsonlite::fromJSON(
    credentials_path,
    simplifyVector = FALSE
  )
  credentials$private_key <- gsub("\\\\n", "\n", private_key)

  path <- tempfile(pattern = "dreamleague-google-", fileext = ".json")
  writeLines(jsonlite::toJSON(credentials, auto_unbox = TRUE, pretty = TRUE), path)
  Sys.chmod(path, mode = "0600")
  path
}
