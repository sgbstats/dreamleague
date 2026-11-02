source(file.path("..", "..", "dreamleague", "google-credentials.R"))

test_that("injects the private key into a temporary credentials file", {
  metadata_path <- tempfile(fileext = ".json")
  writeLines(
    '{"type":"service_account","project_id":"example","client_email":"example@example.com"}',
    metadata_path
  )
  original_path <- Sys.getenv(
    "DREAMLEAGUE_GOOGLE_CREDENTIALS",
    unset = NA_character_
  )
  original_key <- Sys.getenv("GOOGLE_PRIVATE_KEY", unset = NA_character_)
  result_path <- NULL
  on.exit(
    {
      unlink(metadata_path)
      if (!is.null(result_path)) {
        unlink(result_path)
      }
      if (is.na(original_path)) {
        Sys.unsetenv("DREAMLEAGUE_GOOGLE_CREDENTIALS")
      } else {
        Sys.setenv(DREAMLEAGUE_GOOGLE_CREDENTIALS = original_path)
      }
      if (is.na(original_key)) {
        Sys.unsetenv("GOOGLE_PRIVATE_KEY")
      } else {
        Sys.setenv(GOOGLE_PRIVATE_KEY = original_key)
      }
    },
    add = TRUE
  )

  Sys.setenv(
    DREAMLEAGUE_GOOGLE_CREDENTIALS = metadata_path,
    GOOGLE_PRIVATE_KEY = "line-one\\nline-two"
  )
  result_path <- initialize_google_credentials()
  credentials <- jsonlite::fromJSON(result_path, simplifyVector = FALSE)

  expect_identical(credentials$private_key, "line-one\nline-two")
  expect_identical(credentials$project_id, "example")
  expect_false("private_key" %in% names(jsonlite::fromJSON(metadata_path)))
})

test_that("requires GOOGLE_PRIVATE_KEY", {
  metadata_path <- tempfile(fileext = ".json")
  writeLines('{"type":"service_account"}', metadata_path)
  original_path <- Sys.getenv(
    "DREAMLEAGUE_GOOGLE_CREDENTIALS",
    unset = NA_character_
  )
  original_key <- Sys.getenv("GOOGLE_PRIVATE_KEY", unset = NA_character_)
  on.exit(
    {
      unlink(metadata_path)
      if (is.na(original_path)) {
        Sys.unsetenv("DREAMLEAGUE_GOOGLE_CREDENTIALS")
      } else {
        Sys.setenv(DREAMLEAGUE_GOOGLE_CREDENTIALS = original_path)
      }
      if (is.na(original_key)) {
        Sys.unsetenv("GOOGLE_PRIVATE_KEY")
      } else {
        Sys.setenv(GOOGLE_PRIVATE_KEY = original_key)
      }
    },
    add = TRUE
  )

  Sys.setenv(DREAMLEAGUE_GOOGLE_CREDENTIALS = metadata_path)
  Sys.unsetenv("GOOGLE_PRIVATE_KEY")

  expect_error(initialize_google_credentials(), "GOOGLE_PRIVATE_KEY")
})
