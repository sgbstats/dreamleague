DREAMLEAGUE_SUPABASE_URL <- "https://uapfyikxspmcifgdergl.supabase.co"
DREAMLEAGUE_SUPABASE_BUCKET <- "dreamleague"

supabase_storage_config <- function(
  url = DREAMLEAGUE_SUPABASE_URL,
  api_key = Sys.getenv("SUPABASE_API_KEY", ""),
  bucket = DREAMLEAGUE_SUPABASE_BUCKET
) {
  url <- sub("/+$", "", url)
  if (!grepl("^https://[^/]+$", url)) {
    stop("SUPABASE_URL must be an HTTPS project URL", call. = FALSE)
  }
  if (!nzchar(api_key)) {
    stop("SUPABASE_API_KEY is not configured", call. = FALSE)
  }
  if (!grepl("^[A-Za-z0-9_-]+$", bucket)) {
    stop(
      "DREAMLEAGUE_SUPABASE_BUCKET contains invalid characters",
      call. = FALSE
    )
  }

  list(url = url, api_key = api_key, bucket = bucket)
}

supabase_object_url <- function(config, object_path) {
  if (!nzchar(object_path) || grepl("^/|//", object_path)) {
    stop("object_path must be a relative, non-empty path", call. = FALSE)
  }

  encoded_path <- vapply(
    strsplit(object_path, "/", fixed = TRUE)[[1]],
    utils::URLencode,
    character(1),
    reserved = TRUE
  )
  paste0(
    config$url,
    "/storage/v1/object/",
    utils::URLencode(config$bucket, reserved = TRUE),
    "/",
    paste(encoded_path, collapse = "/")
  )
}

supabase_storage_headers <- function(config) {
  c(
    Authorization = paste("Bearer", config$api_key),
    apikey = config$api_key
  )
}

supabase_require_success <- function(response, action) {
  if (httr::http_error(response)) {
    stop(
      sprintf(
        "Supabase Storage %s failed (HTTP %s): %s",
        action,
        httr::status_code(response),
        httr::content(response, as = "text", encoding = "UTF-8")
      ),
      call. = FALSE
    )
  }
  invisible(response)
}

supabase_download_object <- function(
  object_path = "dreamleague/data.RDa",
  destination,
  config = supabase_storage_config(),
  timeout_seconds = 60
) {
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  response <- httr::GET(
    supabase_object_url(config, object_path),
    httr::add_headers(.headers = supabase_storage_headers(config)),
    httr::write_disk(destination, overwrite = TRUE),
    httr::timeout(timeout_seconds)
  )
  supabase_require_success(response, "download")
  if (!file.exists(destination) || file.info(destination)$size[[1]] == 0) {
    stop("Supabase Storage download produced an empty file", call. = FALSE)
  }
  invisible(list(
    object_path = object_path,
    bytes = unname(file.info(destination)$size[[1]]),
    status_code = httr::status_code(response)
  ))
}

load_dreamleague_bundle <- function(path) {
  bundle <- new.env(parent = emptyenv())
  load(path, envir = bundle)
  required_objects <- c("dl", "daily", "time", "cupties")
  missing_objects <- required_objects[
    !vapply(
      required_objects,
      exists,
      logical(1),
      envir = bundle,
      inherits = FALSE
    )
  ]
  if (length(missing_objects) > 0) {
    stop(
      "Data bundle is missing objects: ",
      paste(missing_objects, collapse = ", "),
      call. = FALSE
    )
  }
  bundle
}
