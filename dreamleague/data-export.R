write_data_zip <- function(bundle, destination, source, retrieved_at) {
  required <- c("dl", "daily", "time", "cupties")
  if (!all(vapply(required, exists, logical(1), envir = bundle, inherits = FALSE))) {
    stop("No data has been retrieved. Please try again later.", call. = FALSE)
  }
  export_dir <- tempfile("dreamleague-export-")
  dir.create(export_dir)
  on.exit(unlink(export_dir, recursive = TRUE), add = TRUE)

  snapshot <- new.env(parent = emptyenv())
  for (name in required) {
    value <- get(name, envir = bundle, inherits = FALSE)
    if (is.null(value)) {
      stop("No data has been retrieved. Please try again later.", call. = FALSE)
    }
    assign(name, value, envir = snapshot)
    if (is.data.frame(value) || is.matrix(value)) {
      utils::write.csv(
        value, file.path(export_dir, paste0(name, ".csv")), row.names = FALSE
      )
    }
  }
  save(list = required, envir = snapshot, file = file.path(export_dir, "data.RDa"))
  writeLines(
    c(
      paste("Data source:", source),
      paste("Retrieved at (UTC):", format(retrieved_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
      "Objects: dl, daily, time, cupties",
      "CSV files contain the tabular objects; data.RDa preserves all four R objects."
    ),
    file.path(export_dir, "metadata.txt")
  )
  zip::zipr(destination, files = list.files(export_dir), root = export_dir)
  if (!file.exists(destination) || file.info(destination)$size[[1]] == 0) {
    stop("Data ZIP creation failed.", call. = FALSE)
  }
  invisible(destination)
}
