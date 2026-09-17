suppressPackageStartupMessages({
  library(jsonlite)
  library(googledrive)
})

benchmark_bundle_names <- c("dl", "daily", "time", "cupties")

read_json_file <- function(path, simplify_vector = FALSE) {
  jsonlite::fromJSON(
    paste(readLines(path, warn = FALSE), collapse = ""),
    simplifyVector = simplify_vector
  )
}

load_benchmark_bundle <- function(path = "dreamleague/data.RDa") {
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  missing <- setdiff(benchmark_bundle_names, ls(env, all.names = TRUE))
  if (length(missing) > 0) {
    stop(
      "Baseline is missing objects: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  mget(benchmark_bundle_names, envir = env, inherits = FALSE)
}

column_schema <- function(col) {
  schema <- list(class = class(col))
  if (is.factor(col)) {
    schema$levels <- levels(col)
  }
  if (inherits(col, "POSIXt")) {
    schema$tzone <- attr(col, "tzone")
  }
  schema
}

object_schema <- function(x) {
  if (is.data.frame(x)) {
    return(list(
      type = "data.frame",
      data_frame_class = class(x),
      columns = lapply(x, column_schema)
    ))
  }
  list(
    type = "list",
    elements = lapply(x, column_schema)
  )
}

bundle_schema <- function(bundle) {
  lapply(bundle, object_schema)
}

as_json_bundle <- function(bundle) {
  lapply(bundle, function(x) {
    if (is.data.frame(x)) {
      x[] <- lapply(x, function(col) {
        if (inherits(col, "POSIXt")) {
          format(col, tz = "UTC", usetz = TRUE)
        } else if (inherits(col, "Date")) {
          as.character(col)
        } else {
          col
        }
      })
    } else {
      x <- lapply(x, function(value) {
        if (inherits(value, "POSIXt")) {
          as.numeric(value)
        } else if (inherits(value, "Date")) {
          as.character(value)
        } else {
          as.character(value)
        }
      })
    }
    x
  })
}

restore_column <- function(x, schema) {
  classes <- schema$class
  if ("Date" %in% classes) {
    return(as.Date(x))
  }
  if ("POSIXct" %in% classes) {
    result <- if (is.numeric(x)) {
      as.POSIXct(x, origin = "1970-01-01", tz = "UTC")
    } else {
      as.POSIXct(x, tz = "UTC")
    }
    attr(result, "tzone") <- schema$tzone
    return(result)
  }
  if ("logical" %in% classes) {
    return(as.logical(x))
  }
  if ("integer" %in% classes) {
    return(as.integer(x))
  }
  if ("numeric" %in% classes) {
    return(as.numeric(x))
  }
  if ("ordered" %in% classes || "factor" %in% classes) {
    result <- factor(x, levels = schema$levels %||% sort(unique(x)))
    if ("ordered" %in% classes) {
      result <- ordered(result, levels = levels(result))
    }
    return(result)
  }
  if ("glue" %in% classes) {
    return(glue::as_glue(as.character(x)))
  }
  if ("character" %in% classes) {
    return(as.character(x))
  }
  x
}

`%||%` <- function(x, y) if (is.null(x)) y else x

restore_from_schema <- function(x, schema) {
  if (identical(schema$type, "data.frame")) {
    for (nm in names(schema$columns)) {
      x[[nm]] <- restore_column(x[[nm]], schema$columns[[nm]])
    }
    row.names(x) <- seq_len(nrow(x))
    if (!is.null(schema$data_frame_class)) {
      class(x) <- schema$data_frame_class
    }
    return(x)
  }
  out <- x
  for (nm in names(schema$elements)) {
    out[[nm]] <- restore_column(out[[nm]], schema$elements[[nm]])
  }
  out
}

write_json_artifacts <- function(bundle, directory, separate = FALSE) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  json_bundle <- as_json_bundle(bundle)
  schema <- bundle_schema(bundle)
  if (separate) {
    for (nm in names(json_bundle)) {
      write_json(
        json_bundle[[nm]],
        file.path(directory, paste0(nm, ".json")),
        pretty = FALSE,
        auto_unbox = TRUE,
        na = "null",
        digits = 17,
        dataframe = "rows"
      )
    }
  } else {
    write_json(
      json_bundle,
      file.path(directory, "bundle.json"),
      pretty = FALSE,
      auto_unbox = TRUE,
      na = "null",
      dataframe = "rows"
    )
  }
  write_json(
    schema,
    file.path(directory, "schema.json"),
    pretty = FALSE,
    auto_unbox = TRUE
  )
  invisible(directory)
}

csv_empty_string <- "__DREAMLEAGUE_EMPTY_STRING__"

write_csv_artifacts <- function(bundle, directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  for (nm in names(bundle)[vapply(bundle, is.data.frame, logical(1))]) {
    csv_data <- bundle[[nm]]
    csv_data[] <- lapply(csv_data, function(col) {
      if (is.character(col)) {
        col[!is.na(col) & col == ""] <- csv_empty_string
      }
      col
    })
    write.csv(
      csv_data,
      file.path(directory, paste0(nm, ".csv")),
      row.names = FALSE,
      na = "NA"
    )
  }
  time <- data.frame(
    name = names(bundle$time),
    value = vapply(
      bundle$time,
      function(value) {
        sprintf("%.6f", as.numeric(value))
      },
      character(1)
    ),
    stringsAsFactors = FALSE
  )
  write.csv(
    time,
    file.path(directory, "time.csv"),
    row.names = FALSE,
    na = "NA"
  )
  write_json(
    bundle_schema(bundle),
    file.path(directory, "schema.json"),
    pretty = FALSE,
    auto_unbox = TRUE
  )
  invisible(directory)
}

write_benchmark_artifacts <- function(bundle, directory, format) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (format == "rda") {
    save(
      list = names(bundle),
      file = file.path(directory, "bundle.RDa"),
      envir = list2env(bundle)
    )
    write_json(
      bundle_schema(bundle),
      file.path(directory, "schema.json"),
      pretty = FALSE,
      auto_unbox = TRUE
    )
    return(invisible(directory))
  }
  if (format == "json_bundle") {
    return(write_json_artifacts(bundle, directory, FALSE))
  }
  if (format == "json_separate") {
    return(write_json_artifacts(bundle, directory, TRUE))
  }
  if (format == "csv") {
    return(write_csv_artifacts(bundle, directory))
  }
  stop("Unknown format: ", format, call. = FALSE)
}

read_csv_bundle <- function(directory, schema) {
  out <- list()
  for (nm in c("dl", "daily", "cupties")) {
    out[[nm]] <- read.csv(
      file.path(directory, paste0(nm, ".csv")),
      stringsAsFactors = FALSE,
      check.names = FALSE,
      na.strings = "NA"
    )
    out[[nm]][] <- lapply(out[[nm]], function(col) {
      if (is.character(col)) {
        col[col == csv_empty_string] <- ""
      }
      col
    })
    out[[nm]] <- restore_from_schema(out[[nm]], schema[[nm]])
  }
  time <- read.csv(
    file.path(directory, "time.csv"),
    stringsAsFactors = FALSE,
    na.strings = "NA"
  )
  time_schema <- schema$time$elements
  out$time <- setNames(
    lapply(seq_len(nrow(time)), function(i) {
      restore_column(time$value[[i]], time_schema[[time$name[[i]]]])
    }),
    time$name
  )
  out[c("dl", "daily", "time", "cupties")]
}

read_benchmark_bundle <- function(directory, format) {
  schema <- read_json_file(file.path(directory, "schema.json"))
  if (format == "rda") {
    env <- new.env(parent = emptyenv())
    load(file.path(directory, "bundle.RDa"), envir = env)
    return(mget(benchmark_bundle_names, envir = env, inherits = FALSE))
  }
  if (format == "csv") {
    return(read_csv_bundle(directory, schema))
  }
  if (format == "json_bundle") {
    raw <- read_json_file(file.path(directory, "bundle.json"))
  } else if (format == "json_separate") {
    raw <- setNames(
      lapply(benchmark_bundle_names, function(nm) {
        read_json_file(file.path(directory, paste0(nm, ".json")))
      }),
      benchmark_bundle_names
    )
  } else {
    stop("Unknown format: ", format, call. = FALSE)
  }
  out <- lapply(benchmark_bundle_names, function(nm) {
    value <- raw[[nm]]
    if (identical(schema[[nm]]$type, "data.frame")) {
      column_names <- names(schema[[nm]]$columns)
      if (length(value) == 0L) {
        value <- as.data.frame(
          setNames(
            lapply(column_names, function(x) character()),
            column_names
          ),
          stringsAsFactors = FALSE
        )
      } else {
        value <- as.data.frame(
          do.call(
            rbind,
            lapply(value, function(row) {
              as.data.frame(
                lapply(row, function(cell) {
                  if (is.null(cell)) NA else cell
                }),
                stringsAsFactors = FALSE
              )
            })
          ),
          stringsAsFactors = FALSE
        )
      }
      value <- value[, column_names, drop = FALSE]
    }
    restore_from_schema(value, schema[[nm]])
  })
  names(out) <- benchmark_bundle_names
  out
}

compare_bundles <- function(reference, candidate, tolerance = 1e-4) {
  if (!identical(names(reference), names(candidate))) {
    return(FALSE)
  }
  all(vapply(
    names(reference),
    function(nm) {
      isTRUE(all.equal(
        reference[[nm]],
        candidate[[nm]],
        tolerance = tolerance,
        check.attributes = TRUE
      ))
    },
    logical(1)
  ))
}

validate_local_formats <- function(bundle, formats) {
  local_root <- tempfile(pattern = "dreamleague-format-check-")
  dir.create(local_root)
  on.exit(unlink(local_root, recursive = TRUE, force = TRUE), add = TRUE)

  for (format in formats) {
    directory <- file.path(local_root, format)
    write_benchmark_artifacts(bundle, directory, format)
    candidate <- read_benchmark_bundle(directory, format)
    if (!compare_bundles(bundle, candidate)) {
      stop(
        "Local round-trip validation failed for format: ",
        format,
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

validate_remote_format <- function(bundle, remote, format, directory) {
  pull_artifacts(remote, directory)
  candidate <- read_benchmark_bundle(directory, format)
  if (!compare_bundles(bundle, candidate)) {
    stop(
      "Remote round-trip validation failed for format: ",
      format,
      call. = FALSE
    )
  }
  invisible(TRUE)
}

authenticate_drive <- function(
  credentials_path = Sys.getenv(
    "DREAMLEAGUE_GOOGLE_CREDENTIALS",
    "credentials.json"
  )
) {
  if (!file.exists(credentials_path)) {
    stop(
      "Google credentials file not found: ",
      credentials_path,
      call. = FALSE
    )
  }
  googledrive::drive_auth(path = credentials_path, scopes = "drive")
  invisible(TRUE)
}

resolve_testing_target <- function(
  target = Sys.getenv("DREAMLEAGUE_TESTING_TARGET", "")
) {
  if (!nzchar(target)) {
    stop("DREAMLEAGUE_TESTING_TARGET is not set.", call. = FALSE)
  }
  if (grepl("^[A-Za-z0-9_-]{20,}$", target)) {
    googledrive::as_id(target)
  } else {
    googledrive::drive_get(target)
  }
}

upload_artifacts <- function(directory, target, prefix) {
  paths <- list.files(directory, full.names = TRUE, recursive = FALSE)
  names <- paste0(prefix, basename(paths))
  for (i in seq_along(paths)) {
    googledrive::drive_upload(
      paths[[i]],
      path = target,
      name = names[[i]],
      overwrite = FALSE
    )
  }
  names
}

find_remote_artifacts <- function(target, names) {
  listing <- googledrive::drive_ls(target)
  listing[match(names, listing$name), , drop = FALSE]
}

remove_remote_artifacts <- function(
  target,
  names,
  listing_fn = find_remote_artifacts,
  remove_fn = googledrive::drive_rm
) {
  remote <- tryCatch(
    listing_fn(target, names),
    error = function(e) NULL
  )
  if (is.null(remote) || nrow(remote) == 0) {
    return(invisible(NULL))
  }

  for (i in seq_len(nrow(remote))) {
    tryCatch(
      remove_fn(remote[i, ]),
      error = function(e) invisible(NULL)
    )
  }
  invisible(NULL)
}

pull_artifacts <- function(remote, directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_len(nrow(remote))) {
    local_name <- sub("^.*-[^-]+-", "", remote$name[[i]])
    googledrive::drive_download(
      remote[i, ],
      path = file.path(directory, local_name),
      overwrite = TRUE
    )
  }
  invisible(directory)
}

summarise_timings <- function(measurements) {
  split_data <- split(measurements, measurements$format)
  do.call(
    rbind,
    lapply(split_data, function(x) {
      data.frame(
        format = x$format[[1]],
        trials = sum(x$valid),
        download_median_s = median(x$download_s[x$valid]),
        decode_median_s = median(x$decode_s[x$valid]),
        total_median_s = median(x$total_s[x$valid]),
        total_iqr_s = IQR(x$total_s[x$valid]),
        valid = all(x$valid)
      )
    })
  )
}

run_drive_benchmark <- function(
  baseline_path = "dreamleague/data.RDa",
  output_dir = file.path("data", "diagnostics", "drive-benchmark"),
  trials = 5L,
  keep_artifacts = FALSE,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  bundle <- load_benchmark_bundle(baseline_path)
  authenticate_drive()
  target <- resolve_testing_target()
  run_id <- paste(
    format(Sys.time(), "%Y%m%d-%H%M%S"),
    sample.int(999999, 1),
    sep = "-"
  )
  formats <- c("rda", "json_bundle", "json_separate", "csv")
  validate_local_formats(bundle, formats)
  local_root <- tempfile(pattern = "dreamleague-benchmark-")
  dir.create(local_root)
  on.exit(unlink(local_root, recursive = TRUE, force = TRUE), add = TRUE)
  all_measurements <- list()
  artifact_rows <- list()
  remote_names <- list()
  for (format in formats) {
    local_format <- file.path(local_root, format)
    write_benchmark_artifacts(bundle, local_format, format)
    prefix <- paste0("dreamleague-benchmark-", run_id, "-", format, "-")
    remote_names[[format]] <- upload_artifacts(local_format, target, prefix)
    artifact_rows[[format]] <- data.frame(
      format = format,
      file_count = length(remote_names[[format]]),
      bytes = sum(file.info(list.files(local_format, full.names = TRUE))$size)
    )
  }
  on.exit(
    if (!keep_artifacts) {
      for (nms in remote_names) {
        remove_remote_artifacts(target, nms)
      }
    },
    add = TRUE
  )
  for (format in formats) {
    remote <- find_remote_artifacts(target, remote_names[[format]])
    validation_dir <- file.path(local_root, paste0(format, "-validation"))
    validate_remote_format(bundle, remote, format, validation_dir)
    for (trial in seq_len(trials)) {
      trial_dir <- file.path(local_root, paste0(format, "-trial-", trial))
      download_start <- proc.time()[["elapsed"]]
      pull_artifacts(remote, trial_dir)
      download_end <- proc.time()[["elapsed"]]
      decode_start <- proc.time()[["elapsed"]]
      local_files <- read_benchmark_bundle(trial_dir, format)
      valid <- compare_bundles(bundle, local_files)
      if (!valid) {
        stop(
          "Remote round-trip validation failed during trial for format: ",
          format,
          call. = FALSE
        )
      }
      decode_end <- proc.time()[["elapsed"]]
      all_measurements[[length(all_measurements) + 1L]] <- data.frame(
        format = format,
        trial = trial,
        download_s = download_end - download_start,
        decode_s = decode_end - decode_start,
        total_s = decode_end - download_start,
        valid = valid
      )
    }
  }
  measurements <- do.call(rbind, all_measurements)
  summary <- summarise_timings(measurements)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  write.csv(
    measurements,
    file.path(output_dir, paste0("measurements-", run_id, ".csv")),
    row.names = FALSE
  )
  write.csv(
    summary,
    file.path(output_dir, paste0("summary-", run_id, ".csv")),
    row.names = FALSE
  )
  write.csv(
    do.call(rbind, artifact_rows),
    file.path(output_dir, paste0("artifacts-", run_id, ".csv")),
    row.names = FALSE
  )
  writeLines(
    c(
      paste("DreamLeague Drive format benchmark", run_id),
      paste("Trials:", trials),
      "",
      capture.output(summary)
    ),
    file.path(output_dir, paste0("summary-", run_id, ".txt"))
  )
  list(
    run_id = run_id,
    measurements = measurements,
    summary = summary,
    artifacts = do.call(rbind, artifact_rows)
  )
}

result <- run_drive_benchmark(
  trials = as.integer(Sys.getenv("DREAMLEAGUE_BENCHMARK_TRIALS", "5")),
  keep_artifacts = identical(
    Sys.getenv("DREAMLEAGUE_BENCHMARK_KEEP", "false"),
    "true"
  )
)
