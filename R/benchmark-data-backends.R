benchmark_data_backend <- function(name, fetch_bundle, iteration) {
  destination <- tempfile(fileext = ".RDa")
  on.exit(unlink(destination), add = TRUE)
  started <- Sys.time()
  fetch_seconds <- NA_real_
  load_seconds <- NA_real_

  outcome <- tryCatch(
    {
      fetch_started <- Sys.time()
      fetch_bundle(destination)
      fetch_seconds <- as.numeric(difftime(
        Sys.time(),
        fetch_started,
        units = "secs"
      ))
      load_started <- Sys.time()
      load_dreamleague_bundle(destination)
      load_seconds <- as.numeric(difftime(
        Sys.time(),
        load_started,
        units = "secs"
      ))
      list(status = "success", error = NA_character_)
    },
    error = function(e) list(status = "failed", error = conditionMessage(e))
  )

  data.frame(
    backend = name,
    iteration = iteration,
    started_at = started,
    status = outcome$status,
    error = outcome$error,
    total_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
    fetch_seconds = fetch_seconds,
    load_seconds = load_seconds,
    stringsAsFactors = FALSE
  )
}

make_drive_bundle_fetcher <- function() {
  source("R/dl-file-pull.R")
  try_drive_auth()

  function(destination) {
    listing <- get_remote_listing("data.RDa")
    if (inherits(listing, "drive_listing_error") || nrow(listing) == 0) {
      stop(
        "Google Drive data.RDa is not available for benchmarking",
        call. = FALSE
      )
    }
    remote <- listing |>
      dplyr::filter(.data$name == "data.RDa") |>
      dplyr::slice_max(.data$modified_time, n = 1, with_ties = FALSE)
    googledrive::drive_download(
      remote[1, ],
      path = destination,
      overwrite = TRUE
    )
  }
}

make_supabase_bundle_fetcher <- function() {
  config <- supabase_storage_config()
  function(destination) {
    supabase_download_object(destination = destination, config = config)
  }
}

summarise_backend_benchmark <- function(results) {
  successful <- results[results$status == "success", , drop = FALSE]
  summaries <- lapply(
    split(successful$total_seconds, successful$backend),
    function(x) {
      data.frame(
        successful_reads = length(x),
        median_seconds = median(x),
        p90_seconds = unname(stats::quantile(x, 0.9)),
        mean_seconds = mean(x)
      )
    }
  )
  do.call(rbind, summaries) |>
    tibble::rownames_to_column("backend")
}

bootstrap_paired_median_ci <- function(differences, repetitions = 5000L) {
  if (length(differences) < 2) {
    return(c(lower = NA_real_, upper = NA_real_))
  }
  samples <- replicate(
    repetitions,
    median(sample(differences, length(differences), replace = TRUE))
  )
  stats::quantile(samples, c(0.025, 0.975), names = FALSE)
}

assess_supabase_migration <- function(results, min_improvement = 0.10) {
  failures <- aggregate(
    status ~ backend,
    data = transform(results, status = status == "failed"),
    FUN = sum
  )
  names(failures)[2] <- "failures"
  medians <- aggregate(
    total_seconds ~ backend,
    data = results[results$status == "success", , drop = FALSE],
    FUN = median
  )
  drive_median <- medians$total_seconds[medians$backend == "Google Drive"]
  supabase_median <- medians$total_seconds[
    medians$backend == "Supabase Storage"
  ]
  drive_failures <- failures$failures[failures$backend == "Google Drive"]
  supabase_failures <- failures$failures[failures$backend == "Supabase Storage"]

  paired <- merge(
    results[
      results$backend == "Google Drive" & results$status == "success",
      c("iteration", "total_seconds")
    ],
    results[
      results$backend == "Supabase Storage" & results$status == "success",
      c("iteration", "total_seconds")
    ],
    by = "iteration",
    suffixes = c("_drive", "_supabase")
  )
  differences <- paired$total_seconds_supabase - paired$total_seconds_drive
  median_difference_ci <- bootstrap_paired_median_ci(differences)

  if (length(drive_median) != 1 || length(supabase_median) != 1) {
    return(list(
      qualifies = FALSE,
      reason = "Both backends require successful reads."
    ))
  }

  improvement <- (drive_median - supabase_median) / drive_median
  qualifies <- supabase_failures <= drive_failures &&
    improvement >= min_improvement &&
    is.finite(median_difference_ci[[2]]) &&
    median_difference_ci[[2]] < 0
  list(
    qualifies = qualifies,
    improvement = improvement,
    drive_median_seconds = drive_median,
    supabase_median_seconds = supabase_median,
    drive_failures = drive_failures,
    supabase_failures = supabase_failures,
    paired_reads = length(differences),
    paired_median_difference_ci = median_difference_ci,
    reason = if (qualifies) {
      "Supabase meets the latency and reliability threshold."
    } else {
      "Supabase does not yet meet the latency and reliability threshold."
    }
  )
}

run_data_backend_benchmark <- function(repetitions = 20L) {
  drive_fetch <- make_drive_bundle_fetcher()
  supabase_fetch <- make_supabase_bundle_fetcher()
  results <- lapply(seq_len(repetitions), function(iteration) {
    ordered <- if (iteration %% 2L == 1L) {
      list("Google Drive" = drive_fetch, "Supabase Storage" = supabase_fetch)
    } else {
      list("Supabase Storage" = supabase_fetch, "Google Drive" = drive_fetch)
    }
    do.call(
      rbind,
      lapply(names(ordered), function(name) {
        benchmark_data_backend(name, ordered[[name]], iteration)
      })
    )
  }) |>
    do.call(what = rbind)

  list(
    results = results,
    summary = summarise_backend_benchmark(results),
    decision = assess_supabase_migration(results)
  )
}
