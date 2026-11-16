library(testthat)

source(".github/scripts/select-test-sets.R")

# Accept one area or a comma-separated union; run shared tests only once.
test_set <- Sys.getenv("DREAMLEAGUE_TEST_SET", "all")
areas <- strsplit(test_set, ",", fixed = TRUE)[[1]]
if (identical(test_set, "none")) quit(status = 0)
if (!identical(test_set, "all") && any(!areas %in% names(test_area_filters))) {
  stop("Unknown DREAMLEAGUE_TEST_SET area: ", test_set, call. = FALSE)
}
filter <- if (identical(test_set, "all")) NULL else {
  paste(test_area_filters[areas], collapse = "|")
}

test_dir("tests/testthat", filter = filter)
