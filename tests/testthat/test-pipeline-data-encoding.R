test_that("preprocessing CSV inputs are valid UTF-8", {
  csvs <- Sys.glob(file.path("..", "..", "data", "*.csv"))
  expect_true(length(csvs) > 0)
  for (path in csvs) {
    lines <- readLines(path, warn = FALSE, encoding = "bytes")
    expect_true(
      all(validUTF8(lines)),
      info = paste(basename(path), "must be saved as UTF-8")
    )
  }
})

test_that("cupties.csv can be trimmed as in preprocessing", {
  cupties <- read.csv(file.path("..", "..", "data", "cupties.csv"))
  expect_no_error(dplyr::mutate(cupties, dplyr::across(where(is.character), trimws)))
})
