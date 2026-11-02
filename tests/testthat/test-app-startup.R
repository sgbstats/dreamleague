test_that("R starts in the app directory without an untracked renv installation", {
  result <- processx::run(
    file.path(R.home("bin"), "Rscript"),
    c("-e", "cat('startup succeeded')"),
    wd = normalizePath(file.path("..", "..", "dreamleague")),
    error_on_status = FALSE
  )
  expect_identical(result$status, 0L, info = result$stderr)
  expect_identical(result$stdout, "startup succeeded")
})
