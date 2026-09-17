source(file.path("..", "..", "R", "supabase-storage.R"))
source(file.path("..", "..", "R", "benchmark-data-backends.R"))

test_that("Supabase qualifies when faster with no additional failures", {
  results <- data.frame(
    backend = rep(c("Google Drive", "Supabase Storage"), each = 3),
    status = rep("success", 6),
    iteration = rep(1:3, 2),
    total_seconds = c(1, 1.1, 0.9, 0.7, 0.8, 0.75)
  )

  assessment <- assess_supabase_migration(results)
  expect_true(assessment$qualifies)
  expect_gt(assessment$improvement, 0.1)
})

test_that("Supabase does not qualify when it is not sufficiently faster", {
  results <- data.frame(
    backend = rep(c("Google Drive", "Supabase Storage"), each = 3),
    status = rep("success", 6),
    iteration = rep(1:3, 2),
    total_seconds = c(1, 1, 1, 0.95, 0.96, 0.94)
  )

  expect_false(assess_supabase_migration(results)$qualifies)
})

test_that("benchmark summary reports each backend", {
  results <- data.frame(
    backend = rep(c("Google Drive", "Supabase Storage"), each = 2),
    status = c("success", "success", "success", "failed"),
    iteration = rep(1:2, 2),
    total_seconds = c(1, 1.2, 0.8, 0.9)
  )

  summary <- summarise_backend_benchmark(results)
  expect_identical(sort(summary$backend), c("Google Drive", "Supabase Storage"))
  expect_identical(
    summary$successful_reads[summary$backend == "Google Drive"],
    2L
  )
})

test_that("Supabase does not qualify with more failures", {
  results <- data.frame(
    backend = c(rep("Google Drive", 3), rep("Supabase Storage", 3)),
    status = c("success", "success", "success", "success", "success", "failed"),
    iteration = rep(1:3, 2),
    total_seconds = c(1, 1, 1, 0.5, 0.5, 0.5)
  )

  expect_false(assess_supabase_migration(results)$qualifies)
})
