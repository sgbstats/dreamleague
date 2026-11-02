source(file.path("..", "..", ".github", "scripts", "select-test-sets.R"))

test_that("every test file is tagged with a test set prefix", {
  files <- list.files(".", pattern = "^test-.*\\.R$")
  expect_gt(length(files), 0)
  expect_true(
    all(grepl("^test-(app|pipeline|shared)-", files)),
    info = paste(files[!grepl("^test-(app|pipeline|shared)-", files)], collapse = ", ")
  )
})

test_that("app changes run only app tests", {
  selected <- select_test_sets(c("dreamleague/app.R", "tests/testthat/test-app-startup.R"))
  expect_identical(selected$tests, "app")
  expect_true(selected$supabase)
})

test_that("pipeline changes run only pipeline tests", {
  expect_identical(select_test_sets("R/wengerboys-lib.R")$tests, "pipeline")
  expect_identical(select_test_sets(".github/workflows/dl-preprocessing.yml")$tests, "pipeline")
  expect_identical(select_test_sets("data/league92_clubs.csv")$tests, "pipeline")
  expect_identical(select_test_sets("tests/testthat/fixtures/player_page.html")$tests, "pipeline")
  expect_false(select_test_sets("R/wengerboys-lib.R")$supabase)
})

test_that("Supabase code and the CI workflow trigger the live round trip", {
  expect_true(select_test_sets("R/supabase-storage.R")$supabase)
  expect_true(select_test_sets(".github/workflows/ci-cd.yml")$supabase)
})

test_that("mixed, shared and unknown changes run everything", {
  expect_identical(select_test_sets(c("dreamleague/app.R", "R/dl-file-pull.R"))$tests, "all")
  expect_identical(select_test_sets("tests/testthat/test-shared-supabase-roundtrip.R")$tests, "all")
  expect_identical(select_test_sets("tests/testthat.R")$tests, "all")
  expect_identical(select_test_sets("*")$tests, "all")
  expect_true(select_test_sets("*")$supabase)
})

test_that("documentation-only changes run no tests", {
  selected <- select_test_sets(c("README.md", "LICENSE", ""))
  expect_identical(selected$tests, "none")
  expect_false(selected$supabase)
})

test_that("the CLI writes GitHub step outputs", {
  changed <- withr::local_tempfile(lines = "dreamleague/app.R")
  output <- withr::local_tempfile()
  writeLines(character(), output)
  withr::local_envvar(GITHUB_OUTPUT = output)
  script <- normalizePath(file.path("..", "..", ".github", "scripts", "select-test-sets.R"))
  status <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(script), shQuote(changed)), stdout = FALSE, stderr = FALSE)
  expect_identical(status, 0L)
  expect_identical(readLines(output), c("tests=app", "supabase=true"))
})
