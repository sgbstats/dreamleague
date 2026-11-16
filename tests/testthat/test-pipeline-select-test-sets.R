source(file.path("..", "..", ".github", "scripts", "select-test-sets.R"))

test_that("every test file is tagged with a test set prefix", {
  files <- list.files(".", pattern = "^test-.*\\.R$")
  expect_gt(length(files), 0)
  expect_true(
    all(grepl("^test-(app|pipeline|shared)-", files)),
    info = paste(files[!grepl("^test-(app|pipeline|shared)-", files)], collapse = ", ")
  )
  expect_true(all(vapply(sub("^test-", "", files), function(file) {
    matches_any(file, test_area_filters)
  }, logical(1))), info = "Every test must be registered in test_area_filters")
})

test_that("app changes run only app tests", {
  selected <- select_test_sets(c("dreamleague/app.R", "tests/testthat/test-app-startup.R"))
  expect_identical(selected$tests, "app,storage")
  expect_true(selected$supabase)
  expect_true(selected$app)
})

test_that("pipeline changes run only pipeline tests", {
  expect_identical(select_test_sets("R/wengerboys-lib.R")$tests, "wengerboys")
  expect_identical(select_test_sets(".github/workflows/dl-preprocessing.yml")$tests, "pipeline")
  expect_identical(select_test_sets("data/league92_clubs.csv")$tests, "wengerboys")
  expect_identical(select_test_sets("tests/testthat/fixtures/player_page.html")$tests, "wengerboys")
  expect_false(select_test_sets("R/wengerboys-lib.R")$supabase)
  expect_false(select_test_sets("R/wengerboys-lib.R")$app)
})

test_that("Supabase code triggers its tests and dependent areas", {
  expect_true(select_test_sets("R/supabase-storage.R")$supabase)
  expect_identical(
    select_test_sets("R/supabase-storage.R")$tests,
    "pipeline,wengerboys,storage,benchmarks"
  )
  expect_false(select_test_sets("R/supabase-storage.R")$app)
  expect_false(select_test_sets(".github/workflows/ci-cd.yml")$supabase)
})

test_that("mixed changes combine only affected areas", {
  expect_identical(
    select_test_sets(c("dreamleague/app.R", "R/dl-file-pull.R"))$tests,
    "app,pipeline,storage,benchmarks"
  )
  expect_identical(select_test_sets("tests/testthat/test-shared-supabase-roundtrip.R")$tests, "app,storage")
  expect_identical(select_test_sets("tests/testthat/test-shared-github-token.R")$tests, "app,ci")
})

test_that("shared infrastructure and unknown history run everything", {
  expect_identical(select_test_sets("tests/testthat.R")$tests, "all")
  expect_identical(select_test_sets("*")$tests, "all")
  expect_true(select_test_sets("*")$supabase)
  expect_true(select_test_sets("*")$app)
  expect_false(select_test_sets("tests/testthat.R")$app)
})

test_that("area tags select tests without promoting test-only changes", {
  for (area in names(test_area_filters)) {
    files <- list.files(".", pattern = "^test-.*\\.R$")
    matching <- files[grepl(test_area_filters[[area]], sub("^test-", "", files))]
    expect_gt(length(matching), 0)
    selected <- select_test_sets(paste0("tests/testthat/", matching))
    expect_contains(strsplit(selected$tests, ",", fixed = TRUE)[[1]], area)
    expect_false(selected$app)
  }
  expect_identical(select_test_sets("R/benchmark-drive-data-formats.R")$tests, "benchmarks")
  expect_identical(select_test_sets(".github/workflows/ci-cd.yml")$tests, "ci")
  expect_identical(select_test_sets("R\\wengerboys-lib.R")$tests, "wengerboys")
  expect_error(select_test_sets("tests/testthat/test-untagged.R"), "no registered area")
})

test_that("app packaging changes require app tests and staging", {
  for (path in c("Dockerfile", ".dockerignore", "render.yaml")) {
    selected <- select_test_sets(path)
    expect_identical(selected$tests, "app,ci")
    expect_true(selected$app)
  }
})

test_that("documentation-only changes run no tests", {
  selected <- select_test_sets(c("README.md", "LICENSE", ""))
  expect_identical(selected$tests, "none")
  expect_false(selected$supabase)
  expect_false(selected$app)
})

test_that("the CLI writes GitHub step outputs", {
  changed <- withr::local_tempfile(lines = "dreamleague/app.R")
  output <- withr::local_tempfile()
  writeLines(character(), output)
  withr::local_envvar(GITHUB_OUTPUT = output)
  script <- normalizePath(file.path("..", "..", ".github", "scripts", "select-test-sets.R"))
  status <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(script), shQuote(changed)), stdout = FALSE, stderr = FALSE)
  expect_identical(status, 0L)
  expect_identical(readLines(output), c("tests=app,storage", "supabase=true", "app=true"))
})

test_that("deployment jobs require an app change output", {
  root <- file.path("..", "..")
  workflow <- yaml::read_yaml(file.path(root, ".github", "workflows", "ci-cd.yml"))
  expect_identical(workflow$jobs$test$outputs$app, "${{ steps.select.outputs.app }}")
  for (job in c("test-github-token", "promote-staging", "deploy-staging", "deploy-production")) {
    expect_match(workflow$jobs[[job]][["if"]], "needs.test.outputs.app == 'true'", fixed = TRUE)
  }
  blueprint <- yaml::read_yaml(
    file.path(root, "render.yaml"),
    handlers = list("float#fix" = identity)
  )
  staging <- Filter(function(service) service$name == "dreamleague-staging", blueprint$services)[[1]]
  expect_setequal(unlist(staging$buildFilter$paths), c("dreamleague/**", "Dockerfile", ".dockerignore", "render.yaml"))
})

test_that("the runner applies area unions without running unrelated tests", {
  root <- normalizePath(file.path("..", ".."))
  withr::local_dir(root)
  for (area in c("wengerboys", "ci,wengerboys", "app,storage", "all")) {
    withr::local_envvar(DREAMLEAGUE_TEST_SET = area)
    env <- new.env(parent = globalenv())
    selected <- NULL
    env$test_dir <- function(path, filter) {
      files <- sub("^test-", "", list.files(path, pattern = "^test-.*\\.R$"))
      selected <<- if (is.null(filter)) files else files[grepl(filter, files)]
    }
    sys.source("tests/testthat.R", envir = env)
    expect_gt(length(selected), 0)
    if (area == "wengerboys") {
      expect_true(all(grepl("^pipeline-wengerboys", selected)))
    } else if (area == "ci,wengerboys") {
      expect_true(any(grepl("^pipeline-wengerboys", selected)))
      expect_true(any(grepl("^pipeline-select-test-sets", selected)))
      expect_false(any(grepl("^app-", selected)))
    } else if (area == "app,storage") {
      expect_true(any(grepl("^app-", selected)))
      expect_equal(sum(grepl("^shared-supabase-roundtrip", selected)), 1L)
      expect_false(any(grepl("^pipeline-wengerboys", selected)))
    } else {
      expect_length(selected, length(list.files("tests/testthat", pattern = "^test-.*\\.R$")))
    }
  }
  withr::local_envvar(DREAMLEAGUE_TEST_SET = "invalid")
  expect_error(sys.source("tests/testthat.R", envir = new.env()), "Unknown DREAMLEAGUE_TEST_SET")
})
