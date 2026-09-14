source(file.path("..", "..", "R", "supabase-storage.R"))

test_that("Supabase configuration validates required values", {
  expect_error(
    supabase_storage_config(api_key = ""),
    "SUPABASE_API_KEY is not configured"
  )
  expect_error(
    supabase_storage_config(url = "http://example.com", api_key = "key"),
    "HTTPS project URL"
  )
  expect_error(
    supabase_storage_config(api_key = "key", bucket = "invalid bucket"),
    "invalid characters"
  )
})

test_that("Supabase Storage object URLs encode object paths", {
  config <- supabase_storage_config(
    url = "https://example.supabase.co/",
    api_key = "key",
    bucket = "dreamleague"
  )

  expect_identical(
    supabase_object_url(config, "bundles/data file.RDa"),
    "https://example.supabase.co/storage/v1/object/dreamleague/bundles/data%20file.RDa"
  )
  expect_error(supabase_object_url(config, "/data.RDa"), "relative")
})

test_that("data bundle loading requires all app objects", {
  invalid_bundle <- tempfile(fileext = ".RDa")
  dl <- data.frame(id = 1)
  save(dl, file = invalid_bundle)

  expect_error(load_dreamleague_bundle(invalid_bundle), "daily, time, cupties")
})
