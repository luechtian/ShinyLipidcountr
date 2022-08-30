test_that("read metadata works", {

  test_path <- system.file("extdata", package = "shinylipidcountr")

  metadata <- readRDS(test_path("fixtures", "metadata.rds"))

  expect_equal(read_metadata(test_path), metadata)

  expect_equal(read_metadata(test_path, clean_samples = TRUE),
               janitor::clean_names(metadata))

})


test_that("Join metadata to the rawdata works", {

  raw_data <- readRDS(test_path("fixtures", "raw_data_merge.rds"))

  metadata <- readRDS(test_path("fixtures", "metadata.rds"))

  raw_meta <- readRDS(test_path("fixtures", "raw_meta_merge.rds"))

  expect_equal(join_metadata(raw_data, metadata), raw_meta)

})
