test_that("read lipidview-files works", {

  test_path <- list.files(path = system.file("extdata",
                                             package = "shinylipidcountr"),
                          pattern = ".txt",
                          full.names = TRUE)

  lv_data_read <- readRDS(test_path("fixtures", "lv_data_read.rds"))

  expect_equal(read_lipidview_files(test_path), lv_data_read)

})


test_that("read lipidview-files works", {

  test_path <- NULL

  expect_error(read_lipidview_files(test_path))

})


test_that("clean up of lipidview-files works", {

  lv_data_read <- readRDS(test_path("fixtures", "lv_data_read.rds"))

  lv_data_clean <- readRDS(test_path("fixtures", "lv_data_clean.rds"))

  expect_equal(clean_lipidview_files(lv_data_read), lv_data_clean)

})


test_that("read lipidxplorer-files works", {

  test_path <- system.file("extdata", "lx-out.csv", package = "shinylipidcountr")

  lx_data_read <- readRDS(test_path("fixtures", "lx_data_read.rds"))

  expect_equal(read_lipidxplorer_files(test_path), lx_data_read)

})


test_that("clean up of lipidxplorer-files works", {

  lx_data_read <- readRDS(test_path("fixtures", "lx_data_read.rds"))

  lx_data_clean <- readRDS(test_path("fixtures", "lx_data_clean.rds"))

  lx_data_clean_true <- readRDS(test_path("fixtures", "lx_data_clean_true.rds"))

  expect_equal(clean_lipidxplorer_files(lx_data_read), lx_data_clean)

  expect_equal(clean_lipidxplorer_files(lx_data_read, clean_samples = TRUE),
               lx_data_clean_true)

})


test_that("merge lx-file and lv-files works", {

  lv_data_clean <- readRDS(test_path("fixtures", "lv_data_clean.rds"))

  lx_data_clean <- readRDS(test_path("fixtures", "lx_data_clean.rds"))

  data_merge <- readRDS(test_path("fixtures", "raw_data_merge.rds"))

  expect_equal(merge_with_lipidview(lx_data_clean, lv_data_clean), data_merge)

})
