test_that("get_samples works", {

  lv_data_clean <- readRDS(test_path("fixtures", "lv_data_clean.rds")) %>% head

  expect_equal(get_samples(lv_data_clean),
               c("Sample 1", "Sample 2", "Sample 3",
                 "Sample 4", "Sample 5", "Sample 6"))

})

test_that("check_names for raw data works", {

  lv_data_clean <- readRDS(test_path("fixtures", "lv_data_clean.rds"))

  lx_data_clean_true <- readRDS(test_path("fixtures", "lx_data_clean_true.rds"))

  lx_data_clean <- readRDS(test_path("fixtures", "lx_data_clean.rds"))

  expect_error(check_sample_names(lv_data_clean, lx_data_clean_true),
               "Sample names from LipidView and LipidXplorer don't match")

  expect_message(check_sample_names(lv_data_clean, lx_data_clean),
                 "Sample names check passed!")

})
