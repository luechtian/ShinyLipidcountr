test_that("check_names for raw and meta data works", {

  raw_data <- readRDS(test_path("fixtures", "raw_data_merge.rds"))

  raw_data_check1 <- readRDS(test_path("fixtures", "raw_data_merge.rds")) %>%
    dplyr::full_join(dplyr::tibble(sample = "Sample01"))

  metadata <- readRDS(test_path("fixtures", "metadata.rds"))

  metadata_check1 <- readRDS(test_path("fixtures", "metadata.rds")) %>%
    dplyr::mutate('Sample01' = 'Sample 1')

  metadata_check2 <- readRDS(test_path("fixtures", "metadata.rds")) %>%
    dplyr::mutate(class = stringr::str_replace(class, "sample input", "Sample Input"))

  expect_warning(check_raw_meta(raw_data_check1, metadata),
                 "No samples in rawdata missing. // Sample01 in metadata missing")

  expect_warning(check_raw_meta(raw_data, metadata_check1),
                 "Sample01 in rawdata missing. // No samples in metadata missing")

  expect_error(check_raw_meta(raw_data, metadata_check2),
                 "The first three elements of column 'class' must be")

})
