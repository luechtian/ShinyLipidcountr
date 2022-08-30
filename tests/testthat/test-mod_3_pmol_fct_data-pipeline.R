test_that("joining means of internal standards works", {

  raw_meta1 <- readRDS(test_path("fixtures", "raw_meta_merge.rds")) %>%
    dplyr::filter(class == "CE", sample == 'Sample 1')

  raw_meta2 <- readRDS(test_path("fixtures", "raw_meta_merge.rds")) %>%
    dplyr::filter(class == "TG", sample == 'Sample 1')

  expect_equal(join_istd_means(raw_meta1)$is_intensity %>% unique, 16958910.7)

  expect_equal(join_istd_means(raw_meta2)$is_intensity %>% unique, 9805117.5)

})


test_that("calculating pmol values works", {

  raw_meta1 <- readRDS(test_path("fixtures", "raw_meta_merge.rds")) %>%
    join_istd_means() %>%
    dplyr::filter(class == "CE", sample == "Sample 1", species == "18:1")

  raw_meta2 <- readRDS(test_path("fixtures", "raw_meta_merge.rds")) %>%
    join_istd_means() %>%
    dplyr::filter(class == "TG", sample == "Sample 1", species == "52:2")

  expect_equal(intensity_to_pmol(raw_meta1)$pmol %>% unique, 4.4922712)

  expect_equal(intensity_to_pmol(raw_meta2)$pmol %>% unique, 2.0516538)

})
