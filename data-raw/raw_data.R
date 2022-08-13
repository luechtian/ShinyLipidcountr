#---- Tibble: test_raw_data_lx ----
test_raw_data_lx <-
  system.file("extdata", "lx-out.csv", package = "shinylipidcountr") %>%
  shinylipidcountr::read_lipidxplorer_files() %>%
  shinylipidcountr::clean_lipidxplorer_files(clean_samples = FALSE)

#---- Tibble: test_raw_data_lv ----
test_raw_data_lv <-
  system.file("extdata", package = "shinylipidcountr") %>%
  shinylipidcountr::list_txt_files() %>%
  shinylipidcountr::read_txt_files() %>%
  shinylipidcountr::clean_txt_files(clean_samples = FALSE) %>%
  tidyr::pivot_longer(
    cols = c(-species, -class, -scan_name),
    names_to = "sample",
    values_to = "intensity")

#---- Tibble: test_raw_data ----
## Merged tibble: raw_data_lx & raw_data_lv
test_raw_data <- shinylipidcountr::merge_with_lipidview(test_raw_data_lx,
                                                        test_raw_data_lv)


usethis::use_data(test_raw_data,
                  test_raw_data_lv,
                  test_raw_data_lx,
                  overwrite = TRUE)
