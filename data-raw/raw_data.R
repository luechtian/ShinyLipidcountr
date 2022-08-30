#---- Tibble: test_raw_data_lx ----
test_raw_data_lx <-
  system.file("extdata", "lx-out.csv", package = "shinylipidcountr") %>%
  shinylipidcountr::read_lipidxplorer_files() %>%
  shinylipidcountr::clean_lipidxplorer_files(clean_samples = FALSE)

#---- Tibble: test_raw_data_lv ----
test_raw_data_lv <-
  system.file("extdata", package = "shinylipidcountr") %>%
  list.files(pattern = ".txt", full.names = TRUE) %>%
  shinylipidcountr::read_lipidview_files() %>%
  shinylipidcountr::clean_lipidview_files()

#---- Tibble: test_raw_data ----
## Merged tibble: raw_data_lx & raw_data_lv
test_raw_data <- shinylipidcountr::merge_with_lipidview(test_raw_data_lx,
                                                        test_raw_data_lv)


usethis::use_data(test_raw_data,
                  overwrite = TRUE)
