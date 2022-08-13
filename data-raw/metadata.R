#---- Data frame: test_metadata ----
test_metadata <- system.file("extdata", "metadata.csv",
                        package = "shinylipidcountr") %>%
  read.csv(check.names = FALSE)

usethis::use_data(test_metadata, overwrite = TRUE)
