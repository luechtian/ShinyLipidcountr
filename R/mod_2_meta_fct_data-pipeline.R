# Load metadata csv-file
read_metadata <- function(path, pattern = "metadata", clean_samples = FALSE){
  metadata <- list.files(path = path,
                         pattern = pattern,
                         full.names = T) %>%
    read.csv(check.names = FALSE)

  if(clean_samples == TRUE){
    metadata %>% janitor::clean_names()
  } else { metadata }

}

# Join metadata to the rawdata
join_metadata <- function(raw_data, metadata){

  # Extract sample input [ul] from meta data
  sample_input <- metadata %>%
    dplyr::filter(class == "sample input") %>%
    dplyr::select(-class, -rf_values) %>%
    tidyr::pivot_longer(everything(),
                        names_to = "sample",
                        values_to = "sample_input",
                        values_transform = as.numeric)

  sample_groups <- metadata %>%
    dplyr::filter(class == "group") %>%
    dplyr::select(-class, -rf_values) %>%
    tidyr::pivot_longer(everything(),
                        names_to = "sample",
                        values_to = "group",
                        values_transform = as.character)

  sample_blanks <- metadata %>%
    dplyr::filter(class == "blank") %>%
    dplyr::select(-class, -rf_values) %>%
    tidyr::pivot_longer(everything(),
                        names_to = "sample",
                        values_to = "blank",
                        values_transform = as.character)

  # Extract internal standard concentrations [pmol]
  # and response factors (rf-values) from meta data
  standard_input <- metadata %>%
    dplyr::filter(class != "sample input",
                  class != "group",
                  class != "blank") %>%
    tidyr::pivot_longer(
      cols = c(-class, -rf_values),
      names_to = "sample",
      values_to = "standard_input",
      values_transform = as.numeric)

  # Join metadata
  joined_metadata <- standard_input %>%
    dplyr::left_join(sample_input, by = "sample") %>%
    dplyr::left_join(sample_groups, by = "sample") %>%
    dplyr::left_join(sample_blanks, by = "sample")


  # Join raw_data and metadata and keep order for species, sample and groups
  dplyr::left_join(raw_data, joined_metadata, by = c("class", "sample")) %>%
    dplyr::mutate(species = forcats::fct_inorder(species, ordered = NA),
                  sample = forcats::fct_inorder(sample, ordered = NA),
                  group = forcats::fct_inorder(group, ordered = NA),
                  sample_name = stringr::str_remove(sample, "sample_"),
                  sample_name = paste(sample_name, "-", group),
                  sample_name = forcats::fct_inorder(sample_name, ordered = NA))

}
