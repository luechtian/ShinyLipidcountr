# Extract all internal standard rows, calculate the mean for each class/sample
# and join the mean-data to the rawdata
# at the end, get rid off internal standard rows
join_istd_means <- function(raw_data){

  # Extract the intensities of internal standards and
  # return the mean of IS-intensities to the rawdata
  is_data <- raw_data %>%
    dplyr::filter(stringr::str_detect(species, "IS")) %>%
    dplyr::group_by(class, sample) %>%
    dplyr::summarise(is_intensity = mean(intensity), .groups = "drop")

  raw_data %>%
    dplyr::left_join(is_data, by = c("class", "sample")) %>%
    dplyr::filter(!stringr::str_detect(species, "IS"))

}

# [intensities] to [pmol]
intensity_to_pmol <- function(raw_data){

  raw_data %>%
    dplyr::mutate(pmol = (standard_input / is_intensity) * intensity) %>%
    dplyr::select(-is_intensity, -intensity)

}
