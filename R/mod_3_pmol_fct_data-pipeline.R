# join_istd_means---------------------------------------------------------------
#' Join averaged IS intensities to samples by class-column
#'
#' @description join_istd_means() extracts all rows with "IS" in
#' 'species'-column and calculates the mean of same lipidclass-standards.
#' Afterwards these intensity-means are joined by 'class' and 'sample' in column
#' 'is_intensity'
#'
#' @param raw_data tibble. Joined raw and metadata table.
#'
#' @return tibble
#' @export
#'
#' @examples
#' join_metadata(test_raw_data, test_metadata) %>%
#'   join_istd_means()
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

# intensity_to_pmol-------------------------------------------------------------
#' Calculate pmol values
#'
#' @description intensity_to_pmol() calculates pmol-values from intensity of
#' endogenous lipid species, intensity-mean of the class-specific internal
#' standard and the internal standard concentration in pmol.
#' (standard_input / is_intensity) * intensity)
#'
#' @param raw_data tibble. Comes after join_istd_means()
#'
#' @return tibble
#' @export
#'
#' @examples
#' join_metadata(test_raw_data, test_metadata) %>%
#'   join_istd_means() %>%
#'   intensity_to_pmol()
intensity_to_pmol <- function(raw_data){

  raw_data %>%
    dplyr::mutate(pmol = (standard_input / is_intensity) * intensity) %>%
    dplyr::select(-is_intensity, -intensity)

}
