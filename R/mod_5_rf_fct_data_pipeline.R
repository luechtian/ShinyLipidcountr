impute_rf <- function(raw_data, blank_sub = TRUE, kon = "kon"){

  if (any(is.na(raw_data$rf_values)) == TRUE){

    # Calculate pmol-values of kon's
    missing_rf <- raw_data %>%
      dplyr::filter(stringr::str_detect(as.character(sample), kon) &
                    is.na(rf_values))

    # blank substraction (depending on blank_sub-argument of function), then calculate response factors
    new_rf <- missing_rf %>%
      dplyr::mutate(rf_values = if(blank_sub == TRUE){
        sample_input / pmol_corr} else {sample_input / pmol}) %>%
      dplyr::group_by(class, species) %>%
      dplyr::summarise(rf_values = mean(rf_values), .groups = "drop")

    # join and unite the calculated rf-values and get rid off kon_b and c.
    # unite()-function merges 2 columns and returns characters.
    # Therefore remove NA-strings and convert back to numeric)
    dplyr::left_join(raw_data,
                     new_rf,
                     by = c("class", "species")) %>%
      tidyr::unite("rf_values",
                   "rf_values.y",
                   "rf_values.x",
                   sep = "",
                   remove = TRUE,
                   na.rm = TRUE) %>%
      dplyr::mutate(rf_values = stringr::str_remove_all(rf_values, "NA"),
                    rf_values = as.numeric(rf_values)) %>%
      # OR-function in str_detect to keep "Kon A" for further processing, but remove all other "Kon"s
      dplyr::filter(!stringr::str_detect(sample, kon))

  } else {
    raw_data
  }

}


apply_rf <- function(raw_data){

  if(length(raw_data) > 0){
    raw_data %>%
      dplyr::mutate(pmol_corr = pmol_corr * rf_values)
  }

}
