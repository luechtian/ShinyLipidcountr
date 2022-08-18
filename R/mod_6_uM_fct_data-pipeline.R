# [pmol_corr] to[uM]
pmol_to_uM <- function(raw_data){

  raw_data %>%
    dplyr::mutate(uM = pmol_corr / sample_input)

}

# Classes with identical species from different scans
# Sum pmol of identical species
sum_same_species <- function(raw_data){

  raw_data %>%
    dplyr::group_by(dplyr::across(c(-scan_name, -pmol, -pmol_corr, -uM))) %>%
    dplyr::summarise(pmol = sum(pmol),
                     pmol_corr = sum(pmol_corr),
                     uM = sum(uM),
                     .groups = "drop")
}
