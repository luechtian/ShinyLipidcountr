blank_sub <- function(raw_data){

  blank_data <- raw_data %>%
    dplyr::filter(sample %in% blank) %>%
    dplyr::select(class, species, blank = sample, kon_pmol = pmol,
                  if(any(colnames(raw_data) == "scan_name")){"scan_name"})

  dplyr::left_join(raw_data, blank_data, by = c("class", "species", "blank",
                                                if(any(colnames(raw_data) == "scan_name")){"scan_name"})) %>%
    dplyr::mutate(pmol_corr = pmol - kon_pmol,
                  pmol_corr = ifelse(pmol_corr < 0, 0, pmol_corr)) %>%
    dplyr::filter(!sample %in% blank) %>%
    dplyr::select(-blank, -kon_pmol)

}
