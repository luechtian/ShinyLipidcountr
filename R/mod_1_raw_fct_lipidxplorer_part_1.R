#' lipidxplorer_part_1
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
read_lipidxplorer_files <- function(path){

  readr::read_csv(path, show_col_types = FALSE, na = c("", "None")) %>%
    janitor::clean_names() %>%
    dplyr::filter(class != "###") %>%
    dplyr::select(class, chemical_species, molecular_species, lipid_category,
                  mass, tidyselect::starts_with("intensity"),
                  tidyselect::starts_with("fatty_acid_intensity")) %>%
    tidyr::pivot_longer(
      cols = c(-class, -chemical_species, -molecular_species,
               -lipid_category, -mass),
      names_to = "sample",
      values_to = "intensity")

}

clean_lipidxplorer_files <- function(data){
  # separate intensities of precursors from intensities of the summed FA's
  dplyr::mutate(data, fatty_acid_comp = ifelse(stringr::str_detect(sample, "fatty"),
                                               "fa_intensity", "intensity"),
                sample = stringr::str_remove(sample, "fatty_acid_")) %>%
    tidyr::pivot_wider(id_cols = c(class, chemical_species, molecular_species,
                                   lipid_category, mass, sample),
                       names_from = fatty_acid_comp, values_from = intensity,
                       names_repair = "check_unique") %>%

    dplyr::mutate(sample = stringr::str_remove(sample, "intensity_"),
                  sample = stringr::str_remove(sample, "_s_mz_xml"),
                  chemical_species = ifelse(lipid_category == "Internal standard",
                                            paste0("IS ", chemical_species),
                                            chemical_species),
                  molecular_species = ifelse(lipid_category == "Internal standard",
                                             paste0("IS ", molecular_species),
                                             molecular_species)) %>%
    dplyr::rename(species = chemical_species) %>%
    # remove class name from species
    dplyr::mutate(species = ifelse(class != species,
                                   stringr::str_remove_all(species, paste0(class, " ")),
                                   species))

}
