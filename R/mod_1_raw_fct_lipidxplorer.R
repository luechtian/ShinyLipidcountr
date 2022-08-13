
read_lipidxplorer_files <- function(path){

  readr::read_csv(path, show_col_types = FALSE, na = c("", "None")) %>%
    dplyr::filter(Class != "###") %>%
    dplyr::select(Class, ChemicalSpecies, MolecularSpecies, LipidCategory, Mass,
                  tidyselect::starts_with("Intensity"),
                  tidyselect::starts_with("FattyAcidIntensity"))

}

clean_lipidxplorer_files <- function(rawdata, clean_samples = TRUE){

  if(clean_samples == TRUE){

    data <- rawdata %>%
      janitor::clean_names() %>%
      tidyr::pivot_longer(
        cols = c(-class, -chemical_species, -molecular_species, -lipid_category,
                 -mass),
        names_to = "sample",
        values_to = "intensity")

    # separate fullms- and ms2-intensities
    # ms1 data in column 'intensity'
    # ms2 data in column 'fa_intensity'
    data <- data %>%
      dplyr::mutate(fatty_acid_comp = ifelse(stringr::str_detect(sample, "fatty"),
                                      "fa_intensity", "intensity"),
                    sample = stringr::str_remove(sample, "fatty_acid_")) %>%
      tidyr::pivot_wider(id_cols = c(class, chemical_species, molecular_species,
                                     lipid_category, mass, sample),
                         names_from = fatty_acid_comp, values_from = intensity,
                         names_repair = "check_unique")

    # remove unnecessary strings from sample names for better reading
    data <- data %>%
      dplyr::mutate(sample = stringr::str_remove(sample, "intensity_"),
                    sample = stringr::str_remove(sample, "_s_mz_xml"))

  } else if(clean_samples == FALSE){

    data <- rawdata %>%
      tidyr::pivot_longer(
        cols = c(-Class, -ChemicalSpecies, -MolecularSpecies, -LipidCategory,
                 -Mass),
        names_to = "Sample",
        values_to = "Intensity") %>%
      janitor::clean_names()

    # separate fullms- and ms2-intensities
    # ms1 data in column 'intensity'
    # ms2 data in column 'fa_intensity'
    data <- data %>%
      dplyr::mutate(fatty_acid_comp = ifelse(stringr::str_detect(sample, "Fatty"),
                                             "fa_intensity", "intensity"),
                    sample = stringr::str_remove(sample, "FattyAcid")) %>%
      tidyr::pivot_wider(id_cols = c(class, chemical_species, molecular_species,
                                     lipid_category, mass, sample),
                         names_from = fatty_acid_comp, values_from = intensity,
                         names_repair = "check_unique")

    # remove unnecessary strings from sample names for better reading
    data <- data %>%
      dplyr::mutate(sample = stringr::str_remove(sample, "Intensity:"),
                    sample = stringr::str_remove(sample, "-s.mzXML"))

  }

  # enhance internal standard species name with 'IS', so the function
  # 'join_istd_means()' can detect internal standards
  data <- data %>%
    dplyr::mutate(chemical_species = ifelse(lipid_category == "Internal standard",
                                            paste0("IS ", chemical_species),
                                            chemical_species),
                  molecular_species = ifelse(lipid_category == "Internal standard",
                                             paste0("IS ", molecular_species),
                                             molecular_species))
  # remove class name from species for better reading
  data <- data %>%
    dplyr::rename(species = chemical_species) %>%
    dplyr::mutate(species = ifelse(class != species,
                                   stringr::str_remove_all(species, paste0(class, " ")),
                                   species))

  return(data)
}


merge_with_lipidview <- function(lx_data, lv_data){

  lx_data_for_merging <-  lx_data %>%
    dplyr::rename(scan_name = molecular_species) %>%
    dplyr::select(class, species, scan_name, sample, intensity)

  rbind(lv_data, lx_data_for_merging)
}
