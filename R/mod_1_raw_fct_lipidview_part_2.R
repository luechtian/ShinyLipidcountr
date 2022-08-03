#' lipidview_part_2
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# Merge all lipidclass based textfiles from LipidView
merge_lipidview_files <- function(path){

  files <- list.files(path, pattern = ".txt", full.names = TRUE)

  # Parse lipid class from file names
  splitted_files <- stringr::str_split(files, pattern = "_", simplify = TRUE)
  names(files) <- stringr::str_remove_all(splitted_files[, ncol(splitted_files)], ".txt")

  # Read and merge all LipidView txt.files
  raw_df <- purrr::map_df(files, readr::read_tsv, skip = 1, .id = "class") %>%
    janitor::clean_names()

  # General clean up of the LipidView dataframe:
  ## Get rid off unnecessary columns and strings, rename columns
  raw_df %>%
    dplyr::filter(!stringr::str_detect(sample_name, "Sample ID")) %>%
    dplyr::select(-sample_name, -x2, -x3, -x4) %>%
    dplyr::rename("species" = x5,
                  "scan_name" = x6) %>%
    tidyr::pivot_longer(
      cols = c(-species, -class, -scan_name),
      names_to = "sample",
      values_to = "intensity") %>%
    dplyr::mutate(species = stringr::str_remove_all(species, "\\+NH4"),
                  scan_name = stringr::str_remove(scan_name, "\\(-FA "),
                  scan_name = stringr::str_remove(scan_name, "\\)"),
                  scan_name = stringr::str_remove(scan_name, "\\("))
}

# Load metadata csv-file
get_meta_data <- function(path, pattern = ".csv"){
  read.csv(list.files(path = path,
                      pattern = pattern,
                      full.names = T)) %>%
    janitor::clean_names()
}

read_metadata <- function(path, pattern = "metadata"){
  list.files(path = path, pattern = pattern, full.names = T) %>%
    readr::read_csv(col_types = readr::cols()) %>%
    janitor::clean_names()
}

# Extract all internal standard rows, calculate the mean for each class/sample
# and join the mean-data to the rawdata
# at the end, get rid off internal standard rows
join_istd_means <- function(raw_data){

  # Extract the intensities of internal standards and
  # return the mean of IS-intensities to the rawdata
  is_data <- raw_data %>%
    dplyr::filter(stringr::str_detect(species, "IS"),
                  class != "TAG") %>%
    dplyr::group_by(class, sample) %>%
    dplyr::summarise(is_intensity = mean(intensity), .groups = "drop")

  # Special case for TAG:
  # Because "IS D5-TAG 17:0/17:1/17:0" is the only ISTD for
  # TAG quantification in use, it is filtered by hardcode for now.
  is_data <- raw_data %>%
    dplyr::filter(stringr::str_detect(species, "IS"),
                  species %in% c("IS D5-TAG 17:0/17:1/17:0",
                                 "IS D5-17:0/17:1/17:0")) %>%
    dplyr::group_by(class, sample) %>%
    dplyr::summarise(is_intensity = sum(intensity), .groups = "drop") %>%
    dplyr::full_join(is_data, by = c("class", "sample", "is_intensity"))

  raw_data %>%
    dplyr::left_join(is_data, by = c("class", "sample")) %>%
    dplyr::filter(!stringr::str_detect(species, "IS"))

}

# Join metadata to the rawdata
join_metadata <- function(raw_data, metadata){

  # Extract sample input [ul] from meta data
  sample_input <- metadata %>%
    dplyr::filter(class == "sample input") %>%
    dplyr::select(-class, -rf_values) %>%
    tidyr::pivot_longer(everything(),
                        names_to = "sample",
                        values_to = "sample_input") %>%
    dplyr::mutate(sample_input = as.numeric(sample_input))

  sample_groups <- metadata %>%
    dplyr::filter(class == "group") %>%
    dplyr::select(-class, -rf_values) %>%
    tidyr::pivot_longer(everything(),
                        names_to = "sample",
                        values_to = "group")

  # Extract internal standard concentrations [pmol]
  # and response factors (rf-values) from meta data
  standard_input <- metadata %>%
    dplyr::filter(class != "sample input",
                  class != "group",
                  # if multi_blank_sub() is in use, it is necessary to remove blank-row
                  class != "blank") %>%
    tidyr::pivot_longer(
      cols = c(-class, -rf_values),
      names_to = "sample",
      values_to = "standard_input") %>%
    dplyr::mutate(standard_input = as.numeric(standard_input))

  # Join metadata
  joined_metadata <- standard_input %>%
    dplyr::left_join(sample_input, by = "sample") %>%
    dplyr::left_join(sample_groups, by = "sample")


  # Join raw_data and metadata and keep order for species, sample and groups
  dplyr::left_join(raw_data, joined_metadata, by = c("class", "sample")) %>%
    dplyr::mutate(species = forcats::fct_inorder(species, ordered = NA),
                  sample = forcats::fct_inorder(sample, ordered = NA),
                  group = forcats::fct_inorder(group, ordered = NA),
                  sample_name = stringr::str_remove(sample, "sample_"),
                  sample_name = paste(sample_name, "-", group),
                  sample_name = forcats::fct_inorder(sample_name, ordered = NA))

}

# Calculate missing rf-values from Kon B & C
if_rf_missing <- function(raw_data){

  if (any(is.na(raw_data$rf_values)) == TRUE) {

    # Calculate pmol-values of kon_a, _b and _c
    missing_rf <- raw_data %>%
      dplyr::filter(stringr::str_detect(sample, "kon") & is.na(rf_values)) %>%
      dplyr::mutate(pmol = (standard_input / is_intensity * intensity))

    # create kon_a variable for better reading
    kon_a <- missing_rf %>%
      dplyr::filter(sample == "kon_a") %>%
      dplyr::select(class, species, kon_a_pmol = pmol)

    # add new column with pmol-values of kon_a for blank substraction and
    missing_rf <- dplyr::left_join(missing_rf, kon_a, by = c("class", "species")) %>%
      dplyr::mutate(rf_values = sample_input / (pmol - kon_a_pmol)) %>%
      dplyr::filter(!stringr::str_detect(sample, "kon_a")) %>%
      dplyr::group_by(class, species) %>%
      dplyr::summarise(rf_values = mean(rf_values), .groups = "drop")

    # join and unite the calculated rf-values and get rid off kon_b and c.
    # unite()-function merges 2 columns and returns characters.
    # Therefore remove NA-strings and convert back to numeric)
    dplyr::left_join(raw_data,
                     missing_rf,
                     by = c("class", "species")) %>%
      tidyr::unite("rf_values",
                   "rf_values.y",
                   "rf_values.x",
                   sep = "",
                   remove = TRUE,
                   na.rm = TRUE) %>%
      dplyr::mutate(rf_values = stringr::str_remove_all(rf_values, "NA"),
                    rf_values = as.numeric(rf_values)) %>%
      dplyr::filter(!(sample %in% c("kon_b", "kon_c")))

  } else {
    raw_data
  }

}

# More flexible version of "if_rf_missing()"
impute_rf <- function(raw_data, blank_sub = TRUE, blank_id = "kon_a"){

  if (any(is.na(raw_data$rf_values)) == TRUE) {

    # Calculate pmol-values of kon's
    missing_rf <- raw_data %>%
      dplyr::filter(stringr::str_detect(sample, "kon") & is.na(rf_values)) %>%
      intensity_to_pmol()

    # blank substraction (depending on blank_sub-argument of function), then calculate response factors
    new_rf <- missing_rf %>%
      dplyr::mutate(rf_values = if(blank_sub == TRUE){
        sample_input / (pmol - blank(., blank_id)$pmol)} else {sample_input / pmol}) %>%
      dplyr::filter(!stringr::str_detect(sample, "kon_a")) %>%
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
      dplyr::filter(stringr::str_detect(sample, "kon_a") |
                      !stringr::str_detect(sample, "kon") )

  } else {
    raw_data
  }

}
## Small helperfunction
blank <- function(raw_data, blank_id = "kon_a"){

  #Abort + error message, if blank_id doesnt fit to any sample
  if ( nrow(dplyr::filter(raw_data, sample == blank_id)) == 0 ){
    rlang::abort(paste("blank_id doesn't exist. Maybe this is your blank_id:",
                       filter(raw_data, stringr::str_detect(sample, "kon")) %>%
                         pull(sample) %>%
                         unique() %>%
                         as.character()
    )
    )
  }

  dplyr::filter(raw_data, sample == blank_id)
}

# Classes with identical species from different scans
# Sum intensities of identical species
sum_same_species <- function(raw_data){

  raw_data %>%
    dplyr::group_by(dplyr::across(c(-scan_name, -pmol, -pmol_corr, -uM))) %>%
    dplyr::summarise(pmol = sum(pmol),
                     pmol_corr = sum(pmol_corr),
                     uM = sum(uM),
                     .groups = "drop") %>%
    dplyr::ungroup()

}

# DAG combinatorics
combine_dag_species <- function(raw_data){

  if("DAG" %in% raw_data$class){

    dag_data <- raw_data %>%
      dplyr::filter(class == "DAG")

    dag_species <- dag_data %>%
      dplyr::select(species) %>%
      dplyr::distinct() %>%
      purrr::as_vector()

    for (x in dag_species) {

      species_scans <- dag_data %>%
        dplyr::group_by(species) %>%
        dplyr::distinct(scan_name) %>%
        dplyr::filter(species == x)

      ids <- dplyr::as_tibble(arrangements::combinations(x = species_scans$scan_name,
                                                         k = 2,
                                                         replace = T)) %>%
        dplyr::rename(fa1 = V1,
                      fa2 = V2) %>%
        dplyr::mutate(c1 = stringr::str_split(fa1, ":"),
                      c2 = stringr::str_split(fa2, ":"),
                      db1 = as.numeric(purrr::map_chr(c1, 2)),
                      db2 = as.numeric(purrr::map_chr(c2, 2)),
                      c1 = as.numeric(purrr::map_chr(c1, 1)),
                      c2 = as.numeric(purrr::map_chr(c2, 1))) %>%
        dplyr::mutate(c_sum = c1 + c2,
                      db_sum = db1 + db2,
                      species = paste(c_sum, db_sum, sep = ":"),
                      #species = paste("DAG", species)
        ) %>%
        dplyr::filter(species == x) %>%
        dplyr::mutate(id = dplyr::row_number(),
                      dist_species = paste(fa1, fa2, sep = "_"),
                      dist_species = paste("DAG", dist_species)) %>%
        tidyr::pivot_longer(c(fa1, fa2),
                            values_to = "scan_name") %>%
        dplyr::select(species, dist_species, scan_name, id) %>%
        dplyr::filter(!duplicated(scan_name))

      dag_data <- dplyr::left_join(dag_data, ids, by = c("species", "scan_name")) %>%
        tidyr::unite(id, starts_with("id"), na.rm = TRUE, remove = TRUE) %>%
        tidyr::unite(dist_species, starts_with("dist_species"), na.rm = TRUE)

    }

    dag_data <- dag_data %>%
      dplyr::mutate(dist_species = stringr::str_replace(dist_species, "_DAG", "DAG"),
                    id = stringr::str_remove(id, "_"),
                    dist_species = dplyr::na_if(dist_species, "")) %>%
      # line above can be removed, if TL is fixed!) %>%
      dplyr::group_by(dplyr::across(c(-pmol, -pmol_corr, -scan_name))) %>%
      # Wichtige Line unten! Wenn ein Neutralverlust Null ist, dann wird es NA.
      # D.h. Kombi aus beiden zusammengehörigen Neutralverlusten wird auch NA.
      dplyr::mutate(pmol_corr = dplyr::na_if(pmol_corr, 0)) %>%
      dplyr::summarise(pmol = sum(pmol),
                       pmol_corr = sum(pmol_corr), .groups = "drop") %>%
      dplyr::ungroup() %>%
      dplyr::select(-id, -species) %>%
      dplyr::rename(species = dist_species) %>%
      dplyr::mutate(species = stringr::str_remove(species, "DAG ")) %>%
      tidyr::drop_na(species)

    dag_data <- dplyr::mutate(dag_data,
                              pmol_corr = ifelse(is.na(pmol_corr), 0, pmol_corr)
    )

    raw_data %>%
      dplyr::filter(class != "DAG") %>%
      dplyr::full_join(dag_data) %>%
      dplyr::mutate(species = forcats::fct_inorder(species, ordered = NA))

  } else { raw_data }

}

# DAG combinatorics (old)
## before blank sub, after apply_rf! function need intensities!
combine_dag_species_old <- function(raw_data){

  if("DAG" %in% raw_data$class){

    dag_data <- raw_data %>%
      dplyr::filter(class == "DAG")

    dag_species <- dag_data %>%
      dplyr::select(species) %>%
      dplyr::distinct() %>%
      purrr::as_vector()

    for (x in dag_species) {

      species_scans <- dag_data %>%
        dplyr::group_by(species) %>%
        dplyr::distinct(scan_name) %>%
        dplyr::filter(species == x)

      ids <- dplyr::as_tibble(arrangements::combinations(x = species_scans$scan_name,
                                                         k = 2,
                                                         replace = T)) %>%
        dplyr::rename(fa1 = V1,
                      fa2 = V2) %>%
        dplyr::mutate(c1 = stringr::str_split(fa1, ":"),
                      c2 = stringr::str_split(fa2, ":"),
                      db1 = as.numeric(purrr::map_chr(c1, 2)),
                      db2 = as.numeric(purrr::map_chr(c2, 2)),
                      c1 = as.numeric(purrr::map_chr(c1, 1)),
                      c2 = as.numeric(purrr::map_chr(c2, 1))) %>%
        dplyr::mutate(c_sum = c1 + c2,
                      db_sum = db1 + db2,
                      species = paste(c_sum, db_sum, sep = ":"),
                      species = paste("DAG", species)
        ) %>%
        dplyr::filter(species == x) %>%
        dplyr::mutate(id = dplyr::row_number(),
                      dist_species = paste(fa1, fa2, sep = "_"),
                      dist_species = paste("DAG", dist_species)) %>%
        tidyr::pivot_longer(c(fa1, fa2),
                            values_to = "scan_name") %>%
        dplyr::select(species, dist_species, scan_name, id) %>%
        dplyr::filter(!duplicated(scan_name))

      dag_data <- dplyr::left_join(dag_data, ids, by = c("species", "scan_name")) %>%
        tidyr::unite(id, starts_with("id"), na.rm = TRUE, remove = TRUE) %>%
        tidyr::unite(dist_species, starts_with("dist_species"), na.rm = TRUE)

    }

    dag_data <- dag_data %>%
      dplyr::mutate(dist_species = stringr::str_replace(dist_species, "_DAG", "DAG"),
                    id = stringr::str_remove(id, "_"),
                    intensity = ifelse(sample == "kon_a", intensity,
                                       dplyr::na_if(intensity, 0)),
                    dist_species = dplyr::na_if(dist_species, "")) %>%
      # line above can be removed, if TL is fixed!) %>%
      dplyr::group_by(dplyr::across(c(-intensity, -scan_name))) %>%
      dplyr::summarise(intensity = sum(intensity), .groups = "drop") %>%
      dplyr::ungroup() %>%
      dplyr::select(-id, -species) %>%
      dplyr::rename(species = dist_species) %>%
      tidyr::drop_na(intensity, species)

    raw_data %>%
      dplyr::filter(class != "DAG") %>%
      dplyr::full_join(dag_data)

  } else { raw_data }

}

# TAG combinatorics
combine_tag_species <- function(raw_data){

  if("TAG" %in% raw_data$class){

    tag_data <- raw_data %>%
      dplyr::filter(class == "TAG")

    tag_species <- tag_data %>%
      dplyr::select(species) %>%
      dplyr::distinct() %>%
      purrr::as_vector()

    for (x in tag_species) {

      species_scans <- tag_data %>%
        dplyr::filter(class == "TAG") %>%
        dplyr::group_by(species) %>%
        dplyr::distinct(scan_name) %>%
        dplyr::filter(species == x)

      ids <- dplyr::as_tibble(arrangements::combinations(x = species_scans$scan_name,
                                                         k = 3,
                                                         replace = T)) %>%
        dplyr::rename(fa1 = V1,
                      fa2 = V2,
                      fa3 = V3) %>%
        dplyr::mutate(c1 = stringr::str_split(fa1, ":"),
                      c2 = stringr::str_split(fa2, ":"),
                      c3 = stringr::str_split(fa3, ":"),
                      db1 = as.numeric(purrr::map_chr(c1, 2)),
                      db2 = as.numeric(purrr::map_chr(c2, 2)),
                      db3 = as.numeric(purrr::map_chr(c3, 2)),
                      c1 = as.numeric(purrr::map_chr(c1, 1)),
                      c2 = as.numeric(purrr::map_chr(c2, 1)),
                      c3 = as.numeric(purrr::map_chr(c3, 1))) %>%
        dplyr::mutate(c_sum = c1 + c2 + c3,
                      db_sum = db1 + db2 + db3,
                      species = paste(c_sum, db_sum, sep = ":"),
                      species = paste("TAG", species)) %>%
        dplyr::filter(species == x) %>%
        dplyr::mutate(id = dplyr::row_number(),
                      dist_species = paste(fa1, fa2, fa3, sep = "_"),
                      dist_species = paste("TAG", dist_species)) %>%
        tidyr::pivot_longer(c(fa1, fa2, fa3),
                            values_to = "scan_name") %>%
        dplyr::select(species, dist_species, scan_name, id) %>%
        tidyr::unite(scan_id, scan_name, id, remove = FALSE) %>%
        dplyr::filter(!duplicated(scan_id)) %>%
        dplyr::select(-scan_id)

      tag_data <- dplyr::left_join(tag_data, ids, by = c("species", "scan_name")) %>%
        tidyr::unite(id, tidyselect::starts_with("id"), na.rm = TRUE, remove = TRUE) %>%
        tidyr::unite(dist_species, tidyselect::starts_with("dist_species"), na.rm = TRUE)

    }

    tag_data <- tag_data %>%
      dplyr::mutate(dist_species = stringr::str_replace(dist_species, "_TAG", "TAG"),
                    id = stringr::str_remove(id, "_"),
                    intensity = ifelse(sample == "kon_a", intensity, dplyr::na_if(intensity, 0)),
                    dist_species = dplyr::na_if(dist_species, "")) %>%
      # line above can be removed, if TL is fixed!
      dplyr::group_by(dplyr::across(c(-intensity, -scan_name))) %>%
      dplyr::summarise(intensity = sum(intensity), .groups = "drop") %>%
      dplyr::ungroup() %>%
      dplyr::select(-id, -species) %>%
      dplyr::rename(species = dist_species) %>%
      tidyr::drop_na(intensity, species) # 'species' can be removed, if TL is fixed!

    raw_data %>%
      dplyr::filter(class != "TAG") %>%
      dplyr::full_join(tag_data)

  } else { raw_data }

}

# Apply Response factor
apply_rf <- function(raw_data){

  raw_data %>%
    dplyr::mutate(intensity = intensity * rf_values) %>%
    dplyr::select(-rf_values)

}

# [intensities] to [pmol]
intensity_to_pmol <- function(raw_data){

  raw_data %>%
    dplyr::mutate(pmol = (standard_input / is_intensity) * intensity) %>%
    dplyr::select(-is_intensity, -intensity)

}

# Blank substraction
blank_substraction <- function(raw_data, blank = "kon_a"){

  blank_data <- raw_data %>%
    dplyr::filter(sample == blank) %>%
    dplyr::select(class, species, kon_a_pmol = pmol,
                  if(any(colnames(raw_data) == "scan_name")){"scan_name"})

  dplyr::left_join(raw_data, blank_data, by = c("class", "species",
                                                if(any(colnames(raw_data) == "scan_name")){"scan_name"})) %>%
    dplyr::mutate(pmol_corr = pmol - kon_a_pmol,
                  pmol_corr = ifelse(pmol_corr < 0, 0, pmol_corr)) %>%
    dplyr::filter(!stringr::str_detect(sample, blank)) %>%
    dplyr::select(-kon_a_pmol)

}

# Multiple blank substraction
## Use this method, if EVERY lipidclass has multiple blanks (Kon A)
## dont forget the "blank"-row in metadata-file to assign a blank to a sample
## This feature isn't implented yet for impute_rf()-function
multi_blank_sub <- function(raw_data, meta_data, blank_id = "kon_a"){

  sample_blanks <- meta_data %>%
    dplyr::filter(class == "blank") %>%
    dplyr::select(-class, -rf_values) %>%
    tidyr::pivot_longer(everything(),
                        names_to = "sample",
                        values_to = "blank") %>%
    dplyr::mutate(blank = snakecase::to_any_case(make.names(blank)))

  blank_data <- raw_data %>%
    dplyr::filter(stringr::str_detect(sample, blank_id)) %>%
    dplyr::select(class, species, blank = sample, kon_pmol = pmol,
                  if(any(colnames(raw_data) == "scan_name")){"scan_name"}) %>%
    dplyr::mutate(blank = snakecase::to_any_case(as.character(blank)))

  dplyr::left_join(raw_data, sample_blanks, by = c("sample")) %>%
    dplyr::left_join(blank_data, by = c("class", "species", "blank",
                                        if(any(colnames(raw_data) == "scan_name")){"scan_name"})) %>%
    dplyr::mutate(pmol_corr = pmol - kon_pmol,
                  pmol_corr = ifelse(pmol_corr < 0, 0, pmol_corr)) %>%
    dplyr::filter(!stringr::str_detect(sample, blank_id)) %>%
    dplyr::select(-blank, -kon_pmol)

}

# [pmol_corr] to[uM]
pmol_to_uM <- function(raw_data){

  raw_data %>%
    dplyr::mutate(uM = pmol_corr / sample_input)

}
