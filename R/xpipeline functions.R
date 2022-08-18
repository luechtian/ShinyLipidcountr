################################################################################

## Small helperfunction
# check_blank <- function(raw_data, blank_id = "kon_a"){
#
#   #Abort + error message, if blank_id doesnt fit to any sample
#   if ( nrow(dplyr::filter(raw_data, sample == blank_id)) == 0 ){
#     rlang::abort(paste("blank_id doesn't exist. Maybe this is your blank_id:",
#                        filter(raw_data, stringr::str_detect(sample, "kon")) %>%
#                          dplyr::pull(sample) %>%
#                          unique() %>%
#                          as.character()
#     )
#     )
#   }
#
#   dplyr::filter(raw_data, sample == blank_id)
# }

################################################################################

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

################################################################################

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
                     .groups = "drop") %>%
    dplyr::ungroup()

}
