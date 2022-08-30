# check_raw_meta----------------------------------------------------------------
#' Compares sample names from metadata file and rawdata. If names are not
#' identical, check_raw_meta() will stop the process
#'
#' @param raw tibble. Data table containing LipidView and/or LipidXplorer data
#' @param metadata data.frame. Data table containing metadata
#' @param ignore string. Pattern to ignore e.g. "kon" -> Ignores Kon A, B and C
#'
#' @return warning- or errormessage if sample names dont match.
#' @export
#'
#' @examples
#' check_raw_meta(test_raw_data, test_metadata)
check_raw_meta <- function(raw, metadata, ignore = ""){

  raw <- get_samples(raw, ignore = ignore)

  meta <- metadata %>%
    dplyr::filter(class == "blank") %>%
    dplyr::select(-class, -rf_values) %>%
    tidyr::pivot_longer(everything(),
                        names_to = "sample",
                        values_to = "blank") %>%
    get_samples(ignore = ignore)

  not_in_meta <- raw[!raw %in% meta]

  not_in_raw <- meta[!meta %in% raw]

  check1 <- setequal(raw, meta)

  check2 <- all(c("group", "blank", "sample input") %in% metadata$class)

  if(check1 == FALSE){

    warning(
      "Sample names from rawdata and metadata don't match. ",
      if(length(not_in_raw) == 0){"No samples"}, not_in_raw,
      " in rawdata missing.",
      " // ",
      if(length(not_in_meta) == 0){"No samples"}, not_in_meta,
      " in metadata missing. ",
      "Please adjust before continuing."
    )

  }

  if(check2 == FALSE){

    stop(
      "The first three elements of column 'class' must be 'group', 'blank' and 'sample input'! Please adjust the metadata-table."
    )

  }

}
