# check_raw_meta----------------------------------------------------------------
#' Compares sample names from metadata file and rawdata. If names are not
#' identical, check_raw_meta() will stop the process
#'
#' @param raw tibble. Data table containing LipidView and/or LipidXplorer data
#' @param meta data.frame. Data table containing metadata
#' @param column string. Name of the sample column
#' @param ignore string. Pattern to ignore e.g. "kon" -> Ignores Kon A, B and C
#'
#' @return vector
#' @export
#'
#' @examples
#'
#'
check_raw_meta <- function(raw, meta, ignore = ""){

  raw <- get_samples(raw, ignore = ignore)

  meta <- meta %>%
    dplyr::filter(class == "blank") %>%
    dplyr::select(-class, -rf_values) %>%
    tidyr::pivot_longer(everything(),
                        names_to = "sample",
                        values_to = "blank") %>%
    get_samples(ignore = ignore)

  not_in_meta <- raw[!raw %in% meta]

  not_in_raw <- meta[!meta %in% raw]

  check <- setequal(raw, meta)

  if(check == FALSE){

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

}
