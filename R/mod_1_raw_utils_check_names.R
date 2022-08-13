# check_sample_names------------------------------------------------------------
#' Compares sample names from LipidView and LipidXplorer. If names are not
#' identical, check_sample_names() will stop the process
#'
#' @param data tibble. Data table containing LipidView or LipidXplorer data
#' @param column string. Name of the sample column
#' @param ignore string. Pattern to ignore e.g. "kon" -> Ignores Kon A, B and C
#'
#' @return vector
#' @export
#'
#' @examples
#' get_samples(
#'
check_sample_names <- function(data1, data2, ignore = "kon"){

  check <- setequal(
    get_samples(data1, ignore = ignore),
    get_samples(data2, ignore = ignore)
  )

  not_equal_lv <-
    get_samples(data1)[!get_samples(data2) %in% get_samples(data1)]

  not_equal_lx <-
    get_samples(data2)[!get_samples(data2) %in% get_samples(data1)]

  if(check == FALSE){

    stop(
      paste("Sample names from LipidView and LipidXplorer don't match, e.g.",
            not_equal_lv[1], " <-> ", not_equal_lx[1],
            ". Please adjust before continuing.")
    )

  }

}

# get_samples----------------------------------------------------------------
#' Subsets all samples from data without controls
#'
#' @param data tibble. Data table containing LipidView or LipidXplorer data
#' @param column string. Name of the sample column
#' @param ignore string. Pattern to ignore e.g. "kon" -> Ignores Kon A, B and C
#'
#' @return vector
#' @export
#'
#' @examples
#'
#'
get_samples <- function(data, column = "sample", ignore = "kon"){

  samples <- dplyr::pull(data, column) %>% unique()

  samples_for_subsetting <- janitor::make_clean_names(samples)

  ignore <- janitor::make_clean_names(ignore)

  samples[!stringr::str_detect(samples_for_subsetting, ignore)]
}
