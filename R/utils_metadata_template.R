#' meta_data_template
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
metadata_template <- function(data){

  meta_rows <- c("group", "blank", "sample input", unique(data$class))
  meta_cols <- c("class", "rf_values", unique(data$sample))

  empty_metadata <- setNames(data.frame(matrix(ncol = length(meta_cols),
                                               nrow = length(meta_rows))), meta_cols)
  empty_metadata$class <- meta_rows

  empty_metadata %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), as.character))
}
