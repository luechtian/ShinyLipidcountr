# check_blank------------------------------------------------------------
#' Checks sample-column from raw data, if any blank is there from metadata
#'
#' @param data_pmol tibble. Data table containing LipidView and/or LipidXplorer data. Already merged with metadata.
#'
#' @return vector
#' @export
check_blank <- function(data_pmol){

  blanks <- data_pmol %>%
    dplyr::filter(sample %in% blank)

  if(nrow(blanks) == 0){

    stop(
      "No sample to blank assignment.
      Check consistency of your blanks in metadata and raw data.
      Otherwise click on 'Omit blank subtraction'."
    )
  }

}
