#' Vector with Excelsheet names
#'
#' A \code{vector} containing strings to set the order of sheets in the
#' Excelworkbook.
#'
#' @format A \code{vector} with 71 elements
"lipid_class_order"


#' Tibble with merged lipidomics data from LipidView and LipidXplorer
#'
#' A dataset containing intensities and species of several lipid classes.
#'
#' @format A data frame with 9546 rows and 5 variables:
#' \describe{
#'   \item{class}{lipid class abbreviation}
#'   \item{species}{lipid species, IS x:y;z, IS = internal standard, x = c-atoms, y = double bonds, z = hydroxilation grade}
#'   \item{sample}{sample name/number according to sample entry sheet}
#'   \item{intensity}{peak intensity, in cps}
#' }
#' @source system.file("extdata", package = "shinylipidcountr")
"test_raw_data"

#' Data frame with experimental data from lipid extraction
#'
#' A dataset containing internal standards with concentration per sample used for extraction
#'
#' @format A data frame with 21 rows and 23 variables:
#' \describe{
#'   \item{class}{lipid class abbreviation for the internal standards}
#'   \item{rf_values}{response factors}
#'   \item{Sample 1, etc}{contains experimental data like extractions inputs}
#'   \item{row 'group'}{sample name to summarise replicates}
#'   \item{row 'blank'}{blank assignment for blank extraction, if multiple blanks were used}
#'   \item{row 'sample input'}{sample volume used for extraction, in microliter}
#'   \item{intensity}{peak intensity of precursor, in cps}
#'   \item{PC, SM, etc}{concentration of internal standards used for extraction, in picomoles}
#' }
#' @source system.file("extdata", package = "shinylipidcountr")
"test_metadata"
