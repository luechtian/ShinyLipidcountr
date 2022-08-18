#' Vector with lipid class abbreviations
#'
#' A \code{vector} containing lipid classes that is for parsing
#' titles of LipidView text files. This vector will be used by function
#'  \code{name_lipid_file()}.
#'
#' @format A \code{vector} with 59 elements
"lipid_classes"


#' Tibble with merged lipidomics data from LipidView and LipidXplorer
#'
#' A dataset containing intensities and species of several lipid classes.
#'
#' @format A data frame with 9546 rows and 5 variables:
#' \describe{
#'   \item{class}{lipid class abbreviation}
#'   \item{species}{lipid species, IS x:y;z, IS = internal standard, x = c-atoms, y = double bonds, z = hydroxilation grade}
#'   \item{scan_name}{precursor ion scan, if Qtrap data or molecular species, if QE data}
#'   \item{sample}{sample name/number according to sample entry sheet}
#'   \item{intensity}{peak intensity, in cps}
#' }
#' @source system.file("extdata", package = "shinylipidcountr")
"test_raw_data"

#' Tibble with lipidomics data from LipidView
#'
#' A dataset containing intensities and species of several lipid classes.
#'
#' @format A data frame with 6867 rows and 5 variables:
#' \describe{
#'   \item{class}{lipid class abbreviation}
#'   \item{species}{lipid species, IS x:y;z, IS = internal standard, x = c-atoms, y = double bonds, z = hydroxilation grade}
#'   \item{scan_name}{precursor ion scan}
#'   \item{sample}{sample name/number according to sample entry sheet}
#'   \item{intensity}{peak intensity, in cps}
#' }
#' @source system.file("extdata", package = "shinylipidcountr")
"test_raw_data_lv"

#' Tibble with lipidomics data from LipidXplorer
#'
#' A dataset containing intensities and species of several lipid classes.
#'
#' @format A data frame with 10298 rows and 8 variables:
#' \describe{
#'   \item{class}{lipid class abbreviation}
#'   \item{species}{lipid species, IS x:y;z, IS = internal standard, x = c-atoms, y = double bonds, z = hydroxilation grade}
#'   \item{molecular_species}{molecular species, IS x1:y1_x2:y2, x1:y1 = fatty acid1, x2:y2 = fatty acid2}
#'   \item{lipid_category}{lipid category, IS = Internal standard, PC = Glycerophospholipid, SM = Sphingolipid, TG = Glycerolipid}
#'   \item{mass}{exact mass of lipid, in Da}
#'   \item{sample}{sample name/number according to sample entry sheet}
#'   \item{intensity}{peak intensity of precursor, in cps}
#'   \item{fa_intensity}{sum intensity of fatty acid fragments, in cps}
#' }
#' @source system.file("extdata", package = "shinylipidcountr")
"test_raw_data_lx"

#' Data frame with experimental data from lipid extraction
#'
#' A dataset containing internal standards with concentration per sample used for extraction
#'
#' @format A data frame with 18 rows and 23 variables:
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
