% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{test_raw_data}
\alias{test_raw_data}
\title{Tibble with merged lipidomics data from LipidView and LipidXplorer}
\format{
A data frame with 9546 rows and 5 variables:
\describe{
  \item{class}{lipid class abbreviation}
  \item{species}{lipid species, IS x:y;z, IS = internal standard, x = c-atoms, y = double bonds, z = hydroxilation grade}
  \item{sample}{sample name/number according to sample entry sheet}
  \item{intensity}{peak intensity, in cps}
}
}
\source{
system.file("extdata", package = "shinylipidcountr")
}
\usage{
test_raw_data
}
\description{
A dataset containing intensities and species of several lipid classes.
}
\keyword{datasets}
