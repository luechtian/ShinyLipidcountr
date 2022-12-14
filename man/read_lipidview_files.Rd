% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_1_raw_fct_data-pipeline.R
\name{read_lipidview_files}
\alias{read_lipidview_files}
\title{Read and merge txt.files, which are generated by LipidView}
\usage{
read_lipidview_files(textfiles)
}
\arguments{
\item{textfiles}{string. Path of your textfiles}
}
\value{
tibble. Merged textfiles into one tibble
}
\description{
read_lipidview_files() merges all available txt.files in one
data frame. It combines all columns by the column name.
}
\examples{
list.files(path = system.file("extdata", package = "shinylipidcountr"),
           pattern = ".txt", full.names = TRUE) \%>\%
  read_lipidview_files()
}
