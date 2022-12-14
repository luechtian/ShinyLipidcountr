% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_1_raw_fct_data-pipeline.R
\name{read_lipidxplorer_files}
\alias{read_lipidxplorer_files}
\title{Read csv-files, which is generated by LipidXplorer}
\usage{
read_lipidxplorer_files(path)
}
\arguments{
\item{path}{string. Path of your csv-file}
}
\value{
tibble.
}
\description{
Read csv-files, which is generated by LipidXplorer
}
\examples{
system.file("extdata", "lx-out.csv", package = "shinylipidcountr") \%>\%
  read_lipidxplorer_files()
}
