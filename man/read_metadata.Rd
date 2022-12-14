% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_2_meta_fct_data-pipeline.R
\name{read_metadata}
\alias{read_metadata}
\title{Read metadata csv.file}
\usage{
read_metadata(path, pattern = "metadata", clean_samples = FALSE)
}
\arguments{
\item{path}{string. Path of your textfiles}

\item{pattern}{string. Identifies the csv.file containing the metadata}

\item{clean_samples}{TRUE activates janitor::clean_names() for samples}
}
\value{
data.frame
}
\description{
Read csv.file, which contains metadata from extractions and
samples, e.g. sample volume in ul, standard ipnut in pmol, response factors
and sample groups.
}
\examples{
system.file("extdata", package = "shinylipidcountr") \%>\%
  read_metadata()
}
