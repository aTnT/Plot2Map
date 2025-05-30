% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/example.R
\name{sample_file}
\alias{sample_file}
\title{Get path to example sample file}
\usage{
sample_file(file = NULL)
}
\arguments{
\item{file}{Name of file. If \code{NULL}, all sample files will be listed.}
}
\description{
This package comes bundled with a number of sample files in its \code{inst/extdata}
directory. This function make them easy to access
}
\examples{
\dontrun{
sample_file()
sample_file("SampleUnformattedPlots.csv")
}
}
