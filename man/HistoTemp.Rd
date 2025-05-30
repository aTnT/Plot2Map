% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TempVis.R
\name{HistoTemp}
\alias{HistoTemp}
\title{Visualize histogram of temporal fix effect}
\usage{
HistoTemp(df, year, outDir = "results")
}
\arguments{
\item{df}{A data frame containing AGB data.}

\item{year}{Numeric value indicating the year of analysis.}

\item{outDir}{Character string specifying the output directory for saving the histogram (default: "results").}
}
\value{
Invisibly returns NULL. The function creates a plot as a side effect.
}
\description{
This function creates a histogram to visualize the effect of temporal adjustment
or forest fraction correction on AGB values. It generates a plot comparing the
distribution of AGB values before and after the adjustment.
}
\examples{
set.seed(42)
sample_plots <- plots[sample(nrow(plots), 10), ]
sample_plots <- BiomePair(sample_plots)
sample_plots <- TempApply(sample_plots, 2004)
head(sample_plots)
HistoTemp(sample_plots, 2004)
}
