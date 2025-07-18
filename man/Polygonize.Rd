% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Polygonize.R
\name{Polygonize}
\alias{Polygonize}
\title{Create polygons from plot coordinates}
\usage{
Polygonize(df, SRS)
}
\arguments{
\item{df}{A data frame containing plot coordinates and identification labels.}

\item{SRS}{The Spatial Reference System to assign to the resulting polygons.}
}
\value{
A data frame with polygon information, including PLOT_ID, SIZE_HA, POINT_X, and POINT_Y.
}
\description{
This function creates polygons from subplot corner coordinates or irregular plot shapes,
converting from a source SRS to EPSG:4326 / WSG84.
It can handle both rectangular and non-rectangular plots, as well as circular plots.
}
\examples{
\dontrun{
  plot_data <- data.frame(
    id = c(rep("plot1", 4), rep("plot2", 4)),
    POINT_X = c(0, 1, 1, 0, 2, 3, 3, 2),
    POINT_Y = c(0, 0, 1, 1, 2, 2, 3, 3)
  )
  polygons <- Polygonize(plot_data, 4326)
}
}
