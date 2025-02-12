% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BlockMeans.R
\name{sampleAGBmap}
\alias{sampleAGBmap}
\title{Sample block mean mapped AGB over a region of interest}
\usage{
sampleAGBmap(
  roi,
  weighted_mean = FALSE,
  agb_raster = NULL,
  esacci_biomass_year = "latest",
  esacci_biomass_version = "latest",
  esacci_folder = "data/ESACCI-BIOMASS",
  n_cores = 1,
  timeout = 600
)
}
\arguments{
\item{roi}{An sf or SpatVector object representing the Region of Interest.}

\item{weighted_mean}{Logical, if TRUE the weighted mean is calculated considering the
approximate fraction of each cell that is covered by the polygon (default is FALSE).}

\item{agb_raster}{A SpatRaster object with the custom AGB map. If NULL, ESA CCI BIOMASS AGB tiles will be downloaded and used.
This dataset comprises estimates of forest above-ground biomass for the years 2010, 2015, 2016, 2017, 2018, 2019,
2020 and 2021. They are derived from a combination of Earth observation data, depending on the year, from the Copernicus
Sentinel-1 mission, Envisat’s ASAR (Advanced Synthetic Aperture Radar) instrument and JAXA’s (Japan Aerospace Exploration
Agency) Advanced Land Observing Satellite (ALOS-1 and ALOS-2), along with additional information from Earth observation
sources. The data has been produced as part of the European Space Agency's (ESA's) Climate Change Initiative (CCI)
programme by the Biomass CCI team.}

\item{esacci_biomass_year}{The ESA CCI BIOMASS AGB tiles year to use. Use either 2010, 2015, 2016, 2017, 2018, 2019,
2020, 2021 or "latest" (default).}

\item{esacci_biomass_version}{The ESA CCI BIOMASS AGB tiles version to use. Use either "v2.0", "v3.0", "v4.0",
"v5.0", "v5.01" or "latest" (default).}

\item{esacci_folder}{Directory to save downloaded ESA CCI BIOMASS AGB files. Default is the relative path "data/ESACCI-BIOMASS".}

\item{n_cores}{Number of cores to use for parallel download.}

\item{timeout}{Number of seconds for reaching file download timeout.}
}
\value{
Numeric value representing the mean AGB for the polygon.
}
\description{
This function samples the block mean mapped Above Ground Biomass (AGB) over a given polygon.
It can use either a custom AGB map provided as input or download and use ESA CCI BIOMASS AGB tiles.
}
\examples{
# Load required libraries
library(sf)

# Define a region of interest (ROI) in the Congo Rainforest
roi_congo <- st_polygon(list(rbind(
  c(25.0089, 0.4735), c(25.0189, 0.4735),
  c(25.0189, 0.4835), c(25.0089, 0.4835),
  c(25.0089, 0.4735)
)))
roi_sf_congo <- st_sfc(roi_congo, crs = 4326)

# Example 1: Calculate mean AGB for the Congo ROI (unweighted)
sampleAGBmap(roi_sf_congo)

# Example 2: Calculate mean AGB for the Congo ROI (weighted)
sampleAGBmap(roi_sf_congo, weighted_mean = TRUE)

}
\references{
\href{https://dx.doi.org/10.5285/bf535053562141c6bb7ad831f5998d77}{Santoro, M.; Cartus, O. (2024): ESA Biomass Climate Change Initiative (Biomass_cci): Global datasets of forest above-ground biomass for the years 2010, 2015, 2016, 2017, 2018, 2019, 2020 and 2021, v5.01. NERC EDS Centre for Environmental Data Analysis, 22 August 2024.}
}
