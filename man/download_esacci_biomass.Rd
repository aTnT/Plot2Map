% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ESACCIBiomass.R
\name{download_esacci_biomass}
\alias{download_esacci_biomass}
\title{Download ESA CCI biomass GeoTIFF data}
\usage{
download_esacci_biomass(
  esacci_biomass_year = "latest",
  esacci_biomass_version = "latest",
  esacci_folder = "data/ESACCI-BIOMASS",
  n_cores = parallel::detectCores() - 1,
  timeout = 600,
  file_names = NULL
)
}
\arguments{
\item{esacci_biomass_year}{The ESA CCI BIOMASS AGB tiles year to use. Use either 2010, 2015, 2016, 2017, 2018, 2019,
2020, 2021, 2022 or "latest" (default).}

\item{esacci_biomass_version}{The ESA CCI BIOMASS AGB tiles version to use. Use either "v2.0", "v3.0", "v4.0",
"v5.0", "v5.01", "v6.0" or "latest" (default).}

\item{esacci_folder}{Directory to save downloaded ESA CCI BIOMASS AGB files. Default is the relative path "data/ESACCI-BIOMASS".}

\item{n_cores}{Number of cores to use for parallel download.}

\item{timeout}{Number of seconds for reaching file download timeout.}

\item{file_names}{Character vector of specific filenames to download. If NULL (default), all files will be downloaded.}
}
\value{
A character vector of downloaded file paths.
}
\description{
This function downloads ESA CCI Biomass GeoTIFF data from the CEDA Archive.
}
\references{
\href{https://dx.doi.org/10.5285/95913ffb6467447ca72c4e9d8cf30501}{Santoro, M.; Cartus, O. (2025): ESA Biomass Climate Change Initiative (Biomass_cci): Global datasets of forest above-ground biomass for the years 2007, 2010, 2015, 2016, 2017, 2018, 2019, 2020, 2021 and 2022, v6.0. NERC EDS Centre for Environmental Data Analysis, 17 April 2025. doi:10.5285/95913ffb6467447ca72c4e9d8cf30501.}
}
