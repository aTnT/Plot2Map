#' Sample plots data (dataframe points)
#'
#' A  dataframe containing sample plots with longitude and latitude point data, Above Ground Biomass in t/ha,
#' average year of plot survey, and plot size in ha (SIZE_HA).
#'
#' @format ## `plots`
#' A data frame with 8321 rows and 6 columns:
#' \describe{
#'   \item{PLOT_ID}{Plot ID}
#'   \item{POINT_X}{Longitude}
#'   \item{POINT_Y}{Latitude}
#'   \item{AGB_T_HA}{Above Ground Biomass (t/ha)}
#'   \item{AVG_YEAR}{Average year of plot survey}
#'   \item{SIZE_HA}{Plot size (ha)}
#' }
"plots"


#' Global Reference Dataset for Above-Ground Biomass (AGBref)
#'
#' This dataset is part of the AGBref initiative, a global reference dataset for above-ground
#' biomass (AGB) validation derived from national forest inventories (NFIs), permanent research
#' plots, and local airborne LiDAR-based maps. It contains harmonized AGB estimates across multiple
#' spatial resolutions and epochs, together with associated uncertainties and ecological metadata.
#' It is primarily intended to support the validation of satellite-derived global biomass maps and
#' national carbon accounting.
#'
#' @format ## `AGBref`
#' A data frame containing global reference AGB measurements:
#' \describe{
#'   \item{POINT_X}{Longitude of the grid cell centroid.}
#'   \item{POINT_Y}{Latitude of the grid cell centroid.}
#'   \item{TC_PLT_SD}{Standard deviation of tree cover (%) at the plot locations within the grid cell, based on remote sensing, is used to assess variability in plot representativeness.}
#'   \item{TC_PLT_MEAN}{Mean tree cover (%) at the plot locations within the grid cell.}
#'   \item{TC_GRID_SD}{Standard deviation of tree cover (%) at the entire grid cell (not just at plot locations).}
#'   \item{TC_GRID_MEAN}{Mean tree cover (%) at the grid cell level, derived from satellite data (e.g., Global Forest Change dataset).}
#'   \item{n}{Number of plots used within the grid cell to compute biomass estimates.}
#'   \item{AGB_T_HA}{Harmonized above-ground biomass (in tons per hectare, T/ha) at the grid cell level. Adjusted for forest area definition, temporal mismatch, and other preprocessing steps.}
#'   \item{SIZE_HA}{Total plot area within the grid cell in hectares.}
#'   \item{OPEN}{Indicator for openness of data.}
#'   \item{VER}{Denotes versioning or verification status. To be updated when new versions are released.}
#'   \item{varTot}{Total variance of AGB within the grid cell. Accounts for the spatial-scale mismatch considering the target grid cell and associated measurement-related (tree measurement, allometric model use) uncertainties.}
#'   \item{AVG_YEAR}{Average year of plot measurement for plots within the grid cell. Useful for matching map epochs (e.g., 2005, 2010, etc.).}
#'   \item{BIO}{Biome category of the grid cell, e.g., "Tropical rainforest", "Temperate broadleaf and mixed forests". Derived from ecological zone datasets.}
#'   \item{CODE}{Source code of the dataset provider. Example: AUS1 for Australia-based NFI. Refer to supplementary table in the manuscript.}
#'   \item{INVENTORY}{Type or scale of data source, e.g., regional, national, or local.}
#' }
#'
#' @note This should be considered a version 0 and will immediately updated with a more
#' comprehensive version alongside peer review process of the data descriptor paper.
#'
#' @source \url{https://zenodo.org/records/15495069}
#'
#' @examples
#' data(AGBref)
#' head(AGBref)
"AGBref"


#' Random Forest model for tree measurement uncertainty prediction
#'
#' @name rf1
#' @docType data
#' @keywords datasets
#' @format A random forest model object for predicting tree measurement uncertainty
#'
#' @description
#' A pre-trained Random Forest model that predicts tree measurement uncertainty
#' based on plot-level characteristics.

#'
#' @details
#' The model takes the following inputs:
#' \describe{
#'   \item{agb}{Above-ground biomass in tons per hectare (AGB_T_HA)}
#'   \item{size}{Plot size in square meters (SIZE_HA converted to m²)}
#'   \item{gez}{Global Ecological Zone as a factor with levels: "Boreal", "Subtropical", "Temperate", "Tropical"}
#' }
#'
#' @examples
#' # Load the model
#' rf1_path <- sample_file("rf1.RData")
#' load(rf1_path)
#' print(rf1)
#'
#' # Format input data
#' plotsPred <- data.frame(
#'   agb = c(150, 200),
#'   size = c(10000, 25000), # in m²
#'   gez = factor(c("Tropical", "Temperate"),
#'                levels = c("Boreal", "Subtropical", "Temperate", "Tropical"))
#' )
#' print(plotsPred)
#'
#' # Predict measurement uncertainty
#' sdTree <- predict(rf1, plotsPred)$predictions
#' print(sdTree)
NULL
