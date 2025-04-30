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
