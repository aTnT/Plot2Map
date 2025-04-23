## Updates made to the new framework:
# 20/01/25:
# Minor changes to make code more robust to eventual changes in the BIOMASS uptream package
# 07/04/25:
# Updated documentation


#' Calculate AGB and standard deviations from tree-level data
#'
#' This function calculates plot-level Above Ground Biomass (AGB) and Standard Deviation using tree-level data and plot locations.
#' It uses the BIOMASS package for AGB calculation and standard deviation estimation.
#'
#' @param plot A data frame containing tree-level data. Must include the following columns:
#'   \describe{
#'     \item{id}{Unique identifier for each tree or plot (character or numeric).}
#'     \item{diameter}{Diameter at breast height (DBH) of trees in centimeters (numeric).}
#'     \item{genus}{Genus of the tree species (character).}
#'     \item{species}{Species of the tree (character).}
#'     \item{height}{Tree height in meters (optional, numeric).}
#'   }
#'
#' @param xy A data frame containing plot location data. Must include the following columns:
#'   \describe{
#'     \item{id}{Unique identifier for each plot (character or numeric).}
#'     \item{x}{X-coordinate of the plot location (numeric).}
#'     \item{y}{Y-coordinate of the plot location (numeric).}
#'     \item{size}{Plot size in square meters (numeric).}
#'     \item{year}{Year of measurement or survey (numeric).}
#'   }
#'
#' @inheritParams BIOMASS::getWoodDensity
#'
#' @return A data frame with plot-level AGB estimates and standard deviations, including columns:
#'   \describe{
#'     \item{PLOT_ID}{Unique identifier for each plot.}
#'     \item{POINT_X}{X-coordinate of the plot location.}
#'     \item{POINT_Y}{Y-coordinate of the plot location.}
#'     \item{SIZE_HA}{Plot size in hectares.}
#'     \item{AVG_YEAR}{Average year of measurement across trees in the plot.}
#'     \item{AGB_T_HA}{Above Ground Biomass scaled to tons per hectare.}
#'     \item{sdTree}{Standard deviation of AGB within the plot.}
#'   }
#'
#' @details
#' The function performs the following steps:
#' 1. Filters trees with diameter >= 10cm.
#' 2. Corrects taxonomy and retrieves wood density data.
#' 3. Computes or uses provided tree height data.
#' 4. Runs Monte Carlo simulation for AGB estimation.
#' 5. Calculates plot-level AGB and standard deviation.
#' 6. Scales values per hectare.
#'
#' @import dplyr
#' @importFrom BIOMASS correctTaxo getWoodDensity modelHD retrieveH AGBmonteCarlo
#'
#' @examples
#' plotsTree <- utils::read.csv(sample_file("SampleTree.csv"))
#' head(plotsTree)
#'
#' xyTree <- utils::read.csv(sample_file("SampleTreeXY.csv"))
#' head(xyTree)
#'
#' plot_uncertainties <- MeasurementErr(plotsTree, xyTree, region = "India")
#' head(plot_uncertainties)
#'
#' @export
sd_tree <- function(plot, xy, region = "World") {

  plot <- subset(plot, diameter>=10) #filter those above 10cm in diameter
  #  blowup <- plot[1,5] / 10000
  # print(paste('plot size is', blowup, 'ha'))

  #taxonomy correction
  #tax <- correctTaxo(genus = plot$genus, species = plot$species)
  #plot$genus <- tax$genusCorrected
  #plot$species <- tax$speciesCorrected

  #get wood density
  wd <- BIOMASS::getWoodDensity(genus = plot$genus,
                                species = plot$species,
                                stand = plot$id, region=region)
  plot$wd <- wd$meanWD
  plot$sd.wd<- wd$sdWD

  #compute local HD model / input your own H data if you have
  if("height" %in% colnames(plot)){
    message('Using actual tree height from the provided plot data.')
  }else{
    message('No tree height data found in original plot data. Calculating height using BIOMASS height-diameter model.')
    HDmodel <- modelHD(D = BIOMASS::NouraguesHD$D, H = BIOMASS::NouraguesHD$H,
                       method='weibull',  useWeight = TRUE, drawGraph = FALSE, plot = NULL)
    dataHlocal <- retrieveH(D = plot$diameter, model = HDmodel)
    plot$height <- dataHlocal$H
  }

  #run MC simulation
  if("height" %in% colnames(plot)){
    mc <- by(plot, plot$id,
             function(x) BIOMASS::AGBmonteCarlo(D = x$diameter, WD = x$wd, errWD = x$sd.wd,
                                       H = x$height, errH = x$height, Dpropag ='chave2004'),simplify = F)
  }else{
    mc <- by(plot, plot$id,
             function(x) BIOMASS::AGBmonteCarlo(D = x$diameter, WD = x$wd, errWD = x$sd.wd,
                                       HDmodel = HDmodel, Dpropag = "chave2004"),simplify = F)}

  #get agb and sd
  agb <- unlist(sapply(mc, "[", "meanAGB"))
  sd <- unlist(sapply(mc, "[", "sdAGB"))

  #add XY
  #plot.fin <- left_join(plot, xy, by = c('id' = 'id')) #needs full to avoid gaps
  plot.fin <- dplyr::left_join(plot, unique(xy), by = dplyr::join_by(id))

  #remove unecessaries
  plot.fin <- plot.fin[,c("id","x","y", 'size', 'year')] # retain columns of interest

  #summarize per plot and add key results
  plot.fin$x <- as.numeric(plot.fin$x)
  plot.fin$y <- as.numeric(plot.fin$y)

  plot.fin <- plot.fin |>
    dplyr::group_by(id) |>
    dplyr::summarise_all(mean)

  #scale values per ha
  agb <- agb / (plot.fin$size/10000)
  sd <- sd / (plot.fin$size/10000)
  plot.fin$agb <- agb
  plot.fin$sd <- sd
  plot.fin <- as.data.frame(plot.fin[,c("id","x","y", 'size', 'year', 'agb', 'sd')]) # retain columns of interest
  plot.fin$size <- plot.fin$size / 10000
  names(plot.fin) <- c('PLOT_ID', 'POINT_X', 'POINT_Y', 'SIZE_HA', 'AVG_YEAR',
                       'AGB_T_HA', 'sdTree')
  return(plot.fin)

}





#' Calculate Sampling Error Uncertainty
#'
#' Computes standard error of the mean for plot biomass estimates using standard error propagation methods
#'
#' @param plotAGB Numeric vector of plot-level AGB values
#' @param plotSize Numeric vector of plot sizes in hectares
#' @param method Character specifying error propagation method ("basic" or "bootstrap")
#' @return Numeric vector of sampling error standard deviations
#' @export
samplingUncertainty <- function(plotAGB, plotSize, method = "basic") {
  if(method == "basic") {
    se <- plotAGB / sqrt(plotSize)
  } else {
    stop("Bootstrap method not yet implemented")
  }
  return(se)
}

#' Calculate Growth Model Uncertainty
#'
#' Estimates uncertainty from biomass growth predictions using species-specific growth parameters
#'
#' @param genus Character vector of tree genera
#' @param species Character vector of tree species
#' @param growthParams Data frame containing growth parameters by species
#' @return Numeric vector of growth model standard deviations
#' @export
growthUncertainty <- function(genus, species, growthParams) {
  # Implementation would depend on specific growth model structure
  # Placeholder for demonstration purposes
  rep(NA_real_, length(genus))
}





#' Calculate Total Plot Uncertainty
#'
#' This function computes total uncertainty for forest inventory plots by combining three variance components:
#' 1. Tree-level biomass estimation uncertainty
#' 2. Sampling error uncertainty
#' 3. Growth model uncertainty
#'
#' @param sdTree Numeric vector of standard deviations from tree-level AGB estimates
#' @param sdSE Numeric vector of standard errors from sampling variability
#' @param sdGrowth Numeric vector of standard deviations from growth model predictions
#' @return Numeric vector of total uncertainty values
#' @export
totalUncertainty <- function(sdTree, sdSE, sdGrowth) {
  sqrt(sdTree^2 + sdSE^2 + sdGrowth^2)
}



