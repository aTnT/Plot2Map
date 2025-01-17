## Updates made to the new framework:
# 06/01/2025:
# Replaced filter() and left_join() with dplyr::filter() and dplyr::left_join() to avoid potential namespace conflicts.
# Improved error handling and messages.


## Notes:



#' Apply temporal adjustment to plot biomass
#'
#' This function adjusts plot biomass values to align with the map year by adding or subtracting
#' annual growth increment to older or newer plots. It uses growth data from a model-based
#' estimate of growth-age relationships.
#'
#' @param df A data frame containing plot data.
#' @param domain Character string specifying the ecological domain of interest.
#' @param year Numeric value indicating the map AGB year (e.g., 2010 for GlobBiomass).
#'
#' @return A data frame with adjusted AGB values.
#'
#' @importFrom dplyr filter left_join
#' @importFrom utils read.csv
#'
#' @export
TempApply <- function(df, domain, year) {
  gr <- read.csv(file.path("data", "GR_Uniques.csv"))
  gr$GEZ <- as.character(gr$GEZ)
  gr$ZONE <- as.character(gr$ZONE)
  gr$FAO.ecozone <- as.character(gr$FAO.ecozone)

  df$AVG_YEAR <- as.numeric(df$AVG_YEAR)

  # Filter eco-region first
  if (domain %in% unique(df$GEZ)) {
    df0 <- dplyr::filter(df, GEZ == domain)

    # String check
    df0$ZONE <- ifelse(df0$ZONE == 'Northern America', 'N.America', df0$ZONE)
    df0$ZONE <- ifelse(df0$ZONE == 'Southern America', 'S.America', df0$ZONE)
    df0$ZONE <- ifelse(df0$ZONE == 'Central America', 'C.America', df0$ZONE)

    # Join growth rate table using 3 variables to assure uniqueness
    df.old <- dplyr::left_join(df0, gr, by = c('GEZ', 'ZONE', 'FAO.ecozone'))

    # Filter above and below map year (i.e. 2010 for GlobBiomass), keep no changes to map year
    below <- subset(df.old, AVG_YEAR < year) #non-NAs
    above <- subset(df.old, AVG_YEAR > year) #non-NAs
    static <- subset(df.old, is.na(AVG_YEAR) | AVG_YEAR == year) #NAs AVG_YEAR OR 2010 subsets

    # Apply growth rates (GR1 = primary, GR2 = old secondary, GR3 = young secondary)
    below$AGB_ORIG <- below$AGB_T_HA
    above$AGB_ORIG <- above$AGB_T_HA
    below$AGB_T_HA <- below$AGB_T_HA + (ifelse(below$AGB_T_HA < 100, below$GR3, below$GR2) * (year - below$AVG_YEAR))
    above$AGB_T_HA <- above$AGB_T_HA - (ifelse(above$AGB_T_HA < 100, above$GR3, above$GR2) * (above$AVG_YEAR - year))

    below$AGB_T_HA <- ifelse(below$AGB_T_HA > 152,
                             below$AGB_ORIG + (below$GR1 * (year - below$AVG_YEAR)), below$AGB_T_HA)

    above$AGB_T_HA <- ifelse(above$AGB_T_HA > 152,
                             above$AGB_ORIG - (above$GR1 * (above$AVG_YEAR - year)), above$AGB_T_HA) #retain if not in primary/GR3 class

    above$AGB_T_HA <- ifelse(above$AGB_T_HA < 0,
                             above$AGB_ORIG, above$AGB_T_HA) #retain original if it gets negative

    # Combine all: static and recomputed
    static$AGB_ORIG <- static$AGB_T_HA

    df.new <- rbind(below,above,static)
    df.new$AGB_T_HA_ORIG <- df.new$AGB_ORIG

    # Checker of rows
    if (sum(nrow(df.old)) == sum(nrow(df.new))) {
      message('Growth rates applied correspondingly per eco-region.')
    } else {
      stop('Something is wrong, nr of dataframe rows different before/after applying growth rates.')
    }

    #remove last joined growth rates columns for further row binding
    remove <- c('GR1', 'GR2', 'GR3', 'AGB_ORIG')
    df.new <- df.new[, !(names(df.new) %in% remove)]

    df.new$AGB_T_HA <- ifelse(is.na(df.new$AGB_T_HA), df$AGB_T_HA, df.new$AGB_T_HA)
    return(df.new)
  } #this part doesn't give NAs already
}


#' Calculate temporal variance in plot biomass
#'
#' This function calculates the temporal variance in plot biomass based on growth rate
#' standard deviations. It adjusts the AGB values and computes the standard deviation
#' of growth for each plot.
#'
#' @param df A data frame containing plot data.
#' @param domain Character string specifying the ecological domain of interest.
#' @param year Numeric value indicating the map AGB year (e.g., 2010 for GlobBiomass).
#'
#' @return A data frame with adjusted AGB values and calculated growth standard deviations.
#'
#' @importFrom dplyr filter left_join
#' @importFrom utils read.csv
#'
#' @export
TempVar <- function(df, domain, year) {
  gr <- read.csv(file.path("data", "GR_SD.csv"))
  gr$GEZ <- as.character(gr$GEZ)
  gr$ZONE <- as.character(gr$ZONE)
  gr$FAO.ecozone <- as.character(gr$FAO.ecozone)

  # Filter eco-region first
  if (domain %in% unique(df$GEZ)) {
    df0 <- dplyr::filter(df, GEZ == domain)
    df0$ZONE <- ifelse(df0$ZONE == 'Northern America', 'N.America', df0$ZONE)
    df0$ZONE <- ifelse(df0$ZONE == 'Southern America', 'S.America', df0$ZONE)
    df0$ZONE <- ifelse(df0$ZONE == 'Central America', 'C.America', df0$ZONE)

    # Join growth rate table using 3 variables to assure uniqueness
    df.old <- dplyr::left_join(df0, gr, by = c('GEZ', 'ZONE', 'FAO.ecozone'))

    # Filter above and below map year (i.e. 2010 for GlobBiomass), keep no changes to map year
    below <- subset(df.old, AVG_YEAR < year) #non-NAs
    above <- subset(df.old, AVG_YEAR > year) #non-NAs
    static <- subset(df.old, is.na(AVG_YEAR) | AVG_YEAR == year) #NAs AVG_YEAR OR 2010 subsets

    # Apply growth rates (SD1 = primary, SD2 = old secondary, SD3 = young secondary)
    below$AGB_T_HA <- below$AGB_T_HA_ORIG + (ifelse(below$AGB_T_HA_ORIG < 100, below$SD3, below$SD2) * (year - below$AVG_YEAR))
    above$AGB_T_HA <- above$AGB_T_HA_ORIG - (ifelse(above$AGB_T_HA_ORIG < 100, above$SD3, above$SD2) * (above$AVG_YEAR - year))

    below$AGB_T_HA <- ifelse(below$AGB_T_HA > 152,
                             below$AGB_T_HA_ORIG + (below$SD1 * (year - below$AVG_YEAR)), below$AGB_T_HA)

    above$AGB_T_HA <- ifelse(above$AGB_T_HA > 152,
                             above$AGB_T_HA_ORIG - (below$SD1 * (above$AVG_YEAR - year)), above$AGB_T_HA)

    above$AGB_T_HA <- ifelse(above$AGB_T_HA < 0,
                             above$AGB_T_HA_ORIG, above$AGB_T_HA)

    #combine all: static and recomputed
    df.new <- rbind(below, above, static)

    # Checker of rows
    if (sum(nrow(df.old)) == sum(nrow(df.new))) {
      message('Growth rates applied correspondingly per eco-region.')
    } else {
      stop('Something is wrong, nr of dataframe rows different before/after applying growth rates.')
    }

    # Retain last joined growth rates columns for further row binding
    df.new$sdGrowth <- abs(df.new$AGB_T_HA - df.new$AGB_T_HA_ORIG)
    retain <- c(names(df0), 'sdGrowth')
    df.new <- df.new[, (names(df.new) %in% retain)]
    df.new$AGB_T_HA <- df.old$AGB_T_HA
    df.new$sdGrowth <- ifelse(is.na(df.new$sdGrowth), mean(df.new$sdGrowth, na.rm = TRUE), df.new$sdGrowth)
    df.new$sdGrowth <- ifelse(is.nan(df.new$sdGrowth), mean(df.new$sdGrowth, na.rm = TRUE), df.new$sdGrowth)
    return(df.new)
  }
}



#
# old_TempApply <- function(df, domain, year){
#   dataDir <- "data"
#   gr <- read.csv(paste0(dataDir,'/GR_Uniques.csv'))
#   gr$GEZ <- as.character(gr$GEZ)
#   gr$ZONE <- as.character(gr$ZONE)
#   gr$FAO.ecozone <- as.character(gr$FAO.ecozone)
#
#   df$AVG_YEAR <- as.numeric(df$AVG_YEAR)
#
#   #filter +- 10 years older or newer plots to the map year
#   #df <- subset(df, df$AVG_YEAR < year+11
#   #        & df$AVG_YEAR > year-11)
#
#
#   #filter eco-region first
#   if (domain %in% unique(df$GEZ)==T){
#     df0 <- dplyr::filter (df, GEZ == domain)
#
#     #string check
#     df0$ZONE <- ifelse(df0$ZONE == 'Northern America', 'N.America', df0$ZONE)
#     df0$ZONE <- ifelse(df0$ZONE == 'Southern America', 'S.America', df0$ZONE)
#     df0$ZONE <- ifelse(df0$ZONE == 'Central America', 'C.America', df0$ZONE)
#
#     #join growth rate table using 3 variables to assure uniqueness
#     df.old <- dplyr::left_join(df0, gr, by = c('GEZ'='GEZ', 'ZONE'='ZONE', 'FAO.ecozone'='FAO.ecozone'))
#
#     #filter above and below map year (i.e. 2010 for GlobBiomass), keep no changes to map year
#     below <- subset(df.old, AVG_YEAR < year) #non-NAs
#     above <- subset(df.old, AVG_YEAR > year) #non-NAs
#     static <- subset(df.old, is.na(AVG_YEAR) | AVG_YEAR == year) #NAs AVG_YEAR OR 2010 subsets
#
#
#     #apply growth rates (GR1 = primary, GR2 = old secondary, GR3 = young secondary)
#     below$AGB_ORIG <- below$AGB_T_HA
#     above$AGB_ORIG <- above$AGB_T_HA
#     below$AGB_T_HA <- below$AGB_T_HA + (ifelse(below$AGB_T_HA < 100, below$GR3, below$GR2) * (year - below$AVG_YEAR))
#     above$AGB_T_HA <- above$AGB_T_HA - (ifelse(above$AGB_T_HA < 100, above$GR3, above$GR2) * (above$AVG_YEAR -  year))
#
#     below$AGB_T_HA <- ifelse(below$AGB_T_HA > 152,
#                              below$AGB_ORIG + (below$GR1 * (year - below$AVG_YEAR)), below$AGB_T_HA)
#
#     above$AGB_T_HA <- ifelse(above$AGB_T_HA > 152,
#                              above$AGB_ORIG - (above$GR1 * (above$AVG_YEAR - year)), above$AGB_T_HA) #retain if not in primary/GR3 class
#
#     above$AGB_T_HA <- ifelse(above$AGB_T_HA < 0,
#                              above$AGB_ORIG, above$AGB_T_HA) #retain original if it gets negative
#
#
#     #combine all: static and recomputed
#     static$AGB_ORIG <- static$AGB_T_HA
#
#     df.new <- rbind(below,above,static)
#     df.new$AGB_T_HA_ORIG <- df.new$AGB_ORIG
#
#     #checker of rows
#     if (sum(nrow(df.old)) == sum(nrow(df.new))) {
#       print('growth rates applied correspondingly per eco-region!')
#     } else {
#       print('something is wrong..row sums not equal..')
#     }
#
#
#     #remove last joined growth rates columns for further row binding
#     remove <- c('GR1', 'GR2', 'GR3', 'AGB_ORIG')
#     df.new <- df.new[ , !(names(df.new) %in% remove)]
#
#     df.new$AGB_T_HA <- ifelse(is.na(df.new$AGB_T_HA),df$AGB_T_HA, df.new$AGB_T_HA)
#     return(df.new)
#   } #this part doesn't give NAs already
#
# }
#
#
#
#
# old_TempVar <- function(df, domain, year){
#   dataDir <- "data"
#   gr <- read.csv(paste0(dataDir,'/GR_SD.csv'))
#   gr$GEZ <- as.character(gr$GEZ)
#   gr$ZONE <- as.character(gr$ZONE)
#   gr$FAO.ecozone <- as.character(gr$FAO.ecozone)
#
#   #filter eco-region first
#   if (domain %in% unique(df$GEZ)==T){
#     df0 <- dplyr::filter(df, GEZ == domain) #this part doesn't give NAs already
#     df0$ZONE <- ifelse(df0$ZONE == 'Northern America', 'N.America', df0$ZONE)
#     df0$ZONE <- ifelse(df0$ZONE == 'Southern America', 'S.America', df0$ZONE)
#     df0$ZONE <- ifelse(df0$ZONE == 'Central America', 'C.America', df0$ZONE)
#
#     #join growth rate table using 3 variables to assure uniqueness
#     df.old <- dplyr::left_join(df0, gr, by = c('GEZ'='GEZ', 'ZONE'='ZONE', 'FAO.ecozone'='FAO.ecozone'))
#
#     #filter above and below map year (i.e. 2010 for GlobBiomass), keep no changes to map year
#     below <- subset(df.old, AVG_YEAR < year) #non-NAs
#     above <- subset(df.old, AVG_YEAR > year) #non-NAs
#     static <- subset(df.old, is.na(AVG_YEAR) | AVG_YEAR == year) #NAs AVG_YEAR OR 2010 subsets
#
#
#     #apply growth rates (SD1 = primary, SD2 = old secondary, SD3 = young secondary)
#
#     below$AGB_T_HA <- below$AGB_T_HA_ORIG + (ifelse(below$AGB_T_HA_ORIG < 100, below$SD3, below$SD2) * (year - below$AVG_YEAR))
#     above$AGB_T_HA <- above$AGB_T_HA_ORIG - (ifelse(above$AGB_T_HA_ORIG < 100, above$SD3, above$SD2) * (above$AVG_YEAR -  year))
#
#     below$AGB_T_HA <- ifelse(below$AGB_T_HA > 152,
#                              below$AGB_T_HA_ORIG + (below$SD1 * (year - below$AVG_YEAR)), below$AGB_T_HA)
#
#     above$AGB_T_HA <- ifelse(above$AGB_T_HA > 152,
#                              above$AGB_T_HA_ORIG - (above$SD1 * (above$AVG_YEAR - year)), above$AGB_T_HA) #retain if not in primary/SD3 class
#
#     above$AGB_T_HA <- ifelse(above$AGB_T_HA < 0,
#                              above$AGB_T_HA_ORIG, above$AGB_T_HA) #retain original if it gets negative
#
#
#     #combine all: static and recomputed
#     df.new <- rbind(below,above,static)
#
#
#     #checker of rows
#     if (sum(nrow(df.old)) == sum(nrow(df.new))) {
#       print('growth rates applied correspondingly per eco-region!')
#     } else {
#       print('something is wrong..row sums not equal..')
#     }
#
#     #retain last joined growth rates columns for further row binding
#     df.new$sdGrowth <-  abs(df.new$AGB_T_HA - df.new$AGB_T_HA_ORIG)
#     retain <- c(names(df0), 'sdGrowth')
#     df.new <- df.new[ , (names(df.new) %in% retain)]
#     df.new$AGB_T_HA <-  df.old$AGB_T_HA
#     df.new$sdGrowth <- ifelse(is.na(df.new$sdGrowth), mean(df.new$sdGrowth,na.rm=T),df.new$sdGrowth)
#     df.new$sdGrowth <- ifelse(is.nan(df.new$sdGrowth), mean(df.new$sdGrowth,na.rm=T),df.new$sdGrowth)
#     df.new}
# }


# # Tests:
# library(testthat)
#
# # Test comparison between old and new versions
# test_that("Old and new TempApply functions produce consistent results", {
#   # Create sample data
#   test_data <- data.frame(
#     GEZ = c("Tropical", "Tropical", "Temperate"),
#     ZONE = c("Africa", "S.America", "Europe"),
#     FAO.ecozone = c("Tropical rainforest", "Tropical moist forest", "Temperate oceanic forest"),
#     AVG_YEAR = c(2005, 2015, 2010),
#     AGB_T_HA = c(200, 150, 100)
#   )
#
#   # Run both versions
#   old_result <- old_TempApply(test_data, "Tropical", 2010)
#   new_result <- TempApply(test_data, "Tropical", 2010)
#
#   # Compare results
#   expect_equal(old_result$AGB_T_HA, new_result$AGB_T_HA, tolerance = 1e-6)
#   expect_equal(old_result$AGB_T_HA_ORIG, new_result$AGB_T_HA_ORIG, tolerance = 1e-6)
# })
#
# test_that("Old and new TempVar functions produce consistent results", {
#   # Create sample data
#   test_data <- data.frame(
#     GEZ = c("Tropical", "Tropical", "Temperate"),
#     ZONE = c("Africa", "S.America", "Europe"),
#     FAO.ecozone = c("Tropical rainforest", "Tropical moist forest", "Temperate oceanic forest"),
#     AVG_YEAR = c(2005, 2015, 2010),
#     AGB_T_HA = c(200, 150, 100),
#     AGB_T_HA_ORIG = c(190, 160, 100)
#   )
#
#   # Run both versions
#   old_result <- old_TempVar(test_data, "Tropical", 2010)
#   new_result <- TempVar(test_data, "Tropical", 2010)
#
#   # Compare results
#   expect_equal(old_result$AGB_T_HA, new_result$AGB_T_HA, tolerance = 1e-6)
#   expect_equal(old_result$sdGrowth, new_result$sdGrowth, tolerance = 1e-6)
# })
#
# # Test internal consistency
# test_that("TempApply function behaves consistently", {
#   test_data <- data.frame(
#     GEZ = c("Tropical", "Tropical", "Temperate"),
#     ZONE = c("Africa", "S.America", "Europe"),
#     FAO.ecozone = c("Tropical rainforest", "Tropical moist forest", "Temperate oceanic forest"),
#     AVG_YEAR = c(2005, 2015, 2010),
#     AGB_T_HA = c(200, 150, 100)
#   )
#
#   result <- TempApply(test_data, "Tropical", 2010)
#
#   # Check output structure
#   expect_s3_class(result, "data.frame")
#   expect_true(all(c("AGB_T_HA", "AGB_T_HA_ORIG") %in% names(result)))
#
#   # Check for expected adjustments
#   expect_true(all(result$AGB_T_HA[result$AVG_YEAR < 2010] >= result$AGB_T_HA_ORIG[result$AVG_YEAR < 2010]))
#   expect_true(all(result$AGB_T_HA[result$AVG_YEAR > 2010] <= result$AGB_T_HA_ORIG[result$AVG_YEAR > 2010]))
# })
#
# test_that("TempVar function behaves consistently", {
#   test_data <- data.frame(
#     GEZ = c("Tropical", "Tropical", "Temperate"),
#     ZONE = c("Africa", "S.America", "Europe"),
#     FAO.ecozone = c("Tropical rainforest", "Tropical moist forest", "Temperate oceanic forest"),
#     AVG_YEAR = c(2005, 2015, 2010),
#     AGB_T_HA = c(200, 150, 100),
#     AGB_T_HA_ORIG = c(190, 160, 100)
#   )
#
#   result <- TempVar(test_data, "Tropical", 2010)
#
#   # Check output structure
#   expect_s3_class(result, "data.frame")
#   expect_true(all(c("AGB_T_HA", "sdGrowth") %in% names(result)))
#
#   # Check for expected calculations
#   expect_true(all(result$sdGrowth >= 0))
#   expect_false(any(is.na(result$sdGrowth)))
# })


