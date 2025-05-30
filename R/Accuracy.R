## Updates made to the new framework:
# 07/03/2025:
# Replaced plyr functions with dplyr equivalents.
# Removed redundant operations and simplified the code structure where possible.
# 05/05/2025:
# Made function more flexible to detect and use any column with "plotAGB_" prefix
# Added dynamic column detection and robust error handling for missing columns
# Updated examples to ensure they work with the improved flexibility

#' Calculate accuracy metrics for AGB estimates
#'
#' This function calculates accuracy metrics for Above Ground Biomass (AGB) estimates
#' from plot data and map data, grouped into specified intervals. It computes means,
#' root mean squared differences (RMSD), variances, and other statistics per AGB bin
#' and overall, saving the results to a CSV file.
#'
#' @param df Dataframe containing plot and map AGB data. Defaults to \code{plotsBACC}.
#' @param intervals Number of intervals for binning AGB values. Must be 6, 7, or 8. Defaults to 8.
#' @param dir Directory where the results will be saved. Defaults to \code{resultsFolder}.
#' @param str String to append to the output CSV file name. Defaults to an empty string.
#'
#' @return A dataframe with accuracy metrics for each AGB bin and a total row.
#'
#' @importFrom dplyr filter select group_by summarize mutate inner_join arrange bind_rows
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom rlang sym
#'
#' @export
#' @examples
#' \donttest{
#' # Example with the right column structure
#' # (Output of invDasymetry would include plotAGB_10 column)
#' example_agb_data <- data.frame(
#'   plotAGB_10 = c(120, 150, 180, 200, 220),
#'   mapAGB = c(110, 140, 190, 180, 240),
#'   SIZE_HA = c(0.5, 0.75, 1.0, 1.2, 0.9),
#'   x = c(1, 2, 3, 4, 5),
#'   y = c(10, 20, 30, 40, 50)
#' )
#'
#' # Run accuracy assessment
#' results <- Accuracy(df = example_agb_data, intervals = 6)
#' print(results)
#' }
Accuracy <- function(df, intervals = 8, dir = "results", str = '') {
  df$sdMap <- 1
  df$sdPlot <- 1

  # Assign AGB bins
  if (intervals == 8) {
    bins <- c(-Inf, 50, 100, 150, 200, 250, 300, 400, Inf)
    bins.str <- c('0-50', '50-100', '100-150', '150-200', '200-250', '250-300', '300-400', '>400')
  } else if (intervals == 7) {
    bins <- c(-1, 50, 100, 150, 200, 250, 300, Inf)
    bins.str <- c('0-50', '50-100', '100-150', '150-200', '200-250', '250-300', '>300')
  } else if (intervals == 6) {
    bins <- c(-1, 100, 150, 200, 250, 300, Inf)
    bins.str <- c('0-100', '100-150', '150-200', '200-250', '250-300', '>300')
  }

  # Find column with plotAGB_ prefix
  plotAGB_col <- grep("^plotAGB_", names(df), value = TRUE)[1]
  if (is.na(plotAGB_col)) {
    stop("No column with prefix 'plotAGB_' found in the dataframe")
  }

  # Assign grouping of AGB values for plot and map separately per bin
  grp1 <- df |>
    dplyr::mutate(group = cut(!!rlang::sym(plotAGB_col), breaks = bins))

  # Aggregate the mean AGB of bins
  agg.plot <- grp1 |>
    dplyr::group_by(group) |>
    dplyr::summarise(!!rlang::sym("plotAGB") := mean(!!rlang::sym(plotAGB_col)), .groups = 'drop')

  agg.map <- grp1 |>
    dplyr::group_by(group) |>
    dplyr::summarise(mapAGB = mean(mapAGB, na.rm = TRUE), .groups = 'drop')

  # Calculate accuracy metrics - select dynamically
  cols_to_keep <- c(plotAGB_col, 'mapAGB', 'sdPlot', 'sdMap', 'group')
  cols_to_keep <- intersect(cols_to_keep, names(grp1))
  grp2 <- grp1[, cols_to_keep]

  # Rename plotAGB column for easier reference in calculations
  names(grp2)[names(grp2) == plotAGB_col] <- "plotAGB_calc"

  msd <- grp2 |>
    dplyr::group_by(group) |>
    dplyr::summarise(val = mean((plotAGB_calc - mapAGB)^2), .groups = 'drop')

  # Check if sdPlot and sdMap columns exist
  if (all(c("sdPlot", "sdMap") %in% names(grp2))) {
    check <- grp2 |>
      dplyr::group_by(group) |>
      dplyr::summarise(val = (mean((plotAGB_calc - mapAGB)^2) - mean(sdPlot^2)) - mean(sdMap^2, na.rm = TRUE), .groups = 'drop')
  } else {
    # Create dummy check if columns don't exist
    check <- data.frame(group = msd$group, val = NA)
  }

  plotvar <- grp2 |>
    dplyr::group_by(group) |>
    dplyr::summarise(val = mean(sdPlot^2, na.rm = TRUE), .groups = 'drop')

  mapvar <- grp2 |>
    dplyr::group_by(group) |>
    dplyr::summarise(val = mean(sdMap^2, na.rm = TRUE), .groups = 'drop')

  len <- length(bins.str)
  agg.plot <- agg.plot[1:len, ]
  agg.map <- agg.map[1:len, ]

  # Join accuracy metrics with original table
  df.new <- dplyr::left_join(agg.plot, agg.map, by = "group") |>
    dplyr::left_join(msd, by = "group") |>
    dplyr::left_join(plotvar, by = "group") |>
    dplyr::left_join(mapvar, by = "group") |>
    dplyr::left_join(check, by = "group") |>
    dplyr::select(-group)

  # Add plot tally
  plot.count <- as.data.frame(table(grp1$group))

  # Combine all
  df.new <- cbind(plot.count, df.new)
  names(df.new) <- c('bins', 'plot_count', 'plot', 'map', 'msd', 'plot_var', 'map_var', 'checker')
  df.new$bins <- bins.str

  # Add last row for totals
  lastrow <- data.frame(
    bins = 'total',
    plot_count = sum(df.new$plot_count, na.rm = TRUE),
    plot = mean(df[[plotAGB_col]], na.rm = TRUE),
    map = mean(df$mapAGB, na.rm = TRUE),
    msd = mean((df[[plotAGB_col]] - df$mapAGB)^2, na.rm = TRUE),
    plot_var = if("sdPlot" %in% names(df)) mean(df$sdPlot^2, na.rm = TRUE) else NA,
    map_var = if("sdMap" %in% names(df)) mean(df$sdMap^2, na.rm = TRUE) else NA
  )
  lastrow$checker <- (lastrow$msd + lastrow$plot_var) - lastrow$map_var
  df.new <- rbind(df.new, lastrow)

  # Round df into 2 decimals
  round_df <- function(x, digits) {
    numeric_columns <- sapply(x, is.numeric)
    x[numeric_columns] <- round(x[numeric_columns], digits)
    x
  }

  # Add bias column and rename
  df.new1 <- df.new |>
    dplyr::mutate(bias = map - plot) |>
    dplyr::rename(
      'AGB bin (Mg/ha)' = bins,
      'n' = plot_count,
      'AGBref (Mg/ha)' = plot,
      'AGBmap (Mg/ha)' = map,
      'RMSD' = msd,
      'varPlot' = plot_var,
      'varMap' = map_var,
      'IVar' = checker,
      'AGBmap-AGBref' = bias
    ) |>
    na.omit()

  df.new1$IVar <- ifelse(df.new1$IVar < 0, 0, 1)
  df.new1$RMSD <- sqrt(df.new1$RMSD)
  df.new1 <- round_df(df.new1, 0)
  df.new1 <- df.new1[, -c(7:9)]

  # Create the directory if it doesn't exist
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  utils::write.csv(df.new1, file.path(dir, paste0('acc_', str, '.csv')), row.names = FALSE)
  return(df.new1)
}






# ###FUNCTION TO CREATE ACCURACY TABLE AFTER PLOT-MAP VALIDATION
#
# old_Accuracy <- function(df=plotsBACC, intervals=8, dir=resultsFolder, str=''){
#   df$sdMap <- 1
#   df$sdPlot <- 1
#
#   #assign AGB bins
#   if (intervals == 8){
#     bins <- c(-Inf,50,100,150,200,250,300,400,Inf) #7 intervals
#     bins.str <-c('0-50','50-100','100-150','150-200','200-250','250-300','300-400', '>400')
#   }
#   if (intervals == 7){
#     bins <- c(-1,50,100,150,200,250,300,Inf) #7 intervals
#     bins.str <-c('0-50','050-100','100-150','150-200','200-250','250-300', '>300')
#   }
#   if (intervals == 6){
#     bins <- c(-1,100,150,200,250,300,Inf) #6 intervals
#     bins.str <-c('0-100','100-150','150-200','200-250','250-300', '>300')
#   }
#
#   #assign grouping of AGB values for plot and map separately per bin
#   grp1 <- transform(df, group=cut(df$plotAGB_10,  breaks=bins))
#
#   #aggregate the mean AGB of bins
#   agg.plot <- ddply(grp1, .(group), summarise, plotAGB_10=mean(plotAGB_10), .drop=F)
#   agg.map <- ddply(grp1, .(group), summarise, mapAGB=mean(mapAGB, na.rm=T), .drop=F)
#
#   ##calculate accuracy metrics -- assures values derived are from PLOT BINS
#   grp2 <- grp1[,c('plotAGB_10','mapAGB','sdPlot','sdMap','group')] #retains plotAGB, mapAGB, plotVar, group
#
#   #rmsd per mean agb bin
#   msd <- grp2 |>
#     group_by(group) |>
#     summarise(val=mean((plotAGB_10-mapAGB)^2)) #same with mse
#
#   #mean((actual-predicted)^2) #mse
#   #checker using msd - mean plot error (measurement)  =  map variance
#   check <- grp2 |>
#     group_by(group) |>
#     summarise(val=(mse(plotAGB_10, mapAGB) - mean(sdPlot^2)) - mean(sdMap^2, na.rm=T))
#
#   #plot (measurement) variance per bin
#   plotvar <- grp2 |>
#     group_by(group) |>
#     summarise(val=mean(sdPlot^2, na.rm=T))
#
#   #map SE turned variance per mean agb bin
#   mapvar <- grp2 |>
#     group_by(group) |>
#     summarise(val= mean(sdMap^2, na.rm=T))
#
#   #print(sd of error)
#   len <- length(bins.str) #row control
#   agg.plot <- agg.plot[c(1:len),]
#   agg.map <- agg.map[c(1:len),]
#
#   #join accuracy metrics with original table
#   df.new <- data.frame(agg.plot, agg.map)
#   df.new <- left_join(df.new, msd, by = c('group'='group'))
#   df.new <- left_join(df.new, plotvar, by = c('group'='group'))
#   df.new <- left_join(df.new, mapvar, by = c('group'='group'))
#   df.new <- left_join(df.new, check, by = c('group'='group'))
#   df.new <- df.new[,-c(1,3)]
#
#   #add plot tally #origin of bins, plotcount
#   plot.count <- data.frame(table(grp1$group)) #orders accordingly
#
#   #combine all
#   df.new <- data.frame(plot.count, df.new)
#   names(df.new) <- c('bins', 'plot_count', 'plot', 'map', 'msd','plot_var', 'map_var', 'checker')
#   df.new$bins <- bins.str
#
#   #add last row for totals
#   col1 <- 'total'
#   col2 <- sum(df.new$plot_count, na.rm=T)
#   col3 <- mean(df$plotAGB_10, na.rm=T)
#   col4 <- mean(df$mapAGB, na.rm=T)
#   col5 <- mse(df$plotAGB_10, df$mapAGB)
#   col6 <- mean(df$sdPlot^2, na.rm=T)
#   col7 <- mean(df$sdMap^2, na.rm=T)
#   col8 <- (col5+ col6) - col7 #checker
#   lastrow <- data.frame(col1,col2,col3,col4,col5,col6,col7,col8)
#   names(lastrow) <- names(df.new)
#   df.new <- rbind(df.new,lastrow)
#
#   #round df into 2 decimals
#   round_df <- function(x, digits) {
#     numeric_columns <- sapply(x, mode) == 'numeric'
#     x[numeric_columns] <-  round(x[numeric_columns], digits)
#     x
#   }
#
#   #add bias column
#   df.new1 <- cbind (df.new, bias = df.new[4] - df.new[3])
#   names(df.new1) <- c('AGB bin (Mg/ha)','n', 'AGBref (Mg/ha)', 'AGBmap (Mg/ha)',
#                       'RMSD','varPlot', 'varMap', 'IVar', 'AGBmap-AGBref')
#   df.new1 <- na.omit(df.new1)
#
#   df.new1$IVar <- ifelse(df.new1$IVar < 0, 0, 1)
#   df.new1$RMSD <- sqrt(df.new1$RMSD)
#   df.new1 <- round_df(df.new1, 0)
#   df.new1 <- df.new1[,-c(7:9)]
#   write.csv(df.new1, paste0(dir,'/acc_',str, '.csv'), row.names = F)
#   return(df.new1)
# }

