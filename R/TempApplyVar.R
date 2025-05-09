#' Apply temporal adjustment and compute temporal variance in plot biomass
#'
#' This function adjusts plot biomass values to a specified map year using growth-rate increments
#' and computes the temporal variance (standard deviation) of biomass changes based on growth-rate
#' standard deviations. It combines the functionality of \code{\link{TempApply}} and
#' \code{\link{TempVar}} into a single call.
#'
#' @inheritParams Deforested
#' @param map_year Numeric value indicating the AGB map year.
#' @param gez Character string specifying the Global Ecological Zones (GEZ) of interest to apply. If "all" (default),
#'  all GEZ in plt will be calculated.
#' @param gr Optional data.frame of growth rates containing columns \code{GEZ}, \code{ZONE},
#'   \code{FAO.ecozone}, \code{GR1}, \code{GR2}, \code{GR3}. If \code{NULL}, default data
#'   from \pkg{Plot2Map} is used.
#' @param sds Optional data.frame of growth rate standard deviations containing columns
#'   \code{GEZ}, \code{ZONE}, \code{FAO.ecozone}, \code{SD1}, \code{SD2}, \code{SD3}. If
#'   \code{NULL}, default data from \pkg{Plot2Map} is used.
#'
#' @return A data.frame with the following columns:
#' \describe{
#'   \item{\code{AGB_T_HA}}{Adjusted biomass values for the specified \code{map_year}.}
#'   \item{\code{AGB_T_HA_ORIG}}{Original biomass values before adjustment.}
#'   \item{\code{sdGrowth}}{Temporal standard deviation of the biomass adjustment.}
#'   \item{Additional}{All other columns from the input \code{plt}.}
#' }
#'
#' @importFrom dplyr filter left_join inner_join anti_join bind_rows
#' @importFrom utils read.csv
#' @export
#'
#' @examples
#' library(Plot2Map)
#' set.seed(42)
#' sample_plots <- plots[sample(nrow(plots), 10), ]
#' sample_plots_gez <- BiomePair(sample_plots)
#' result <- TempApplyVar(sample_plots_gez, map_year = 2004)
#' head(result)
TempApplyVar <- function(plt, map_year, gez = "all", gr = NULL, sds = NULL) {
  # Validate and convert input
  plt <- check_and_convert_plt(plt, ez = TRUE)

  # Load growth rates
  if (is.null(gr)) {
    path_gr <- sample_file("GR_Uniques.csv")
    gr <- utils::read.csv(path_gr, stringsAsFactors = FALSE)
  }
  # Load growth SDs
  if (is.null(sds)) {
    path_sd <- sample_file("GR_SD.csv")
    sds <- utils::read.csv(path_sd, stringsAsFactors = FALSE)
  }
  # Ensure key columns are character
  gr$GEZ <- as.character(gr$GEZ); gr$ZONE <- as.character(gr$ZONE); gr$FAO.ecozone <- as.character(gr$FAO.ecozone)
  sds$GEZ <- as.character(sds$GEZ); sds$ZONE <- as.character(sds$ZONE); sds$FAO.ecozone <- as.character(sds$FAO.ecozone)

  # Merge growth tables
  growth <- dplyr::inner_join(gr, sds, by = c("GEZ", "ZONE", "FAO.ecozone"))

  # Ensure AVG_YEAR numeric
  plt$AVG_YEAR <- as.numeric(plt$AVG_YEAR)

  # Check for missing growth data
  if (identical(gez, "all")) {
    missing <- dplyr::anti_join(plt, growth, by = c("GEZ", "ZONE", "FAO.ecozone"))
    if (nrow(missing) > 0) {
      combos <- unique(missing[, c("GEZ", "ZONE", "FAO.ecozone")])
      stop("Missing growth data for combinations:\n",
           paste(apply(combos, 1, paste, collapse = ", "), collapse = "\n"))
    }
  } else {
    if (! (gez %in% unique(plt$GEZ))) {
      stop("GEZ '", gez, "' not found in plt$GEZ")
    }
  }

  # Filter by GEZ
  plt0 <- if (identical(gez, "all")) plt else dplyr::filter(plt, GEZ == gez)

  # Normalize zone strings to match table keys
  plt0$ZONE <- ifelse(plt0$ZONE == "Northern America", "N.America", plt0$ZONE)
  plt0$ZONE <- ifelse(plt0$ZONE == "Southern America", "S.America", plt0$ZONE)
  plt0$ZONE <- ifelse(plt0$ZONE == "Central America",  "C.America", plt0$ZONE)

  # Join to growth data
  df <- dplyr::left_join(plt0, growth, by = c("GEZ", "ZONE", "FAO.ecozone"))

  # Split by AVG_YEAR
  below <- df[df$AVG_YEAR < map_year, , drop = FALSE]
  above <- df[df$AVG_YEAR > map_year, , drop = FALSE]
  static <- df[is.na(df$AVG_YEAR) | df$AVG_YEAR == map_year, , drop = FALSE]

  # Adjust below
  if (nrow(below)) {
    below$AGB_T_HA_ORIG <- below$AGB_T_HA
    dt <- map_year - below$AVG_YEAR
    inc <- ifelse(below$AGB_T_HA < 100, below$GR3, below$GR2)
    below$AGB_T_HA <- below$AGB_T_HA + inc * dt
    # cap with primary rate if above threshold
    below$AGB_T_HA <- ifelse(below$AGB_T_HA > 152,
                             below$AGB_T_HA_ORIG + below$GR1 * dt, below$AGB_T_HA)
  }
  # Adjust above
  if (nrow(above)) {
    above$AGB_T_HA_ORIG <- above$AGB_T_HA
    dt2 <- above$AVG_YEAR - map_year
    dec <- ifelse(above$AGB_T_HA < 100, above$GR3, above$GR2)
    above$AGB_T_HA <- above$AGB_T_HA - dec * dt2
    above$AGB_T_HA <- ifelse(above$AGB_T_HA > 152,
                             above$AGB_T_HA_ORIG - above$GR1 * dt2, above$AGB_T_HA)
    above$AGB_T_HA <- ifelse(above$AGB_T_HA < 0,
                             above$AGB_T_HA_ORIG, above$AGB_T_HA)
  }
  # Static (no change)
  if (nrow(static)) {
    static$AGB_T_HA_ORIG <- static$AGB_T_HA
  }

  # Combine
  out <- dplyr::bind_rows(below, above, static)

  # Compute temporal SD
  out$sdGrowth <- abs(out$AGB_T_HA - out$AGB_T_HA_ORIG)
  # Fill any missing
  mean_sd <- mean(out$sdGrowth, na.rm = TRUE)
  out$sdGrowth[is.na(out$sdGrowth) | is.nan(out$sdGrowth)] <- mean_sd

  # Remove growth columns
  drop <- c("GR1", "GR2", "GR3", "SD1", "SD2", "SD3")
  out <- out[, !(names(out) %in% drop)]

  return(out)
}
