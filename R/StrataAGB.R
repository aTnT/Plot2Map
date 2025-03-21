## Updates made to the new framework:
# 21/03/25:
# Refactored to separate strata assignment and computation logic


#' Assign strata weights to circular plots
#'
#' Assigns predefined strata weights to circular plots based on their strata classification.
#' This is an internal helper function used by `StrataAGB` and is not intended for direct use.
#'
#' @param plots A data frame containing plot data with at least a `stratum` column (character or factor).
#' @param strata_weights A named numeric vector where names are strata levels and values are weights.
#'                       Defaults to `c("A" = 0.22, "B" = 0.22, "C" = 0.68)`.
#'
#' @return A data frame with an additional `wt` column containing the assigned weights.
#' @export
#' @examples
#'   # Example: Assign weights to a simple plot data frame
#'   strata_plots <- data.frame(stratum = c("A", "B", "C"))
#'   weighted_plots <- assign_strata_weights(strata_plots)
#'   print(weighted_plots)
assign_strata_weights <- function(plots, strata_weights = c("A" = 0.22, "B" = 0.22, "C" = 0.68)) {
  if (!"stratum" %in% names(plots)) {
    stop("The 'plots' data frame must contain a 'stratum' column")
  }
  if (!all(plots$stratum %in% names(strata_weights))) {
    stop("All strata in 'plots$stratum' must match names in 'strata_weights'")
  }
  plots$wt <- strata_weights[plots$stratum]
  plots
}

#' Compute weighted AGB by strata
#'
#' Assigns strata weights to circular plots and computes the weighted mean and standard deviation
#' of above-ground biomass (AGB) and tree standard deviation based on strata sizes.
#'
#' @param plots A data frame containing plot data with columns `AGB_T_HA` (numeric, AGB in tons per hectare),
#'              `sdTree` (numeric, standard deviation of tree measurements), and optionally `stratum`
#'              (character or factor, strata classification). If `stratum` is missing, it will be assigned
#'              based on the default weights.
#' @param strata_weights A named numeric vector specifying weights for each stratum. Defaults to
#'                       `c("A" = 0.22, "B" = 0.22, "C" = 0.68)`, representing proportional areas.
#' @param verbose Logical; if `TRUE`, prints the weighted mean AGB and standard deviation. Defaults to `TRUE`.
#'
#' @return A list containing:
#'   \item{wm}{Weighted mean of AGB (tons per hectare).}
#'   \item{wsd}{Weighted mean of tree standard deviation.}
#'   \item{weighted_plots}{The input data frame with weights assigned in a `wt` column.}
#' @importFrom stats weighted.mean
#' @export
#' @examples
#'   # Example 1: Basic usage with synthetic data
#'   strata_plots <- data.frame(
#'     AGB_T_HA = c(100, 150, 200),
#'     sdTree = c(10, 15, 20),
#'     stratum = c("A", "B", "C")
#'   )
#'   result <- StrataAGB(strata_plots)
#'   print(result$wm)  # Weighted mean AGB
#'   print(result$wsd) # Weighted standard deviation
#'
#'   # Example 2: Custom weights and no printing
#'   custom_weights <- c("Low" = 0.3, "High" = 0.7)
#'   plots <- data.frame(
#'     AGB_T_HA = c(120, 180),
#'     sdTree = c(12, 18),
#'     stratum = c("Low", "High")
#'   )
#'   result <- StrataAGB(plots, strata_weights = custom_weights, verbose = FALSE)
#'   print(result$weighted_plots)
StrataAGB <- function(plots, strata_weights = c("A" = 0.22, "B" = 0.22, "C" = 0.68), verbose = TRUE) {
  # Input validation
  required_cols <- c("AGB_T_HA", "sdTree")
  if (!all(required_cols %in% names(plots))) {
    stop("The 'plots' data frame must contain 'AGB_T_HA' and 'sdTree' columns")
  }
  if (!is.numeric(plots$AGB_T_HA) || !is.numeric(plots$sdTree)) {
    stop("'AGB_T_HA' and 'sdTree' must be numeric")
  }
  if (!all(strata_weights >= 0) || sum(strata_weights) == 0) {
    stop("'strata_weights' must be non-negative and sum to a positive value")
  }

  # Assign strata weights if stratum column exists, otherwise assign defaults
  if ("stratum" %in% names(plots)) {
    weighted_plots <- assign_strata_weights(plots, strata_weights)
  } else {
    if (nrow(plots) != 5) {  # Original had 5 plots; adjust if needed
      stop("Without 'stratum', 'plots' must have exactly 5 rows to match default weights")
    }
    weighted_plots <- plots
    weighted_plots$wt <- c(0.22, 0.22, 0.68, 0.68, 0.68)  # Original hardcoded weights
  }

  # Compute weighted statistics
  wm  <- stats::weighted.mean(weighted_plots$AGB_T_HA, weighted_plots$wt)
  wsd <- stats::weighted.mean(weighted_plots$sdTree, weighted_plots$wt)

  # Print results if verbose
  if (verbose) {
    cat("Plot AGB is", round(wm, 2), "with SD", round(wsd, 2), "\n")
  }

  # Return results
  list(
    wm = wm,
    wsd = wsd,
    weighted_plots = weighted_plots
  )
}


# ### FUNCTION TO ASSIGN STRATA TO CIRCULAR PLOTS AND
# ### COMPUTE WEIGHTED AGB BASED ON STRATA SIZE
# old_StrataAGB <- function(plt=plots){
#   plots$wt <- c(.22,.22,.68,.68,.68)
#   wm <- weighted.mean(plots$AGB_T_HA, plots$wt)
#   wsd <- weighted.mean(plots$sdTree, plots$wt)
#
#   print(paste("plot AGB is", round(wm,2),
#               "with SD", round(wsd,2)))
#
# }



# Tests
# if (!requireNamespace("testthat", quietly = TRUE)) install.packages("testthat")
# library(testthat)
#
# # Tests for assign_strata_weights
# test_that("assign_strata_weights assigns weights correctly", {
#   # Test 1: Default weights
#   plots <- data.frame(stratum = c("A", "B", "C"))
#   result <- assign_strata_weights(plots)
#   expect_equal(result$wt, c(0.22, 0.22, 0.68), tolerance = 1e-8)
#
#   # Test 2: Custom weights
#   custom_weights <- c("Low" = 0.3, "High" = 0.7)
#   plots <- data.frame(stratum = c("Low", "High"))
#   result <- assign_strata_weights(plots, strata_weights = custom_weights)
#   expect_equal(result$wt, c(0.3, 0.7), tolerance = 1e-8)
#
#   # Test 3: Error when stratum column is missing
#   plots <- data.frame(value = 1:3)
#   expect_error(assign_strata_weights(plots), "The 'plots' data frame must contain a 'stratum' column")
#
#   # Test 4: Error when strata don't match weights
#   plots <- data.frame(stratum = c("A", "D"))
#   expect_error(assign_strata_weights(plots), "All strata in 'plots\\$stratum' must match names in 'strata_weights'")
# })
#
# # Tests for StrataAGB
# test_that("StrataAGB computes weighted AGB and SD correctly", {
#   # Test 1: Default weights with stratum column
#   plots <- data.frame(
#     AGB_T_HA = c(100, 150, 200),
#     sdTree = c(10, 15, 20),
#     stratum = c("A", "B", "C")
#   )
#   result <- StrataAGB(plots, verbose = FALSE)
#   expected_wm <- (100 * 0.22 + 150 * 0.22 + 200 * 0.68) / (0.22 + 0.22 + 0.68)
#   expected_wsd <- (10 * 0.22 + 15 * 0.22 + 20 * 0.68) / (0.22 + 0.22 + 0.68)
#   expect_equal(result$wm, expected_wm, tolerance = 1e-8)
#   expect_equal(result$wsd, expected_wsd, tolerance = 1e-8)
#   expect_equal(result$weighted_plots$wt, c(0.22, 0.22, 0.68), tolerance = 1e-8)
#
#   # Test 2: Default weights without stratum column (5 rows)
#   plots <- data.frame(
#     AGB_T_HA = c(100, 150, 200, 180, 170),
#     sdTree = c(10, 15, 20, 18, 17)
#   )
#   result <- StrataAGB(plots, verbose = FALSE)
#   expected_wm <- (100 * 0.22 + 150 * 0.22 + 200 * 0.68 + 180 * 0.68 + 170 * 0.68) / (0.22 + 0.22 + 0.68 * 3)
#   expected_wsd <- (10 * 0.22 + 15 * 0.22 + 20 * 0.68 + 18 * 0.68 + 17 * 0.68) / (0.22 + 0.22 + 0.68 * 3)
#   expect_equal(result$wm, expected_wm, tolerance = 1e-8)
#   expect_equal(result$wsd, expected_wsd, tolerance = 1e-8)
#   expect_equal(result$weighted_plots$wt, c(0.22, 0.22, 0.68, 0.68, 0.68), tolerance = 1e-8)
#
#   # Test 3: Custom weights
#   custom_weights <- c("Low" = 0.3, "High" = 0.7)
#   plots <- data.frame(
#     AGB_T_HA = c(120, 180),
#     sdTree = c(12, 18),
#     stratum = c("Low", "High")
#   )
#   result <- StrataAGB(plots, strata_weights = custom_weights, verbose = FALSE)
#   expected_wm <- (120 * 0.3 + 180 * 0.7) / (0.3 + 0.7)
#   expected_wsd <- (12 * 0.3 + 18 * 0.7) / (0.3 + 0.7)
#   expect_equal(result$wm, expected_wm, tolerance = 1e-8)
#   expect_equal(result$wsd, expected_wsd, tolerance = 1e-8)
#
#   # Test 4: Error when required columns are missing
#   plots <- data.frame(AGB_T_HA = 1:3)
#   expect_error(StrataAGB(plots), "The 'plots' data frame must contain 'AGB_T_HA' and 'sdTree' columns")
#
#   # Test 5: Error when AGB_T_HA or sdTree is non-numeric
#   plots <- data.frame(
#     AGB_T_HA = c("100", "150"),
#     sdTree = c(10, 15),
#     stratum = c("A", "B")
#   )
#   expect_error(StrataAGB(plots), "'AGB_T_HA' and 'sdTree' must be numeric")
#
#   # Test 6: Error when weights are invalid
#   plots <- data.frame(
#     AGB_T_HA = c(100, 150),
#     sdTree = c(10, 15),
#     stratum = c("A", "B")
#   )
#   expect_error(StrataAGB(plots, strata_weights = c("A" = -0.1, "B" = 0.1)),
#                "'strata_weights' must be non-negative and sum to a positive value")
#
#   # Test 7: Error when row count doesn't match default weights without stratum
#   plots <- data.frame(
#     AGB_T_HA = c(100, 150),
#     sdTree = c(10, 15)
#   )
#   expect_error(StrataAGB(plots), "Without 'stratum', 'plots' must have exactly 5 rows to match default weights")
# })
#


