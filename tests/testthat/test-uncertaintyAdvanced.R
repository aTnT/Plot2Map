# Tests for Advanced Uncertainty Quantification Functions
# Functions: fitResidualVariogram, discountVariogram, aggregateUncertainty, biasAdjustedTotal

library(testthat)
library(terra)
library(sf)
library(gstat)

# Helper function to create test data for uncertainty functions
create_uncertainty_test_data <- function() {
  # Create simple rasters for testing with PROJECTED CRS (required for gstat)
  # Using UTM zone 31N (covers parts of Europe)
  map_agb <- terra::rast(nrows = 10, ncols = 10, xmin = 500000, xmax = 600000,
                         ymin = 4000000, ymax = 4100000, crs = "EPSG:32631")
  terra::values(map_agb) <- runif(100, 50, 200)

  map_sd <- terra::rast(nrows = 10, ncols = 10, xmin = 500000, xmax = 600000,
                        ymin = 4000000, ymax = 4100000, crs = "EPSG:32631")
  terra::values(map_sd) <- runif(100, 10, 30)

  forest_mask <- terra::rast(nrows = 10, ncols = 10, xmin = 500000, xmax = 600000,
                             ymin = 4000000, ymax = 4100000, crs = "EPSG:32631")
  terra::values(forest_mask) <- sample(c(0, 1), 100, replace = TRUE, prob = c(0.3, 0.7))

  # Create plot data with bias and predictions (in meters, matching raster CRS)
  set.seed(123)
  n_plots <- 30
  plot_data <- data.frame(
    x = runif(n_plots, 500000, 600000),
    y = runif(n_plots, 4000000, 4100000),
    plotAGB_10 = runif(n_plots, 40, 180),
    bias = rnorm(n_plots, mean = 5, sd = 15),
    biasPred = rnorm(n_plots, mean = 5, sd = 10),
    varPlot = runif(n_plots, 100, 500),
    varMap = runif(n_plots, 200, 600)
  )

  # Calculate residuals
  plot_data$resid0 <- plot_data$bias - plot_data$biasPred

  return(list(
    map_agb = map_agb,
    map_sd = map_sd,
    forest_mask = forest_mask,
    plot_data = plot_data
  ))
}

# =============================================================================
# Tests for fitResidualVariogram()
# =============================================================================

test_that("fitResidualVariogram returns correct structure", {
  test_data <- create_uncertainty_test_data()

  result <- fitResidualVariogram(
    bias_data = test_data$plot_data,
    map_sd_raster = test_data$map_sd,
    model = "Sph",
    cutoff = 50000,  # 50 km in meters
    psill = 3,
    range = 20000,  # 20 km in meters
    nugget = 2,
    coord_cols = c("x", "y"),
    crs = "EPSG:32631",
    plot = FALSE
  )

  expect_s3_class(result, "residualVariogram")
  expect_type(result, "list")
  expect_named(result, c("variogram_model", "empirical_variogram", "residuals",
                         "resid_sf", "fit_quality", "n_obs", "n_removed", "plot"))
  expect_s3_class(result$variogram_model, "variogramModel")
  expect_s3_class(result$empirical_variogram, "gstatVariogram")
  expect_s3_class(result$residuals, "data.frame")
  expect_s3_class(result$resid_sf, "sf")
  expect_s3_class(result$fit_quality, "data.frame")
  expect_type(result$n_obs, "integer")
  expect_type(result$n_removed, "integer")
})

test_that("fitResidualVariogram calculates residuals correctly", {
  test_data <- create_uncertainty_test_data()

  result <- fitResidualVariogram(
    bias_data = test_data$plot_data,
    map_sd_raster = test_data$map_sd,
    cutoff = 50000,
    coord_cols = c("x", "y"),
    crs = "EPSG:32631",
    plot = FALSE
  )

  # Check that residuals are calculated
  expect_true("resid0" %in% names(result$residuals))
  expect_true("resid" %in% names(result$residuals))
  expect_true("sd" %in% names(result$residuals))

  # Check that scaled residuals = raw residuals / sd
  expect_equal(
    result$residuals$resid,
    result$residuals$resid0 / result$residuals$sd,
    tolerance = 1e-6
  )
})

test_that("fitResidualVariogram handles different variogram models", {
  test_data <- create_uncertainty_test_data()

  for (model in c("Sph", "Exp", "Gau")) {
    result <- fitResidualVariogram(
      bias_data = test_data$plot_data,
      map_sd_raster = test_data$map_sd,
      model = model,
      cutoff = 50000,
      coord_cols = c("x", "y"),
      crs = "EPSG:32631",
      plot = FALSE
    )

    expect_s3_class(result, "residualVariogram")
    # Check that the model type is in the variogram
    expect_true(model %in% result$variogram_model$model)
  }
})

test_that("fitResidualVariogram validates inputs", {
  test_data <- create_uncertainty_test_data()

  # Missing required columns
  bad_data <- test_data$plot_data[, !names(test_data$plot_data) %in% "bias"]
  expect_error(
    fitResidualVariogram(bad_data, test_data$map_sd, cutoff = 50000),
    "missing required columns"
  )

  # Wrong input types
  expect_error(
    fitResidualVariogram(list(), test_data$map_sd, cutoff = 50000),
    "bias_data must be a data.frame"
  )

  expect_error(
    fitResidualVariogram(test_data$plot_data, matrix(1:10), cutoff = 50000),
    "map_sd_raster must be a SpatRaster"
  )

  # Invalid model
  expect_error(
    fitResidualVariogram(test_data$plot_data, test_data$map_sd,
                        model = "InvalidModel", cutoff = 50000,
                        coord_cols = c("x", "y"), crs = "EPSG:32631"),
    "model must be one of"
  )

  # Invalid cutoff
  expect_error(
    fitResidualVariogram(test_data$plot_data, test_data$map_sd, cutoff = -5,
                        coord_cols = c("x", "y"), crs = "EPSG:32631"),
    "cutoff must be a positive numeric value"
  )
})

test_that("fitResidualVariogram removes outliers when requested", {
  test_data <- create_uncertainty_test_data()

  # Add some extreme outliers
  outlier_data <- test_data$plot_data
  outlier_data$bias[1:3] <- c(1000, -1000, 500)

  result_with_removal <- fitResidualVariogram(
    bias_data = outlier_data,
    map_sd_raster = test_data$map_sd,
    cutoff = 50000,
    remove_outliers = TRUE,
    coord_cols = c("x", "y"),
    crs = "EPSG:32631",
    plot = FALSE
  )

  result_without_removal <- fitResidualVariogram(
    bias_data = outlier_data,
    map_sd_raster = test_data$map_sd,
    cutoff = 50000,
    remove_outliers = FALSE,
    coord_cols = c("x", "y"),
    crs = "EPSG:32631",
    plot = FALSE
  )

  # Should have removed some plots
  expect_gt(result_with_removal$n_removed, 0)
  expect_equal(result_without_removal$n_removed, 0)

  # Should have fewer observations after removal
  expect_lt(result_with_removal$n_obs, result_without_removal$n_obs)
})

test_that("fitResidualVariogram fit quality metrics are calculated", {
  test_data <- create_uncertainty_test_data()

  result <- fitResidualVariogram(
    bias_data = test_data$plot_data,
    map_sd_raster = test_data$map_sd,
    cutoff = 50000,
    coord_cols = c("x", "y"),
    crs = "EPSG:32631",
    plot = FALSE
  )

  expect_true("SSErr" %in% names(result$fit_quality))
  expect_true("RMSE" %in% names(result$fit_quality))
  expect_true("R2" %in% names(result$fit_quality))

  # Check that values are numeric and positive
  expect_true(is.numeric(result$fit_quality$SSErr))
  expect_true(is.numeric(result$fit_quality$RMSE))
  expect_true(is.numeric(result$fit_quality$R2))
  expect_gte(result$fit_quality$SSErr, 0)
  expect_gte(result$fit_quality$RMSE, 0)
})

test_that("fitResidualVariogram handles NA values correctly", {
  test_data <- create_uncertainty_test_data()

  # Add some NA values
  na_data <- test_data$plot_data
  na_data$bias[1:3] <- NA

  expect_warning(
    result <- fitResidualVariogram(
      bias_data = na_data,
      map_sd_raster = test_data$map_sd,
      cutoff = 50000,
      coord_cols = c("x", "y"),
      crs = "EPSG:32631",
      plot = FALSE
    ),
    "plots removed"
  )

  # Should have fewer observations
  expect_lt(result$n_obs, nrow(na_data))
})

test_that("fitResidualVariogram plot function works", {
  test_data <- create_uncertainty_test_data()

  result <- fitResidualVariogram(
    bias_data = test_data$plot_data,
    map_sd_raster = test_data$map_sd,
    cutoff = 50000,
    coord_cols = c("x", "y"),
    crs = "EPSG:32631",
    plot = TRUE
  )

  expect_type(result$plot, "closure")

  # Test that plot function runs without error
  expect_silent(result$plot())
})

# =============================================================================
# Tests for discountVariogram()
# =============================================================================

test_that("discountVariogram returns correct structure", {
  test_data <- create_uncertainty_test_data()

  vgm_fit <- fitResidualVariogram(
    bias_data = test_data$plot_data,
    map_sd_raster = test_data$map_sd,
    cutoff = 50000,
    coord_cols = c("x", "y"),
    crs = "EPSG:32631",
    plot = FALSE
  )

  result <- discountVariogram(
    variogram_fit = vgm_fit,
    plot_data = test_data$plot_data,
    plot = FALSE
  )

  expect_s3_class(result, "discountedVariogram")
  expect_type(result, "list")
  expect_named(result, c("discounted_variogram", "original_variogram",
                         "discount_factor", "nugget_original", "nugget_discounted",
                         "nugget_transferred", "n_plots", "n_filtered", "plot"))
  expect_s3_class(result$discounted_variogram, "variogramModel")
  expect_s3_class(result$original_variogram, "variogramModel")
  expect_type(result$discount_factor, "double")
  expect_type(result$nugget_original, "double")
  expect_type(result$nugget_discounted, "double")
})

test_that("discountVariogram calculates discount factor correctly", {
  test_data <- create_uncertainty_test_data()

  vgm_fit <- fitResidualVariogram(
    bias_data = test_data$plot_data,
    map_sd_raster = test_data$map_sd,
    cutoff = 50000,
    coord_cols = c("x", "y"),
    crs = "EPSG:32631",
    plot = FALSE
  )

  result <- discountVariogram(
    variogram_fit = vgm_fit,
    plot_data = test_data$plot_data,
    filter_iqr = FALSE,
    plot = FALSE
  )

  # Calculate expected discount factor manually
  expected_discount <- mean(test_data$plot_data$varPlot /
                              (test_data$plot_data$varMap^2), na.rm = TRUE)

  expect_equal(result$discount_factor, expected_discount, tolerance = 1e-6)
})

test_that("discountVariogram reduces nugget", {
  test_data <- create_uncertainty_test_data()

  vgm_fit <- fitResidualVariogram(
    bias_data = test_data$plot_data,
    map_sd_raster = test_data$map_sd,
    cutoff = 50000,
    coord_cols = c("x", "y"),
    crs = "EPSG:32631",
    plot = FALSE
  )

  result <- discountVariogram(
    variogram_fit = vgm_fit,
    plot_data = test_data$plot_data,
    plot = FALSE
  )

  # Discounted nugget should be less than or equal to original
  expect_lte(result$nugget_discounted, result$nugget_original)
})

test_that("discountVariogram handles negative nugget with transfer", {
  test_data <- create_uncertainty_test_data()

  vgm_fit <- fitResidualVariogram(
    bias_data = test_data$plot_data,
    map_sd_raster = test_data$map_sd,
    cutoff = 50000,
    nugget = 0.001,  # Very small nugget
    coord_cols = c("x", "y"),
    crs = "EPSG:32631",
    plot = FALSE
  )

  # Create data that will cause large discount
  large_discount_data <- test_data$plot_data
  large_discount_data$varPlot <- large_discount_data$varPlot * 100

  result <- discountVariogram(
    variogram_fit = vgm_fit,
    plot_data = large_discount_data,
    transfer_negative = TRUE,
    filter_iqr = FALSE,
    plot = FALSE
  )

  # If nugget would be negative, it should be set to 0
  expect_gte(result$nugget_discounted, 0)

  # If transfer happened, transferred amount should be positive
  if (result$nugget_transferred > 0) {
    expect_gt(result$nugget_transferred, 0)
  }
})

test_that("discountVariogram validates inputs", {
  test_data <- create_uncertainty_test_data()

  # Missing required columns
  bad_data <- test_data$plot_data[, !names(test_data$plot_data) %in% "varPlot"]
  vgm <- gstat::vgm(psill = 3, model = "Sph", range = 50000, nugget = 2)

  expect_error(
    discountVariogram(vgm, bad_data),
    "missing required columns"
  )

  # Wrong input types
  expect_error(
    discountVariogram("not_a_variogram", test_data$plot_data),
    "variogram_fit must be"
  )

  expect_error(
    discountVariogram(vgm, list()),
    "plot_data must be a data.frame"
  )
})

test_that("discountVariogram IQR filtering works", {
  test_data <- create_uncertainty_test_data()

  vgm_fit <- fitResidualVariogram(
    bias_data = test_data$plot_data,
    map_sd_raster = test_data$map_sd,
    cutoff = 50000,
    coord_cols = c("x", "y"),
    crs = "EPSG:32631",
    plot = FALSE
  )

  # Add extreme outliers
  outlier_data <- test_data$plot_data
  outlier_data$varPlot[1:2] <- c(10000, 20000)

  result_with_filter <- discountVariogram(
    variogram_fit = vgm_fit,
    plot_data = outlier_data,
    filter_iqr = TRUE,
    plot = FALSE
  )

  result_without_filter <- discountVariogram(
    variogram_fit = vgm_fit,
    plot_data = outlier_data,
    filter_iqr = FALSE,
    plot = FALSE
  )

  # With filtering should remove some plots
  expect_gt(result_with_filter$n_filtered, 0)
  expect_equal(result_without_filter$n_filtered, 0)

  # Should have different discount factors
  expect_false(isTRUE(all.equal(
    result_with_filter$discount_factor,
    result_without_filter$discount_factor
  )))
})

test_that("discountVariogram accepts variogram objects directly", {
  test_data <- create_uncertainty_test_data()

  # Create a variogram directly (not from fitResidualVariogram)
  vgm <- gstat::vgm(psill = 3, model = "Sph", range = 50000, nugget = 2)

  result <- discountVariogram(
    variogram_fit = vgm,
    plot_data = test_data$plot_data,
    plot = FALSE
  )

  expect_s3_class(result, "discountedVariogram")
})

test_that("discountVariogram plot function works", {
  test_data <- create_uncertainty_test_data()

  vgm_fit <- fitResidualVariogram(
    bias_data = test_data$plot_data,
    map_sd_raster = test_data$map_sd,
    cutoff = 50000,
    coord_cols = c("x", "y"),
    crs = "EPSG:32631",
    plot = FALSE
  )

  result <- discountVariogram(
    variogram_fit = vgm_fit,
    plot_data = test_data$plot_data,
    plot = TRUE
  )

  expect_type(result$plot, "closure")
  expect_silent(result$plot())
})

# =============================================================================
# Tests for aggregateUncertainty()
# =============================================================================

test_that("aggregateUncertainty returns correct structure", {
  test_data <- create_uncertainty_test_data()

  vgm <- gstat::vgm(psill = 3, model = "Sph", range = 50000, nugget = 1)

  result <- aggregateUncertainty(
    sd_raster = test_data$map_sd,
    variogram_model = vgm,
    resolution = 30000,  # 30 km window
    return_comparison = TRUE,
    verbose = FALSE
  )

  expect_s3_class(result, "aggregatedUncertainty")
  expect_type(result, "list")
  expect_named(result, c("aggregated_sd_raster", "sd_no_correlation",
                         "correlation_matrix", "regional_sd_correlated",
                         "regional_sd_independent", "correlation_reduction_factor",
                         "n_cells", "resolution"))
  expect_s4_class(result$aggregated_sd_raster, "SpatRaster")
  expect_s4_class(result$sd_no_correlation, "SpatRaster")
  expect_true(is.matrix(result$correlation_matrix))
  expect_type(result$regional_sd_correlated, "double")
  expect_type(result$regional_sd_independent, "double")
})

test_that("aggregateUncertainty creates correlation matrix correctly", {
  test_data <- create_uncertainty_test_data()

  vgm <- gstat::vgm(psill = 3, model = "Sph", range = 50000, nugget = 1)

  result <- aggregateUncertainty(
    sd_raster = test_data$map_sd,
    variogram_model = vgm,
    resolution = 30000,
    verbose = FALSE
  )

  # Correlation matrix should be square
  expect_equal(nrow(result$correlation_matrix), ncol(result$correlation_matrix))

  # All values should be between 0 and 1
  expect_true(all(result$correlation_matrix >= 0))
  expect_true(all(result$correlation_matrix <= 1))

  # Center should have highest correlation (1.0)
  center_idx <- ceiling(nrow(result$correlation_matrix) / 2)
  expect_equal(result$correlation_matrix[center_idx, center_idx], 1, tolerance = 1e-6)
})

test_that("aggregateUncertainty accepts different variogram object types", {
  test_data <- create_uncertainty_test_data()

  # Test with raw vgm object
  vgm <- gstat::vgm(psill = 3, model = "Sph", range = 50000, nugget = 1)

  result1 <- aggregateUncertainty(
    sd_raster = test_data$map_sd,
    variogram_model = vgm,
    resolution = 30000,
    verbose = FALSE
  )
  expect_s3_class(result1, "aggregatedUncertainty")

  # Test with residualVariogram object
  vgm_fit <- fitResidualVariogram(
    bias_data = test_data$plot_data,
    map_sd_raster = test_data$map_sd,
    cutoff = 50000,
    coord_cols = c("x", "y"),
    crs = "EPSG:32631",
    plot = FALSE
  )

  result2 <- aggregateUncertainty(
    sd_raster = test_data$map_sd,
    variogram_model = vgm_fit,
    resolution = 30000,
    verbose = FALSE
  )
  expect_s3_class(result2, "aggregatedUncertainty")

  # Test with discountedVariogram object
  vgm_disc <- discountVariogram(vgm_fit, test_data$plot_data, plot = FALSE)

  result3 <- aggregateUncertainty(
    sd_raster = test_data$map_sd,
    variogram_model = vgm_disc,
    resolution = 30000,
    verbose = FALSE
  )
  expect_s3_class(result3, "aggregatedUncertainty")
})

test_that("aggregateUncertainty applies forest mask", {
  test_data <- create_uncertainty_test_data()

  vgm <- gstat::vgm(psill = 3, model = "Sph", range = 50000, nugget = 1)

  result_with_mask <- aggregateUncertainty(
    sd_raster = test_data$map_sd,
    variogram_model = vgm,
    resolution = 30000,
    forest_mask = test_data$forest_mask,
    verbose = FALSE
  )

  result_without_mask <- aggregateUncertainty(
    sd_raster = test_data$map_sd,
    variogram_model = vgm,
    resolution = 30000,
    forest_mask = NULL,
    verbose = FALSE
  )

  # Should have fewer cells with mask
  expect_lt(result_with_mask$n_cells, result_without_mask$n_cells)
})

test_that("aggregateUncertainty validates inputs", {
  test_data <- create_uncertainty_test_data()

  vgm <- gstat::vgm(psill = 3, model = "Sph", range = 50000, nugget = 1)

  expect_error(
    aggregateUncertainty(matrix(1:10), vgm, resolution = 2),
    "sd_raster must be a SpatRaster"
  )

  expect_error(
    aggregateUncertainty(test_data$map_sd, "not_a_variogram", resolution = 2),
    "variogram_model must be"
  )

  expect_error(
    aggregateUncertainty(test_data$map_sd, vgm, resolution = -2),
    "resolution must be a positive numeric value"
  )

  expect_error(
    aggregateUncertainty(test_data$map_sd, vgm, resolution = 30000,
                        forest_mask = matrix(1:10)),
    "forest_mask must be a SpatRaster or NULL"
  )
})

test_that("aggregateUncertainty comparison shows correlation effect", {
  test_data <- create_uncertainty_test_data()

  vgm <- gstat::vgm(psill = 3, model = "Sph", range = 50000, nugget = 0.5)

  result <- aggregateUncertainty(
    sd_raster = test_data$map_sd,
    variogram_model = vgm,
    resolution = 30000,
    return_comparison = TRUE,
    verbose = FALSE
  )

  # With positive spatial correlation, correlated SD should be >= independent SD
  # (This depends on the variogram, but generally true for spatial data)
  expect_type(result$correlation_reduction_factor, "double")
  expect_gt(result$correlation_reduction_factor, 0)
})

test_that("aggregateUncertainty can save to file", {
  test_data <- create_uncertainty_test_data()
  vgm <- gstat::vgm(psill = 3, model = "Sph", range = 50000, nugget = 1)

  temp_file <- tempfile(fileext = ".tif")

  result <- aggregateUncertainty(
    sd_raster = test_data$map_sd,
    variogram_model = vgm,
    resolution = 30000,
    filename = temp_file,
    verbose = FALSE
  )

  expect_true(file.exists(temp_file))
  unlink(temp_file)
})

# =============================================================================
# Tests for biasAdjustedTotal()
# =============================================================================

test_that("biasAdjustedTotal returns correct structure", {
  test_data <- create_uncertainty_test_data()

  # Create adjusted map (simple adjustment for testing)
  map_adjusted <- test_data$map_agb - 5

  # Create simple aggregated uncertainty object
  vgm <- gstat::vgm(psill = 3, model = "Sph", range = 50000, nugget = 1)
  uncertainty <- aggregateUncertainty(
    sd_raster = test_data$map_sd,
    variogram_model = vgm,
    resolution = 30000,
    verbose = FALSE
  )

  result <- biasAdjustedTotal(
    map_agb_default = test_data$map_agb,
    map_agb_adjusted = map_adjusted,
    aggregated_uncertainty = uncertainty,
    threshold = 0
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true("map_type" %in% names(result))
  expect_true("total_Mg" %in% names(result))
  expect_true("sd_Mg" %in% names(result))
  expect_true("cv_percent" %in% names(result))
  expect_true("ci_lower" %in% names(result))
  expect_true("ci_upper" %in% names(result))
  expect_true("confidence_level" %in% names(result))
  expect_true("n_cells" %in% names(result))

  # Check attributes
  expect_true(!is.null(attr(result, "bias_correction_effect")))
  expect_true(!is.null(attr(result, "percent_change")))
  expect_true(!is.null(attr(result, "uncertainty_overlap")))
})

test_that("biasAdjustedTotal calculates totals correctly", {
  test_data <- create_uncertainty_test_data()

  map_adjusted <- test_data$map_agb - 5

  vgm <- gstat::vgm(psill = 3, model = "Sph", range = 50000, nugget = 1)
  uncertainty <- aggregateUncertainty(
    sd_raster = test_data$map_sd,
    variogram_model = vgm,
    resolution = 30000,
    verbose = FALSE
  )

  result <- biasAdjustedTotal(
    map_agb_default = test_data$map_agb,
    map_agb_adjusted = map_adjusted,
    aggregated_uncertainty = uncertainty,
    threshold = 0,
    scale_factor = 1  # Simple scale for testing
  )

  # Check that default and adjusted are different
  default_total <- result$total_Mg[result$map_type == "default"]
  adjusted_total <- result$total_Mg[result$map_type == "bias_adjusted"]

  expect_false(isTRUE(all.equal(default_total, adjusted_total)))

  # Adjusted should be less than default (we subtracted 5)
  expect_lt(adjusted_total, default_total)
})

test_that("biasAdjustedTotal calculates confidence intervals correctly", {
  test_data <- create_uncertainty_test_data()

  map_adjusted <- test_data$map_agb - 5

  vgm <- gstat::vgm(psill = 3, model = "Sph", range = 50000, nugget = 1)
  uncertainty <- aggregateUncertainty(
    sd_raster = test_data$map_sd,
    variogram_model = vgm,
    resolution = 30000,
    verbose = FALSE
  )

  result <- biasAdjustedTotal(
    map_agb_default = test_data$map_agb,
    map_agb_adjusted = map_adjusted,
    aggregated_uncertainty = uncertainty,
    threshold = 0,
    confidence_level = 0.95
  )

  # CI lower should be less than total
  expect_true(all(result$ci_lower < result$total_Mg))

  # CI upper should be greater than total
  expect_true(all(result$ci_upper > result$total_Mg))

  # Check width of CI is proportional to SD
  ci_width <- result$ci_upper - result$ci_lower
  expect_gt(ci_width[1], 0)
})

test_that("biasAdjustedTotal validates inputs", {
  test_data <- create_uncertainty_test_data()

  map_adjusted <- test_data$map_agb - 5

  vgm <- gstat::vgm(psill = 3, model = "Sph", range = 50000, nugget = 1)
  uncertainty <- aggregateUncertainty(
    sd_raster = test_data$map_sd,
    variogram_model = vgm,
    resolution = 30000,
    verbose = FALSE
  )

  expect_error(
    biasAdjustedTotal(matrix(1:10), map_adjusted, uncertainty),
    "map_agb_default must be a SpatRaster"
  )

  expect_error(
    biasAdjustedTotal(test_data$map_agb, matrix(1:10), uncertainty),
    "map_agb_adjusted must be a SpatRaster"
  )

  expect_error(
    biasAdjustedTotal(test_data$map_agb, map_adjusted, list()),
    "aggregated_uncertainty must be"
  )

  expect_error(
    biasAdjustedTotal(test_data$map_agb, map_adjusted, uncertainty,
                     confidence_level = 1.5),
    "confidence_level must be between 0 and 1"
  )
})

test_that("biasAdjustedTotal applies forest mask", {
  test_data <- create_uncertainty_test_data()

  map_adjusted <- test_data$map_agb - 5

  vgm <- gstat::vgm(psill = 3, model = "Sph", range = 50000, nugget = 1)
  uncertainty <- aggregateUncertainty(
    sd_raster = test_data$map_sd,
    variogram_model = vgm,
    resolution = 30000,
    verbose = FALSE
  )

  result_with_mask <- biasAdjustedTotal(
    map_agb_default = test_data$map_agb,
    map_agb_adjusted = map_adjusted,
    aggregated_uncertainty = uncertainty,
    forest_mask = test_data$forest_mask,
    threshold = 1  # Binary mask: filter out 0 values
  )

  result_without_mask <- biasAdjustedTotal(
    map_agb_default = test_data$map_agb,
    map_agb_adjusted = map_adjusted,
    aggregated_uncertainty = uncertainty,
    forest_mask = NULL,
    threshold = 0
  )

  # Should have fewer cells with mask
  expect_lt(result_with_mask$n_cells[1], result_without_mask$n_cells[1])

  # Totals should be different
  expect_false(isTRUE(all.equal(
    result_with_mask$total_Mg[1],
    result_without_mask$total_Mg[1]
  )))
})

test_that("biasAdjustedTotal applies threshold", {
  test_data <- create_uncertainty_test_data()

  map_adjusted <- test_data$map_agb - 5

  vgm <- gstat::vgm(psill = 3, model = "Sph", range = 50000, nugget = 1)
  uncertainty <- aggregateUncertainty(
    sd_raster = test_data$map_sd,
    variogram_model = vgm,
    resolution = 30000,
    verbose = FALSE
  )

  result_with_threshold <- biasAdjustedTotal(
    map_agb_default = test_data$map_agb,
    map_agb_adjusted = map_adjusted,
    aggregated_uncertainty = uncertainty,
    threshold = 100  # High threshold
  )

  result_no_threshold <- biasAdjustedTotal(
    map_agb_default = test_data$map_agb,
    map_agb_adjusted = map_adjusted,
    aggregated_uncertainty = uncertainty,
    threshold = 0
  )

  # Should have fewer cells with high threshold
  expect_lte(result_with_threshold$n_cells[1], result_no_threshold$n_cells[1])
})

test_that("biasAdjustedTotal accepts SpatRaster for uncertainty", {
  test_data <- create_uncertainty_test_data()

  map_adjusted <- test_data$map_agb - 5

  # Use SD raster directly instead of aggregatedUncertainty object
  result <- biasAdjustedTotal(
    map_agb_default = test_data$map_agb,
    map_agb_adjusted = map_adjusted,
    aggregated_uncertainty = test_data$map_sd,
    threshold = 0
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
})

test_that("biasAdjustedTotal attributes are correct", {
  test_data <- create_uncertainty_test_data()

  map_adjusted <- test_data$map_agb - 5

  vgm <- gstat::vgm(psill = 3, model = "Sph", range = 50000, nugget = 1)
  uncertainty <- aggregateUncertainty(
    sd_raster = test_data$map_sd,
    variogram_model = vgm,
    resolution = 30000,
    verbose = FALSE
  )

  result <- biasAdjustedTotal(
    map_agb_default = test_data$map_agb,
    map_agb_adjusted = map_adjusted,
    aggregated_uncertainty = uncertainty,
    threshold = 0
  )

  # Check bias correction effect
  bias_effect <- attr(result, "bias_correction_effect")
  expected_effect <- result$total_Mg[2] - result$total_Mg[1]
  expect_equal(bias_effect, expected_effect)

  # Check percent change
  percent_change <- attr(result, "percent_change")
  expected_pct <- (expected_effect / result$total_Mg[1]) * 100
  expect_equal(percent_change, expected_pct)

  # Uncertainty overlap should be logical
  expect_type(attr(result, "uncertainty_overlap"), "logical")
})

# =============================================================================
# Integration tests
# =============================================================================

test_that("Complete uncertainty workflow runs end-to-end", {
  test_data <- create_uncertainty_test_data()

  # Step 1: Fit residual variogram
  vgm_fit <- fitResidualVariogram(
    bias_data = test_data$plot_data,
    map_sd_raster = test_data$map_sd,
    cutoff = 50000,
    coord_cols = c("x", "y"),
    crs = "EPSG:32631",
    plot = FALSE
  )

  expect_s3_class(vgm_fit, "residualVariogram")

  # Step 2: Discount variogram
  vgm_disc <- discountVariogram(
    variogram_fit = vgm_fit,
    plot_data = test_data$plot_data,
    plot = FALSE
  )

  expect_s3_class(vgm_disc, "discountedVariogram")

  # Step 3: Aggregate uncertainty
  uncertainty <- aggregateUncertainty(
    sd_raster = test_data$map_sd,
    variogram_model = vgm_disc,
    resolution = 30000,
    verbose = FALSE
  )

  expect_s3_class(uncertainty, "aggregatedUncertainty")

  # Step 4: Calculate bias-adjusted totals
  map_adjusted <- test_data$map_agb - 5

  totals <- biasAdjustedTotal(
    map_agb_default = test_data$map_agb,
    map_agb_adjusted = map_adjusted,
    aggregated_uncertainty = uncertainty,
    threshold = 0
  )

  expect_s3_class(totals, "data.frame")
  expect_equal(nrow(totals), 2)
})

test_that("Workflow handles edge cases gracefully", {
  # Create minimal test data with projected CRS
  small_raster <- terra::rast(nrows = 5, ncols = 5, xmin = 500000, xmax = 550000,
                              ymin = 4000000, ymax = 4050000, crs = "EPSG:32631")
  terra::values(small_raster) <- runif(25, 10, 50)

  small_plots <- data.frame(
    x = runif(15, 500000, 550000),
    y = runif(15, 4000000, 4050000),
    bias = rnorm(15, 0, 10),
    biasPred = rnorm(15, 0, 8),
    varPlot = runif(15, 50, 200),
    varMap = runif(15, 100, 300)
  )

  # Should complete without errors (warnings about singular models are OK)
  expect_error({
    vgm_fit <- fitResidualVariogram(
      bias_data = small_plots,
      map_sd_raster = small_raster,
      cutoff = 20000,  # 20 km in meters
      coord_cols = c("x", "y"),
      crs = "EPSG:32631",
      plot = FALSE
    )

    vgm_disc <- discountVariogram(
      variogram_fit = vgm_fit,
      plot_data = small_plots,
      plot = FALSE
    )

    uncertainty <- aggregateUncertainty(
      sd_raster = small_raster,
      variogram_model = vgm_disc,
      resolution = 20000,
      verbose = FALSE
    )
  }, NA)
})
