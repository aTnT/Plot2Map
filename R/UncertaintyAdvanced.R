## Updates made:
# 13/11/25:
# Initial implementation:
# - fitResidualVariogram: Fit variogram to bias model residuals with automatic CRS handling
# - discountVariogram: Discount variogram nugget to remove plot-level variance
# - aggregateUncertainty: Aggregate pixel-level uncertainty with spatial correlation
# - biasAdjustedTotal: Calculate regional totals with confidence intervals
# All functions use sf/gstat workflow 
# Fixed threshold parameter: changed from AGB values to tree cover % for consistency with Plot2Map


#' Fit Variogram to Residuals After Bias Modeling
#'
#' Fits a variogram model to the scaled residuals from bias modeling to characterize
#' spatial autocorrelation in the remaining prediction errors. This is a critical step
#' for uncertainty quantification as it allows proper aggregation of standard errors
#' accounting for spatial correlation.
#'
#' @param bias_data A data.frame with plot locations and bias modeling results.
#'   Must contain columns: \code{bias} (observed bias), \code{biasPred} (predicted bias
#'   from \code{trainBiasModel()}), coordinates (lon/lat or x/y), and will be joined
#'   with SD values from \code{map_sd_raster}.
#' @param map_sd_raster SpatRaster containing pixel-level standard deviation of AGB
#'   (Mg/ha). Used to scale residuals: resid = (bias - biasPred) / sd.
#' @param model Character. Variogram model type to fit. Options: "Sph" (Spherical,
#'   default), "Exp" (Exponential), "Gau" (Gaussian), "Mat" (Matern). See
#'   \code{gstat::vgm()} for details.
#' @param cutoff Numeric. Maximum distance in kilometers for variogram calculation.
#'   Default: 50. Should be roughly half the maximum distance between plots.
#'   The function automatically converts to meters after UTM transformation.
#' @param width Numeric. Width of distance bins in kilometers for empirical variogram.
#'   Default: \code{cutoff/15}. Smaller values give more detail but more noise.
#' @param psill Numeric. Initial partial sill for variogram fitting. Default: 3.
#'   Adjust based on variance of scaled residuals.
#' @param range Numeric. Initial range parameter in kilometers. Default: 10.
#'   Represents distance at which correlation becomes negligible.
#' @param nugget Numeric. Initial nugget (y-intercept) for variogram. Default: 4.
#'   Represents measurement error and micro-scale variation.
#' @param coord_cols Character vector of length 2. Column names for coordinates in
#'   \code{bias_data}. Default: \code{c("lon", "lat")}. Use \code{c("x", "y")} if
#'   data are already projected.
#' @param crs Character or numeric. Coordinate reference system for \code{bias_data}.
#'   Default: "EPSG:4326" (WGS84). Must match \code{map_sd_raster} CRS or be
#'   transformable to it.
#' @param remove_outliers Logical. Should extreme residuals (|resid| > 3) be removed
#'   before variogram fitting? Default: TRUE. Helps prevent fitting issues.
#' @param plot Logical. Should a diagnostic plot be generated? Default: TRUE. Shows
#'   empirical variogram points and fitted model curve.
#'
#' @return A list with class "residualVariogram" containing:
#'   \describe{
#'     \item{variogram_model}{gstat vgm object with fitted variogram parameters}
#'     \item{empirical_variogram}{gstat variogram object with empirical semivariances}
#'     \item{residuals}{data.frame with columns: coordinates, bias, biasPred, sd,
#'       resid0 (raw residuals), resid (scaled residuals)}
#'     \item{resid_sf}{sf object with residuals (used for variogram fitting)}
#'     \item{fit_quality}{data.frame with fitting diagnostics: SSErr (sum of squared
#'       errors), RMSE, R2 (pseudo-R²)}
#'     \item{n_obs}{Integer. Number of observations used in fitting}
#'     \item{n_removed}{Integer. Number of outliers removed (if remove_outliers=TRUE)}
#'     \item{plot}{ggplot object if plot=TRUE, NULL otherwise}
#'   }
#'
#' @details
#' This function implements a residual variogram fitting approach with the following workflow:
#'
#' \enumerate{
#'   \item Extract SD values at plot locations from \code{map_sd_raster}
#'   \item Calculate raw residuals: resid0 = bias - biasPred
#'   \item Scale residuals by map SD: resid = resid0 / sd
#'   \item Optionally remove outliers (|resid| > 3)
#'   \item Convert to sf object for gstat
#'   \item Fit empirical variogram with \code{gstat::variogram()}
#'   \item Fit theoretical variogram model with \code{gstat::fit.variogram()}
#' }
#'
#' \strong{Scaled residuals:} Scaling by SD is critical because it standardizes
#' residuals to unitless values, allowing variogram fitting across regions with
#' different absolute error magnitudes. The fitted variogram will be used to
#' create correlation matrices for uncertainty aggregation.
#'
#' \strong{Model selection:} The default "Sph" (Spherical) model is appropriate for
#' most ecological data as it reaches a finite sill at a defined range. Use "Exp"
#' (Exponential) if correlation decays more gradually, or "Gau" (Gaussian) for very
#' smooth spatial processes.
#'
#' \strong{Initial parameters:} The psill, range, and nugget parameters are starting
#' values for nonlinear optimization. Poor initial values can lead to convergence
#' issues. Inspect the empirical variogram first to choose reasonable starting values.
#'
#' @references
#' Araza, A., de Bruin, S., Herold, M., Quegan, S., Labrière, N., Rodriguez-Veiga, P.,
#' ... & Burt, A. (2022). A comprehensive framework for assessing the accuracy and
#' uncertainty of global above-ground biomass maps. Remote Sensing of Environment, 272, 112917.
#'
#' Christensen, W. F. (2011). Filtered kriging for spatial data with heterogeneous
#' measurement error variances. Biometrics, 67(3), 947-957.
#'
#' @examples
#' \dontrun{
#' # Assuming you've run bias modeling workflow:
#' # 1. Extract covariates and calculate bias
#' bias_data <- extractBiasCovariates(
#'   plot_data = plots,
#'   map_agb = agb_map,
#'   map_sd = sd_map,
#'   covariates = list(height = height_raster, treecover = tc_raster)
#' )
#'
#' # 2. Train bias model
#' bias_model <- trainBiasModel(
#'   bias_data = bias_data,
#'   predictors = c("map", "sd", "height", "treecover"),
#'   cv_folds = 10
#' )
#'
#' # 3. Add predictions to bias_data
#' bias_data$biasPred <- predict(bias_model$model, data = bias_data)
#'
#' # 4. Fit residual variogram
#' vgm_fit <- fitResidualVariogram(
#'   bias_data = bias_data,
#'   map_sd_raster = sd_map,
#'   model = "Sph",
#'   cutoff = 50,
#'   psill = 3,
#'   range = 10,
#'   nugget = 4,
#'   coord_cols = c("x", "y"),
#'   crs = "EPSG:4326"
#' )
#'
#' # Inspect results
#' print(vgm_fit$variogram_model)
#' print(vgm_fit$fit_quality)
#' print(vgm_fit$plot)
#'
#' # Check residual distribution
#' hist(vgm_fit$residuals$resid, main = "Scaled Residuals")
#' summary(vgm_fit$residuals$resid)
#' }
#'
#' @seealso
#' \code{\link{trainBiasModel}} for bias model training,
#' \code{\link{discountVariogram}} for variogram nugget adjustment,
#' \code{\link{aggregateUncertainty}} for spatial uncertainty aggregation
#'
#' @export
#' @importFrom terra extract crs
#' @importFrom sf st_as_sf st_transform st_crs st_coordinates st_bbox st_is_longlat
#' @importFrom gstat variogram vgm fit.variogram variogramLine
#' @importFrom stats var
fitResidualVariogram <- function(bias_data,
                                  map_sd_raster,
                                  model = "Sph",
                                  cutoff = 50,
                                  width = cutoff / 15,
                                  psill = 3,
                                  range = 10,
                                  nugget = 4,
                                  coord_cols = c("lon", "lat"),
                                  crs = "EPSG:4326",
                                  remove_outliers = TRUE,
                                  plot = TRUE) {

  # Input validation
  if (!inherits(bias_data, "data.frame")) {
    stop("bias_data must be a data.frame")
  }

  if (!inherits(map_sd_raster, "SpatRaster")) {
    stop("map_sd_raster must be a SpatRaster")
  }

  required_cols <- c("bias", "biasPred", coord_cols)
  missing_cols <- setdiff(required_cols, names(bias_data))
  if (length(missing_cols) > 0) {
    stop("bias_data missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (!model %in% c("Sph", "Exp", "Gau", "Mat", "Lin")) {
    stop("model must be one of: Sph, Exp, Gau, Mat, Lin")
  }

  if (cutoff <= 0 || !is.numeric(cutoff)) {
    stop("cutoff must be a positive numeric value")
  }

  # Create a copy to avoid modifying input
  residual_data <- bias_data

  # Extract SD values at plot locations first (before creating sf)
  # Create temporary coordinates matrix
  coords_matrix <- as.matrix(residual_data[, coord_cols])
  sd_values <- terra::extract(map_sd_raster, coords_matrix)

  if (ncol(sd_values) != 1) {
    stop("map_sd_raster must contain exactly one layer")
  }

  residual_data$sd <- sd_values[, 1]

  # Calculate raw residuals
  residual_data$resid0 <- residual_data$bias - residual_data$biasPred

  # Calculate scaled residuals
  residual_data$resid <- residual_data$resid0 / residual_data$sd

  # Remove plots with invalid values
  bad_idx <- is.na(residual_data$sd) | residual_data$sd <= 0 |
    is.na(residual_data$resid) | is.infinite(residual_data$resid)

  if (any(bad_idx)) {
    warning(sum(bad_idx), " plots removed due to missing, zero, or invalid values")
    residual_data <- residual_data[!bad_idx, ]
  }

  if (nrow(residual_data) < 10) {
    stop("Insufficient valid observations for variogram fitting (n = ",
         nrow(residual_data), ", need at least 10)")
  }

  # Remove outliers if requested
  n_removed <- 0
  if (remove_outliers) {
    outlier_idx <- abs(residual_data$resid) > 3
    n_removed <- sum(outlier_idx)
    if (n_removed > 0) {
      residual_data <- residual_data[!outlier_idx, ]
      message(n_removed, " outliers removed (|resid| > 3)")
    }
  }

  # Convert to sf object for CRS handling and storage
  residual_sf <- sf::st_as_sf(
    residual_data,
    coords = coord_cols,
    crs = crs,
    agr = "constant"  # Attribute-geometry relationship
  )

  # Transform to match raster CRS
  raster_crs <- sf::st_crs(terra::crs(map_sd_raster))
  if (!sf::st_crs(residual_sf) == raster_crs) {
    residual_sf <- sf::st_transform(residual_sf, crs = raster_crs)
  }

  # Check if CRS is geographic (lon/lat) and transform to projected if needed
  # gstat::variogram requires projected coordinates (not geographic)
  if (sf::st_is_longlat(residual_sf)) {
    # Transform to appropriate UTM zone based on centroid
    bbox <- sf::st_bbox(residual_sf)
    lon_center <- mean(c(bbox["xmin"], bbox["xmax"]))

    # Calculate UTM zone
    utm_zone <- floor((lon_center + 180) / 6) + 1
    utm_crs <- ifelse(mean(sf::st_coordinates(residual_sf)[, 2]) >= 0,
                      paste0("EPSG:326", utm_zone),  # Northern hemisphere
                      paste0("EPSG:327", utm_zone))  # Southern hemisphere

    message("Transforming to projected CRS (", utm_crs, ") for variogram calculation")
    residual_sf <- sf::st_transform(residual_sf, crs = utm_crs)
  }

  # Convert cutoff and width to meters if needed (after UTM transform)
  # User provides cutoff in km, but after UTM projection distances are in meters
  cutoff_m <- cutoff * 1000
  width_m <- width * 1000

  # Fit empirical variogram (gstat works directly with sf objects)
  empirical_vgm <- tryCatch({
    gstat::variogram(
      resid ~ 1,
      data = residual_sf,
      cutoff = cutoff_m,
      width = width_m
    )
  }, error = function(e) {
    stop("Empirical variogram calculation failed: ", e$message,
         "\nCheck that you have enough data points (n = ", nrow(residual_sf),
         ") with sufficient spatial spread.")
  })

  # Check if variogram has enough bins
  if (nrow(empirical_vgm) < 3) {
    stop("Insufficient distance bins in empirical variogram (", nrow(empirical_vgm),
         " bins). Try increasing cutoff or decreasing width.")
  }

  # Create initial variogram model
  # Convert range to meters (user provides in km)
  range_m <- range * 1000

  initial_vgm <- gstat::vgm(
    psill = psill,
    model = model,
    range = range_m,
    nugget = nugget
  )

  # Fit theoretical variogram model
  fitted_vgm <- tryCatch(
    {
      gstat::fit.variogram(empirical_vgm, initial_vgm)
    },
    error = function(e) {
      stop("Variogram fitting failed: ", e$message,
           "\nTry adjusting initial parameters (psill, range, nugget) or model type")
    }
  )

  # Calculate fit quality metrics
  # Predict semivariance at empirical distances
  pred_gamma <- gstat::variogramLine(fitted_vgm, dist_vector = empirical_vgm$dist)$gamma
  obs_gamma <- empirical_vgm$gamma

  ss_err <- sum((obs_gamma - pred_gamma)^2)
  ss_tot <- sum((obs_gamma - mean(obs_gamma))^2)
  r2 <- 1 - (ss_err / ss_tot)
  rmse <- sqrt(mean((obs_gamma - pred_gamma)^2))

  fit_quality <- data.frame(
    SSErr = ss_err,
    RMSE = rmse,
    R2 = r2
  )

  # Create diagnostic plot if requested
  vgm_plot <- NULL
  if (plot) {
    # Create plot using base graphics (compatible with existing Plot2Map style)
    vgm_plot <- function() {
      # Convert distances from meters to km for display
      plot(empirical_vgm$dist / 1000, empirical_vgm$gamma,
           pch = 16, col = "black", cex = 1.5,
           xlab = "Distance (km)", ylab = "Semivariance",
           main = paste0("Residual Variogram (", model, " model)"),
           ylim = c(0, max(empirical_vgm$gamma) * 1.1))

      # Add fitted line
      pred_dist <- seq(0, max(empirical_vgm$dist), length.out = 100)
      pred_vgm <- gstat::variogramLine(fitted_vgm, dist_vector = pred_dist)
      lines(pred_vgm$dist / 1000, pred_vgm$gamma, col = "red", lwd = 2)

      # Add legend with parameters
      nugget_val <- fitted_vgm$psill[1]
      sill_val <- sum(fitted_vgm$psill)
      range_val <- fitted_vgm$range[2] / 1000  # Convert to km for display

      legend("bottomright",
             legend = c(
               paste0("Nugget: ", round(nugget_val, 3)),
               paste0("Sill: ", round(sill_val, 3)),
               paste0("Range: ", round(range_val, 2), " km"),
               paste0("RMSE: ", round(rmse, 3)),
               paste0("R²: ", round(r2, 3))
             ),
             bty = "n", cex = 0.9)
    }
  }

  # Return structured list
  result <- list(
    variogram_model = fitted_vgm,
    empirical_variogram = empirical_vgm,
    residuals = residual_data,
    resid_sf = residual_sf,
    fit_quality = fit_quality,
    n_obs = nrow(residual_data),
    n_removed = n_removed,
    plot = vgm_plot
  )

  class(result) <- c("residualVariogram", "list")
  return(result)
}


#' Discount Variogram Nugget for Plot Measurement Error
#'
#' Adjusts the variogram nugget to account for plot-level measurement uncertainty
#' using the method of Christensen (2011). This is essential for proper uncertainty
#' quantification as it separates spatial variation (which aggregates differently)
#' from measurement error (which does not).
#'
#' @param variogram_fit A residualVariogram object from \code{fitResidualVariogram()},
#'   or a gstat vgm object to be discounted.
#' @param plot_data A data.frame with plot-level variance estimates. Must contain
#'   columns: \code{varPlot} (plot measurement variance in (Mg/ha)²) and
#'   \code{varMap} (map pixel variance in (Mg/ha)²). Typically obtained from
#'   \code{calculateTotalUncertainty()} or similar.
#' @param filter_iqr Logical. Should plots be filtered by IQR to remove extreme
#'   variance ratios? Default: TRUE. Christensen (2011) recommends this to avoid
#'   bias from outliers.
#' @param iqr_mult Numeric. IQR multiplier for outlier detection. Default: 1.5.
#'   Plots with varPlot/varMap² outside the range
#'   \code{[Q1 - iqr_mult * IQR, Q3 + iqr_mult * IQR]} are removed.
#' @param transfer_negative Logical. Should negative nugget values be transferred to
#'   the partial sill? Default: TRUE. Prevents invalid variogram models when
#'   discounting reduces nugget below zero.
#' @param plot Logical. Should a comparison plot be generated? Default: TRUE. Shows
#'   original vs. discounted variogram models.
#'
#' @return A list with class "discountedVariogram" containing:
#'   \describe{
#'     \item{discounted_variogram}{gstat vgm object with adjusted nugget}
#'     \item{original_variogram}{gstat vgm object before discounting}
#'     \item{discount_factor}{Numeric. The discount value subtracted from nugget:
#'       mean(varPlot / varMap²)}
#'     \item{nugget_original}{Numeric. Nugget before discounting}
#'     \item{nugget_discounted}{Numeric. Nugget after discounting (≥ 0)}
#'     \item{nugget_transferred}{Numeric. Amount transferred to partial sill if
#'       nugget would have been negative}
#'     \item{n_plots}{Integer. Number of plots used in calculation}
#'     \item{n_filtered}{Integer. Number of plots removed by IQR filtering}
#'     \item{plot}{Function to generate comparison plot if plot=TRUE, NULL otherwise}
#'   }
#'
#' @details
#' This function implements the variogram discounting method from Christensen (2011)
#' as applied in Section 5 of the Mexico demo notebook (lines 1177-1213).
#'
#' \strong{Christensen (2011) method:}
#'
#' The variogram nugget includes both true micro-scale spatial variation and plot
#' measurement error. To isolate spatial variation, we calculate:
#'
#' \deqn{discount = mean(varPlot / varMap^2)}{discount = mean(varPlot / varMap^2)}
#'
#' And adjust the nugget:
#'
#' \deqn{nugget_{new} = nugget_{old} - discount}{nugget_new = nugget_old - discount}
#'
#' \strong{IQR filtering:} The variance ratio varPlot/varMap² can have extreme
#' values for plots with very low map variance or unusually high plot variance.
#' Christensen (2011) recommends filtering by the interquartile range (IQR) to
#' remove these outliers before calculating the mean discount factor.
#'
#' \strong{Negative nugget handling:} If discounting results in a negative nugget,
#' the nugget is set to zero and the negative amount is added to the partial sill.
#' This ensures the variogram model remains valid while preserving total sill.
#'
#' \strong{Interpretation:} A large discount factor indicates that plot measurement
#' error is a substantial component of the original nugget. After discounting, the
#' remaining nugget represents true micro-scale spatial variation plus any remaining
#' measurement error components.
#'
#' @references
#' Christensen, W. F. (2011). Filtered kriging for spatial data with heterogeneous
#' measurement error variances. Biometrics, 67(3), 947-957.
#'
#' @examples
#' \dontrun{
#' # Assuming you have a fitted variogram and plot variance data:
#' vgm_fit <- fitResidualVariogram(
#'   bias_data = bias_data,
#'   map_sd_raster = sd_map,
#'   coord_cols = c("x", "y"),
#'   crs = "EPSG:4326"
#' )
#'
#' # Calculate plot and map variances (example)
#' variance_data <- data.frame(
#'   varPlot = bias_data$sdPlot^2,  # From field measurement uncertainty
#'   varMap = bias_data$sd^2         # From map pixel SD
#' )
#'
#' # Discount the variogram
#' vgm_discounted <- discountVariogram(
#'   variogram_fit = vgm_fit,
#'   plot_data = variance_data,
#'   filter_iqr = TRUE,
#'   iqr_mult = 1.5
#' )
#'
#' # Inspect results
#' print(vgm_discounted$discount_factor)
#' print(vgm_discounted$discounted_variogram)
#'
#' # Show comparison plot
#' if (!is.null(vgm_discounted$plot)) {
#'   vgm_discounted$plot()
#' }
#' }
#'
#' @seealso
#' \code{\link{fitResidualVariogram}} for variogram fitting,
#' \code{\link{aggregateUncertainty}} for using discounted variogram in aggregation
#'
#' @export
discountVariogram <- function(variogram_fit,
                               plot_data,
                               filter_iqr = TRUE,
                               iqr_mult = 1.5,
                               transfer_negative = TRUE,
                               plot = TRUE) {

  # Input validation
  if (inherits(variogram_fit, "residualVariogram")) {
    original_vgm <- variogram_fit$variogram_model
  } else if (inherits(variogram_fit, "variogramModel")) {
    original_vgm <- variogram_fit
  } else {
    stop("variogram_fit must be a residualVariogram object or gstat vgm object")
  }

  if (!inherits(plot_data, "data.frame")) {
    stop("plot_data must be a data.frame")
  }

  required_cols <- c("varPlot", "varMap")
  missing_cols <- setdiff(required_cols, names(plot_data))
  if (length(missing_cols) > 0) {
    stop("plot_data missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Remove NA and zero variance values
  valid_idx <- !is.na(plot_data$varPlot) &
    !is.na(plot_data$varMap) &
    plot_data$varMap > 0 &
    plot_data$varPlot > 0

  if (!any(valid_idx)) {
    stop("No valid variance pairs (varPlot > 0 and varMap > 0)")
  }

  variance_data <- plot_data[valid_idx, ]
  n_removed_na <- sum(!valid_idx)
  if (n_removed_na > 0) {
    message(n_removed_na, " plots removed due to NA or zero variances")
  }

  # Calculate variance ratio
  variance_data$ratio <- variance_data$varPlot / (variance_data$varMap^2)

  # Filter by IQR if requested
  n_filtered <- 0
  if (filter_iqr) {
    q1 <- quantile(variance_data$ratio, 0.25, na.rm = TRUE)
    q3 <- quantile(variance_data$ratio, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - iqr_mult * iqr
    upper_bound <- q3 + iqr_mult * iqr

    iqr_idx <- variance_data$ratio >= lower_bound &
      variance_data$ratio <= upper_bound

    n_filtered <- sum(!iqr_idx)

    if (n_filtered > 0) {
      message(n_filtered, " plots removed by IQR filtering")
      variance_data <- variance_data[iqr_idx, ]
    }
  }

  if (nrow(variance_data) < 5) {
    stop("Insufficient plots remaining after filtering (n = ",
         nrow(variance_data), ", need at least 5)")
  }

  # Calculate discount factor (Christensen 2011)
  discount_factor <- mean(variance_data$ratio, na.rm = TRUE)

  # Extract original nugget
  nugget_idx <- which(original_vgm$model == "Nug")
  if (length(nugget_idx) == 0) {
    stop("Original variogram has no nugget component. Cannot discount.")
  }

  nugget_original <- original_vgm$psill[nugget_idx]

  # Calculate discounted nugget
  nugget_discounted <- nugget_original - discount_factor

  # Handle negative nugget
  nugget_transferred <- 0
  if (nugget_discounted < 0 && transfer_negative) {
    nugget_transferred <- -nugget_discounted
    nugget_discounted <- 0
    message("Negative nugget (", round(nugget_original - discount_factor, 4),
            ") transferred to partial sill")
  } else if (nugget_discounted < 0) {
    warning("Discounted nugget is negative (", round(nugget_discounted, 4),
            "). Consider setting transfer_negative=TRUE or reducing discount.")
    nugget_discounted <- 0
  }

  # Create discounted variogram
  discounted_vgm <- original_vgm
  discounted_vgm$psill[nugget_idx] <- nugget_discounted

  # Transfer negative nugget to partial sill if requested
  if (nugget_transferred > 0) {
    # Find first non-nugget component (partial sill)
    sill_idx <- which(original_vgm$model != "Nug")[1]
    if (!is.na(sill_idx)) {
      discounted_vgm$psill[sill_idx] <- discounted_vgm$psill[sill_idx] +
        nugget_transferred
    }
  }

  # Create comparison plot if requested
  comparison_plot <- NULL
  if (plot) {
    comparison_plot <- function() {
      # Plot variogram comparison
      max_dist <- max(original_vgm$range[!is.na(original_vgm$range)]) * 1.5
      dist_seq <- seq(0, max_dist, length.out = 100)

      original_line <- gstat::variogramLine(original_vgm, dist_vector = dist_seq)
      discounted_line <- gstat::variogramLine(discounted_vgm, dist_vector = dist_seq)

      y_max <- max(original_line$gamma, discounted_line$gamma) * 1.1

      plot(original_line$dist, original_line$gamma,
           type = "l", col = "blue", lwd = 2,
           xlab = "Distance (km)", ylab = "Semivariance",
           main = "Variogram Comparison: Original vs. Discounted",
           ylim = c(0, y_max))

      lines(discounted_line$dist, discounted_line$gamma,
            col = "red", lwd = 2, lty = 2)

      # Add horizontal lines for nugget values
      abline(h = nugget_original, col = "blue", lty = 3)
      abline(h = nugget_discounted, col = "red", lty = 3)

      legend("bottomright",
             legend = c(
               "Original variogram",
               "Discounted variogram",
               paste0("Original nugget: ", round(nugget_original, 3)),
               paste0("Discounted nugget: ", round(nugget_discounted, 3)),
               paste0("Discount factor: ", round(discount_factor, 4)),
               paste0("n = ", nrow(variance_data))
             ),
             col = c("blue", "red", "blue", "red", "black", "black"),
             lty = c(1, 2, 3, 3, 0, 0),
             lwd = c(2, 2, 1, 1, 0, 0),
             bty = "n", cex = 0.9)
    }
  }

  # Return structured list
  result <- list(
    discounted_variogram = discounted_vgm,
    original_variogram = original_vgm,
    discount_factor = discount_factor,
    nugget_original = nugget_original,
    nugget_discounted = nugget_discounted,
    nugget_transferred = nugget_transferred,
    n_plots = nrow(variance_data),
    n_filtered = n_filtered,
    plot = comparison_plot
  )

  class(result) <- c("discountedVariogram", "list")
  return(result)
}


#' Aggregate Uncertainty Accounting for Spatial Correlation
#'
#' Aggregates pixel-level standard deviation to regional scale while accounting for
#' spatial correlation using a variogram-based correlation matrix. This is critical
#' for accurate uncertainty quantification as ignoring spatial correlation leads to
#' underestimation of regional-scale uncertainty.
#'
#' @param sd_raster SpatRaster containing pixel-level standard deviation of AGB (Mg/ha).
#'   Must have same extent and resolution as the AGB map.
#' @param variogram_model A discountedVariogram object from \code{discountVariogram()},
#'   or a gstat vgm object containing the fitted (and optionally discounted) variogram
#'   parameters. Used to create the spatial correlation matrix.
#' @param resolution Numeric. Resolution (in degrees or CRS units) for aggregation.
#'   Default: 0.1 (roughly 10 km at equator). Defines the window size for focal
#'   aggregation. Should match the scale of regional reporting units.
#' @param forest_mask Optional SpatRaster with forest/non-forest mask. Only forest
#'   pixels (mask > 0) will be included in aggregation. Default: NULL (all pixels used).
#' @param filename Optional. File path to save aggregated SD raster. Default: NULL
#'   (raster kept in memory).
#' @param return_comparison Logical. Should comparison statistics (with vs. without
#'   correlation) be calculated? Default: TRUE. Useful for assessing the impact of
#'   spatial correlation on regional uncertainty.
#' @param scale_factor Numeric. Conversion factor from pixel units to regional totals.
#'   Default: 10000 (converts 100m pixels to hectares when using 0.1° aggregation).
#'   Used to scale SD from mean per-pixel values to regional total values.
#' @param verbose Logical. Print progress messages? Default: TRUE.
#'
#' @return A list with class "aggregatedUncertainty" containing:
#'   \describe{
#'     \item{aggregated_sd_raster}{SpatRaster with aggregated SD values accounting
#'       for spatial correlation (Mg)}
#'     \item{sd_no_correlation}{SpatRaster with aggregated SD assuming independence
#'       (Mg). Only if return_comparison=TRUE}
#'     \item{correlation_matrix}{Matrix used for focal window correlation weighting}
#'     \item{regional_sd_correlated}{Numeric. Total regional SD accounting for
#'       correlation (Mg)}
#'     \item{regional_sd_independent}{Numeric. Total regional SD assuming independence
#'       (Mg). Only if return_comparison=TRUE}
#'     \item{correlation_reduction_factor}{Numeric. Ratio of correlated to independent
#'       SD. Values < 1 indicate spatial correlation reduces total uncertainty.
#'       Only if return_comparison=TRUE}
#'     \item{n_cells}{Integer. Number of cells included in aggregation}
#'     \item{resolution}{Numeric. Aggregation resolution used}
#'   }
#'
#' @details
#' This function implements the focal window correlation approach from Section 5 of
#' the Mexico demo notebook (lines 1215-1305). The workflow is:
#'
#' \enumerate{
#'   \item Create correlation matrix from variogram using \code{corMatrix()} helper
#'   \item Apply forest mask if provided
#'   \item Use \code{terra::focal()} with correlation matrix as weights
#'   \item Calculate weighted sum: sum(SD_i * SD_j * cor_ij)
#'   \item Take square root to get aggregated SD
#'   \item Scale to regional totals using scale_factor
#' }
#'
#' \strong{Spatial correlation and aggregation:}
#'
#' When aggregating uncertainty from pixels to regions, we must account for spatial
#' correlation. The variance of a sum of correlated variables is:
#'
#' \deqn{Var(\sum X_i) = \sum_i \sum_j SD_i \times SD_j \times \rho_{ij}}{Var(sum X_i) = sum_i sum_j SD_i * SD_j * rho_ij}
#'
#' If correlation is ignored (independence assumption), this reduces to:
#'
#' \deqn{Var(\sum X_i) = \sum_i SD_i^2}{Var(sum X_i) = sum_i SD_i^2}
#'
#' Positive spatial correlation increases regional variance compared to the
#' independence assumption, because nearby pixels tend to have errors in the same
#' direction.
#'
#' \strong{Correlation matrix:}
#'
#' The correlation matrix is created from the variogram using the helper function
#' \code{corMatrix()}, which converts semivariance γ(h) to correlation ρ(h):
#'
#' \deqn{\rho(h) = 1 - \gamma(h) / sill}{rho(h) = 1 - gamma(h) / sill}
#'
#' This matrix defines the correlation weights for the focal window aggregation.
#'
#' \strong{Resolution and scale:}
#'
#' The resolution parameter should match your reporting units (e.g., 0.1° for regional
#' estimates, 1° for country-level). The scale_factor converts from pixel-level SD
#' to regional total SD based on pixel size and aggregation area.
#'
#' @references
#' Araza, A., de Bruin, S., Herold, M., Quegan, S., Labrière, N., Rodriguez-Veiga, P.,
#' ... & Burt, A. (2022). A comprehensive framework for assessing the accuracy and
#' uncertainty of global above-ground biomass maps. Remote Sensing of Environment, 272, 112917.
#'
#' @examples
#' \dontrun{
#' # Assuming you have a discounted variogram:
#' vgm_discounted <- discountVariogram(vgm_fit, variance_data)
#'
#' # Aggregate uncertainty with spatial correlation
#' uncertainty_agg <- aggregateUncertainty(
#'   sd_raster = sd_map,
#'   variogram_model = vgm_discounted,
#'   resolution = 0.1,
#'   forest_mask = forest_raster,
#'   return_comparison = TRUE
#' )
#'
#' # Inspect results
#' print(uncertainty_agg$regional_sd_correlated)
#' print(uncertainty_agg$regional_sd_independent)
#' print(uncertainty_agg$correlation_reduction_factor)
#'
#' # Plot aggregated SD
#' terra::plot(uncertainty_agg$aggregated_sd_raster,
#'             main = "Aggregated SD (with correlation)")
#' }
#'
#' @seealso
#' \code{\link{discountVariogram}} for variogram discounting,
#' \code{\link{biasAdjustedTotal}} for calculating regional totals with uncertainty
#'
#' @export
#' @importFrom terra focal global mask res
#' @importFrom gstat variogramLine
aggregateUncertainty <- function(sd_raster,
                                  variogram_model,
                                  resolution = 0.1,
                                  forest_mask = NULL,
                                  filename = NULL,
                                  return_comparison = TRUE,
                                  scale_factor = 10000,
                                  verbose = TRUE) {

  # Input validation
  if (!inherits(sd_raster, "SpatRaster")) {
    stop("sd_raster must be a SpatRaster")
  }

  if (inherits(variogram_model, "discountedVariogram")) {
    vgm <- variogram_model$discounted_variogram
  } else if (inherits(variogram_model, "residualVariogram")) {
    vgm <- variogram_model$variogram_model
  } else if (inherits(variogram_model, "variogramModel")) {
    vgm <- variogram_model
  } else {
    stop("variogram_model must be a discountedVariogram, residualVariogram, or gstat vgm object")
  }

  if (resolution <= 0 || !is.numeric(resolution)) {
    stop("resolution must be a positive numeric value")
  }

  if (!is.null(forest_mask) && !inherits(forest_mask, "SpatRaster")) {
    stop("forest_mask must be a SpatRaster or NULL")
  }

  # Apply forest mask if provided
  if (!is.null(forest_mask)) {
    if (verbose) message("Applying forest mask...")
    sd_raster <- terra::mask(sd_raster, forest_mask, maskvalues = 0)
  }

  # Create correlation matrix from variogram
  if (verbose) message("Creating correlation matrix from variogram...")

  # Helper function to create correlation matrix
  corMatrix <- function(resolution, vgm) {
    # Get pixel resolution from raster
    pixel_res <- terra::res(sd_raster)[1]

    # Calculate window size in pixels
    window_size <- ceiling(resolution / pixel_res)
    if (window_size %% 2 == 0) window_size <- window_size + 1  # Ensure odd

    # Create distance matrix for focal window
    half_win <- floor(window_size / 2)
    x <- seq(-half_win, half_win) * pixel_res
    y <- seq(-half_win, half_win) * pixel_res
    dist_grid <- outer(x, y, function(x, y) sqrt(x^2 + y^2))

    # Get sill from variogram
    sill <- sum(vgm$psill)

    # Convert distances to semivariances using variogram model
    dist_vec <- as.vector(dist_grid)
    vgm_line <- gstat::variogramLine(vgm, dist_vector = dist_vec)
    gamma_matrix <- matrix(vgm_line$gamma, nrow = window_size, ncol = window_size)

    # Convert semivariance to correlation: ρ(h) = 1 - γ(h) / sill
    cor_matrix <- 1 - (gamma_matrix / sill)

    # Ensure correlation is between 0 and 1
    cor_matrix[cor_matrix < 0] <- 0
    cor_matrix[cor_matrix > 1] <- 1

    return(cor_matrix)
  }

  cor_matrix <- corMatrix(resolution, vgm)

  # Calculate aggregated SD with spatial correlation
  if (verbose) message("Aggregating SD with spatial correlation...")

  # Focal aggregation: sum(SD_i * SD_j * cor_ij)
  # This is equivalent to: SD * focal(SD, weights = cor_matrix)
  weighted_sd <- terra::focal(
    sd_raster,
    w = cor_matrix,
    fun = function(x, ...) {
      # Get center pixel SD
      center_idx <- ceiling(length(x) / 2)
      center_sd <- x[center_idx]
      if (is.na(center_sd)) return(NA)

      # Calculate weighted sum
      sum(center_sd * x * cor_matrix, na.rm = TRUE)
    },
    na.rm = TRUE
  )

  # Take square root to get aggregated SD
  aggregated_sd <- sqrt(weighted_sd)

  # Scale to regional totals
  aggregated_sd <- aggregated_sd * scale_factor

  # Save to file if requested
  if (!is.null(filename)) {
    if (verbose) message("Writing aggregated SD raster to file...")
    terra::writeRaster(aggregated_sd, filename, overwrite = TRUE)
  }

  # Calculate regional total SD (correlated)
  regional_sd_correlated <- sqrt(terra::global(
    weighted_sd,
    fun = "sum",
    na.rm = TRUE
  )[1, 1]) * scale_factor

  # Calculate comparison with independence assumption if requested
  sd_no_correlation <- NULL
  regional_sd_independent <- NULL
  correlation_reduction <- NULL

  if (return_comparison) {
    if (verbose) message("Calculating comparison (independence assumption)...")

    # Under independence: Var(sum) = sum(Var) = sum(SD^2)
    var_sum <- terra::global(
      sd_raster^2,
      fun = "sum",
      na.rm = TRUE
    )[1, 1]

    regional_sd_independent <- sqrt(var_sum) * scale_factor

    # Calculate reduction factor
    correlation_reduction <- regional_sd_correlated / regional_sd_independent

    # Create raster assuming independence (for comparison)
    # Focal sum of variances, then sqrt
    sd_no_correlation <- terra::focal(
      sd_raster^2,
      w = matrix(1, nrow = nrow(cor_matrix), ncol = ncol(cor_matrix)),
      fun = sum,
      na.rm = TRUE
    )
    sd_no_correlation <- sqrt(sd_no_correlation) * scale_factor
  }

  # Count cells
  n_cells <- terra::global(sd_raster, fun = function(x) sum(!is.na(x)))[1, 1]

  # Return structured list
  result <- list(
    aggregated_sd_raster = aggregated_sd,
    sd_no_correlation = sd_no_correlation,
    correlation_matrix = cor_matrix,
    regional_sd_correlated = regional_sd_correlated,
    regional_sd_independent = regional_sd_independent,
    correlation_reduction_factor = correlation_reduction,
    n_cells = n_cells,
    resolution = resolution
  )

  class(result) <- c("aggregatedUncertainty", "list")
  return(result)
}


#' Calculate Bias-Adjusted Regional Totals with Uncertainty
#'
#' Calculates regional AGB totals for both default and bias-adjusted maps, including
#' uncertainty bounds accounting for spatial correlation. Provides a complete comparison
#' showing the impact of bias correction and proper uncertainty quantification.
#'
#' @param map_agb_default SpatRaster containing the default (uncorrected) AGB map (Mg/ha).
#' @param map_agb_adjusted SpatRaster containing the bias-adjusted AGB map (Mg/ha).
#'   Typically from \code{adjustMapBias()}.
#' @param aggregated_uncertainty An aggregatedUncertainty object from
#'   \code{aggregateUncertainty()}, containing the aggregated SD raster and regional
#'   SD values. Alternatively, a SpatRaster with aggregated SD values.
#' @param forest_mask Optional SpatRaster with forest/non-forest mask (typically tree
#'   cover percentage). Used together with \code{threshold} to define forest areas.
#'   Default: NULL (all pixels used).
#' @param threshold Numeric value (0-100) specifying the minimum tree cover percentage
#'   to be considered forest. Only used if \code{forest_mask} is provided. Default: 10
#'   (areas with <10% tree cover are excluded). Set to 0 to include all areas.
#' @param confidence_level Numeric. Confidence level for intervals. Default: 0.95
#'   (95% confidence intervals). Common values: 0.90, 0.95, 0.99.
#' @param scale_factor Numeric. Conversion factor from pixel values to regional totals.
#'   Default: 10000 (converts 100m pixels to hectares for 0.1° aggregation).
#'   Must match the scale_factor used in \code{aggregateUncertainty()}.
#' @param return_details Logical. Should detailed statistics be included? Default: TRUE.
#'   Includes CV%, relative uncertainty, number of cells, etc.
#'
#' @return A data.frame with one row per map type (default, bias_adjusted) and columns:
#'   \describe{
#'     \item{map_type}{Character. "default" or "bias_adjusted"}
#'     \item{total_Mg}{Numeric. Regional AGB total (Mg)}
#'     \item{sd_Mg}{Numeric. Standard deviation of total (Mg)}
#'     \item{cv_percent}{Numeric. Coefficient of variation (SD/mean * 100)}
#'     \item{ci_lower}{Numeric. Lower confidence bound (Mg)}
#'     \item{ci_upper}{Numeric. Upper confidence bound (Mg)}
#'     \item{confidence_level}{Numeric. Confidence level used (e.g., 0.95)}
#'     \item{n_cells}{Integer. Number of pixels included}
#'     \item{mean_agb_Mg_ha}{Numeric. Mean AGB per hectare (only if return_details=TRUE)}
#'     \item{sd_mean_Mg_ha}{Numeric. SD of mean per hectare (only if return_details=TRUE)}
#'   }
#'
#'   Additional attributes:
#'   \describe{
#'     \item{bias_correction_effect}{Numeric. Difference in totals (adjusted - default) in Mg}
#'     \item{percent_change}{Numeric. Percent change from default to adjusted}
#'     \item{uncertainty_overlap}{Logical. Do confidence intervals overlap?}
#'   }
#'
#' @details
#' This function integrates the complete bias modeling and uncertainty quantification
#' workflow from Sections 4-5 of the Mexico demo notebook (lines 1079-1305).
#'
#' \strong{Regional totals calculation:}
#'
#' For each map (default and bias-adjusted):
#' \enumerate{
#'   \item Apply forest mask and threshold
#'   \item Sum all pixel values and multiply by scale_factor
#'   \item Extract or calculate regional SD
#'   \item Calculate confidence intervals: total ± (z * SD)
#' }
#'
#' \strong{Confidence intervals:}
#'
#' Confidence intervals are calculated using the normal approximation:
#'
#' \deqn{CI = total \pm z_{\alpha/2} \times SD}{CI = total +/- z_alpha/2 * SD}
#'
#' where z is the critical value from the standard normal distribution (e.g., 1.96
#' for 95% confidence).
#'
#' \strong{Coefficient of variation (CV%):}
#'
#' CV is a normalized measure of uncertainty:
#'
#' \deqn{CV = (SD / mean) \times 100}{CV = (SD / mean) * 100}
#'
#' Values < 10% indicate low uncertainty, 10-30% moderate, > 30% high.
#'
#' \strong{Interpretation:}
#'
#' Compare the two rows to assess:
#' \itemize{
#'   \item \strong{Bias correction impact:} How much did the total change?
#'   \item \strong{Statistical significance:} Do confidence intervals overlap?
#'   \item \strong{Relative uncertainty:} Is CV% acceptable for reporting?
#'   \item \strong{IPCC compliance:} Are uncertainty bounds suitable for inventory?
#' }
#'
#' @references
#' Araza, A., de Bruin, S., Herold, M., Quegan, S., Labrière, N., Rodriguez-Veiga, P.,
#' ... & Burt, A. (2022). A comprehensive framework for assessing the accuracy and
#' uncertainty of global above-ground biomass maps. Remote Sensing of Environment, 272, 112917.
#'
#' IPCC (2006). IPCC Guidelines for National Greenhouse Gas Inventories.
#'
#' @examples
#' \dontrun{
#' # Complete workflow:
#' # 1. Bias modeling
#' bias_data <- extractBiasCovariates(plots, map_agb, map_sd, covariates)
#' bias_model <- trainBiasModel(bias_data, predictors = c("map", "sd", "height"))
#' bias_map <- predictBiasMap(bias_model, covariate_stack)
#' adjustment <- adjustMapBias(map_agb, bias_map, forest_mask)
#'
#' # 2. Uncertainty quantification
#' bias_data$biasPred <- predict(bias_model$model, data = bias_data)
#' vgm_fit <- fitResidualVariogram(bias_data, map_sd, coord_cols = c("x", "y"), crs = "EPSG:4326")
#' vgm_disc <- discountVariogram(vgm_fit, variance_data)
#' uncertainty <- aggregateUncertainty(map_sd, vgm_disc, resolution = 0.1)
#'
#' # 3. Calculate totals with uncertainty
#' totals <- biasAdjustedTotal(
#'   map_agb_default = map_agb,
#'   map_agb_adjusted = adjustment$adjusted_map,
#'   aggregated_uncertainty = uncertainty,
#'   forest_mask = forest_mask,
#'   confidence_level = 0.95
#' )
#'
#' # Inspect results
#' print(totals)
#' print(attr(totals, "bias_correction_effect"))
#' print(attr(totals, "uncertainty_overlap"))
#' }
#'
#' @seealso
#' \code{\link{adjustMapBias}} for bias correction,
#' \code{\link{aggregateUncertainty}} for uncertainty aggregation
#'
#' @export
#' @importFrom terra global mask
#' @importFrom stats qnorm
biasAdjustedTotal <- function(map_agb_default,
                               map_agb_adjusted,
                               aggregated_uncertainty,
                               forest_mask = NULL,
                               threshold = 10,
                               confidence_level = 0.95,
                               scale_factor = 10000,
                               return_details = TRUE) {

  # Input validation
  if (!inherits(map_agb_default, "SpatRaster")) {
    stop("map_agb_default must be a SpatRaster")
  }

  if (!inherits(map_agb_adjusted, "SpatRaster")) {
    stop("map_agb_adjusted must be a SpatRaster")
  }

  if (!inherits(aggregated_uncertainty, "aggregatedUncertainty") &&
      !inherits(aggregated_uncertainty, "SpatRaster")) {
    stop("aggregated_uncertainty must be an aggregatedUncertainty object or SpatRaster")
  }

  if (confidence_level <= 0 || confidence_level >= 1) {
    stop("confidence_level must be between 0 and 1")
  }

  # Extract regional SD from aggregated uncertainty
  if (inherits(aggregated_uncertainty, "aggregatedUncertainty")) {
    regional_sd <- aggregated_uncertainty$regional_sd_correlated
  } else {
    # If just a raster, calculate regional SD
    regional_sd <- sqrt(terra::global(
      aggregated_uncertainty^2,
      fun = "sum",
      na.rm = TRUE
    )[1, 1])
  }

  # Helper function to calculate totals for one map
  calculate_map_total <- function(agb_map, map_name) {
    # Apply forest mask with threshold if provided
    if (!is.null(forest_mask)) {
      mask_layer <- forest_mask
      # Apply threshold to forest mask (tree cover %)
      if (threshold > 0) {
        mask_layer[mask_layer < threshold] <- NA
      }
      agb_map <- terra::mask(agb_map, mask_layer)
    }

    # Calculate total
    total_Mg <- terra::global(agb_map, fun = "sum", na.rm = TRUE)[1, 1] * scale_factor

    # Count cells
    n_cells <- terra::global(agb_map, fun = function(x) sum(!is.na(x)))[1, 1]

    # Calculate mean per hectare
    mean_agb <- total_Mg / (n_cells * scale_factor)

    # Calculate z-score for confidence level
    z <- stats::qnorm(1 - (1 - confidence_level) / 2)

    # Calculate confidence intervals
    ci_lower <- total_Mg - (z * regional_sd)
    ci_upper <- total_Mg + (z * regional_sd)

    # Calculate CV%
    cv_percent <- (regional_sd / total_Mg) * 100

    # SD of mean per hectare
    sd_mean <- regional_sd / (n_cells * scale_factor)

    # Create results row
    result <- data.frame(
      map_type = map_name,
      total_Mg = total_Mg,
      sd_Mg = regional_sd,
      cv_percent = cv_percent,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      confidence_level = confidence_level,
      n_cells = n_cells,
      stringsAsFactors = FALSE
    )

    if (return_details) {
      result$mean_agb_Mg_ha <- mean_agb
      result$sd_mean_Mg_ha <- sd_mean
    }

    return(result)
  }

  # Calculate totals for both maps
  default_result <- calculate_map_total(map_agb_default, "default")
  adjusted_result <- calculate_map_total(map_agb_adjusted, "bias_adjusted")

  # Combine results
  results <- rbind(default_result, adjusted_result)

  # Calculate additional comparison metrics
  bias_effect <- adjusted_result$total_Mg - default_result$total_Mg
  percent_change <- (bias_effect / default_result$total_Mg) * 100

  # Check if confidence intervals overlap
  overlap <- !(adjusted_result$ci_lower > default_result$ci_upper ||
                 adjusted_result$ci_upper < default_result$ci_lower)

  # Add attributes
  attr(results, "bias_correction_effect") <- bias_effect
  attr(results, "percent_change") <- percent_change
  attr(results, "uncertainty_overlap") <- overlap

  return(results)
}
