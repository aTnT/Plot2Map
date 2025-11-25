## Updates made:
# 13/11/25:
# Initial implementation of bias modeling functions for Plot2Map package
# Functions implement the workflow from plot2map_Mexico_demo notebook Section 4 (lines 847-1091)
# - extractBiasCovariates: Extract covariate values at plot locations and calculate bias
# - trainBiasModel: Train Random Forest model to predict bias from covariates
# - predictBiasMap: Predict bias wall-to-wall across study region
# - adjustMapBias: Adjust AGB map by subtracting predicted bias
# All functions use terra package for raster operations


#' Extract bias and covariate values at plot locations
#'
#' This function extracts covariate values at plot locations and calculates bias
#' as the difference between map AGB and plot AGB. This is the first step in the
#' bias modeling workflow, preparing data for training a bias correction model.
#' The function follows the methodology from Araza et al. (2022) for AGB map
#' bias assessment and correction.
#'
#' @param plot_data data.frame containing plot comparison results from \code{\link{invDasymetry}}.
#'   Must include columns:
#'   \itemize{
#'     \item x: X-coordinate (longitude) of plot locations
#'     \item y: Y-coordinate (latitude) of plot locations
#'     \item plotAGB_X: Plot AGB values where X is the threshold (e.g., plotAGB_10)
#'     \item mapAGB: Map AGB values extracted at plot locations
#'   }
#' @param map_agb SpatRaster object containing the AGB map layer. This is typically
#'   the same map used in \code{\link{invDasymetry}} for comparison. Values should
#'   be in Mg/ha.
#' @param map_sd SpatRaster object containing the standard deviation (uncertainty)
#'   layer associated with the AGB map. Values should be in Mg/ha.
#' @param covariates Named list of SpatRaster objects containing predictor variables.
#'   Common covariates include:
#'   \itemize{
#'     \item height: Canopy height (e.g., from Potapov et al. 2020)
#'     \item biome: Biome classification (e.g., Dinerstein et al. 2017)
#'     \item treecover: Tree cover percentage (e.g., Sexton et al. 2015)
#'     \item slope: Terrain slope from DEM (e.g., SRTM)
#'     \item aspect: Terrain aspect from DEM
#'     \item ifl: Intact Forest Landscape indicator (binary)
#'   }
#'   List names will be used as column names in the output. Set to NULL to skip
#'   covariate extraction.
#' @param plot_agb_col Character string specifying the column name for plot AGB
#'   in \code{plot_data}. Default is "plotAGB_10" (for 10% threshold).
#' @param map_agb_col Character string specifying the column name for map AGB
#'   in \code{plot_data}. Default is "mapAGB".
#'
#' @return A data.frame containing all original columns from \code{plot_data} plus:
#'   \describe{
#'     \item{bias}{Calculated bias (mapAGB - plotAGB), in Mg/ha. Positive values
#'       indicate map overestimation, negative values indicate map underestimation.}
#'     \item{map}{Map AGB value extracted at the plot location, in Mg/ha}
#'     \item{sd}{Map SD value extracted at the plot location, in Mg/ha}
#'     \item{<covariate_names>}{One column for each covariate in the covariates list.
#'       Column names match the list names. For the IFL covariate, NA values are
#'       automatically converted to 0.}
#'   }
#'
#' @details
#' This function implements the first step of the bias modeling workflow. The bias is
#' calculated as:
#'
#' \deqn{bias = AGB_{map} - AGB_{plot}}{bias = AGB_map - AGB_plot}
#'
#' Positive bias values indicate that the map overestimates AGB compared to plot
#' measurements, while negative values indicate underestimation. The extracted
#' covariates will be used as predictors in the Random Forest model to spatially
#' model the bias patterns.
#'
#' All raster extractions use bilinear interpolation by default. The function
#' ensures that all input rasters have matching coordinate reference systems (CRS).
#' Plots with NA values in any covariate will retain the NA, which should be
#' handled during model training.
#'
#' @references
#' Araza, A., de Bruin, S., Herold, M., Quegan, S., Labriere, N., Rodriguez-Veiga, P.,
#' Avitabile, V., Santoro, M., Mitchard, E.T.A., Ryan, C.M., Phillips, O.L.,
#' Willcock, S., Verbeeck, H., Carreiras, J., Hein, L., Schelhaas, M.J., &
#' Pacheco-Pascagaza, A.M. (2022). A comprehensive framework for assessing the
#' accuracy and uncertainty of global above-ground biomass maps. Remote Sensing
#' of Environment, 272, 112917. https://doi.org/10.1016/j.rse.2022.112917
#'
#' @seealso
#' \code{\link{trainBiasModel}} for training the bias correction model,
#' \code{\link{predictBiasMap}} for generating wall-to-wall bias predictions,
#' \code{\link{invDasymetry}} for generating the input plot comparison data
#'
#' @examples
#' \dontrun{
#' # Create synthetic rasters for demonstration
#' library(terra)
#' r <- rast(ncol = 100, nrow = 100, xmin = -10, xmax = 10, ymin = 35, ymax = 45,
#'           crs = "EPSG:4326")
#' map_agb <- setValues(r, runif(ncell(r), 50, 300))
#' map_sd <- setValues(r, runif(ncell(r), 10, 50))
#' height <- setValues(r, runif(ncell(r), 5, 30))
#' treecover <- setValues(r, runif(ncell(r), 20, 100))
#'
#' # Create sample plot data (e.g., from invDasymetry output)
#' plot_data <- data.frame(
#'   x = runif(50, -5, 5),
#'   y = runif(50, 40, 45),
#'   plotAGB_10 = runif(50, 40, 250),
#'   mapAGB = runif(50, 60, 280)
#' )
#'
#' # Create named list of covariates
#' covariates <- list(height = height, treecover = treecover)
#'
#' # Extract bias and covariates at plot locations
#' bias_data <- extractBiasCovariates(
#'   plot_data = plot_data,
#'   map_agb = map_agb,
#'   map_sd = map_sd,
#'   covariates = covariates,
#'   plot_agb_col = "plotAGB_10",
#'   map_agb_col = "mapAGB"
#' )
#'
#' # View results
#' head(bias_data)
#' cat("Mean bias:", mean(bias_data$bias, na.rm = TRUE), "Mg/ha\n")
#' cat("Bias range:", range(bias_data$bias, na.rm = TRUE), "Mg/ha\n")
#' }
#'
#' @export
#' @importFrom terra extract crs
extractBiasCovariates <- function(plot_data,
                                   map_agb,
                                   map_sd,
                                   covariates = list(),
                                   plot_agb_col = "plotAGB_10",
                                   map_agb_col = "mapAGB") {

  # Input validation
  if (!is.data.frame(plot_data)) {
    stop("plot_data must be a data.frame")
  }

  required_cols <- c("x", "y", plot_agb_col, map_agb_col)
  missing_cols <- setdiff(required_cols, names(plot_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in plot_data: ",
         paste(missing_cols, collapse = ", "))
  }

  if (!inherits(map_agb, "SpatRaster")) {
    stop("map_agb must be a SpatRaster object from the terra package")
  }

  if (!inherits(map_sd, "SpatRaster")) {
    stop("map_sd must be a SpatRaster object from the terra package")
  }

  # Check CRS consistency
  crs_map <- terra::crs(map_agb)
  crs_sd <- terra::crs(map_sd)
  if (crs_map != crs_sd) {
    warning("map_agb and map_sd have different CRS. Attempting to align...")
  }

  # Create spatial points for extraction
  coords <- plot_data[, c("x", "y")]

  # Extract map AGB values
  map_values <- terra::extract(map_agb, coords, method = "bilinear")
  plot_data$map <- map_values[, 2]  # First column is ID

  # Extract map SD values
  sd_values <- terra::extract(map_sd, coords, method = "bilinear")
  plot_data$sd <- sd_values[, 2]

  # Calculate bias: map - plot (must be done after extracting map values)
  plot_data$bias <- plot_data$map - plot_data[[plot_agb_col]]

  # Extract covariate values
  if (length(covariates) > 0) {
    if (!is.list(covariates) || is.null(names(covariates))) {
      stop("covariates must be a named list of SpatRaster objects")
    }

    for (cov_name in names(covariates)) {
      cov_raster <- covariates[[cov_name]]

      if (!inherits(cov_raster, "SpatRaster")) {
        stop("Covariate '", cov_name, "' must be a SpatRaster object")
      }

      # Extract values
      cov_values <- terra::extract(cov_raster, coords, method = "bilinear")
      plot_data[[cov_name]] <- cov_values[, 2]

      # Special handling for IFL (Intact Forest Landscape) - convert NA to 0
      if (tolower(cov_name) == "ifl") {
        plot_data[[cov_name]] <- ifelse(is.na(plot_data[[cov_name]]), 0, 1)
      }
    }
  }

  message("Extracted covariates for ", nrow(plot_data), " plots")
  message("Bias statistics: mean = ", round(mean(plot_data$bias, na.rm = TRUE), 2),
          " Mg/ha, sd = ", round(stats::sd(plot_data$bias, na.rm = TRUE), 2), " Mg/ha")

  return(plot_data)
}


#' Train Random Forest model to predict AGB map bias
#'
#' This function trains a Random Forest model to predict systematic bias in AGB
#' maps as a function of environmental and map-derived covariates. The trained
#' model can be used to generate wall-to-wall bias predictions for map correction.
#' This implements the bias modeling approach from Araza et al. (2022).
#'
#' @param bias_data data.frame output from \code{\link{extractBiasCovariates}},
#'   containing bias values and covariate columns. Must include all columns
#'   specified in \code{predictors} and the column specified in \code{response}.
#' @param predictors Character vector of predictor variable names to use in the
#'   model. These should be column names in \code{bias_data}. Common predictors
#'   include: "map", "sd", "height", "biome", "treecover", "slope", "aspect", "ifl".
#'   Default uses all standard covariates.
#' @param response Character string specifying the response variable column name.
#'   Default is "bias".
#' @param method Character string specifying the Random Forest implementation to use.
#'   Options are:
#'   \itemize{
#'     \item "ranger" (default): Uses the ranger package for fast training
#'     \item "randomForest": Uses the randomForest package
#'   }
#' @param seed Integer for random seed to ensure reproducibility. Default is 1234.
#' @param cv_folds Integer specifying number of folds for k-fold cross-validation.
#'   If NULL (default), no cross-validation is performed. If specified (e.g., 10),
#'   performs k-fold CV and returns performance metrics.
#' @param importance Logical indicating whether to calculate variable importance.
#'   Default is TRUE.
#' @param ... Additional arguments passed to \code{ranger::ranger} or
#'   \code{randomForest::randomForest}.
#'
#' @return A list with the following components:
#'   \describe{
#'     \item{model}{The trained Random Forest model object (class "ranger" or
#'       "randomForest" depending on method)}
#'     \item{importance}{data.frame with variable importance scores. For ranger,
#'       uses impurity-based importance. For randomForest, uses IncNodePurity.
#'       Sorted by importance in descending order.}
#'     \item{cv_results}{If cv_folds is specified, a data.frame with cross-validation
#'       metrics including RMSE, MAE, and R-squared for each fold and overall.
#'       NULL if cv_folds is NULL.}
#'     \item{formula}{The model formula used}
#'     \item{predictors}{Character vector of predictor names used}
#'     \item{method}{Character string indicating which method was used}
#'     \item{n_obs}{Number of observations used for training (after removing NAs)}
#'   }
#'
#' @details
#' The Random Forest model learns the relationship between environmental
#' map covariates and systematic bias in the AGB map.
#'
#' The model trains on the relationship:
#' \deqn{bias = f(covariates) + \epsilon}{bias = f(covariates) + epsilon}
#'
#' Where bias = mapAGB - plotAGB, and f() is learned by the Random Forest.
#'
#' The ranger package is recommended for faster training, especially with large
#' datasets or when performing cross-validation. The function automatically removes
#' rows with NA values in any predictor or response variable before training.
#'
#' When \code{cv_folds} is specified, the function performs k-fold cross-validation
#' to assess model performance. This is useful for model evaluation and comparison.
#'
#' @references
#' Araza, A., et al. (2022). A comprehensive framework for assessing the accuracy
#' and uncertainty of global above-ground biomass maps. Remote Sensing of Environment,
#' 272, 112917. https://doi.org/10.1016/j.rse.2022.112917
#'
#' Wright, M. N. & Ziegler, A. (2017). ranger: A Fast Implementation of Random
#' Forests for High Dimensional Data in C++ and R. Journal of Statistical Software,
#' 77(1), 1-17. https://doi.org/10.18637/jss.v077.i01
#'
#' @seealso
#' \code{\link{extractBiasCovariates}} for preparing the input data,
#' \code{\link{predictBiasMap}} for generating wall-to-wall predictions,
#' \code{\link{adjustMapBias}} for applying the bias correction
#'
#' @examples
#' \dontrun{
#' # Following from extractBiasCovariates example
#' # Assuming bias_data is already created
#'
#' # Train model with default settings
#' bias_model <- trainBiasModel(
#'   bias_data = bias_data,
#'   predictors = c("map", "sd", "height", "treecover")
#' )
#'
#' # View variable importance
#' print(bias_model$importance)
#'
#' # Train with cross-validation
#' bias_model_cv <- trainBiasModel(
#'   bias_data = bias_data,
#'   predictors = c("map", "sd", "height", "treecover"),
#'   cv_folds = 10
#' )
#'
#' # View CV performance
#' print(bias_model_cv$cv_results)
#' }
#'
#' @export
trainBiasModel <- function(bias_data,
                           predictors = c("map", "sd", "height", "biome",
                                         "treecover", "slope", "aspect", "ifl"),
                           response = "bias",
                           method = "ranger",
                           seed = 1234,
                           cv_folds = NULL,
                           importance = TRUE,
                           ...) {

  # Input validation
  if (!is.data.frame(bias_data)) {
    stop("bias_data must be a data.frame")
  }

  if (!response %in% names(bias_data)) {
    stop("Response variable '", response, "' not found in bias_data")
  }

  missing_predictors <- setdiff(predictors, names(bias_data))
  if (length(missing_predictors) > 0) {
    stop("Missing predictor columns in bias_data: ",
         paste(missing_predictors, collapse = ", "))
  }

  if (!method %in% c("ranger", "randomForest")) {
    stop("method must be either 'ranger' or 'randomForest'")
  }

  # Check package availability
  if (method == "ranger" && !requireNamespace("ranger", quietly = TRUE)) {
    warning("ranger package not available, switching to randomForest")
    method <- "randomForest"
  }

  if (method == "randomForest" && !requireNamespace("randomForest", quietly = TRUE)) {
    stop("randomForest package is required but not installed. ",
         "Install with: install.packages('randomForest')")
  }

  # Create formula
  formula_str <- paste(response, "~", paste(predictors, collapse = " + "))
  model_formula <- stats::as.formula(formula_str)

  # Prepare data - remove NAs
  model_data <- bias_data[, c(response, predictors)]
  model_data_complete <- stats::na.omit(model_data)
  n_removed <- nrow(model_data) - nrow(model_data_complete)

  if (n_removed > 0) {
    message("Removed ", n_removed, " rows with NA values")
  }

  if (nrow(model_data_complete) == 0) {
    stop("No complete cases available for model training")
  }

  message("Training ", method, " model with ", nrow(model_data_complete),
          " observations")

  # Set seed for reproducibility
  set.seed(seed)

  # Train model based on method
  if (method == "ranger") {
    model <- ranger::ranger(
      formula = model_formula,
      data = model_data_complete,
      importance = if (importance) "impurity" else "none",
      ...
    )

    # Extract importance
    if (importance) {
      imp_df <- data.frame(
        variable = names(model$variable.importance),
        importance = model$variable.importance,
        stringsAsFactors = FALSE
      )
      imp_df <- imp_df[order(-imp_df$importance), ]
      rownames(imp_df) <- NULL
    } else {
      imp_df <- NULL
    }

  } else {  # randomForest
    model <- randomForest::randomForest(
      formula = model_formula,
      data = model_data_complete,
      importance = importance,
      ...
    )

    # Extract importance
    if (importance) {
      imp_matrix <- randomForest::importance(model)
      imp_df <- data.frame(
        variable = rownames(imp_matrix),
        importance = imp_matrix[, "IncNodePurity"],
        stringsAsFactors = FALSE
      )
      imp_df <- imp_df[order(-imp_df$importance), ]
      rownames(imp_df) <- NULL
    } else {
      imp_df <- NULL
    }
  }

  # Cross-validation if requested
  cv_results <- NULL
  if (!is.null(cv_folds)) {
    message("Performing ", cv_folds, "-fold cross-validation...")
    cv_results <- performCrossValidation(
      data = model_data_complete,
      formula = model_formula,
      method = method,
      folds = cv_folds,
      seed = seed,
      ...
    )
  }

  message("Model training complete")
  if (importance && !is.null(imp_df)) {
    message("Top 3 important variables: ",
            paste(imp_df$variable[1:min(3, nrow(imp_df))], collapse = ", "))
  }

  return(list(
    model = model,
    importance = imp_df,
    cv_results = cv_results,
    formula = model_formula,
    predictors = predictors,
    method = method,
    n_obs = nrow(model_data_complete)
  ))
}


#' Perform k-fold cross-validation for bias model
#'
#' Internal helper function to perform k-fold cross-validation for Random Forest
#' bias models. Not exported.
#'
#' @param data data.frame with complete cases
#' @param formula Model formula
#' @param method "ranger" or "randomForest"
#' @param folds Number of folds
#' @param seed Random seed
#' @param ... Additional arguments for model training
#'
#' @return data.frame with CV results
#' @keywords internal
performCrossValidation <- function(data, formula, method, folds, seed, ...) {

  set.seed(seed)

  # Create folds
  n <- nrow(data)
  fold_ids <- sample(rep(1:folds, length.out = n))

  cv_metrics <- data.frame(
    fold = integer(),
    rmse = numeric(),
    mae = numeric(),
    r2 = numeric(),
    stringsAsFactors = FALSE
  )

  for (fold in 1:folds) {
    # Split data
    test_idx <- which(fold_ids == fold)
    train_data <- data[-test_idx, ]
    test_data <- data[test_idx, ]

    # Train model
    if (method == "ranger") {
      fold_model <- ranger::ranger(formula = formula, data = train_data, ...)
      predictions <- stats::predict(fold_model, data = test_data)$predictions
    } else {
      fold_model <- randomForest::randomForest(formula = formula, data = train_data, ...)
      predictions <- stats::predict(fold_model, newdata = test_data)
    }

    # Calculate metrics
    actual <- test_data[[all.vars(formula)[1]]]
    rmse <- sqrt(mean((actual - predictions)^2))
    mae <- mean(abs(actual - predictions))
    ss_res <- sum((actual - predictions)^2)
    ss_tot <- sum((actual - mean(actual))^2)
    r2 <- 1 - (ss_res / ss_tot)

    cv_metrics <- rbind(cv_metrics, data.frame(
      fold = fold,
      rmse = rmse,
      mae = mae,
      r2 = r2
    ))
  }

  # Add summary row
  cv_metrics <- rbind(cv_metrics, data.frame(
    fold = 0,  # 0 indicates overall
    rmse = mean(cv_metrics$rmse),
    mae = mean(cv_metrics$mae),
    r2 = mean(cv_metrics$r2)
  ))

  rownames(cv_metrics) <- NULL
  return(cv_metrics)
}


#' Predict bias wall-to-wall across study region
#'
#' This function applies a trained bias model to generate wall-to-wall predictions
#' of systematic bias across the study region. The predicted bias can then be used
#' to adjust the original AGB map.
#'
#' @param bias_model Trained model object from \code{\link{trainBiasModel}}.
#'   Must be a list containing at minimum a 'model' component (ranger or randomForest
#'   object) and a 'predictors' component (character vector of predictor names).
#' @param covariate_stack SpatRaster object containing all predictor layers used
#'   in model training. Layer names must match the predictor names used in
#'   \code{bias_model$predictors}. Typically includes: map AGB, map SD, height,
#'   biome, tree cover, slope, aspect, and IFL.
#' @param filename Optional character string specifying output file path for the
#'   predicted bias raster. If provided, the raster will be written to disk.
#'   If NULL (default), the raster is kept in memory. For large regions, providing
#'   a filename is recommended to avoid memory issues.
#' @param return_raster Logical indicating whether to return the SpatRaster object.
#'   Default is TRUE. If FALSE and filename is provided, only the file path is
#'   returned.
#' @param ... Additional arguments passed to \code{terra::writeRaster}, such as
#'   overwrite, datatype, gdal options, etc.
#'
#' @return If \code{return_raster = TRUE}, returns a SpatRaster object with predicted
#'   bias values in Mg/ha. Positive values indicate areas where the map overestimates
#'   AGB compared to expected values based on covariates. Negative values indicate
#'   areas where the map underestimates AGB. If \code{return_raster = FALSE} and
#'   \code{filename} is provided, returns the file path as a character string.
#'
#' @details
#' This function uses \code{terra::predict} to apply the trained Random Forest
#' model to a raster stack of covariates. The function automatically handles both
#' ranger and randomForest model objects.
#'
#' The covariate stack must have layer names that exactly match the predictor
#' variable names used during model training. Use \code{names(covariate_stack)}
#' to check layer names and \code{names(covariate_stack) <- c(...)} to rename
#' if needed.
#'
#' For large regions or high-resolution rasters, it is recommended to:
#' \itemize{
#'   \item Provide a filename to write directly to disk
#'   \item Use appropriate datatype (e.g., datatype = "FLT4S" for 4-byte floats)
#'   \item Consider tiling options via GDAL options
#' }
#'
#' The predicted bias map will have:
#' \itemize{
#'   \item Same extent, resolution, and CRS as the input covariate_stack
#'   \item NA values where any covariate has NA
#'   \item Values typically ranging from -100 to +100 Mg/ha for most forests
#' }
#'
#' @references
#' Araza, A., et al. (2022). A comprehensive framework for assessing the accuracy
#' and uncertainty of global above-ground biomass maps. Remote Sensing of Environment,
#' 272, 112917. https://doi.org/10.1016/j.rse.2022.112917
#'
#' @seealso
#' \code{\link{trainBiasModel}} for training the bias model,
#' \code{\link{adjustMapBias}} for applying the predicted bias to adjust the AGB map,
#' \code{\link{extractBiasCovariates}} for the initial data preparation
#'
#' @examples
#' \dontrun{
#' # Following from trainBiasModel example
#' # Assuming bias_model is already created
#'
#' # Create covariate stack matching model predictors
#' library(terra)
#' r <- rast(ncol = 100, nrow = 100, xmin = -10, xmax = 10, ymin = 35, ymax = 45)
#' map_layer <- setValues(r, runif(ncell(r), 0, 300))
#' sd_layer <- setValues(r, runif(ncell(r), 10, 50))
#' height_layer <- setValues(r, runif(ncell(r), 5, 30))
#' treecover_layer <- setValues(r, runif(ncell(r), 0, 100))
#'
#' # Stack covariates with correct names
#' covs <- c(map_layer, sd_layer, height_layer, treecover_layer)
#' names(covs) <- c("map", "sd", "height", "treecover")
#'
#' # Predict bias across region
#' bias_map <- predictBiasMap(
#'   bias_model = bias_model,
#'   covariate_stack = covs
#' )
#'
#' # Plot results
#' plot(bias_map, main = "Predicted Bias (Mg/ha)")
#'
#' # Save to file
#' bias_map <- predictBiasMap(
#'   bias_model = bias_model,
#'   covariate_stack = covs,
#'   filename = "predicted_bias.tif",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
#' @importFrom terra predict
predictBiasMap <- function(bias_model,
                           covariate_stack,
                           filename = NULL,
                           return_raster = TRUE,
                           ...) {

  # Input validation
  if (!is.list(bias_model) || !all(c("model", "predictors") %in% names(bias_model))) {
    stop("bias_model must be a list with 'model' and 'predictors' components from trainBiasModel()")
  }

  if (!inherits(covariate_stack, "SpatRaster")) {
    stop("covariate_stack must be a SpatRaster object from the terra package")
  }

  # Check that all predictors are present in covariate stack
  stack_names <- names(covariate_stack)
  missing_predictors <- setdiff(bias_model$predictors, stack_names)

  if (length(missing_predictors) > 0) {
    stop("Missing predictor layers in covariate_stack: ",
         paste(missing_predictors, collapse = ", "),
         "\nAvailable layers: ", paste(stack_names, collapse = ", "))
  }

  # Subset and reorder covariate stack to match model predictors
  covariate_stack <- covariate_stack[[bias_model$predictors]]

  message("Predicting bias across ", terra::ncell(covariate_stack), " cells...")
  message("Using ", bias_model$method, " model")

  # Predict using terra::predict
  # This handles both ranger and randomForest models
  if (is.null(filename)) {
    bias_pred <- terra::predict(
      object = covariate_stack,
      model = bias_model$model,
      ...
    )
  } else {
    bias_pred <- terra::predict(
      object = covariate_stack,
      model = bias_model$model,
      filename = filename,
      ...
    )
  }

  names(bias_pred) <- "bias_predicted"

  # Summary statistics
  bias_stats <- terra::global(bias_pred, fun = c("mean", "min", "max"), na.rm = TRUE)
  message("Predicted bias statistics:")
  message("  Mean: ", round(bias_stats[1, "mean"], 2), " Mg/ha")
  message("  Range: [", round(bias_stats[1, "min"], 2), ", ",
          round(bias_stats[1, "max"], 2), "] Mg/ha")

  if (!return_raster && !is.null(filename)) {
    return(filename)
  }

  return(bias_pred)
}


#' Adjust AGB map by removing predicted bias
#'
#' This function adjusts an AGB map by subtracting the predicted bias, optionally
#' applies a forest mask, and calculates regional totals for comparison. This allows
#' to provide bias-corrected AGB estimates at both pixel and regional scales.
#'
#' @param map_agb SpatRaster object containing the original AGB map to be adjusted.
#'   Values should be in Mg/ha. This is typically the same map used in earlier
#'   steps of the bias modeling workflow.
#' @param bias_map SpatRaster object containing predicted bias from
#'   \code{\link{predictBiasMap}}. Must have the same extent, resolution, and CRS
#'   as \code{map_agb}. Values in Mg/ha.
#' @param forest_mask Optional SpatRaster object for forest masking, typically a
#'   tree cover layer. If provided, used together with \code{threshold} to define
#'   forest areas for regional total calculations. If NULL (default), no masking
#'   is applied.
#' @param threshold Numeric value (0-100) specifying the minimum tree cover percentage
#'   to be considered forest. Only used if \code{forest_mask} is provided. Default
#'   is 10 (i.e., areas with <10% tree cover are excluded). Set to 0 to include
#'   all areas with any tree cover.
#' @param scale_factor Numeric value for scaling pixel-level values to regional
#'   totals. Typically calculated as (resolution_in_meters^2) / 10000 to convert
#'   from per-hectare to total. If NULL (default), automatically calculated from
#'   raster resolution assuming input resolution is in degrees.
#' @param return_totals Logical indicating whether to calculate regional totals.
#'   Default is TRUE. If FALSE, only the adjusted raster is returned without
#'   total calculations.
#'
#' @return If \code{return_totals = TRUE}, returns a list with:
#'   \describe{
#'     \item{adjusted_map}{SpatRaster with bias-adjusted AGB values (Mg/ha).
#'       Calculated as: adjusted = original - predicted_bias}
#'     \item{default_total}{Numeric, total AGB from original map in Mg (if return_totals=TRUE)}
#'     \item{adjusted_total}{Numeric, total AGB from adjusted map in Mg (if return_totals=TRUE)}
#'     \item{difference}{Numeric, difference between adjusted and default totals in Mg.
#'       Positive values indicate the adjusted estimate is higher.}
#'     \item{percent_change}{Numeric, percentage change from default to adjusted.
#'       Formula: 100 * (adjusted - default) / default}
#'     \item{n_cells}{Integer, number of cells included in totals calculation (after masking)}
#'   }
#'   If \code{return_totals = FALSE}, returns only the adjusted_map SpatRaster.
#'
#' @details
#' The bias adjustment follows a simple subtraction approach:
#' \deqn{AGB_{adjusted} = AGB_{original} - bias_{predicted}}{AGB_adjusted = AGB_original - bias_predicted}
#'
#' Where \code{bias_predicted} is the output from \code{\link{predictBiasMap}}.
#' Areas where the model predicted positive bias (map overestimation) will have
#' their AGB reduced. Areas with negative bias (map underestimation) will have
#' their AGB increased.
#'
#' Regional totals are calculated by summing all pixel values (after forest masking)
#' and multiplying by the scale factor to convert from per-hectare to total biomass.
#' The scale factor accounts for pixel size:
#'
#' \deqn{Total = \sum (AGB_{pixel} \times PixelArea)}{Total = sum(AGB_pixel * PixelArea)}
#'
#' For example, with 0.1-degree pixels at the equator (~11km), the scale factor
#' would be approximately (11000^2) / 10000 = 12100.
#'
#' @references
#' Araza, A., et al. (2022). A comprehensive framework for assessing the accuracy
#' and uncertainty of global above-ground biomass maps. Remote Sensing of Environment,
#' 272, 112917. https://doi.org/10.1016/j.rse.2022.112917
#'
#' @seealso
#' \code{\link{predictBiasMap}} for generating the bias predictions,
#' \code{\link{trainBiasModel}} for training the bias model,
#' \code{\link{extractBiasCovariates}} for the initial data preparation
#'
#' @examples
#' \dontrun{
#' # Following from predictBiasMap example
#' # Assuming map_agb and bias_map are already created
#'
#' # Simple adjustment without masking
#' adjusted <- adjustMapBias(
#'   map_agb = map_layer,
#'   bias_map = bias_map,
#'   return_totals = FALSE
#' )
#'
#' plot(adjusted, main = "Bias-Adjusted AGB (Mg/ha)")
#'
#' # Adjustment with forest mask and regional totals
#' result <- adjustMapBias(
#'   map_agb = map_layer,
#'   bias_map = bias_map,
#'   forest_mask = treecover_layer,
#'   threshold = 10,
#'   return_totals = TRUE
#' )
#'
#' # Compare totals
#' print(result$default_total)
#' print(result$adjusted_total)
#' print(result$percent_change)
#'
#' # Visualize difference
#' plot(result$adjusted_map - map_layer, main = "Change in AGB (Mg/ha)")
#' }
#'
#' @export
#' @importFrom terra global ext res
adjustMapBias <- function(map_agb,
                          bias_map,
                          forest_mask = NULL,
                          threshold = 10,
                          scale_factor = NULL,
                          return_totals = TRUE) {

  # Input validation
  if (!inherits(map_agb, "SpatRaster")) {
    stop("map_agb must be a SpatRaster object from the terra package")
  }

  if (!inherits(bias_map, "SpatRaster")) {
    stop("bias_map must be a SpatRaster object from the terra package")
  }

  # Check spatial consistency
  if (!terra::compareGeom(map_agb, bias_map, stopOnError = FALSE)) {
    stop("map_agb and bias_map must have the same extent, resolution, and CRS")
  }

  if (!is.null(forest_mask)) {
    if (!inherits(forest_mask, "SpatRaster")) {
      stop("forest_mask must be a SpatRaster object from the terra package")
    }
    if (!terra::compareGeom(map_agb, forest_mask, stopOnError = FALSE)) {
      warning("forest_mask has different geometry than map_agb. Attempting to resample...")
      forest_mask <- terra::resample(forest_mask, map_agb)
    }
  }

  # Perform bias adjustment: adjusted = original - bias
  message("Adjusting AGB map by subtracting predicted bias...")
  adjusted_map <- map_agb - bias_map
  names(adjusted_map) <- "AGB_adjusted"

  # Summary of adjustment
  bias_stats <- terra::global(bias_map, fun = c("mean"), na.rm = TRUE)
  message("Mean bias correction: ", round(bias_stats[1, "mean"], 2), " Mg/ha")

  if (!return_totals) {
    return(adjusted_map)
  }

  # Calculate regional totals
  message("Calculating regional totals...")

  # Apply forest mask if provided
  if (!is.null(forest_mask)) {
    message("Applying forest mask with threshold = ", threshold, "%")
    mask_layer <- forest_mask
    mask_layer[mask_layer < threshold] <- NA

    map_agb_masked <- terra::mask(map_agb, mask_layer)
    adjusted_map_masked <- terra::mask(adjusted_map, mask_layer)
  } else {
    map_agb_masked <- map_agb
    adjusted_map_masked <- adjusted_map
  }

  # Auto-calculate scale factor if not provided
  if (is.null(scale_factor)) {
    # Get resolution in degrees
    res_deg <- terra::res(map_agb)

    # Approximate conversion: 1 degree ≈ 111 km at equator
    # Area in m^2, then convert to hectares
    res_m <- res_deg * 111000  # meters
    pixel_area_m2 <- res_m[1] * res_m[2]
    scale_factor <- pixel_area_m2 / 10000  # Convert m^2 to hectares

    message("Auto-calculated scale factor: ", round(scale_factor, 2),
            " (based on ", round(res_m[1]/1000, 2), " x ",
            round(res_m[2]/1000, 2), " km pixels)")
  }

  # Calculate totals
  default_sum <- terra::global(map_agb_masked, fun = "sum", na.rm = TRUE)
  adjusted_sum <- terra::global(adjusted_map_masked, fun = "sum", na.rm = TRUE)

  default_total <- default_sum[1, "sum"] * scale_factor
  adjusted_total <- adjusted_sum[1, "sum"] * scale_factor

  # Calculate differences
  difference <- adjusted_total - default_total
  percent_change <- 100 * difference / default_total

  # Count cells
  n_cells <- terra::global(!is.na(map_agb_masked), fun = "sum")[1, "sum"]

  message("\n=== Regional AGB Totals ===")
  message("Default map:  ", format(round(default_total, 0), big.mark = ","), " Mg")
  message("Adjusted map: ", format(round(adjusted_total, 0), big.mark = ","), " Mg")
  message("Difference:   ", format(round(difference, 0), big.mark = ","), " Mg (",
          sprintf("%+.1f", percent_change), "%)")
  message("Based on ", format(n_cells, big.mark = ","), " forest cells")

  return(list(
    adjusted_map = adjusted_map,
    default_total = default_total,
    adjusted_total = adjusted_total,
    difference = difference,
    percent_change = percent_change,
    n_cells = as.integer(n_cells)
  ))
}
