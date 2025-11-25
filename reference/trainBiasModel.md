# Train Random Forest model to predict AGB map bias

This function trains a Random Forest model to predict systematic bias in
AGB maps as a function of environmental and map-derived covariates. The
trained model can be used to generate wall-to-wall bias predictions for
map correction. This implements the bias modeling approach from Araza et
al. (2022).

## Usage

``` r
trainBiasModel(
  bias_data,
  predictors = c("map", "sd", "height", "biome", "treecover", "slope", "aspect", "ifl"),
  response = "bias",
  method = "ranger",
  seed = 1234,
  cv_folds = NULL,
  importance = TRUE,
  ...
)
```

## Arguments

- bias_data:

  data.frame output from
  [`extractBiasCovariates`](https://atnt.github.io/Plot2Map/reference/extractBiasCovariates.md),
  containing bias values and covariate columns. Must include all columns
  specified in `predictors` and the column specified in `response`.

- predictors:

  Character vector of predictor variable names to use in the model.
  These should be column names in `bias_data`. Common predictors
  include: "map", "sd", "height", "biome", "treecover", "slope",
  "aspect", "ifl". Default uses all standard covariates.

- response:

  Character string specifying the response variable column name. Default
  is "bias".

- method:

  Character string specifying the Random Forest implementation to use.
  Options are:

  - "ranger" (default): Uses the ranger package for fast training

  - "randomForest": Uses the randomForest package

- seed:

  Integer for random seed to ensure reproducibility. Default is 1234.

- cv_folds:

  Integer specifying number of folds for k-fold cross-validation. If
  NULL (default), no cross-validation is performed. If specified (e.g.,
  10), performs k-fold CV and returns performance metrics.

- importance:

  Logical indicating whether to calculate variable importance. Default
  is TRUE.

- ...:

  Additional arguments passed to
  [`ranger::ranger`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  or `randomForest::randomForest`.

## Value

A list with the following components:

- model:

  The trained Random Forest model object (class "ranger" or
  "randomForest" depending on method)

- importance:

  data.frame with variable importance scores. For ranger, uses
  impurity-based importance. For randomForest, uses IncNodePurity.
  Sorted by importance in descending order.

- cv_results:

  If cv_folds is specified, a data.frame with cross-validation metrics
  including RMSE, MAE, and R-squared for each fold and overall. NULL if
  cv_folds is NULL.

- formula:

  The model formula used

- predictors:

  Character vector of predictor names used

- method:

  Character string indicating which method was used

- n_obs:

  Number of observations used for training (after removing NAs)

## Details

The Random Forest model learns the relationship between environmental
map covariates and systematic bias in the AGB map.

The model trains on the relationship: \$\$bias = f(covariates) +
\epsilon\$\$

Where bias = mapAGB - plotAGB, and f() is learned by the Random Forest.

The ranger package is recommended for faster training, especially with
large datasets or when performing cross-validation. The function
automatically removes rows with NA values in any predictor or response
variable before training.

When `cv_folds` is specified, the function performs k-fold
cross-validation to assess model performance. This is useful for model
evaluation and comparison.

## References

Araza, A., et al. (2022). A comprehensive framework for assessing the
accuracy and uncertainty of global above-ground biomass maps. Remote
Sensing of Environment, 272, 112917.
https://doi.org/10.1016/j.rse.2022.112917

Wright, M. N. & Ziegler, A. (2017). ranger: A Fast Implementation of
Random Forests for High Dimensional Data in C++ and R. Journal of
Statistical Software, 77(1), 1-17. https://doi.org/10.18637/jss.v077.i01

## See also

[`extractBiasCovariates`](https://atnt.github.io/Plot2Map/reference/extractBiasCovariates.md)
for preparing the input data,
[`predictBiasMap`](https://atnt.github.io/Plot2Map/reference/predictBiasMap.md)
for generating wall-to-wall predictions,
[`adjustMapBias`](https://atnt.github.io/Plot2Map/reference/adjustMapBias.md)
for applying the bias correction

## Examples

``` r
if (FALSE) { # \dontrun{
# Following from extractBiasCovariates example
# Assuming bias_data is already created

# Train model with default settings
bias_model <- trainBiasModel(
  bias_data = bias_data,
  predictors = c("map", "sd", "height", "treecover")
)

# View variable importance
print(bias_model$importance)

# Train with cross-validation
bias_model_cv <- trainBiasModel(
  bias_data = bias_data,
  predictors = c("map", "sd", "height", "treecover"),
  cv_folds = 10
)

# View CV performance
print(bias_model_cv$cv_results)
} # }
```
