# Perform k-fold cross-validation for bias model

Internal helper function to perform k-fold cross-validation for Random
Forest bias models. Not exported.

## Usage

``` r
performCrossValidation(data, formula, method, folds, seed, ...)
```

## Arguments

- data:

  data.frame with complete cases

- formula:

  Model formula

- method:

  "ranger" or "randomForest"

- folds:

  Number of folds

- seed:

  Random seed

- ...:

  Additional arguments for model training

## Value

data.frame with CV results
