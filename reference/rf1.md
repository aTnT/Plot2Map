# Random Forest model for tree measurement uncertainty prediction

A pre-trained Random Forest model that predicts tree measurement
uncertainty based on plot-level characteristics.

## Format

A random forest model object for predicting tree measurement uncertainty

## Details

The model takes the following inputs:

- agb:

  Above-ground biomass in tons per hectare (AGB_T_HA)

- size:

  Plot size in square meters (SIZE_HA converted to m²)

- gez:

  Global Ecological Zone as a factor with levels: "Boreal",
  "Subtropical", "Temperate", "Tropical"

## Examples

``` r
# Load the model
rf1_path <- sample_file("rf1.RData")
load(rf1_path)
print(rf1)
#> Ranger result
#> 
#> Call:
#>  ranger(sd ~ ., data = df1, importance = "permutation") 
#> 
#> Type:                             Regression 
#> Number of trees:                  500 
#> Sample size:                      196476 
#> Number of independent variables:  3 
#> Mtry:                             1 
#> Target node size:                 5 
#> Variable importance mode:         permutation 
#> Splitrule:                        variance 
#> OOB prediction error (MSE):       65.30938 
#> R squared (OOB):                  0.653502 

# Format input data
plotsPred <- data.frame(
  agb = c(150, 200),
  size = c(10000, 25000), # in m²
  gez = factor(c("Tropical", "Temperate"),
               levels = c("Boreal", "Subtropical", "Temperate", "Tropical"))
)
print(plotsPred)
#>   agb  size       gez
#> 1 150 10000  Tropical
#> 2 200 25000 Temperate

# Predict measurement uncertainty
sdTree <- predict(rf1, plotsPred)$predictions
print(sdTree)
#> [1] 22.12851 19.19068
```
