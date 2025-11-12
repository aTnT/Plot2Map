# Process a single pair of tree cover and forest loss files

Process a single pair of tree cover and forest loss files

## Usage

``` r
process_pair(
  id,
  year,
  treecover_threshold,
  output_folder,
  treecover_files,
  lossyear_files,
  treecover_ids,
  lossyear_ids,
  baseline
)
```

## Arguments

- id:

  The identifier for the file pair

- year:

  The year for which to compute tree cover

- treecover_threshold:

  The threshold for tree cover

- output_folder:

  The folder to save output rasters

- treecover_files:

  List of tree cover files

- lossyear_files:

  List of forest loss files

- treecover_ids:

  IDs for tree cover files

- lossyear_ids:

  IDs for forest loss files

- baseline:

  Baseline year for tree cover dataset (2000 or 2010)

## Value

A file path to a geotiff raster representing the computed tree cover
