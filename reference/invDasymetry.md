# Inverse dasymetric mapping

This function performs inverse dasymetric mapping on plot data. It
selects plots based on given criteria, optionally aggregates them, and
calculates forest fraction and Above Ground Biomass (AGB) data for each
plot or cell. Inverse dasymetric mapping is particularly useful for
comparing field inventory plot measurements with remote sensing biomass
maps by accounting for differences in spatial scales and forest cover
percentages.

## Usage

``` r
invDasymetry(
  plot_data = NULL,
  clmn = "ZONE",
  value = "Europe",
  aggr = NULL,
  minPlots = 1,
  weighted_mean = FALSE,
  is_poly = TRUE,
  dataset = "custom",
  agb_raster_path = NULL,
  forest_mask_path = NULL,
  threshold = 0,
  map_year = NULL,
  map_resolution = NULL,
  esacci_biomass_year = "latest",
  esacci_biomass_version = "latest",
  esacci_folder = "data/ESACCI-BIOMASS",
  gedi_l4b_folder = "data/GEDI_L4B/",
  gedi_l4b_band = "MU",
  gedi_l4b_resolution = 0.001,
  gfc_folder = "data/GFC",
  gfc_dataset_year = "latest",
  timeout = 600,
  parallel = FALSE,
  n_cores = parallel::detectCores() - 1,
  memfrac = 0.3,
  worker_memfrac = 0.2,
  batch_size = NULL,
  crop_rasters = TRUE
)
```

## Arguments

- plot_data:

  data.frame, Plot dataset containing required columns. For
  non-aggregated mode, the required columns are "POINT_X", "POINT_Y",
  "AGB_T_HA_ORIG", "AGB_T_HA", and "SIZE_HA". For aggregated mode (when
  aggr is not NULL), the "varPlot" column is also required, but will be
  automatically calculated if missing.

- clmn:

  character, Column name for plot selection (e.g., "ZONE", "CONTINENT").
  Set to NULL to process all plots without filtering (default: "ZONE").

- value:

  character, Value to select in the specified column (e.g., "Europe",
  "Africa"). Ignored if clmn is NULL (default: "Europe").

- aggr:

  numeric, Aggregation factor in degrees (e.g., 0.1 for 0.1-degree
  cells). Set to NULL for no aggregation. When aggregated, plots falling
  within the same grid cell are combined using inverse variance
  weighting.

- minPlots:

  integer, Minimum number of plots per aggregated cell. Cells with fewer
  plots will be excluded.

- weighted_mean:

  Logical, if TRUE the weighted mean is calculated considering the
  approximate fraction of each cell that is covered by the roi (default
  is FALSE).

- is_poly:

  logical, Whether input plots are polygons (TRUE) or should be
  converted to polygons (FALSE).

- dataset:

  Character, the dataset to use for AGB estimation. Options are
  "custom", "esacci", or "gedi". Default is "custom".

- agb_raster_path:

  character, File path to the custom AGB raster.

- forest_mask_path:

  character, File path to the forest mask raster.

- threshold:

  numeric, Threshold (0-100) for tree cover calculation and forest
  masking (e.g. 0 or 10). Only pixels with tree cover percentage above
  this threshold will contribute to biomass estimates.

- map_year:

  numeric, The year of the map data. If not provided, it will be
  detected automatically from the available data sources.

- map_resolution:

  numeric, The resolution of the map data in degrees. If not provided,
  it will be detected automatically from the available data sources.
  Used for variance calculation when aggregating.

- esacci_biomass_year:

  The ESA CCI BIOMASS AGB tiles year to use. Use either 2010, 2015,
  2016, 2017, 2018, 2019, 2020, 2021, 2022 or "latest" (default).

- esacci_biomass_version:

  The ESA CCI BIOMASS AGB tiles version to use. Use either "v2.0",
  "v3.0", "v4.0", "v5.0", "v5.01", "v6.0" or "latest" (default).

- esacci_folder:

  Directory to save downloaded ESA CCI BIOMASS AGB files. Default is the
  relative path "data/ESACCI-BIOMASS".

- gedi_l4b_folder:

  Character, the folder to save the downloaded GeoTIFF file. Default is
  "data/GEDI_L4B/".

- gedi_l4b_band:

  Character, the band to filter for. See options in the Details section
  below. Default is "MU".

- gedi_l4b_resolution:

  Numeric, the spatial resolution of the processed output GeoTIFF in
  degrees. The native resolution of the GEDI L4B gridded dataset is 1
  km, approximately 0.001 degrees at the equator. Default is 0.001.

- gfc_folder:

  Character string specifying the directory to download GFC data.

- gfc_dataset_year:

  Numeric value describing which version of the Hansen data to use: any
  year in the 2018-2023 range or "latest" (default).

- timeout:

  Number of seconds for reaching file download timeout.

- parallel:

  logical, Enable parallel processing for faster computation on
  multi-core systems. Default is FALSE.

- n_cores:

  numeric, Number of cores to use for parallel processing.

- memfrac:

  numeric, Memory fraction (0-1) for Terra to use in the main process.
  Default is 0.3.

- worker_memfrac:

  numeric, Memory fraction (0-1) for Terra to use in each worker process
  during parallel execution (future use - currently fixed at 0.2
  internally). Default is 0.2.

- batch_size:

  integer, Number of plots to process in each batch for better memory
  management. If NULL (default), batch size is auto-determined based on
  dataset size.

- crop_rasters:

  logical, Whether to crop rasters to the region of interest before
  processing (TRUE by default). Set to FALSE if you encounter issues
  with cropping or if the plots are widely dispersed.

## Value

A data frame with the following columns:

- plotAGB_X:

  AGB values for the given forest threshold, where X is the threshold
  value (e.g., plotAGB_10 if threshold=10). When aggregated, these
  values are derived from weighted means using inverse variance
  weighting. Units are in tonnes per hectare (t/ha).

- tfPlotAGB:

  Tree-filtered plot AGB (only when not aggregated). This is equivalent
  to the AGB_T_HA values from the input data.

- orgPlotAGB:

  Original plot AGB, derived from AGB_T_HA_ORIG in the input data.

- mapAGB:

  AGB from map sampling, representing the biomass values extracted from
  the reference map at each plot or cell location.

- SIZE_HA:

  Plot size in hectares. For aggregated cells, this is the mean plot
  size within the cell.

- x:

  X-coordinate of plot or cell center (longitude).

- y:

  Y-coordinate of plot or cell center (latitude).

- n:

  Number of plots within each aggregated cell (only included when aggr
  is not NULL).

## Details

The function performs inverse dasymetric mapping through these key
steps:

1.  **Plot Selection**: Selects plots based on the specified criteria
    (clmn and value).

2.  **Spatial Processing** (two modes):

    - **Non-Aggregated Mode** (aggr = NULL): Each plot is processed
      individually.

      - Tree cover percentage is calculated for each plot

      - Biomass values are adjusted based on tree cover percentage

    - **Aggregated Mode** (e.g., aggr = 0.1): Plots are grouped into
      grid cells.

      - New grid cell coordinates are calculated using integer division

      - Plot measurements are aggregated using inverse variance
        weighting

      - Cells with insufficient plot counts (less than minPlots) are
        filtered out

3.  **Forest Cover Correction**:

    - Tree cover percentage is calculated for each plot or cell

    - Only forest pixels (based on threshold) contribute to biomass
      estimates

4.  **Map Comparison**:

    - AGB values are sampled from reference maps

    - Enables direct comparison between field-measured and map-estimated
      biomass

## Note

- Ensure all required columns are present in the input plot data.

- For parallel processing, adjust `n_cores` based on your system's
  capabilities.

- Large datasets may require significant processing time and memory.

- When using aggregation, the function automatically calculates
  `varPlot` if missing.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with sample data
library(Plot2Map)
data(plots)

# Create a sample dataset and process it
set.seed(42)
sampled_plots <- plots[sample(nrow(plots), 100), ]
plot_data <- Deforested(sampled_plots, gfc_folder = "data/GFC", map_year = 2020)
plot_data <- BiomePair(plot_data$non_deforested_plots)
plot_data <- TempApplyVar(plot_data, 2020)

# Example 1: Non-aggregated mode with custom AGB raster
result_individual <- invDasymetry(
  plot_data = plot_data,
  clmn = "ZONE",
  value = "Europe",
  aggr = NULL,  # No aggregation
  threshold = 10,  # 10% tree cover threshold
  dataset = "custom",
  agb_raster_path = "path/to/agb_raster.tif"
)

# Example 2: Aggregated mode with ESA CCI data
result_aggregated <- invDasymetry(
  plot_data = plot_data,
  clmn = "ZONE",
  value = "Europe",
  aggr = 0.25,  # 0.25° aggregation
  minPlots = 2,  # Minimum 2 plots per cell
  threshold = 10,
  dataset = "esacci",
  esacci_biomass_year = "latest"
)

# Example 3: Process all plots without filtering (no zone selection)
result_all_plots <- invDasymetry(
  plot_data = plot_data,
  clmn = NULL,  # No filtering - process all plots
  threshold = 10,
  dataset = "esacci"
)

# Compare plot AGB with map AGB
plot(
  result_aggregated$plotAGB_10,
  result_aggregated$mapAGB,
  xlab = "Plot AGB (t/ha)",
  ylab = "Map AGB (t/ha)",
  main = "Plot vs Map Biomass Comparison"
)
abline(0, 1, col = "red")  # 1:1 line
} # }
```
