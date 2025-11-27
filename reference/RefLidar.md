# Process LiDAR-based Reference Data

This function processes LiDAR-based reference data from raster files,
converting them to a standardized point data format. It includes
multi-band raster handling, automatic pattern detection, and CRS
validation.

## Usage

``` r
RefLidar(
  lidar.dir,
  auto_detect = TRUE,
  raster_type = NULL,
  allow_interactive = TRUE,
  metadata_map = NULL
)
```

## Arguments

- lidar.dir:

  Directory containing ALS raster files

- auto_detect:

  Enable automatic pattern detection for PLOT_ID and YEAR (default:
  TRUE)

- raster_type:

  Optional raster type specification ("AGB", "CV", or "SD"). If NULL,
  auto-detects from filename or prompts user.

- allow_interactive:

  Allow interactive prompts if auto-detection fails (default: TRUE). Set
  to FALSE for automated pipelines.

- metadata_map:

  Optional data frame with explicit file metadata. Must have columns:
  filename (basename only), plot_id, year. Bypasses pattern detection.
  Example:
  `data.frame(filename = c("file1.tif", "file2.tif"), plot_id = c("P1", "P2"), year = c(2020, 2021))`

## Value

A data frame containing processed point data with columns varying based
on the raster type:

- PLOT_ID:

  Unique identifier for each plot (auto-extracted or user-specified)

- POINT_X:

  Longitude coordinate (WGS84)

- POINT_Y:

  Latitude coordinate (WGS84)

- AGB/CV/SD:

  Above Ground Biomass, Coefficient of Variation, or Standard Deviation
  (depending on raster type)

- AVG_YEAR:

  Year of data collection (standardized to 4-digit format)

## Details

The function performs the following steps:

1.  **File Discovery**: Loads raster files and filters out auxiliary
    files (.aux.xml, .ovr, etc.)

2.  **CRS Validation**: Validates coordinate reference systems and
    reprojects to WGS84 (EPSG:4326) if necessary

3.  **Multi-band Processing**: Intelligently detects and extracts AGB
    layers from multi-band rasters (e.g., meanAGB, sdAGB, meanHbin
    layers)

4.  **Pattern Detection**: Automatically detects filename patterns for
    PLOT_ID and YEAR extraction. Supports multiple formats including
    Brazil (ANA_A01_2017_AGB_100m.tif), Central Africa
    (06_Nachtigal_AGB40.tif), Australia (GWW_2012_AGB_100m.tif), Czech
    Republic (BK17a_AGB_mean.tif), and Bulgaria (bul_agb2016_utm.tif)

5.  **Data Extraction**: Converts raster data to points with proper
    metadata

6.  **Quality Validation**: Validates extracted years and provides
    processing summary

The `raster_type` parameter enables fully non-interactive processing for
automated pipelines. When NULL, attempts auto-detection from filename
patterns or prompts user if `allow_interactive = TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with automatic detection
lidar_data <- RefLidar(lidar.dir = "data/SustainableLandscapeBrazil_v04/SLB_AGBmaps")

# Non-interactive mode for automated pipelines
lidar_data <- RefLidar(
  lidar.dir = "data/SustainableLandscapeBrazil_v04/SLB_AGBmaps",
  raster_type = "AGB",
  allow_interactive = FALSE
)

# Explicit metadata mapping (simplest approach)
metadata <- data.frame(
  filename = c("site1.tif", "site2.tif", "site3.tif"),
  plot_id = c("PlotA", "PlotB", "PlotC"),
  year = c(2020, 2021, 2022)
)
lidar_data <- RefLidar(
  lidar.dir = "data/custom_rasters/",
  metadata_map = metadata,
  raster_type = "AGB"
)

# Process Coefficient of Variation rasters
cv_data <- RefLidar(
  lidar.dir = "data/uncertainty_maps/",
  raster_type = "CV",
  allow_interactive = FALSE
)

# Disable auto-detection for custom naming
lidar_data <- RefLidar(
  lidar.dir = "data/custom_naming/",
  auto_detect = FALSE,
  raster_type = "AGB"
)

# Multi-band raster processing
multiband_data <- RefLidar(
  lidar.dir = "data/LiDAR-based_biomass_maps_Central_Africa/",
  raster_type = "AGB"
)

# Processing multiple raster types from same directory
agb_data <- RefLidar(
  lidar.dir = "data/complete_dataset/",
  raster_type = "AGB",
  allow_interactive = FALSE
)
cv_data <- RefLidar(
  lidar.dir = "data/complete_dataset/",
  raster_type = "CV",
  allow_interactive = FALSE
)
combined_data <- merge(agb_data, cv_data,
                       by = c("PLOT_ID", "POINT_X", "POINT_Y", "AVG_YEAR"))
} # }
```
