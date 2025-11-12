# Process LiDAR-based Reference Data (Enhanced Version)

This enhanced function processes LiDAR-based reference data from raster
files, converting them to a standardized point data format. It includes
intelligent multi-band raster handling, automatic pattern detection, and
CRS validation.

## Usage

``` r
RefLidar(
  lidar.dir,
  auto_detect = TRUE,
  pattern_config = NULL,
  raster_type = NULL,
  allow_interactive = TRUE
)
```

## Arguments

- lidar.dir:

  Directory containing ALS raster files

- auto_detect:

  Enable automatic pattern detection for PLOT_ID and YEAR (default:
  TRUE)

- pattern_config:

  Optional manual pattern configuration (overrides auto-detection)

- raster_type:

  Optional raster type specification ("AGB", "CV", or "SD"). If NULL,
  auto-detects from filename or prompts user.

- allow_interactive:

  Allow interactive prompts if auto-detection fails (default: TRUE). Set
  to FALSE for automated pipelines.

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

The enhanced function performs the following steps:

1.  **File Discovery**: Loads raster files and filters out auxiliary
    files (.aux.xml, .ovr, etc.)

2.  **CRS Validation**: Validates and reports coordinate reference
    systems across all files

3.  **Multi-band Processing**: Intelligently detects and extracts AGB
    layers from multi-band rasters

4.  **CRS Transformation**: Reprojects rasters to WGS84 (EPSG:4326) if
    necessary

5.  **Pattern Detection**: Automatically detects filename patterns for
    PLOT_ID and YEAR extraction

6.  **Data Extraction**: Converts raster data to points with proper
    metadata

7.  **Quality Validation**: Validates extracted years and provides
    processing summary

## Enhancements

**Enhancement 1 - Multi-band Support**: Automatically detects and
extracts AGB layers from multi-band rasters (e.g., meanAGB, sdAGB,
meanHbin layers). Supports rasters with uncertainty estimates.

**Enhancement 2 - Intelligent Pattern Detection**: Machine
learning-based filename pattern recognition supports multiple formats
including Brazil (ANA_A01_2017_AGB_100m.tif), Central Africa
(06_Nachtigal_AGB40.tif), Australia (GWW_2012_AGB_100m.tif), Czech
Republic (BK17a_AGB_mean.tif), and Bulgaria (bul_agb2016_utm.tif).

**Enhancement 3 - CRS Validation**: Comprehensive coordinate reference
system validation and transformation with detailed reporting of CRS
compatibility and automatic reprojection to WGS84.

**Enhancement 4 - Non-interactive Mode**: Optional `raster_type`
parameter enables fully non-interactive processing. When provided, skips
interactive prompt. When NULL, attempts auto-detection from filename
patterns.

## Examples

``` r
if (FALSE) { # \dontrun{
# Fully non-interactive usage (recommended for automated pipelines)
lidar_data <- RefLidar(lidar.dir = "data/SustainableLandscapeBrazil_v04/SLB_AGBmaps",
                       raster_type = "AGB")

# Basic usage with automatic detection (may prompt for raster type if detection fails)
lidar_data <- RefLidar(lidar.dir = "data/SustainableLandscapeBrazil_v04/SLB_AGBmaps")

# Disable automatic pattern detection for manual control
lidar_data <- RefLidar(lidar.dir = "path/to/rasters", auto_detect = FALSE, raster_type = "CV")

# Multi-band raster processing (automatically handled)
central_africa_data <- RefLidar("data/LiDAR-based_biomass_maps_Central_Africa/", raster_type = "AGB")
} # }
```
