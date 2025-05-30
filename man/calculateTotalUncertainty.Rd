% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MeasurementErr.R
\name{calculateTotalUncertainty}
\alias{calculateTotalUncertainty}
\title{Comprehensive uncertainty calculation for all plot data types}
\usage{
calculateTotalUncertainty(
  plot_data,
  map_year,
  map_resolution = 100,
  biome_info = TRUE,
  se_data = NULL
)
}
\arguments{
\item{plot_data}{A data frame containing plot data. Required columns:
\itemize{
\item \code{PLOT_ID} - Plot identifier
\item \code{AGB_T_HA} - Above-ground biomass in tons per hectare
\item \code{SIZE_HA} - Plot size in hectares
\item \code{GEZ} - Global ecological zone (e.g., "Tropical", "Temperate", "Boreal", "Subtropical")
}

Additional columns required when \code{biome_info=TRUE}:
\itemize{
\item \code{ZONE} - Continental zone (e.g., "Europe", "Asia", "Africa")
\item \code{AVG_YEAR} - Year of plot measurement for temporal adjustment
}

If \code{sdTree}, \code{sdSE}, or \code{sdGrowth} columns exist, they will be preserved.}

\item{map_year}{Numeric value indicating the map year for temporal adjustment. This is used
to calculate growth uncertainty based on the time difference between plot measurement and map production.}

\item{map_resolution}{Numeric value indicating the map resolution in meters (default: 100).
This affects sampling error calculations based on the difference between plot size and pixel size.}

\item{biome_info}{Logical indicating whether to use biome information (GEZ column) for growth uncertainty
calculation (default: TRUE). When TRUE, the function uses TempVar which requires 'ZONE', 'GEZ', and 'AVG_YEAR'
columns to be present in the plot_data. If FALSE, the function requires that \code{sdGrowth} column
be provided in the input data.}

\item{se_data}{Optional data frame with sampling error data that can be used instead of loading se.csv.
Must contain columns:
\itemize{
\item \code{SIZE_HA} - Plot size in hectares
\item \code{RS_HA} - Pixel resolution in hectares
\item \code{ratio} - Ratio of plot size to pixel size
\item \code{cv} - Coefficient of variation (percent)
}}
}
\value{
A list containing:
\describe{
\item{data}{The input plot data with additional uncertainty columns:
\itemize{
\item \code{sdTree} - Standard deviation from measurement error
\item \code{sdSE} - Standard deviation from sampling error
\item \code{sdGrowth} - Standard deviation from growth/temporal uncertainty
\item \code{varPlot} - Total variance (sum of squared standard deviations)
\item \code{sdTotal} - Total standard deviation (square root of varPlot)
}
}
\item{plot_type}{The detected plot data type: "tree_level", "point", "polygon", "nested", or "lidar"}
\item{uncertainty_components}{The relative contribution of each uncertainty component as a proportion of the total variance}
}
}
\description{
This function provides a unified framework for calculating uncertainty across different plot data types.
It automatically detects the plot data type, preserves existing uncertainty components if present,
and calculates missing components as needed.
}
\details{
The function handles three primary sources of uncertainty:
\itemize{
\item \strong{Measurement uncertainty (sdTree)}: Derived from allometric equations, wood density estimates,
and measurement error. For tree-level data, this is calculated by the BIOMASS package with Monte Carlo methods.
For plot-level data, it's estimated using a random forest model trained on a large dataset.
\item \strong{Sampling uncertainty (sdSE)}: Accounts for the sampling error introduced by differing
plot sizes vs. pixel sizes, following the approach from Réjou-Méchain et al. (2014).
\item \strong{Growth uncertainty (sdGrowth)}: Adjusts for temporal differences between plot measurement year
and map production year, using growth rates specific to each ecological zone.
}

These components are combined using error propagation principles:
\deqn{varPlot = sdTree^2 + sdSE^2 + sdGrowth^2}
\deqn{sdTotal = \sqrt{varPlot}}
}
\section{Calculation Process}{


\subsection{Step 1: Measurement Error (sdTree)}{
\itemize{
\item \strong{For tree-level data}: Uses BIOMASS package Monte Carlo simulations that account for wood density
uncertainty, diameter measurement errors, and allometric model errors.
\item \strong{For plot-level data}: Uses a random forest model trained on thousands of plots across biomes to
predict measurement uncertainty based on plot AGB, plot size, and ecological zone.
\item If a pre-existing sdTree column exists, its values are preserved.
}
}

\subsection{Step 2: Sampling Error (sdSE)}{
\itemize{
\item Uses a random forest model trained on geo-simulation data that relates plot-to-pixel size ratios to
coefficient of variation.
\item The model considers the relationship between plot size (SIZE_HA), remote sensing pixel size (RS_HA),
and their ratio.
\item The resulting coefficient of variation is converted to standard deviation by multiplying by mean AGB.
\item Custom sampling error data can be provided through the \code{se_data} parameter.
}
}

\subsection{Step 3: Growth Uncertainty (sdGrowth)}{
\itemize{
\item If \code{biome_info = TRUE}, uses biome-specific growth rates and uncertainty from the TempVar function.
\item The function considers the time difference between plot measurement year (AVG_YEAR) and map year.
\item Each ecological zone (GEZ) is processed separately to apply biome-specific growth rates.
\item If biome information isn't available or \code{biome_info = FALSE}, the function requires that \code{sdGrowth} column be provided in the input data.
}
}

\subsection{Step 4: Total Uncertainty}{
\itemize{
\item Combines all uncertainty components by summing their variances: \code{varPlot = sdTree^2 + sdSE^2 + sdGrowth^2}
\item Calculates total standard deviation as \code{sdTotal = sqrt(varPlot)}
\item Reports the relative contribution of each component to the total variance.
}
}
}

\examples{
# Example 1: Using existing plot data
# First load the sample plots dataset included with the package
data(plots)

# Create a subset of plots for demonstration
set.seed(42)
sample_plots <- plots[sample(nrow(plots), 5), ]

# Add biome information (required for uncertainty calculation)
sample_plots <- BiomePair(sample_plots)

# Apply TempApplyVar to adjust biomass to map year and calculate temporal variance
sample_plots <- TempApplyVar(sample_plots, map_year = 2020)

# Calculate total uncertainty components for map year 2020
result <- calculateTotalUncertainty(sample_plots, map_year = 2020, map_resolution = 100)

# View the results
head(result$data)

# See the relative contribution of each uncertainty component
result$uncertainty_components

# Example 2: Working with data that already has measurement uncertainty
# First process tree-level data to get measurement uncertainty
# Load sample tree data
tree_data <- utils::read.csv(sample_file("SampleTree.csv"))
tree_locations <- utils::read.csv(sample_file("SampleTreeXY.csv"))

# Calculate AGB and measurement uncertainty
tree_results <- sd_tree(tree_data, tree_locations, region = "World")

# Add required biome information
tree_results <- BiomePair(tree_results)

# Calculate total uncertainty with tree data
tree_uncertainty <- calculateTotalUncertainty(tree_results, map_year = 2020)

# Example 3: Using custom sampling error data
# Create custom sampling error data (coefficient of variation by plot/pixel size ratio)
custom_se_data <- data.frame(
  SIZE_HA = c(0.1, 0.25, 0.5, 1.0),  # Plot sizes in hectares
  RS_HA = c(1, 1, 1, 1),            # Remote sensing pixel size in hectares
  ratio = c(0.1, 0.25, 0.5, 1.0),   # Ratio of plot size to pixel size
  cv = c(20, 15, 10, 5)             # Coefficient of variation (\%)
)

# Calculate uncertainty using custom sampling error model
custom_result <- calculateTotalUncertainty(
  sample_plots,
  map_year = 2020,
  map_resolution = 100,
  se_data = custom_se_data
)

# Example 4: Using manual growth uncertainty values
# First, prepare data with manual sdGrowth values
manual_growth_plots <- sample_plots
# Add manual growth uncertainty of 7\% of AGB
manual_growth_plots$sdGrowth <- manual_growth_plots$AGB_T_HA * 0.07

# Now calculate uncertainty with biome_info = FALSE
simple_result <- calculateTotalUncertainty(
  manual_growth_plots,
  map_year = 2020,
  map_resolution = 100,
  biome_info = FALSE
)
}
