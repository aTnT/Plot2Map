# Tests: invDasymetry

# First remove the gfcanalysis package locally and install this modified development version branch instead:
remove.packages("gfcanalysis")
devtools::install_github("aTnT/gfcanalysis", ref = "download-timeout-added", force = TRUE)

library(testthat)

# Creating the test dataset

test_year <- 2022

# Create a test directory if it doesn't exist
test_dir <- "tests/test_data"
if (!dir.exists(test_dir)) {
  dir.create(test_dir,  recursive = TRUE)
}

# Create a sample dataset of 10 plots:
set.seed(42)
sampled_plots <- plots[sample(nrow(plots), 10), ]
sampled_plots <- Deforested(sampled_plots, gfc_folder = test_dir,  map_year = test_year)
plot_data <- BiomePair(sampled_plots$non_deforested_plots)
 plot_data <- TempApplyVar(plot_data, test_year)
plot_data

# Download ESACCI for testing
esacci_tile_test <- ESACCIAGBtileNames(
  sf::st_buffer(sampled_plots$non_deforested_plots[3,], 0.0001),
  esacci_biomass_year = "latest",
  esacci_biomass_version = "latest"
)

download_esacci_biomass(
  esacci_folder = test_dir,
  file_names = esacci_tile_test
)


# parallel = TRUE
# plot_data = plot_data
# clmn = "ZONE"
# value = "Europe"
# aggr = 0.1
# minPlots = 1
# weighted_mean = FALSE
# is_poly = FALSE
# dataset = "custom"
# agb_raster_path = file.path(test_dir,  "N40W010_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif")
# forest_mask_path = file.path(test_dir,  "GLAD_TCC-2010_treecover_calc_2023_40N_010W.tif")
# threshold = 0
# esacci_folder = test_dir
# gedi_l4b_folder = test_dir
# n_cores = parallel::detectCores() - 2

test_that("invDasymetry with custom dataset works", {

t1 <- invDasymetry (parallel = TRUE, plot_data = plot_data, clmn = "ZONE", value = "Europe", aggr = NULL,
                    minPlots = 1, weighted_mean = FALSE, is_poly = FALSE,
                    dataset = "custom",
                    agb_raster_path = file.path(test_dir,  "N40W010_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif"),
                    forest_mask_path = file.path(test_dir,  "GLAD_TCC-2010_treecover_calc_2023_40N_010W.tif"),
                    threshold = 0,
                    esacci_folder = test_dir,
                    gedi_l4b_folder = test_dir,
                    n_cores = parallel::detectCores() - 2
)

expect_equal(dim(t1), c(762,7))
expect_equal(max(t1$plotAGB_forestTHs), 397.5653, tolerance = 1e-06)
expect_equal(min(t1$plotAGB_forestTHs), 16.17512, tolerance = 1e-06)

})


test_that("invDasymetry with esacci dataset works", {

t2 <- invDasymetry (parallel = TRUE, plot_data = plot_data, clmn = "ZONE", value = "Europe", aggr = NULL,
                    minPlots = 1, weighted_mean = FALSE, is_poly = FALSE,
                    dataset = "esacci",
                    threshold = 0,
                    esacci_folder = test_dir,
                    gedi_l4b_folder = test_dir,
                    n_cores = parallel::detectCores() - 2)

expect_equal(dim(t2), c(762,7))
expect_equal(max(t2$plotAGB_forestTHs), 397.5653, tolerance = 1e-06)
expect_equal(min(t2$plotAGB_forestTHs), 0, tolerance = 1e-06)

})

# # GEDI doesn't have gloval coverage, should be removed:
# test_that("invDasymetry with gedi dataset works", {
#
# t3 <- invDasymetry (parallel = TRUE, plot_data = plot_data, clmn = "ZONE", value = "Africa", aggr = NULL,
#                     minPlots = 1, weighted_mean = FALSE, is_poly = FALSE,
#                     dataset = "gedi",
#                     threshold = 0,
#                     esacci_folder = test_dir,
#                     gedi_l4b_folder = test_dir)
#
# expect_equal(dim(t3), c(762,7))
# expect_equal(max(t3$plotAGB_forestTHs), 397.5653, tolerance = 1e-06)
# expect_equal(min(t3$plotAGB_forestTHs), 16.17512, tolerance = 1e-06)
#
# })
#




library(Plot2Map)

# Create a sample dataset of 10 plots:
set.seed(42)
sampled_plots <- plots[sample(nrow(plots), 10), ]
print(sampled_plots)
#      PLOT_ID     POINT_X   POINT_Y  AGB_T_HA AVG_YEAR SIZE_HA
# 2369     EU2   1.3059145  42.59214  87.87177     2001   0.196
# 5273     EU2  -0.1585048  42.63245 150.97321     2004   0.196
# 1252     EU2  -1.0662342  39.61600  58.63558     2006   0.196
# 356      EU2  -5.1210978  40.02713  37.31206     2004   0.196
# 7700     EU1  15.2187262  59.81792  67.79700     2008   0.015
# 3954     EU2  -7.1823899  37.34714  94.59592     2007   0.196
# 5403     EU2  -7.2162433  37.34765  45.56841     2007   0.196
# 932      EU2  -2.7070556  42.68534  55.14102     2003   0.196
# 5637    AUS1 145.8683679 -20.69546  53.69000     2004   0.160
# 4002     EU2  -3.9889352  40.33963  46.49786     2000   0.196

# Let's preprocess the sampled plot data.

# We start by removing deforested plots

sampled_plots <- Deforested(sampled_plots,  map_year = 2023)
sampled_plots <- BiomePair(sampled_plots$non_deforested_plots)

# Apply temporal adjustment to all Global Ecological Zones (GEZ):
 sampled_plots2 <- TempApplyVar(sampled_plots, 2023)
gezs <- unique(sampled_plots$GEZ)



# Apply temporal adjustment
 sampled_plots2 <- TempApplyVar(sampled_plots, 2023, "Subtropical")


## Apply growth data to whole plot data by identifying AGB map year with associated SD based on
## growth data from IPCC 2019
yr <- 2000+mapYear
gez <- sort(as.vector((raster::unique(sampled_plots$GEZ))))
 plots3 <- ldply(lapply (1:length(gez), function(x) TempApplyVar(plots2, gez[[x]], yr)), data.frame)
 plots.tf <- ldply(lapply (1:length(gez), function(x) TempApplyVar(plots3, gez[[x]], yr)), data.frame)
plots.tf$sdGrowth <- ifelse(is.nan(plots.tf$sdGrowth), mean(plots.tf$sdGrowth,na.rm=T),plots.tf$sdGrowth)



# Estimate errors, using a pre-trained RF model for plot-level data
load('rf1.RData') #pre-trained RF model from 10000+ plots across biomes
plotsPred <- plots2[,c('AGB_T_HA','SIZE_HA', 'GEZ')]
names(plotsPred) <- c('agb', 'size', 'gez')
plotsPred$size <- as.numeric(plotsPred$size) * 10000 #convert size to m2
plotsPred$gez = factor(plotsPred$gez,levels = c("Boreal","Subtropical","Temperate","Tropical"))
plots2$sdTree <- predict(rf1, plotsPred)[[1]]

# Validate AGB map
AGBdata <- invDasymetry("ZONE", "Europe", wghts = TRUE, is_poly = FALSE, own = FALSE)

# Visualize results
Binned(AGBdata$plotAGB_10, AGBdata$mapAGB, "Europe", "binned_plot.png")
Scatter(AGBdata$plotAGB_10, AGBdata$mapAGB, "Europe", "scatter_plot.png")

# Calculate accuracy metrics
Accuracy(AGBdata, 8, outDir, "ValidationRun")
