# Tests: invDasymetry

test_year <- 2022

# Create a test directory if it doesn't exist
test_dir <- "tests/test_data"
if (!dir.exists(test_dir)) {
  dir.create(test_dir,  recursive = TRUE)
}

# Create a sample dataset of 10 plots:
set.seed(42)
sampled_plots <- plots[sample(nrow(plots), 1000), ]
sampled_plots <- Deforested(sampled_plots, gfc_folder = test_dir,  map_year = test_year)
plot_data <- BiomePair(sampled_plots$non_deforested_plots)
plot_data <- TempApply(plot_data, test_year)
plot_data


#fmask_test <- terra::rast(file.path(test_dir,  "GLAD_TCC-2010_treecover_calc_2023_40N_010W.tif"))


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

#agb_raster_test <- terra::rast(file.path(test_dir,  "N40W010_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif"))


# parallel = TRUE
# plot_data = plot_data
# clmn = "ZONE"
# value = "Europe"
# aggr = NULL
# minPlots = 1
# weighted_mean = FALSE
# is_poly = FALSE
# dataset = "custom"
# thresholds = 10
# esacci_biomass_year = "latest"
# esacci_biomass_version = "latest"
# esacci_folder = test_dir
# gedi_l4b_folder = test_dir
# gedi_l4b_band = "MU"
# gedi_l4b_resolution = 0.001
# n_cores = parallel::detectCores() - 1
# timeout = 600
# agb_raster_path = NULL
# forest_mask_path = NULL


benchmark_parallelism <- microbenchmark::microbenchmark(

  result_no_aggr_parallel_optim = invDasymetry (parallel = FALSE, plot_data = plot_data, clmn = "ZONE", value = "Europe", aggr = NULL,
                                                minPlots = 1, weighted_mean = FALSE, is_poly = FALSE,
                                                dataset = "custom",
                                                agb_raster_path = file.path(test_dir,  "N40W010_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif"),
                                                forest_mask_path = file.path(test_dir,  "GLAD_TCC-2010_treecover_calc_2023_40N_010W.tif"),
                                                threshold = 10,
                                                esacci_biomass_year = "latest", esacci_biomass_version = "latest",
                                                esacci_folder = test_dir,
                                                gedi_l4b_folder = test_dir,
                                                gedi_l4b_band = "MU", gedi_l4b_resolution = 0.001,
                                                n_cores = 1, timeout = 600),
  esacci_folder = test_dir,
  gedi_l4b_folder = test_dir)
  result_no_aggr_parallel = invDasymetry (parallel = TRUE, plot_data = plot_data, clmn = "ZONE", value = "Europe", aggr = NULL,
                                          minPlots = 1, weighted_mean = FALSE, is_poly = FALSE,
                                          dataset = "custom",
                                          agb_raster_path = file.path(test_dir,  "N40W010_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif"),
                                          forest_mask_path = file.path(test_dir,  "GLAD_TCC-2010_treecover_calc_2023_40N_010W.tif"),
                                          threshold = 10,
                                          esacci_biomass_year = "latest", esacci_biomass_version = "latest",
                                          esacci_folder = test_dir,
                                          gedi_l4b_folder = test_dir,
                                          gedi_l4b_band = "MU", gedi_l4b_resolution = 0.001,
                                          n_cores = 1, timeout = 600),

  times = 1
)
benchmark_parallelism






t1 <- invDasymetry (parallel = TRUE, plot_data = plot_data, clmn = "ZONE", value = "Europe", aggr = NULL,
                    minPlots = 1, weighted_mean = FALSE, is_poly = FALSE,
                    dataset = "custom",
                    agb_raster_path = file.path(test_dir,  "N40W010_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif"),
                    forest_mask_path = file.path(test_dir,  "GLAD_TCC-2010_treecover_calc_2023_40N_010W.tif"),
                    threshold = 0,
                    esacci_folder = test_dir,
                    gedi_l4b_folder = test_dir
                    )

t2 <- invDasymetry (parallel = TRUE, plot_data = plot_data, clmn = "ZONE", value = "Europe", aggr = NULL,
                    minPlots = 1, weighted_mean = FALSE, is_poly = FALSE,
                    dataset = "esacci",
                    threshold = 0,
                    esacci_folder = test_dir,
                    gedi_l4b_folder = test_dir)

# t3 <- invDasymetry (parallel = TRUE, plot_data = plot_data, clmn = "ZONE", value = "Europe", aggr = NULL,
#                     minPlots = 1, weighted_mean = FALSE, is_poly = FALSE,
#                     dataset = "gedi",
#                     threshold = 0,
#                     esacci_folder = test_dir,
#                     gedi_l4b_folder = test_dir)



# Test with agb_raster_path and forest_mask_path not NULL:
# N = 756
# Unit: seconds
# expr       min        lq      mean    median
# result_no_aggr_parallel  575.6292  575.6292  575.6292  575.6292
# result_no_aggr_not_parallel 1635.1998 1635.1998 1635.1998 1635.1998
# uq       max neval
# 575.6292  575.6292     1
# 1635.1998 1635.1998     1

# N=30
# > benchmark_parallelism
# Unit: seconds
# expr      min       lq     mean   median       uq
# result_no_aggr_parallel 24.75865 24.75865 24.75865 24.75865 24.75865
# result_no_aggr_not_parallel 86.53914 86.53914 86.53914 86.53914 86.53914
# max neval
# 24.75865     1
# 86.53914     1



