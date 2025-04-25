library(testthat)
library(Plot2Map)
library(sf)
library(terra)

# Tests for Deforested.R

test_that("Deforested function behaves consistently across map_years", {
  skip("Skipping because this test requires large datasets")
  
  # Test params
  #plots <- read.csv(file.path("data", "SamplePlots.csv"))
  set.seed(42)
  plots_sample <- c(sample(1:dim(plots)[1], 10), 182, 200, 323, 6765) # handpicking some plots with deforestation
  sampled_plots <- plots[plots_sample,]

  result_2010 <- Deforested(sampled_plots, 2010)
  result_2020 <- Deforested(sampled_plots, 2020)
  result_2023 <- Deforested(sampled_plots, 2023)

  # Compare results
  expect_equal(
    unique(result_2010$all_plots |>
             dplyr::filter(defo > 0) |>
             dplyr::select(defo_start_year)),
    unique(result_2020$all_plots |>
             dplyr::filter(defo > 0) |>
             dplyr::select(defo_start_year))
  )
  expect_equal(
    unique(result_2020$all_plots |>
             dplyr::filter(defo > 0) |>
             dplyr::select(defo_start_year)),
    unique(result_2023$all_plots |>
             dplyr::filter(defo > 0) |>
             dplyr::select(defo_start_year))
  )
})

test_that("Deforested function behaves consistently across gfc_dataset_year", {
  skip("Skipping because this test requires large datasets")
  
  # Test params
  #plots <- read.csv(file.path("data", "SamplePlots.csv"))
  set.seed(42)
  plots_sample <- c(sample(1:dim(plots)[1], 10), 182, 200, 323, 6765) # handpicking some plots with deforestation
  sampled_plots <- plots[plots_sample,]

  result_gfc_2015 <- Deforested(sampled_plots, map_year = 2010, gfc_dataset_year = 2015)
  result_gfc_2019 <- Deforested(sampled_plots, map_year = 2010, gfc_dataset_year = 2019)
  result_gfc_2022 <- Deforested(sampled_plots, map_year = 2010, gfc_dataset_year = 2022)
  result_gfc_2023 <- Deforested(sampled_plots, map_year = 2010, gfc_dataset_year = 2023)
  result_gfc_latest <- Deforested(sampled_plots, map_year = 2010)

  # Compare results
  expect_equal(result_gfc_2015$non_deforested_plots, result_gfc_2019$non_deforested_plots)
  expect_equal(result_gfc_2022$non_deforested_plots, result_gfc_2019$non_deforested_plots)
  expect_equal(result_gfc_2015$non_deforested_plots, result_gfc_2023$non_deforested_plots)
  expect_equal(result_gfc_latest$non_deforested_plots, result_gfc_2023$non_deforested_plots)

  expect_equal(result_gfc_latest$all_plots, result_gfc_2023$all_plots)

  expect_equal(sum(result_gfc_2015$all_plots$defo), 2)
  expect_equal(sum(result_gfc_2019$all_plots$defo), 4)
  expect_equal(sum(result_gfc_2022$all_plots$defo), 5)
  expect_equal(sum(result_gfc_2023$all_plots$defo), sum(result_gfc_latest$all_plots$defo))
})

test_that("Deforested function behaves consistently", {
  skip("Skipping because this test requires access to plots data")
  
  # Test params
  #plots <- read.csv(file.path("data", "SamplePlots.csv"))
  set.seed(42)
  plots_sample <- sample(1:dim(plots)[1], 2)
  sampled_plots <- plots[plots_sample,]

  result <- Deforested(sampled_plots, map_year = 2010)

  # Check output structure
  expect_type(result, "list")
  expect_length(result, 2)
  expect_s3_class(result[[1]], "data.frame")
  expect_s3_class(result[[2]], "data.frame")

  # Check column names
  expected_cols <- c("PLOT_ID", "AGB_T_HA", "AVG_YEAR", "geometry", "defo")
  expect_true(all(expected_cols %in% colnames(result[[2]])))

  # Check deforestation filtering
  expect_true(all(result[[2]]$defo <= 0.05))
})