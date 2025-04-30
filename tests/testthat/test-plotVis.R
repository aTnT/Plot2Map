library(testthat)
library(Plot2Map)

# Tests for PlotVis.R

test_that("Binned function handles data correctly", {
  skip_on_ci() # Skip on continuous integration
  
  # Create test data
  set.seed(42)
  x <- runif(100, 0, 400)
  y <- x + rnorm(100, 0, 30)
  
  # Test with minimal parameters (no file output)
  expect_no_error(Binned(x, y, "Test Binned"))
  
  # Test with invalid inputs
  expect_error(Binned(x, y[1:50], "Error Test"), "Input vectors x and y must have the same length")
  expect_error(Binned(numeric(0), numeric(0), "Empty Test"), "Input vectors cannot be empty")
})

test_that("Binned function creates output files", {
  skip_on_ci() # Skip on continuous integration
  skip("Test that generates files") # Skip during normal testing
  
  # Create test data
  set.seed(42)
  x <- runif(100, 0, 400)
  y <- x + rnorm(100, 0, 30)
  
  # Create a temporary directory for outputs
  temp_dir <- file.path(tempdir(), "test_binned")
  dir.create(temp_dir, showWarnings = FALSE)
  
  # Test file output
  Binned(x, y, "Test Binned File", "test_binned.png", temp_dir)
  expect_true(file.exists(file.path(temp_dir, "test_binned.png")))
})

test_that("Scatter function handles data correctly", {
  skip_on_ci() # Skip on continuous integration
  
  # Create test data
  set.seed(42)
  x <- runif(100, 0, 400)
  y <- x + rnorm(100, 0, 30)
  
  # Test with minimal parameters (no file output)
  expect_no_error(Scatter(x, y, "Test Scatter"))
  
  # Test with invalid inputs
  expect_error(Scatter(x, y[1:50], "Error Test"), "Input vectors x and y must have the same length")
  expect_error(Scatter(numeric(0), numeric(0), "Empty Test"), "Input vectors cannot be empty")
})

test_that("Scatter function creates output files", {
  skip_on_ci() # Skip on continuous integration
  skip("Test that generates files") # Skip during normal testing
  
  # Create test data
  set.seed(42)
  x <- runif(100, 0, 400)
  y <- x + rnorm(100, 0, 30)
  
  # Create a temporary directory for outputs
  temp_dir <- file.path(tempdir(), "test_scatter")
  dir.create(temp_dir, showWarnings = FALSE)
  
  # Test file output
  Scatter(x, y, "Test Scatter File", "test_scatter.png", temp_dir)
  expect_true(file.exists(file.path(temp_dir, "test_scatter.png")))
})

test_that("TwoPlots function handles data correctly", {
  skip_on_ci() # Skip on continuous integration
  
  # Create test data
  set.seed(42)
  x <- runif(100, 0, 400)
  y1 <- x + rnorm(100, 0, 30)
  y2 <- x + rnorm(100, 10, 20)
  
  # Test with minimal parameters (no file output)
  expect_no_error(TwoPlots(x, y1, x, y2, "Test TwoPlots"))
  
  # Test with 'harmo' title
  expect_no_error(TwoPlots(x, y1, x, y2, "Test TwoPlots", title = "harmo"))
  
  # Test with invalid inputs
  expect_error(TwoPlots(x, y1[1:50], x, y2, "Error Test"), 
               "Input vectors x and y must have the same length")
  expect_error(TwoPlots(x, y1, x, y2[1:50], "Error Test"), 
               "Input vectors x1 and y1 must have the same length")
  expect_error(TwoPlots(numeric(0), numeric(0), numeric(0), numeric(0), "Empty Test"), 
               "Input vectors cannot be empty")
})

test_that("TwoPlots function creates output files", {
  skip_on_ci() # Skip on continuous integration
  skip("Test that generates files") # Skip during normal testing
  
  # Create test data
  set.seed(42)
  x <- runif(100, 0, 400)
  y1 <- x + rnorm(100, 0, 30)
  y2 <- x + rnorm(100, 10, 20)
  
  # Create a temporary directory for outputs
  temp_dir <- file.path(tempdir(), "test_twoplots")
  dir.create(temp_dir, showWarnings = FALSE)
  
  # Test file output
  TwoPlots(x, y1, x, y2, "Test TwoPlots File", "test_twoplots.png", "harmo", temp_dir)
  expect_true(file.exists(file.path(temp_dir, "test_twoplots.png")))
})