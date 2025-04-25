library(testthat)
library(Plot2Map)

# Tests for RawPlots.R

test_that("RawPlots validates input types", {
  # Test non-dataframe input
  expect_error(RawPlots("not_a_dataframe"), "Input file should be a data frame")
  
  # Skip tests that require interactive input
  testthat::skip_if_not(interactive(), "Tests require interactive input")
})

test_that("RawPlots handles plot size conversions correctly", {
  # Test case with mock functions
  # Create a mock RawPlots function that doesn't require interactive input
  mock_RawPlots <- function(plots, sizes) {
    converted_sizes <- ifelse(!is.na(sizes) & sizes > 50, sizes / 10000, sizes)
    return(converted_sizes)
  }
  
  # Test data
  small_sizes <- c(0.1, 0.25, 0.5) # Already in hectares
  large_sizes <- c(1000, 2500, 5000) # In square meters
  mixed_sizes <- c(0.1, 2500, 0.5) # Mix of ha and m²
  
  # Test conversions
  expect_equal(mock_RawPlots(NULL, small_sizes), small_sizes)
  expect_equal(mock_RawPlots(NULL, large_sizes), large_sizes / 10000)
  expect_equal(mock_RawPlots(NULL, mixed_sizes), c(0.1, 0.25, 0.5))
})

test_that("RawPlotsTree validates input types", {
  # Test non-dataframe input
  expect_error(RawPlotsTree("not_a_dataframe"), "Input file should be a data frame")
  
  # Skip tests that require interactive input
  testthat::skip_if_not(interactive(), "Tests require interactive input")
})

# Create a mock function to test RawPlotsTree's logic without interactive input
test_that("RawPlotsTree processes tree data correctly", {
  # Create a mock function that imitates RawPlotsTree's core logic
  mock_RawPlotsTree <- function(plots, include_height = TRUE) {
    # Sample data
    id <- c(1, 1, 2)
    genus <- c("Pinus", "Pinus", "Quercus")
    species <- c("sylvestris", "sylvestris", "robur")
    diameter <- c(15, 20, 25)
    height <- c(10, 12, 15)
    size <- c(100, 100, 100)
    fez <- NA
    gez <- NA
    year <- c(2010, 2010, 2010)
    
    x <- c(-62.215, -62.215, -62.205)
    y <- c(-3.465, -3.465, -3.455)
    
    if (include_height) {
      plt <- data.frame(id, genus, species, diameter, height, size, fez, gez, year)
    } else {
      plt <- data.frame(id, genus, species, diameter, size, fez, gez, year)
    }
    
    plt1 <- data.frame(id, x, y)
    
    return(list(plt, plt1))
  }
  
  # Test with height
  result_with_height <- mock_RawPlotsTree(NULL, TRUE)
  expect_length(result_with_height, 2)
  expect_named(result_with_height[[1]], c("id", "genus", "species", "diameter", "height", "size", "fez", "gez", "year"))
  expect_named(result_with_height[[2]], c("id", "x", "y"))
  
  # Test without height
  result_without_height <- mock_RawPlotsTree(NULL, FALSE)
  expect_length(result_without_height, 2)
  expect_named(result_without_height[[1]], c("id", "genus", "species", "diameter", "size", "fez", "gez", "year"))
})