library(testthat)

test_that("TempApplyVar adjusts biomass and computes sdGrowth correctly", {
  # Synthetic plot data: one below, one static, one above map year
  df <- data.frame(
    PLOT_ID     = c("p1", "p2", "p3"),
    POINT_X     = c(0, 0, 0),
    POINT_Y     = c(0, 0, 0),
    ZONE        = rep("Africa", 3),
    FAO.ecozone = rep("Tropical dry forest", 3),
    GEZ         = rep("Tropical", 3),
    AVG_YEAR    = c(1999, 2000, 2005),
    AGB_T_HA    = c(50,    60,   80),
    stringsAsFactors = FALSE
  )
  # Define simple growth-rate and SD tables
  gr_test <- data.frame(
    GEZ = "Tropical", ZONE = "Africa", FAO.ecozone = "Tropical dry forest",
    GR1 = 1, GR2 = 2, GR3 = 3,
    stringsAsFactors = FALSE
  )
  sds_test <- data.frame(
    GEZ = "Tropical", ZONE = "Africa", FAO.ecozone = "Tropical dry forest",
    SD1 = 0.1, SD2 = 0.2, SD3 = 0.3,
    stringsAsFactors = FALSE
  )
  map_year <- 2000
  out <- TempApplyVar(df, map_year, gez = "Tropical", gr = gr_test, sds = sds_test)
  # Below (1999): dt=1, inc=GR3=3 => 50+3*1=53
  expect_equal(out$AGB_T_HA[out$PLOT_ID == "p1"], 53)
  expect_equal(out$AGB_T_HA_ORIG[out$PLOT_ID == "p1"], 50)
  expect_equal(out$sdGrowth[out$PLOT_ID == "p1"], abs(53 - 50))
  # Static (2000 == map_year): no change
  expect_equal(out$AGB_T_HA[out$PLOT_ID == "p2"], 60)
  expect_equal(out$AGB_T_HA_ORIG[out$PLOT_ID == "p2"], 60)
  expect_equal(out$sdGrowth[out$PLOT_ID == "p2"], 0)
  # Above (2005): dt2=5, dec=GR3=3 => 80-3*5=65
  expect_equal(out$AGB_T_HA[out$PLOT_ID == "p3"], 65)
  expect_equal(out$AGB_T_HA_ORIG[out$PLOT_ID == "p3"], 80)
  expect_equal(out$sdGrowth[out$PLOT_ID == "p3"], abs(65 - 80))
})


# Test old Apply and Var versions yield the same AGB_T_HA output
test_that("old TempApply and the new TempApplyVar yield the same AGB_T_HA output", {
  skip("Comparing output with old api - test passed")

  # Create sample data
  set.seed(12345)
  test_data <- plots[sample(nrow(plots), 10), ]
  test_data <- BiomePair(test_data)

  # Run old and new functions
  old_result <- old_TempApply(test_data, "Subtropical", 2010)
  new_result <- TempApplyVar(test_data, 2010, "Subtropical")

  # Compare results
  expect_equal(old_result$AGB_T_HA, new_result$AGB_T_HA, tolerance = 1e-6)
  expect_equal(old_result$AGB_T_HA_ORIG, new_result$AGB_T_HA_ORIG, tolerance = 1e-6)
})


# Test internal consistency
test_that("TempApplyVar function behaves consistently in AGB", {

  set.seed(123)
  test_data <- plots[sample(nrow(plots), 10), ]
  test_data <- BiomePair(test_data)

  result <- TempApplyVar(test_data, 2004)

  # Check output structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("AGB_T_HA", "AGB_T_HA_ORIG") %in% names(result)))

  # Check for expected adjustments
  expect_true(all(result$AGB_T_HA[result$AVG_YEAR < 2004] >= result$AGB_T_HA_ORIG[result$AVG_YEAR < 2004]))
  expect_true(all(result$AGB_T_HA[result$AVG_YEAR > 2004] <= result$AGB_T_HA_ORIG[result$AVG_YEAR > 2004]))
})

test_that("TempApplyVar function behaves consistently in var", {

  set.seed(1234)
  test_data <- plots[sample(nrow(plots), 10), ]
  test_data <- BiomePair(test_data)

  result <- TempApplyVar(test_data, 2005)

  # Check output structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("AGB_T_HA", "sdGrowth") %in% names(result)))

  # Check for expected calculations
  expect_true(all(result$sdGrowth >= 0))
  expect_false(any(is.na(result$sdGrowth)))
})



