library(testthat)
library(Plot2Map)
library(sf)

# Tests for ESACCIBiomass.R

test_that("AGBtileNames and download_esacci_biomass functions behaves consistently", {
  skip("Skipping test that downloads large files")
  
  # Amazon, 1 tile
  amazon_pol <- st_polygon(list(rbind(c(-62.2159, -3.4653), c(-62.2059, -3.4653),
                                     c(-62.2059, -3.4553), c(-62.2159, -3.4553),
                                     c(-62.2159, -3.4653))))
  amazon_pol_sf <- st_sfc(amazon_pol, crs = 4326)

  # Mexico, 2 tiles
  mexico_pol <- st_polygon(list(rbind(c(-99.5,18), c(-101,19), c(-101,19), c(-99.5,19), c(-99.5,18))))
  mexico_pol_sf <- st_sfc(mexico_pol, crs = 4326)

  result_amazon <- ESACCIAGBtileNames(amazon_pol_sf, esacci_biomass_year=2021, esacci_biomass_version="v5.01")
  result_mexico <- ESACCIAGBtileNames(mexico_pol_sf, esacci_biomass_year=2021, esacci_biomass_version="v5.01")

  dwn_amazon <- download_esacci_biomass(file_names = result_amazon)
  dwn_mexico <- download_esacci_biomass(file_names = result_mexico)

  expect_equal(dwn_amazon, "data/ESACCI-BIOMASS/N00W070_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif")
  expect_equal(dwn_mexico, c("data/ESACCI-BIOMASS/N20W110_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif",
                            "data/ESACCI-BIOMASS/N20W100_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2021-fv5.0.tif"))
})