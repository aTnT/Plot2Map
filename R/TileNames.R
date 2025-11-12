## Updates made to the new framework:
# 07/01/2025:
# Used sf and terra packages for compatibility with both sf and SpatVector objects.
# Simplified the year and version selection logic in AGBtileNames and SDtileNames.
# Removed redundant code and improved overall readability.
# 14/03/2025:
# ESACCIAGBtileNames now accepts any type of sf object as input to pol

## Notes:


#' Generate tree cover tile names
#'
#' This function generates file names for tree cover tiles based on a given polygon and year.
#'
#' @param pol An sf or SpatVector object representing the polygon of interest.
#' @param year Numeric, the year for which to generate tile names (2010, 2015, or 2020).
#'
#' @return A character vector of unique file names for tree cover tiles.
#'
#' @importFrom sf st_bbox
#' @importFrom terra ext xmin xmax ymin ymax
#'
#' @export
TCtileNames <- function(pol, year = 2010, treeCoverDir = "data/treecover2010_v3_100m") {
  if (inherits(pol, "SpatVector")) {
    bb <- terra::ext(pol)
    bb_vec <- c(terra::xmin(bb), terra::ymin(bb), terra::xmax(bb), terra::ymax(bb))
  } else if (inherits(pol, "sfc_POLYGON")) {
    bb_vec <- sf::st_bbox(pol)
  } else {
    stop("The object representing the polygon of interest must be of class SpatVector or sfc_POLYGON from terra and sf packages respectivelly.")
  }

  crds <- expand.grid(x = c(bb_vec[1], bb_vec[3]), y = c(bb_vec[2], bb_vec[4]))
  fnms <- character(nrow(crds))

  for (i in 1:nrow(crds)) {
    lon <- 10 * (crds$x[i] %/% 10)
    lat <- 10 * (crds$y[i] %/% 10) + 10
    LtX <- ifelse(lon < 0, "W", "E")
    LtY <- ifelse(lat < 0, "S", "N")
    WE <- paste0(sprintf('%03d', abs(lon)), LtX)
    NS <- paste0(sprintf('%02d', abs(lat)), LtY)

    # Adjust filename based on the year
    if(year == 2015) {
      fnms[i] <- file.path(treeCoverDir, paste0("treecover2015_", NS, "_", WE, ".tif"))
    } else if(year == 2020) {
      fnms[i] <- file.path(treeCoverDir, paste0("treecover2020_", NS, "_", WE, ".tif"))
    } else {
      # Default to 2010 if another year is provided
      fnms[i] <- file.path(treeCoverDir, paste0("treecover2010_", NS, "_", WE, ".tif"))
    }

  }

  unique(fnms)
}

#' Generate AGB tile names
#'
#' This function generates file names for AGB tiles based on a given polygon.
#'
#' @param pol An sf or SpatVector object representing the polygon of interest.
#'
#' @return A character vector of unique file names for AGB tiles.
#'
#' @import stringr
#' @importFrom sf st_bbox
#' @importFrom terra ext xmin xmax ymin ymax
#'
#' @export
AGBtileNames <- function(pol, agbTilesDir="data/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020-fv5.0") {
  if (inherits(pol, "SpatVector")) {
    bb <- terra::ext(pol)
    bb_vec <- c(terra::xmin(bb), terra::ymin(bb), terra::xmax(bb), terra::ymax(bb))
  } else if (inherits(pol, "sfc_POLYGON")) {
    bb_vec <- sf::st_bbox(pol)
  } else {
    stop("The object representing the polygon of interest must be of class SpatVector or sfc_POLYGON from terra and sf packages respectivelly.")
  }

  crds <- expand.grid(x = c(bb_vec[1], bb_vec[3]), y = c(bb_vec[2], bb_vec[4]))
  fnms <- character(nrow(crds))

  for (i in 1:nrow(crds)) {
    lon <- 10 * (crds$x[i] %/% 10)
    lat <- 10 * (crds$y[i] %/% 10) + 10
    LtX <- ifelse(lon < 0, "W", "E")
    LtY <- ifelse(lat < 0, "S", "N")
    WE <- paste0(LtX, sprintf('%03d', abs(lon)))
    NS <- paste0(LtY, sprintf('%02d', abs(lat)))

    # year <- if (grepl('2017', agbTilesDir)) "2017" else if (grepl('2010', agbTilesDir)) "2010" else if (grepl('2018', agbTilesDir)) "2018" else "2020"
    # version <- if (year == "2020") "fv3.0" else "fv2.0"

    # Extract the fixed component of the filename
    agbfiles <- list.files(agbTilesDir)
    if (length(agbfiles) == 0) {
      stop("The agbTilesDir folder is empty or does not exist: ", agbTilesDir)
    }
    fixed_component <- str_extract(agbfiles[1], "_.*(?=$)")
    if (is.na(fixed_component)) {
      stop("Could not extract filename pattern from: ", agbfiles[1])
    }
    # Verify that this component is present in all files
    all_match <- all(str_detect(agbfiles, fixed_component))
    if (!all_match) {
      stop("The agbTilesDir folder contains data with several versions, levels and/or years. Keep each of those in separate folders and pass the required one to  agbTilesDir.")
    }

    #fnms[i] <- file.path(agbTilesDir, paste0(NS, WE, "_ESACCI-BIOMASS-L4-AGB-MERGED-100m-", year, "-", version, ".tif"))
    fnms[i] <- file.path(agbTilesDir, paste0(NS, WE, fixed_component))
  }

  unique(setdiff(fnms, grep("1000m|AGB_SD|aux", fnms, value = TRUE)))
}

#' Generate AGB SD tile names
#'
#' This function generates file names for AGB SD tiles based on a given polygon.
#'
#' @param pol An sf or SpatVector object representing the polygon of interest.
#'
#' @return A character vector of unique file names for AGB SD tiles.
#'
#' @importFrom sf st_bbox
#' @importFrom terra ext xmin xmax ymin ymax
#'
#' @export
SDtileNames <- function(pol, agbTilesDir="data/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020-fv5.0") {
  if (inherits(pol, "SpatVector")) {
    bb <- terra::ext(pol)
    bb_vec <- c(terra::xmin(bb), terra::ymin(bb), terra::xmax(bb), terra::ymax(bb))
  } else {
    bb_vec <- sf::st_bbox(pol)
  }

  crds <- expand.grid(x = c(bb_vec[1], bb_vec[3]), y = c(bb_vec[2], bb_vec[4]))
  fnms <- character(nrow(crds))

  for (i in 1:nrow(crds)) {
    lon <- 10 * (crds$x[i] %/% 10)
    lat <- 10 * (crds$y[i] %/% 10) + 10
    LtX <- ifelse(lon < 0, "W", "E")
    LtY <- ifelse(lat < 0, "S", "N")
    WE <- paste0(LtX, sprintf('%03d', abs(lon)))
    NS <- paste0(LtY, sprintf('%02d', abs(lat)))

    year <- if (grepl('2017', agbTilesDir)) "2017" else if (grepl('2010', agbTilesDir)) "2010" else if (grepl('2018', agbTilesDir)) "2018" else "2020"
    version <- if (year == "2020") "fv3.0" else "fv2.0"

    fnms[i] <- file.path(agbTilesDir, paste0(NS, WE, "_ESACCI-BIOMASS-L4-AGB_SD-MERGED-100m-", year, "-", version, ".tif"))
  }

  unique(fnms)
}





# Up to four treecover (TC) tile names covered by pol
#
# old_TCtileNames <- function(pol, year) {
#
#   treeCoverDir <- file.path("data", "treecover2010_v3_100m")
#
#   # Assuming 'bb' extraction has been corrected for 'terra' objects
#   bb <- ext(pol)  # Use 'terra::ext' for SpatVector objects
#   bb_vec <- c(xmin(bb), ymin(bb), xmax(bb), ymax(bb))
#
#   # Generate combinations of the bounding box corners
#   crds <- expand.grid(x = c(bb_vec[1], bb_vec[3]), y = c(bb_vec[2], bb_vec[4]))
#
#   fnms <- character(length(crds$x))  # Initialize with the correct length based on 'crds'
#   result
#   for(i in 1:nrow(crds)) {
#     lon <- 10 * (crds$x[i] %/% 10)
#     lat <- 10 * (crds$y[i] %/% 10) + 10
#     LtX <- ifelse(lon < 0, "W", "E")
#     LtY <- ifelse(lat < 0, "S", "N")
#     WE <- paste0(sprintf('%03d', abs(lon)), LtX)
#     NS <- paste0(sprintf('%02d', abs(lat)), LtY)
#
#     # Adjust filename based on the year
#     if(year == 2015) {
#       fnms[i] <- file.path(treeCoverDir, paste0("treecover2015_", NS, "_", WE, ".tif"))
#     } else if(year == 2020) {
#       fnms[i] <- file.path(treeCoverDir, paste0("treecover2020_", NS, "_", WE, ".tif"))
#     } else {
#       # Default to 2010 if another year is provided
#       fnms[i] <- file.path(treeCoverDir, paste0("treecover2010_", NS, "_", WE, ".tif"))
#     }
#   }
#
#   return(unique(fnms))
# }
#
# old_AGBtileNames <- function(pol){
#
#   agbTilesDir <- file.path("data", "ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020-fv5.0")
#
#   bb <- unname(bbox(pol))
#   crds <- expand.grid(x=bb[1,],y=bb[2,])
#   fnms <- character(6)
#   match <- c('1000m', 'AGB_SD', 'aux')
#
#   for(i in 1:nrow(crds)){
#
#     if (grepl('2017', agbTilesDir) == TRUE){
#       lon <- 10*(crds[i,1]%/%10)
#       lat <- 10*(crds[i,2]%/%10) + 10
#       LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(LtX, sprintf('%03d',abs(lon)))
#       NS <- paste0(LtY,sprintf('%02d',abs(lat)))
#       fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2017-fv2.0.tif"))
#
#     }else if (grepl('2010', agbTilesDir) == TRUE){
#       lon <- 10*(crds[i,1]%/%10)
#       lat <- 10*(crds[i,2]%/%10) + 10
#       LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(LtX, sprintf('%03d',abs(lon)))
#       NS <- paste0(LtY,sprintf('%02d',abs(lat)))
#       fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2010-fv2.0.tif"))
#     }else if (grepl('2018', agbTilesDir) == TRUE){
#       lon <- 10*(crds[i,1]%/%10)
#       lat <- 10*(crds[i,2]%/%10) + 10
#       LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(LtX, sprintf('%03d',abs(lon)))
#       NS <- paste0(LtY,sprintf('%02d',abs(lat)))
#       fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2018-fv2.0.tif"))
#     }else{
#       lon <- 10*(crds[i,1]%/%10)
#       lat <- 10*(crds[i,2]%/%10) + 10
#       LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(LtX, sprintf('%03d',abs(lon)))
#       NS <- paste0(LtY,sprintf('%02d',abs(lat)))
#       fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020-fv3.0.tif"))
#     }
#   }
#   fnms0 <- unique(grep(paste(match,collapse="|"), fnms, value=T))
#   fnms <- setdiff(fnms,fnms0)
#   unique(fnms)
# }
#
# old_SDtileNames <- function(pol){
#
#   agbTilesDir <- file.path("data", "ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020-fv5.0")
#
#   bb <- unname(bbox(pol))
#   crds <- expand.grid(x=bb[1,],y=bb[2,])
#   fnms <- character(6)
#   match <- '_SD'
#
#   for(i in 1:nrow(crds)){
#
#     if (grepl('2017', agbTilesDir) == TRUE){
#       lon <- 10*(crds[i,1]%/%10)
#       lat <- 10*(crds[i,2]%/%10) + 10
#       LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(LtX, sprintf('%03d',abs(lon)))
#       NS <- paste0(LtY,sprintf('%02d',abs(lat)))
#       fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB_SD-MERGED-100m-2017-fv2.0.tif"))
#
#     }else if (grepl('2010', agbTilesDir) == TRUE){
#       lon <- 10*(crds[i,1]%/%10)
#       lat <- 10*(crds[i,2]%/%10) + 10
#       LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(LtX, sprintf('%03d',abs(lon)))
#       NS <- paste0(LtY,sprintf('%02d',abs(lat)))
#       fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB_SD-MERGED-100m-2010-fv2.0.tif"))
#     }else if (grepl('2018', agbTilesDir) == TRUE){
#       lon <- 10*(crds[i,1]%/%10)
#       lat <- 10*(crds[i,2]%/%10) + 10
#       LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(LtX, sprintf('%03d',abs(lon)))
#       NS <- paste0(LtY,sprintf('%02d',abs(lat)))
#       fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB_SD-MERGED-100m-2018-fv2.0.tif"))
#     }else{
#       lon <- 10*(crds[i,1]%/%10)
#       lat <- 10*(crds[i,2]%/%10) + 10
#       LtX <- ifelse(lon < 0, "W", "E")
#       LtY <- ifelse(lat < 0, "S", "N")
#       WE <- paste0(LtX, sprintf('%03d',abs(lon)))
#       NS <- paste0(LtY,sprintf('%02d',abs(lat)))
#       fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB_SD-MERGED-100m-2020-fv3.0.tif"))
#     }
#   }
#   fnms0 <- unique(grep(paste(match,collapse="|"), fnms, value=T))
#   unique(fnms)
# }

