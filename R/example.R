## Updates made to the new framework:
# 07/07/2025:
# - Fixed sample_lidar_folder to correctly return absolute path using system.file()
# - Added validation to check if files exist and warn if folder is empty


#' Get path to example sample file
#'
#' This package comes bundled with a number of sample files in its `inst/extdata`
#' directory. This function make them easy to access
#'
#' @param file Name of file. If `NULL`, all sample files will be listed.
#' @export
#' @examples
#' \dontrun{
#' sample_file()
#' sample_file("SampleUnformattedPlots.csv")
#' }
sample_file <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "Plot2Map"))
  } else {
    system.file("extdata", file, package = "Plot2Map", mustWork = TRUE)
  }
}


#' Get path to example sample lidar folder
#'
#' This package comes bundled with a number of sample files in its `inst/extdata`
#' directory. This function make them easy to access
#'
#' @param folder Name of folder within the extdata directory.
#' @export
#' @examples
#' \dontrun{
#' sample_lidar_folder("SustainableLandscapeBrazil_v04/SLB_AGBmaps")
#' }
sample_lidar_folder <- function(folder) {
  # Get the full system path to the folder
  path <- system.file("extdata", folder, package = "Plot2Map", mustWork = TRUE)

  # List directory contents to confirm files are present (for debugging)
  files <- dir(path)
  if (length(files) == 0) {
    warning("No files found in folder: ", folder)
  }

  return(path)
}
