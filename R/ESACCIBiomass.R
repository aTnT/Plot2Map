
#' Download ESA CCI Biomass GeoTIFF Data
#'
#' This function downloads ESA CCI Biomass GeoTIFF data from the CEDA Archive.
#'
#' @param esacci_biomass_year The ESACCI BIOMASS AGB tiles year to use. Use either 2010, 2015, 2016, 2017, 2018, 2019,
#' 2020, 2021 or "latest" (default).
#' @param esacci_biomass_version The ESACCI BIOMASS AGB tiles version to use. Use either "v2.0", "v3.0", "v4.0",
#' "v5.0", "v5.01" or "latest" (default).
#' @param output_dir Directory to save downloaded files. Default is current working directory.
#' @param n_cores Number of cores to use for parallel download. Default is all cores except one.
#'
#' @return A character vector of downloaded file paths.
#'
#' @import parallel
#' @import rvest
#' @import httr
#' @import pbapply
#'
#' @export
download_esacci_biomass <- function(esacci_biomass_year = "latest",
                                    esacci_biomass_version = "latest",
                                    output_dir = "data/ESACCI-BIOMASS",
                                    n_cores = parallel::detectCores() - 1) {

  base_url <- "https://data.ceda.ac.uk/neodc/esacci/biomass/data/agb/maps"

  # Validate and process input parameters
  valid_years <- c(2010, 2015, 2016, 2017, 2018, 2019, 2020, 2021)
  valid_versions <- c("v2.0", "v3.0", "v4.0", "v5.0", "v5.01")

  if (esacci_biomass_year == "latest") esacci_biomass_year <- max(valid_years)
  if (esacci_biomass_version == "latest") esacci_biomass_version <- valid_versions[length(valid_versions)]

  if (!(esacci_biomass_year %in% valid_years)) stop("Invalid year specified")
  if (!(esacci_biomass_version %in% valid_versions)) stop("Invalid version specified")

  # Check if output directory exists, if not create it
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message(paste("Created output directory:", output_dir))
  }

  # Construct URL
  url <- file.path(base_url, esacci_biomass_version, "geotiff", as.character(esacci_biomass_year))

  # Fetch file list
  page <- read_html(url)
  file_table <- html_table(page, fill = TRUE)[[1]]
  file_names <- file_table$X1

  # Download function
  download_file <- function(file_name) {
    file_url <- file.path(url, file_name)
    output_file <- file.path(output_dir, file_name)
    tryCatch({
      download.file(file_url, output_file, mode = "wb", quiet = TRUE)
      return(output_file)
    }, error = function(e) {
      warning(paste("Failed to download:", file_name, "-", e$message))
      return(NULL)
    })
  }

  message(paste0("Downloading ESA CCI Biomass ", esacci_biomass_version, " data for year ", esacci_biomass_year, "..."))

  # Setup parallel processing with progress bar
  cl <- makeCluster(n_cores)

  # Parallel download with progress
  downloaded_files <- pblapply(file_names, download_file, cl = cl)

  stopCluster(cl)

  # Remove NULL entries (failed downloads)
  downloaded_files <- unlist(downloaded_files[!sapply(downloaded_files, is.null)])

  return(downloaded_files)
}

