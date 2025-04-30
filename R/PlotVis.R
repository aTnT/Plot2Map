## Updates made to the new framework:
# 30/04/2025:
# - Migrated visualization functions from scripts/Plots.R
# - Added proper roxygen documentation
# - Improved file path handling to avoid working directory manipulation
# - Added consistent error handling
# - Made plotting parameters more configurable


#' Create a binned plot of AGB data
#'
#' This function creates a binned plot of Above Ground Biomass (AGB) data, with points sized
#' according to the number of observations and error bars showing the interquartile range.
#'
#' @param x Numeric vector of reference AGB values.
#' @param y Numeric vector of mapped AGB values (same length as x).
#' @param caption Character string for the plot title/caption.
#' @param fname Character string specifying the output file name for saving the plot. 
#'        If empty, the plot will be displayed but not saved.
#' @param outDir Character string specifying the output directory for saving the plot (default: "results").
#'
#' @return Invisibly returns NULL. The function creates a plot as a side effect.
#'
#' @importFrom graphics plot arrows abline par legend axis
#' @importFrom grDevices png dev.off rgb
#' @importFrom plotrix axis.break
#' @importFrom stats aggregate quantile na.omit
#'
#' @export
#' @examples
#' \dontrun{
#' data("plots")
#' set.seed(42)
#' ref_data <- plots$AGB_T_HA[1:100]
#' map_data <- ref_data + rnorm(100, 0, 20)
#' Binned(ref_data, map_data, "Example Plot", "binned_plot.png")
#' }
Binned <- function(x, y, caption = "", fname = "", outDir = "results") {
  # Input validation
  if (length(x) != length(y)) {
    stop("Input vectors x and y must have the same length")
  }
  if (length(x) == 0) {
    stop("Input vectors cannot be empty")
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(outDir) && fname != "") {
    dir.create(outDir, recursive = TRUE)
  }
  
  # Define intervals for binning
  intervals <- c(0:12 * 25, 7:8 * 50, Inf)
  
  # Bin the data
  ct <- findInterval(x, intervals, left.open = TRUE)
  r <- c(0, 425)
  
  # Calculate statistics for each bin
  ux <- aggregate(x, list(ct), FUN = mean, na.rm = TRUE)[, 2]
  uy <- aggregate(y, list(ct), FUN = mean, na.rm = TRUE)[, 2]
  nu <- aggregate(y, list(ct), FUN = function(x) length(na.omit(x)))[, 2]
  q1 <- aggregate(y, list(ct), FUN = quantile, probs = 0.25, na.rm = TRUE)[, 2]
  q3 <- aggregate(y, list(ct), FUN = quantile, probs = 0.75, na.rm = TRUE)[, 2]
  
  # Set point sizes based on number of samples in each bin
  cx <- (2:7 * 0.25)[findInterval(nu, c(0, 10, 20, 50, 100, 200))]
  
  # Adjust plot range for extremely high values
  if (length(ux) > 0 && ux[length(ux)] > 400) {
    ux[length(ux)] <- 500
    r <- c(0, 525)
  }
  
  # Create the plot
  create_binned_plot <- function() {
    plot(ux, uy, las = 1, main = caption, pch = 16, cex = cx,
         xlab = "Mean reference AGB (Mg/ha)", xlim = r,
         ylab = "Mapped AGB (Mg/ha)", ylim = r, xaxt = "n")
    
    # Add error bars
    arrows(ux, q1, ux, q3, length = 0)
    
    # Add 1:1 line
    abline(0, 1, lty = 2, lwd = 1)
    
    # Add legend
    t <- "#/bin"
    legend("topleft", bty = "n", pch = 16, ncol = 2, pt.cex = 2:7 * 0.25,
           cex = 1, c("< 10", "10-20", "20-50", "50-100", "100-200", "> 200"),
           title = t)
    
    # Custom x-axis
    if (length(ux) > 0 && ux[length(ux)] == 500) {
      axis(1, at = 0:5 * 100, labels = c(0:4 * 100, ">400"))
      plotrix::axis.break(breakpos = 450)
    } else {
      axis(1, at = 0:4 * 100, labels = c(0:4 * 100))
    }
  }
  
  # Display the plot
  create_binned_plot()
  
  # Save the plot if filename is provided
  if (fname != "") {
    file_path <- file.path(outDir, fname)
    png(file_path, width = 1000, height = 1000, res = 150)
    create_binned_plot()
    dev.off()
  }
  
  invisible(NULL)
}


#' Create a scatter plot of AGB data
#'
#' This function creates a simple scatter plot of Above Ground Biomass (AGB) data with a 1:1 line.
#'
#' @param x Numeric vector of reference AGB values.
#' @param y Numeric vector of mapped AGB values (same length as x).
#' @param caption Character string for the plot title/caption.
#' @param fname Character string specifying the output file name for saving the plot.
#'        If empty, the plot will be displayed but not saved.
#' @param outDir Character string specifying the output directory for saving the plot (default: "results").
#'
#' @return Invisibly returns NULL. The function creates a plot as a side effect.
#'
#' @importFrom graphics plot abline
#' @importFrom grDevices png dev.off
#' @importFrom stats na.omit
#'
#' @export
#' @examples
#' \dontrun{
#' data("plots")
#' set.seed(42)
#' ref_data <- plots$AGB_T_HA[1:100]
#' map_data <- ref_data + rnorm(100, 0, 20)
#' Scatter(ref_data, map_data, "Example Scatter Plot", "scatter_plot.png")
#' }
Scatter <- function(x, y, caption = "", fname = "", outDir = "results") {
  # Input validation
  if (length(x) != length(y)) {
    stop("Input vectors x and y must have the same length")
  }
  if (length(x) == 0) {
    stop("Input vectors cannot be empty")
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(outDir) && fname != "") {
    dir.create(outDir, recursive = TRUE)
  }
  
  # Set plot range
  r <- c(0, 1050)
  
  # Adjust range for extremely high values
  chk <- sort(x)
  if (length(chk) > 0 && chk[length(chk)] > 1000) {
    r <- c(0, max(x, na.rm = TRUE))
  }
  
  # Create the plot
  create_scatter_plot <- function() {
    plot(x, y, las = 1, main = caption, pch = 16, cex = 0.5,
         xlab = "Mean reference AGB (Mg/ha)", xlim = r,
         ylab = "Mapped AGB (Mg/ha)", ylim = r)
    abline(0, 1, lty = 2, lwd = 1)
  }
  
  # Display the plot
  create_scatter_plot()
  
  # Save the plot if filename is provided
  if (fname != "") {
    file_path <- file.path(outDir, fname)
    png(file_path, width = 1000, height = 1000, res = 150)
    create_scatter_plot()
    dev.off()
  }
  
  invisible(NULL)
}


#' Create a comparison plot of two AGB datasets
#'
#' This function creates a plot comparing two sets of AGB measurements, useful for comparing
#' different processing methods or datasets. Both sets are shown as binned data with
#' error bars and different colors.
#'
#' @param x Numeric vector of first reference AGB values.
#' @param y Numeric vector of first mapped AGB values (same length as x).
#' @param x1 Numeric vector of second reference AGB values.
#' @param y1 Numeric vector of second mapped AGB values (same length as x1).
#' @param caption Character string for the plot title/caption.
#' @param fname Character string specifying the output file name for saving the plot.
#'        If empty, the plot will be displayed but not saved.
#' @param title Character string specifying the comparison type: 'harmo' for harmonized data
#'        comparison, or any other value for weighted comparison (default: "").
#' @param outDir Character string specifying the output directory for saving the plot (default: "results").
#'
#' @return Invisibly returns NULL. The function creates a plot as a side effect.
#'
#' @importFrom graphics plot arrows abline par legend axis
#' @importFrom grDevices png dev.off rgb
#' @importFrom plotrix axis.break
#' @importFrom stats aggregate quantile na.omit
#'
#' @export
#' @examples
#' \dontrun{
#' data("plots")
#' set.seed(42)
#' ref_data <- plots$AGB_T_HA[1:100]
#' map_data1 <- ref_data + rnorm(100, 0, 20)
#' map_data2 <- ref_data + rnorm(100, 5, 15)
#' TwoPlots(ref_data, map_data1, ref_data, map_data2, 
#'          "Comparison of Methods", "comparison_plot.png", "harmo")
#' }
TwoPlots <- function(x, y, x1, y1, caption = "", fname = "", title = "", outDir = "results") {
  # Input validation
  if (length(x) != length(y)) {
    stop("Input vectors x and y must have the same length")
  }
  if (length(x1) != length(y1)) {
    stop("Input vectors x1 and y1 must have the same length")
  }
  if (length(x) == 0 || length(x1) == 0) {
    stop("Input vectors cannot be empty")
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(outDir) && fname != "") {
    dir.create(outDir, recursive = TRUE)
  }
  
  # Define intervals for binning
  intervals <- c(0:12 * 25, 7:12 * 50, Inf)
  
  # Cap very high values
  x <- ifelse(x > 350, 400, x)
  
  # Set colors for the two datasets
  col1 <- rgb(red = 1, green = 0, blue = 0, alpha = 0.5)
  col2 <- rgb(red = 0, green = 0, blue = 1, alpha = 0.5)
  
  # Process first dataset
  ct <- findInterval(x, intervals, left.open = TRUE)
  r <- c(0, 1.1 * max(x, na.rm = TRUE))
  ux <- aggregate(x, list(ct), FUN = mean, na.rm = TRUE)[, 2]
  uy <- aggregate(y, list(ct), FUN = mean, na.rm = TRUE)[, 2]
  nu <- aggregate(y, list(ct), FUN = function(x) length(na.omit(x)))[, 2]
  q1 <- aggregate(y, list(ct), FUN = quantile, probs = 0.25, na.rm = TRUE)[, 2]
  q3 <- aggregate(y, list(ct), FUN = quantile, probs = 0.75, na.rm = TRUE)[, 2]
  cx <- (2:7 * 0.25)[findInterval(nu, c(0, 10, 20, 50, 100, 200))]
  
  # Adjust plot range
  if (length(ux) > 0 && ux[length(ux)] == 400) {
    r <- c(0, 425)
  } else {
    r <- c(0, 225)
  }
  
  # Process second dataset
  x1 <- ifelse(x1 > 350, 400, x1)
  ct1 <- findInterval(x1, intervals, left.open = TRUE)
  ux1 <- aggregate(x1, list(ct1), FUN = mean, na.rm = TRUE)[, 2]
  uy1 <- aggregate(y1, list(ct1), FUN = mean, na.rm = TRUE)[, 2]
  nu1 <- aggregate(y1, list(ct1), FUN = function(x1) length(na.omit(x1)))[, 2]
  q11 <- aggregate(y1, list(ct1), FUN = quantile, probs = 0.25, na.rm = TRUE)[, 2]
  q33 <- aggregate(y1, list(ct1), FUN = quantile, probs = 0.75, na.rm = TRUE)[, 2]
  cx1 <- (2:7 * 0.25)[findInterval(nu1, c(0, 10, 20, 50, 100, 200))]
  
  # Create the plot
  create_two_plots <- function() {
    # First dataset
    plot(ux, uy, las = 1, main = caption, pch = 16, col = col1, cex = cx,
         xlab = "Mean reference AGB (Mg/ha)", xlim = r,
         ylab = "Mapped AGB (Mg/ha)", ylim = r, xaxt = "n")
    arrows(ux, q1, ux, q3, length = 0, col = col1)
    
    # Second dataset
    par(new = TRUE)
    plot(ux1, uy1, las = 1, main = "", pch = 16, col = col2, cex = cx1,
         xlab = "", xlim = r, ylab = "", ylim = r, xaxt = "n", yaxt = "n")
    arrows(ux1, q11, ux1, q33, length = 0, col = col2)
    
    # Add 1:1 line
    abline(0, 1, lty = 2, lwd = 1)
    
    # Add legends
    if (title == 'harmo') {
      t1 <- "#/bin, original"
      t2 <- "#/bin, harmonized"
    } else {
      t1 <- "#/bin, unweighted"
      t2 <- "#/bin, weighted"
    }
    
    legend("topleft", bty = "n", pch = 16, ncol = 2, pt.cex = 2:7 * 0.25, col = col1,
           cex = 1, c("< 10", "10-20", "20-50", "50-100", "100-200", "> 200"),
           title = t1)
    
    legend("top", bty = "n", pch = 16, ncol = 2, pt.cex = 2:7 * 0.25, col = col2,
           cex = 1, c("< 10", "10-20", "20-50", "50-100", "100-200", "> 200"),
           title = t2)
    
    # Custom x-axis
    if (length(ux) > 0 && ux[length(ux)] == 400) {
      axis(1, at = 0:4 * 100, labels = c(0:3 * 100, ">300"))
      plotrix::axis.break(breakpos = 350)
    } else {
      axis(1, at = 0:3 * 100, labels = c(0:3 * 100))
    }
  }
  
  # Display the plot
  create_two_plots()
  
  # Save the plot if filename is provided
  if (fname != "") {
    file_path <- file.path(outDir, fname)
    png(file_path, width = 1000, height = 1000, res = 150)
    create_two_plots()
    dev.off()
  }
  
  invisible(NULL)
}