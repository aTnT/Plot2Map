# Sample plot data: plot data is lat, lon point data and formatted into a dataframe

plots <- utils::read.csv("data-raw/SamplePlots.csv")

usethis::use_data(plots, overwrite = TRUE)
