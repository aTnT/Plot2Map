# Global Reference Dataset for Above-Ground Biomass (AGBref)
# DOI: https://doi.org/10.5281/zenodo.15495069

# Load the dataset from the original Rdata file
source_file <- file.path("inst", "extdata", "AGBref_fin_10km_2025-04-01.Rdata")
load(source_file)

# The dataset is loaded as 'mpAGB', rename it to 'AGBref' for consistency
AGBref <- mpAGB

# Convert categorical variables to factors for better handling
AGBref$BIO <- as.factor(AGBref$BIO)
AGBref$CODE <- as.factor(AGBref$CODE)
AGBref$INVENTORY <- as.factor(AGBref$INVENTORY)
AGBref$TIER <- as.factor(AGBref$TIER)

# Save the processed dataset
usethis::use_data(AGBref, overwrite = TRUE)
