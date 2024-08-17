# File: geno_analysis_utilities.R

# Utility function to install and load packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
}

# List of required packages
required_packages <- c("Matrix", "tidyr", "dplyr", "ggplot2", "tibble", 
                       "RColorBrewer", "dendextend", "umap", "maps", 
                       "mapdata", "ggmap", "proxy")

# Install and load the required packages
install_and_load(required_packages)

# Function to allow users to install and load additional packages
load_additional_packages <- function(extra_packages) {
  install_and_load(extra_packages)
}

# Example usage: loading additional packages (if needed)
# extra_packages <- c("someExtraPackage")
# load_additional_packages(extra_packages)