# File: geno_kmeans_clustering_analysis.R

# Load necessary libraries
library(ggplot2)
library(umap)
library(RColorBrewer)
library(dplyr)

# Source the necessary utility functions and data loading files
source("/Users/mohammedkhodorfirasal-tal/Documents/Professional/Work/Fellowship - Harvard/Research/Project Code/geno_analysis_utilities.R")
source("/Users/mohammedkhodorfirasal-tal/Documents/Professional/Work/Fellowship - Harvard/Research/Project Code/geno_data_loading.R")

# Load the data manually
load("/Users/mohammedkhodorfirasal-tal/Documents/Professional/Work/Fellowship - Harvard/Research/Data/pop_gen_sample.RData")

# Access the relevant data
bed_mat <- working_data$bed_mat
pop_sample <- working_data$pop_sample
super_pop_sample <- working_data$super_pop_sample

# Function to subset the distance matrix and population sample for the selected populations or super populations
subset_data <- function(pops, distance_matrix, pop_sample) {
  subset_indices <- which(pop_sample %in% pops)
  subset_distance_matrix <- distance_matrix[subset_indices, subset_indices]
  subset_pop_sample <- pop_sample[subset_indices]
  list(distance_matrix = subset_distance_matrix, pop_sample = subset_pop_sample)
}

# Updated function for K-means clustering with adjustable UMAP settings
perform_kmeans_clustering <- function(common_distance_matrix, pop_sample, super_pop_sample) {
  # Prompt the user to select the sample type
  sample_type <- readline(prompt = "Select sample type ('pop_sample' or 'super_pop_sample'): ")
  
  if (sample_type == "pop_sample") {
    available_populations <- unique(pop_sample)
  } else if (sample_type == "super_pop_sample") {
    available_populations <- unique(super_pop_sample)
  } else {
    stop("Invalid sample type.")
  }
  
  cat("Available populations:\n", paste(available_populations, collapse = ", "), "\n")
  selected_populations <- readline(prompt = "Enter populations to include (comma-separated): ")
  selected_populations <- unlist(strsplit(selected_populations, ",\\s*"))
  
  # Subset the distance matrix and population sample based on user selection
  subset_indices <- which(get(sample_type) %in% selected_populations)
  subset_distance_matrix <- common_distance_matrix[subset_indices, subset_indices]
  subset_pop_sample <- get(sample_type)[subset_indices]
  
  # Prompt for UMAP settings
  n_neighbors <- as.integer(readline(prompt = "Enter the number of neighbors (n_neighbors): "))
  min_dist <- as.numeric(readline(prompt = "Enter the minimum distance (min_dist): "))
  n_components <- as.integer(readline(prompt = "Enter the number of components (n_components): "))
  
  # Perform UMAP
  umap_config <- umap.defaults
  umap_config$n_neighbors <- n_neighbors
  umap_config$min_dist <- min_dist
  umap_config$n_components <- n_components
  
  umap_result <- umap(subset_distance_matrix, config = umap_config, input = "dist")
  
  # Prompt for the number of clusters
  n_clusters <- as.integer(readline(prompt = "Enter the number of clusters for K-means: "))
  
  # Perform K-means clustering on the UMAP embedding
  kmeans_result <- kmeans(umap_result$layout, centers = n_clusters)
  
  # Create a data frame for plotting
  kmeans_df <- data.frame(
    x = umap_result$layout[, 1],
    y = umap_result$layout[, 2],
    Population = subset_pop_sample,
    Cluster = factor(kmeans_result$cluster)
  )
  
  # Define color palette
  all_populations <- unique(subset_pop_sample)
  color_palette <- c(brewer.pal(n = 12, name = "Paired"), brewer.pal(n = 8, name = "Set3"), brewer.pal(n = 6, name = "Dark2"))
  population_colors <- setNames(color_palette[1:length(all_populations)], all_populations)
  
  # Plot the K-means clustering results
  ggplot(kmeans_df, aes(x = x, y = y, color = Population)) +
    geom_point(size = 2) +
    labs(
      title = paste("K-means Clustering with", n_clusters, "Clusters"),
      x = "UMAP1",
      y = "UMAP2"
    ) +
    scale_color_manual(values = population_colors) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Example usage
perform_kmeans_clustering(common_distance_matrix, pop_sample, super_pop_sample)

