# File: adjustable_umap_analysis.R

# Load necessary libraries
library(umap)
library(ggplot2)
library(RColorBrewer)

# Source the necessary utility functions and data loading files
source("/Users/mohammedkhodorfirasal-tal/Documents/Professional/Work/Fellowship - Harvard/Research/Project Code/geno_analysis_utilities.R")
source("/Users/mohammedkhodorfirasal-tal/Documents/Professional/Work/Fellowship - Harvard/Research/Project Code/geno_data_loading.R")

# Load the data manually
load("/Users/mohammedkhodorfirasal-tal/Documents/Professional/Work/Fellowship - Harvard/Research/Data/pop_gen_sample.RData")

# Access the relevant data
bed_mat <- working_data$bed_mat
pop_sample <- working_data$pop_sample

# Convert the sparse matrix to a dense matrix
dense_common_bed_mat <- as.matrix(common_bed_mat)

# Calculate a distance matrix (Euclidean in this example)
euclidean_distance_matrix <- dist(dense_common_bed_mat)
euclidean_distance_matrix <- as.matrix(euclidean_distance_matrix)

# Verify the matrix format
if (!is.matrix(euclidean_distance_matrix)) {
  stop("The distance matrix is not in the correct matrix format.")
}

# Define all populations
all_populations <- unique(pop_sample)

# Create a consistent color mapping for all populations
color_palette <- c(brewer.pal(n = 12, name = "Paired"), brewer.pal(n = 8, name = "Set3"), brewer.pal(n = 6, name = "Dark2"))
population_colors <- setNames(color_palette[1:length(all_populations)], all_populations)

# Function to generate UMAP plot with adjustable parameters and sample selection
generate_adjustable_umap_plot <- function(distance_matrix, pop_sample, super_pop_sample, title) {
  # Prompt the user to choose between pop_sample and super_pop_sample
  sample_choice <- readline(prompt = "Select sample type ('pop_sample' or 'super_pop_sample'): ")
  
  if (sample_choice == "pop_sample") {
    selected_sample <- pop_sample
  } else if (sample_choice == "super_pop_sample") {
    selected_sample <- super_pop_sample
  } else {
    stop("Invalid sample type selected.")
  }
  
  # Display available populations and prompt the user to select populations
  available_populations <- unique(selected_sample)
  cat("Available populations:\n", paste(available_populations, collapse = ", "), "\n")
  selected_populations <- readline(prompt = "Enter populations to include (comma-separated): ")
  selected_populations <- unlist(strsplit(trimws(selected_populations), ",\\s*"))
  
  # Filter the selected populations
  selected_indices <- which(selected_sample %in% selected_populations)
  filtered_sample <- selected_sample[selected_indices]
  filtered_distance_matrix <- distance_matrix[selected_indices, selected_indices]
  
  # Define color palette based on selected populations
  color_palette <- c(brewer.pal(n = 12, name = "Paired"), brewer.pal(n = 8, name = "Set3"), brewer.pal(n = 6, name = "Dark2"))
  population_colors <- setNames(color_palette[1:length(unique(filtered_sample))], unique(filtered_sample))
  
  # Prompt the user for UMAP settings
  n_neighbors <- as.integer(readline(prompt = "Enter n_neighbors (e.g., 15, 30): "))
  min_dist <- as.numeric(readline(prompt = "Enter min_dist (e.g., 0.1, 0.5): "))
  n_components <- as.integer(readline(prompt = "Enter n_components (e.g., 2, 10): "))
  
  # Set UMAP configuration
  umap_config <- umap.defaults
  umap_config$n_neighbors <- n_neighbors
  umap_config$min_dist <- min_dist
  umap_config$n_components <- n_components
  
  # Run UMAP
  umap_result <- umap(filtered_distance_matrix, config = umap_config)
  
  # Create a data frame for plotting
  umap_df <- data.frame(
    x = umap_result$layout[, 1],
    y = umap_result$layout[, 2],
    Population = filtered_sample
  )
  
  # Plot the UMAP results
  umap_plot <- ggplot(umap_df, aes(x = x, y = y, color = Population)) +
    geom_point(size = 2) +
    labs(
      title = title,
      x = "UMAP1",
      y = "UMAP2"
    ) +
    scale_color_manual(values = population_colors) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(umap_plot)
}

# Example usage with adjustable settings and population selection
generate_adjustable_umap_plot(euclidean_distance_matrix, pop_sample, super_pop_sample, "Customizable UMAP of Genotypes")