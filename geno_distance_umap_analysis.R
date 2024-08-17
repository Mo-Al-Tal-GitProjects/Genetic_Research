# File: geno_distance_umap_analysis.R

# Source the necessary utility functions and data loading files
source("/Users/mohammedkhodorfirasal-tal/Documents/Professional/Work/Fellowship - Harvard/Research/Project Code/geno_analysis_utilities.R")
source("/Users/mohammedkhodorfirasal-tal/Documents/Professional/Work/Fellowship - Harvard/Research/Project Code/geno_data_loading.R")

# Load the data manually
load("/Users/mohammedkhodorfirasal-tal/Documents/Professional/Work/Fellowship - Harvard/Research/Data/pop_gen_sample.RData")

# Access the relevant data
bed_mat <- working_data$bed_mat
pop_sample <- working_data$pop_sample

# Calculate the distance matrix using Euclidean distance
common_dense_bed_mat <- as.matrix(common_bed_mat)
common_distance_matrix <- as.matrix(dist(common_dense_bed_mat, method = "euclidean"))

# UMAP Analysis
# Set the seed for reproducibility
set.seed(2024)

# Function to generate UMAP plot for a subset of populations and identify outliers
generate_umap_plot <- function(populations, common_bed_mat, common_distance_matrix, pop_sample, population_colors, title) {
  # Subset the populations of interest
  subset_indices <- which(pop_sample %in% populations)
  common_geno <- common_bed_mat[subset_indices, ]
  
  # Subset the distance matrix for the specified populations
  subset_distance_matrix_common <- common_distance_matrix[subset_indices, subset_indices]
  
  # Compute UMAP using the precomputed distance matrix
  umap_result_common <- umap(subset_distance_matrix_common, input = "dist")
  
  # Create a data frame for plotting
  umap_df_common <- data.frame(
    x = umap_result_common$layout[, 1],
    y = umap_result_common$layout[, 2],
    Population = pop_sample[subset_indices]
  )
  
  # Calculate centroids for each population
  centroids <- umap_df_common %>%
    group_by(Population) %>%
    summarize(
      centroid_x = mean(x),
      centroid_y = mean(y)
    )
  
  # Calculate the distance of each point to its population's centroid
  umap_df_common <- umap_df_common %>%
    left_join(centroids, by = "Population") %>%
    mutate(
      dist_to_centroid = sqrt((x - centroid_x)^2 + (y - centroid_y)^2)
    )
  
  # Define a threshold for outliers: points that are farthest from their centroid
  outlier_threshold <- quantile(umap_df_common$dist_to_centroid, 0.98)  # Top 1% distance
  outliers_common <- umap_df_common %>%
    filter(dist_to_centroid > outlier_threshold)
  
  # Plot the UMAP results for the common distance matrix
  umap_plot_common <- ggplot(umap_df_common, aes(x = x, y = y, color = Population)) +
    geom_point(size = 2) +
    labs(
      title = title,
      x = "UMAP1",
      y = "UMAP2"
    ) +
    scale_color_manual(values = population_colors) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Plot the UMAP results for the common distance matrix with highlighted outliers
  umap_plot_common_outliers <- ggplot(umap_df_common, aes(x = x, y = y, color = Population)) +
    geom_point(size = 2) +
    geom_point(data = outliers_common, aes(x = x, y = y), color = "black", shape = 1, size = 2, stroke = 1.5) +
    geom_point(data = outliers_common, aes(x = x, y = y), size = 2) +
    labs(
      title = paste(title, "with Outliers Highlighted"),
      x = "UMAP1",
      y = "UMAP2"
    ) +
    scale_color_manual(values = population_colors) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Display the plots
  print(umap_plot_common)
  print(umap_plot_common_outliers)
}

# Define a color palette and generate the UMAP plot for the selected populations
color_palette <- c(brewer.pal(n = 12, name = "Paired"), brewer.pal(n = 8, name = "Set3"), brewer.pal(n = 6, name = "Dark2"))
population_colors <- setNames(color_palette[1:length(unique(pop_sample))], unique(pop_sample))

# Prompt the user to enter the populations they want to display in UMAP
pop_input <- readline(prompt = "Enter the populations you want to display in UMAP (comma-separated): ")

# Remove any leading or trailing whitespace
pop_input <- trimws(pop_input)

# Split the input string into individual population codes, and strip any quotes
populations_to_display <- unlist(strsplit(pop_input, ",\\s*"))
populations_to_display <- gsub('"', '', populations_to_display)  # Remove any extra quotes

# Generate UMAP plots with and without outliers highlighted
generate_umap_plot(populations_to_display, common_bed_mat, common_distance_matrix, pop_sample, population_colors, "Original UMAP of Selected Genotypes")

