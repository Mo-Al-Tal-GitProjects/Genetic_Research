# File: geno_pca_analysis.R

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

# Calculate the minor allele frequency (MAF)
maf_common <- colMeans(dense_common_bed_mat) / 2

# Compute Pearson residuals using the correct logic
residuals_common_mat <- sweep(dense_common_bed_mat, MARGIN = 2, STATS = 2 * maf_common, FUN = "-")
residuals_common_mat <- sweep(residuals_common_mat, MARGIN = 2, STATS = sqrt(2 * maf_common * (1 - maf_common)), FUN = "/")

# Load the necessary library
library(dbscan)

# Function to generate PCA plot with DBSCAN-based outliers
generate_pca_plot <- function(populations, residuals_common_mat, pop_sample, population_colors, title) {
  # Filter the residuals matrix for the selected populations
  selected_indices <- which(pop_sample %in% populations)
  filtered_residuals_common <- residuals_common_mat[selected_indices, ]
  
  # Remove columns with constant or zero variance
  filtered_residuals_common <- filtered_residuals_common[, apply(filtered_residuals_common, 2, var) > 0]
  
  # Apply PCA to the filtered residuals
  pca_results_common <- prcomp(filtered_residuals_common, center = TRUE, scale. = TRUE)
  
  # Extract the first two principal components
  pca_df_common <- data.frame(
    PC1 = pca_results_common$x[, 1],
    PC2 = pca_results_common$x[, 2],
    Population = pop_sample[selected_indices]
  )
  
  # Apply DBSCAN clustering on the PCA components
  dbscan_result <- dbscan(pca_df_common[, c("PC1", "PC2")], eps = 3, minPts = 5)
  
  # Identify outliers (points labeled as noise)
  pca_df_common$Cluster <- dbscan_result$cluster
  outliers_dbscan <- pca_df_common %>% filter(Cluster == 0)  # Noise points are labeled as 0
  
  # Plot the original PCA results
  pca_plot_common <- ggplot(pca_df_common, aes(x = PC1, y = PC2, color = Population)) +
    geom_point(size = 2) +
    labs(
      title = title,
      x = "Principal Component 1",
      y = "Principal Component 2"
    ) +
    scale_color_manual(values = population_colors) +
    theme_minimal()
  
  # Plot the PCA results with highlighted outliers
  pca_plot_common_outliers <- ggplot(pca_df_common, aes(x = PC1, y = PC2, color = Population)) +
    geom_point(size = 2) +
    geom_point(data = outliers_dbscan, aes(x = PC1, y = PC2), color = "black", shape = 1, size = 2, stroke = 1.5) +
    geom_point(data = outliers_dbscan, aes(x = PC1, y = PC2), size = 2) +
    labs(
      title = paste(title, "with DBSCAN-Based Outliers Highlighted"),
      x = "Principal Component 1",
      y = "Principal Component 2"
    ) +
    scale_color_manual(values = population_colors) +
    theme_minimal()
  
  # Display the plots
  print(pca_plot_common)
  print(pca_plot_common_outliers)
}

# Example usage with DBSCAN-based outliers
generate_pca_plot(populations_to_display, residuals_common_mat, pop_sample, population_colors, "Original PCA of Selected Genotypes")
