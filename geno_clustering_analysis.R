# File: geno_clustering_analysis.R

# Source the necessary utility functions and data loading files
source("/Users/mohammedkhodorfirasal-tal/Documents/Professional/Work/Fellowship - Harvard/Research/Project Code/geno_analysis_utilities.R")

# Load the data directly
load("/Users/mohammedkhodorfirasal-tal/Documents/Professional/Work/Fellowship - Harvard/Research/Data/pop_gen_sample.RData")

# Access the working data
bed_mat <- as.matrix(bed_mat)  # Convert to dense matrix
pop_sample <- pop_sample

# Define the mode_ethnicity function
mode_ethnicity <- function(clusters, populations) {
  cluster_mode <- sapply(unique(clusters), function(cluster) {
    cluster_populations <- populations[clusters == cluster]
    mode_population <- names(sort(table(cluster_populations), decreasing = TRUE))[1]
    return(mode_population)
  })
  return(cluster_mode)
}

# Subset for 25 individuals from the common_bed_mat
sample_indices <- sample(1:nrow(bed_mat), 25)

# Get the population sample subset for these individuals
pop_sample_subset <- pop_sample[sample_indices]

# Create a data frame with original row numbers and corresponding population labels
individual_population_table <- data.frame(Individual = sample_indices, Population = pop_sample_subset)

# Generate pairwise distance matrix using Manhattan distance
pairwise.dist1 <- as.matrix(dist(bed_mat[sample_indices, ], method = "manhattan"))

# Perform hierarchical clustering using Manhattan distance
manhattan.clustering <- hclust(as.dist(pairwise.dist1))

# Plot dendrogram with labels
plot(manhattan.clustering, labels = pop_sample_subset, main = "Manhattan Clustering")

# Cut tree to form clusters for different k values
k_values <- c(2, 3, 4, 5, 6)
for (k in k_values) {
  # Cut tree
  manhattan.clusters <- cutree(manhattan.clustering, k)
  
  # Plot clusters on dendrogram
  plot(manhattan.clustering, labels = pop_sample_subset, main = paste("Manhattan Clustering (k =", k, ")"))
  rect.hclust(manhattan.clustering, k = k, border = 2:(k+1))
  
  # Determine mode ethnicity for each cluster
  manhattan.mode.ethnicity <- mode_ethnicity(manhattan.clusters, pop_sample_subset)
  print(paste("Manhattan Clustering - Mode Ethnicity per Cluster (k =", k, "):"))
  print(manhattan.mode.ethnicity)
}

# Generate pairwise distance matrix using Euclidean distance
pairwise.dist2 <- as.matrix(dist(bed_mat[sample_indices, ], method = "euclidean"))

# Perform hierarchical clustering using Euclidean distance
euclidean.clustering <- hclust(as.dist(pairwise.dist2))

# Plot dendrogram with labels
plot(euclidean.clustering, labels = pop_sample_subset, main = "Euclidean Clustering")

# Cut tree to form clusters for different k values
for (k in k_values) {
  # Cut tree
  euclidean.clusters <- cutree(euclidean.clustering, k)
  
  # Plot clusters on dendrogram
  plot(euclidean.clustering, labels = pop_sample_subset, main = paste("Euclidean Clustering (k =", k, ")"))
  rect.hclust(euclidean.clustering, k = k, border = 2:(k+1))
  
  # Determine mode ethnicity for each cluster
  euclidean.mode.ethnicity <- mode_ethnicity(euclidean.clusters, pop_sample_subset)
  print(paste("Euclidean Clustering - Mode Ethnicity per Cluster (k =", k, "):"))
  print(euclidean.mode.ethnicity)
}
