# File: geno_major_allele_frequency.R

# Source the utility functions and data loading files
source("/Users/mohammedkhodorfirasal-tal/Documents/Professional/Work/Fellowship - Harvard/Research/Project Code/geno_analysis_utilities.R")
source("/Users/mohammedkhodorfirasal-tal/Documents/Professional/Work/Fellowship - Harvard/Research/Project Code/geno_data_loading.R")

# Load the data
geno_data <- load_genetic_data()

# Access the working data
working_data <- geno_data$working_data
bed_mat <- working_data$bed_mat
pop_sample <- working_data$pop_sample

# Calculating Major Allele Frequencies
major_allele_frequencies <- 1 - colSums(bed_mat) / (2 * nrow(bed_mat))
cat("Major Allele Frequencies (First 10):\n")
print(major_allele_frequencies[1:10])

# Removing Rare Variants (MAF < 0.05 & MAF > 0.95)
common_major_allele_indices <- which(major_allele_frequencies <= 0.95 & major_allele_frequencies >= 0.05)
common_bed_mat <- bed_mat[, common_major_allele_indices]
common_major_allele_frequencies <- major_allele_frequencies[common_major_allele_indices]
cat("Common Major Allele Frequencies (First 10):\n")
print(common_major_allele_frequencies[1:10])

# Dimensions of the filtered matrix
cat("Dimensions of Common Bed Matrix:\n")
print(dim(common_bed_mat))

# Unique populations
populations <- unique(pop_sample)
cat("Unique Populations:\n")
print(populations)

# Initialize an empty list to store MAF for each population
maf_list_common <- list()

# Loop through each population and subset the data
for (pop in populations) {
  maf_list_common[[pop]] <- common_major_allele_frequencies[pop_sample == pop]
}

# Convert the list to a data frame
maf_df_common <- do.call(cbind, maf_list_common)
colnames(maf_df_common) <- populations
cat("MAF Data Frame (First 10 Rows):\n")
print(head(maf_df_common))


# Visualizing MAF variation across different populations
# Create a data frame from the list of common major allele frequencies for each population
new_maf_df_common <- as.data.frame(maf_df_common)

# Melting the common MAF data frame for ggplot2
allele_frequencies_df_melted_common <- new_maf_df_common %>%
  rownames_to_column(var = "SNP") %>%
  gather(key = "Population", value = "MAF", -SNP)

# Ensure Population is a factor with levels in the desired order
allele_frequencies_df_melted_common$Population <- factor(allele_frequencies_df_melted_common$Population, levels = populations)

# Creating boxplots for each population (Common MAF)
ggplot(allele_frequencies_df_melted_common, aes(x = Population, y = MAF)) +
  geom_boxplot() +
  labs(title = "Common Major Allele Frequency Distribution by Population",
       x = "Population",
       y = "Common Major Allele Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
