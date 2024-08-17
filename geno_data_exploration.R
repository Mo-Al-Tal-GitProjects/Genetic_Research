# File: geno_data_exploration.R

# Source the utility functions and data loading files
source("~/Documents/Professional/Work/Fellowship - Harvard/Research/Project Code/geno_analysis_utilities.R")
source("~/Documents/Professional/Work/Fellowship - Harvard/Research/Project Code/geno_data_loading.R")

# Load the data
geno_data <- load_genetic_data()

# Access the working data
working_data <- geno_data$working_data

# Displaying Data Structure
lapply(geno_data, str)

# Number of Rows (Individuals) & Columns (SNP):
dim(working_data$bed_mat)

# Display population distribution
table(working_data$pop_sample)

# Major Allele Frequencies (Preview)
head(working_data$bed_mat)

# Summary of the genotype matrix
cat("Matrix dimensions:", dim(working_data$bed_mat), "\n")
cat("Number of non-zero entries:", length(working_data$bed_mat@x), "\n")
cat("Preview of non-zero entries:\n")
print(summary(working_data$bed_mat)[1:20, ])  # Show first 20 non-zero entries


# Summary Statistics for SNPs
snp_summary <- apply(working_data$bed_mat, 2, function(x) {
  list(mean = mean(x), median = median(x), variance = var(x))
})

# Convert list to a data frame for easier viewing
snp_summary_df <- do.call(rbind, lapply(snp_summary, as.data.frame))
colnames(snp_summary_df) <- c("Mean", "Median", "Variance")
head(snp_summary_df)

# Check for missing data in the genotype matrix
missing_data <- sum(is.na(working_data$bed_mat))
cat("Total missing entries:", missing_data, "\n")
