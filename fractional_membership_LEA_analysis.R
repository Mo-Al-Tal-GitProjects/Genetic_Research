# File: fractional_membership_LEA_analysis.R

# Load the necessary libraries
library(LEA)
library(ggplot2)

# Function to generate a plot using LEA and ggplot2
generate_LEA_plot <- function(pop_sample, geno_file, K, colors, plot_title) {
  # Run the snmf function from LEA to perform a STRUCTURE-like analysis
  fit <- snmf(geno_file, K = K, project = "new", entropy = TRUE)
  
  # Extract the Q matrix from the best run (Q matrix contains ancestry proportions)
  best_run <- which.min(cross.entropy(fit, K = K))
  q_mat <- Q(fit, K = K, run = best_run)
  
  # Prepare data frame for plotting
  df <- data.frame(
    Population = factor(pop_sample, levels = unique(pop_sample)),  # Ensure consistent order
    Fraction = q_mat[,1]  # This is for the first cluster, adjust if needed for multiple clusters
  )
  
  # Generate the plot
  plot <- ggplot(df, aes(x = Population, y = Fraction, fill = Population)) +
    geom_boxplot(outlier.color = "white") +
    geom_jitter(size = 0.5, width = 0.2) +
    ylab("Fraction") +
    xlab("Population") +
    ggtitle(plot_title) +
    scale_fill_manual(values = colors) +
    theme_bw()
  
  print(plot)
}

# Define population sample and codes
pop_sample <- rep(c("ACB", "ASW", "CLM", "MXL", "PEL", "PUR"), each = 333)
pop_sample <- c(pop_sample, sample(pop_sample, 2))  # Adjust to make the length 2000
codes <- c("ACB", "ASW", "CLM", "MXL", "PEL", "PUR")

# Call the function with appropriate parameters
generate_LEA_plot(pop_sample, "mytestgeno.geno", K = 2, colors = c("red", "blue"), plot_title = "STRUCTURE with 2 Clusters")
