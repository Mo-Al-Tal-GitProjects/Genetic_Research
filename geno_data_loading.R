# File: geno_data_loading.R

# Source the utility functions file
source("/Users/mohammedkhodorfirasal-tal/Documents/Professional/Work/Fellowship - Harvard/Research/Project Code/geno_analysis_utilities.R")

# Utility function to load genetic data
load_genetic_data <- function(default_path = "/Users/mohammedkhodorfirasal-tal/Documents/Professional/Work/Fellowship - Harvard/Research/Data/pop_gen_sample.RData") {
  
  # Prompt user for file path or use default
  file_path <- readline(prompt = paste("Enter the file path (press Enter to use default):", default_path))
  if (file_path == "") {
    file_path <- default_path
  }
  
  # Check if the file exists
  if (!file.exists(file_path)) {
    stop("The specified file does not exist. Please check the file path and try again.")
  }
  
  # Load the data
  load(file_path)
  
  # Get the names of all loaded objects
  loaded_objects <- ls()
  
  # Create multiple storage options
  original_data <- mget(loaded_objects) # Store the original data
  backup_data <- original_data # Backup copy
  working_data <- original_data # Copy for analysis (modifiable)
  
  # Return the loaded data
  list(original_data = original_data, 
       backup_data = backup_data, 
       working_data = working_data)
}

# Load the data using the function
geno_data <- load_genetic_data()

# Access the original, backup, and working data
original_data <- geno_data$original_data
backup_data <- geno_data$backup_data
working_data <- geno_data$working_data

# Example usage: Display structure of the original data
str(original_data)
