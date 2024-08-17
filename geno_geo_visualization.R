# File: geno_geo_visualization.R

# Source the utility functions and data loading files
source("~/Documents/Professional/Work/Fellowship - Harvard/Research/Project Code/geno_analysis_utilities.R")
source("~/Documents/Professional/Work/Fellowship - Harvard/Research/Project Code/geno_data_loading.R")

# Load the data
geno_data <- load_genetic_data()

# Access the working data
working_data <- geno_data$working_data

# Make a copy of the relevant data to avoid modifying the original working data
pop_geo_data_copy <- working_data$pop_geo_data
pop_sample_copy <- working_data$pop_sample

# Define population and geographical coordinates (latitudes and longitudes)
pop_geo_data <- data.frame(
  Population = c("ACB", "ASW", "BEB", "CDX", "CEU", "CHB", "CHS", "CLM", "ESN", "FIN", 
                 "GBR", "GIH", "GWD", "IBS", "ITU", "JPT", "KHV", "LWK", "MSL", "MXL", 
                 "PEL", "PJL", "PUR", "STU", "TSI", "YRI"),
  Latitude = c(12.9716, 36.7783, 23.6850, 30.5928, 51.1657, 39.9042, 22.3193, 4.7110, 
               9.0820, 61.9241, 55.3781, 20.5937, 13.4432, 40.4637, 41.8719, 35.6895, 
               21.0285, 1.2921, 8.4606, 19.4326, -12.0464, 30.3753, 18.2208, 20.5937, 
               41.9028, 9.0820),
  Longitude = c(-8.7832, -119.4179, 90.3563, 114.3055, 10.4515, 116.4074, 114.1694, 
                -74.0721, 8.6753, 25.7482, -3.4360, 78.9629, -15.3101, -3.7492, 12.5674, 
                139.6917, 105.8542, 36.8219, -11.7799, -99.1332, -77.0428, 69.3451, 
                -66.5901, 78.9629, 12.4964, 8.6753)
)

# Define the color palette
color_palette <- c(brewer.pal(n = 12, name = "Paired"), brewer.pal(n = 8, name = "Set3"), brewer.pal(n = 6, name = "Dark2"))

# Create a consistent color mapping for all populations
all_populations <- unique(pop_geo_data$Population)
population_colors <- setNames(color_palette[1:length(all_populations)], all_populations)

# Load world map data
world_map <- map_data("world")

# Create the map plot
geo_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  geom_point(data = pop_geo_data, aes(x = Longitude, y = Latitude, color = Population), size = 3) +
  scale_color_manual(values = population_colors) +
  labs(title = "Geographical Distribution of Populations", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.text = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 12),
    legend.position = "right"
  )

# Print the plot
print(geo_plot)

# Super Population Visualization
# Define population and super population data
pop_sample <- c("ACB", "ASW", "BEB", "CDX", "CEU", "CHB", "CHS", "CLM", "ESN", "FIN", "GBR", "GIH", "GWD", "IBS", "ITU", "JPT", "KHV", "LWK", "MSL", "MXL", "PEL", "PJL", "PUR", "STU", "TSI", "YRI")
pop_counts <- c(80, 54, 71, 76, 78, 83, 86, 71, 82, 84, 67, 85, 85, 82, 77, 82, 77, 75, 70, 52, 68, 76, 87, 84, 84, 84)
super_pop_sample <- c("AFR", "AFR", "SAS", "EAS", "EUR", "EAS", "EAS", "AMR", "AFR", "EUR", "EUR", "SAS", "AFR", "EUR", "EUR", "EAS", "EAS", "AFR", "AFR", "AMR", "AMR", "SAS", "AMR", "SAS", "EUR", "AFR")

# Create a data frame
pop_data <- data.frame(
  Population = pop_sample,
  Count = pop_counts,
  SuperPopulation = super_pop_sample
)

# Summarize super population counts
super_pop_summary <- pop_data %>%
  group_by(SuperPopulation) %>%
  summarise(SuperCount = sum(Count))

# Merge population data with super population summary
combined_data <- merge(pop_data, super_pop_summary, by = "SuperPopulation")

# Define color palette
color_palette <- c(brewer.pal(n = 12, name = "Paired"), brewer.pal(n = 8, name = "Set3"), brewer.pal(n = 6, name = "Dark2"))

# Create a consistent color mapping for all populations
all_populations <- unique(combined_data$Population)
population_colors <- setNames(color_palette[1:length(all_populations)], all_populations)

# Plot population counts within each super population
pop_plot <- ggplot(combined_data, aes(x = SuperPopulation, y = Count, fill = Population)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = population_colors) +
  labs(
    title = "Population Distribution within Super Populations",
    x = "Super Population",
    y = "Population Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 12)
  )

# Print the plot
print(pop_plot)


# Visualize Population Distribution
population_distribution <- table(working_data$pop_sample)

ggplot(as.data.frame(population_distribution), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Population Distribution", x = "Population", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualize Super Population Distribution
super_population_distribution <- table(working_data$super_pop_sample)

ggplot(as.data.frame(super_population_distribution), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(title = "Super Population Distribution", x = "Super Population", y = "Count") +
  theme_minimal()

# Combo Chart: Population Counts within Super Populations
pop_data <- data.frame(
  Population = working_data$pop_sample,
  SuperPopulation = working_data$super_pop_sample
)

ggplot(pop_data, aes(x = SuperPopulation, fill = Population)) +
  geom_bar(position = "dodge") +
  labs(title = "Population Counts within Super Populations", x = "Super Population", y = "Count") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()


# Mapping of population acronyms to their full descriptions
population_mapping <- data.frame(
  Population = c("ACB", "ASW", "BEB", "CDX", "CEU", "CHB", "CHS", "CLM", "ESN", "FIN", 
                 "GBR", "GIH", "GWD", "IBS", "ITU", "JPT", "KHV", "LWK", "MSL", "MXL", 
                 "PEL", "PJL", "PUR", "STU", "TSI", "YRI"),
  Description = c("African Caribbean in Barbados", "African Ancestry in Southwest US", 
                  "Bengali in Bangladesh", "Chinese Dai in Xishuangbanna", 
                  "Utah Residents (CEPH) with Northern and Western European Ancestry", 
                  "Han Chinese in Beijing, China", "Southern Han Chinese", 
                  "Colombian in Medellin, Colombia", "Esan in Nigeria", 
                  "Finnish in Finland", "British in England and Scotland", 
                  "Gujarati Indian in Houston, Texas", "Gambian in Western Division, The Gambia", 
                  "Iberian Population in Spain", "Indian Telugu in the UK", 
                  "Japanese in Tokyo, Japan", "Kinh in Ho Chi Minh City, Vietnam", 
                  "Luhya in Webuye, Kenya", "Mende in Sierra Leone", 
                  "Mexican Ancestry in Los Angeles, California", 
                  "Peruvian in Lima, Peru", "Punjabi in Lahore, Pakistan", 
                  "Puerto Rican in Puerto Rico", "Sri Lankan Tamil in the UK", 
                  "Toscani in Italy", "Yoruba in Ibadan, Nigeria")
)

# You can use this data frame to look up or print the full descriptions when needed
print(population_mapping)
