# Install necessary packages if not already installed
packages_needed <- c("readr", "ggplot2", "dplyr", "sf", "tidyr", "rnaturalearth", "rnaturalearthdata", "scales")
installed_packages <- installed.packages()
for (pkg in packages_needed) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}

# Load libraries
library(readr)
library(ggplot2)
library(dplyr)
library(sf)
library(tidyr)
library(rnaturalearth)
library(rnaturalearthdata)  # For high-resolution data
library(scales)

# Step 1: Import the CSV file
data <- read_csv("Agriculture_land_two.csv", show_col_types = FALSE)

# Step 2: Delete the first row
data <- data[-1, ]

# Step 3: Clean column names
names(data)[5:ncol(data)] <- gsub("%", "", names(data)[5:ncol(data)])  # Remove percentage signs
names(data)[5:ncol(data)] <- gsub(" ", "", names(data)[5:ncol(data)])  # Remove spaces

# Step 4: Remove any empty columns (optional)
data <- data %>% select_if(~ !all(is.na(.)))

# Step 5: Reshape the data from wide to long format
data_long <- data %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  # Select columns that are valid four-digit years
    names_to = "Year",           # Convert the year columns into a 'Year' column
    values_to = "Agricultural_Land_Percentage"  # The values in the year columns become 'Agricultural_Land_Percentage'
  )

# Convert 'Year' to numeric
data_long$Year <- as.numeric(data_long$Year)

# Convert 'Agricultural_Land_Percentage' to numeric and remove non-numeric entries
data_long <- data_long %>%
  mutate(Agricultural_Land_Percentage = as.numeric(as.character(Agricultural_Land_Percentage))) %>%
  filter(!is.na(Agricultural_Land_Percentage))

# Step 6: Download a shapefile for African countries with attributes
africa_shp <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# Step 7: Filter data for the year 2020 (instead of 2016)
data_2020 <- data_long %>% filter(Year == 2020)

# Step 8: Visualization 1 - Trend Over Time for Selected Countries (Nigeria, Kenya, South Africa)
# Filter for selected African countries
african_countries <- c("Nigeria", "Kenya", "South Africa")
data_filtered <- data_long %>% filter(`Country Name` %in% african_countries)

# Create an area plot for agricultural land percentage over time
trend_plot <- ggplot(data_filtered, aes(x = Year, y = Agricultural_Land_Percentage, fill = `Country Name`)) +
  geom_area(alpha = 0.6, size = 1, color = "black") +  # Create an area chart with color borders
  scale_fill_manual(values = c("South Africa" = "#eab700", 
                               "Kenya" = "#e75500", 
                               "Nigeria" = "#ad15c4")) +  # Customize country colors
  labs(title = "Agricultural Land Over Time for Nigeria, Kenya, and South Africa",
       x = "Year",
       y = "Agricultural Land (% of land area)",
       fill = "Country") +
  scale_x_continuous(breaks = seq(min(data_filtered$Year), max(data_filtered$Year), by = 5)) +  # Custom x-axis breaks
  theme_minimal(base_size = 15) +  # Minimal theme with larger font size for better readability
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    legend.position = "top",
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )

# Display and save the trend plot
ggsave("Trend_Over_Time_Agricultural_Land_2020.png", plot = trend_plot, width = 10, height = 6, dpi = 300)

# Step 9: Visualization 2 - Map of Agricultural Land in Africa (2020)
# Merge shapefile with the 2020 data based on 'iso_a3' and 'Country Code'
merged_data <- merge(africa_shp, data_2020, by.x = "iso_a3", by.y = "Country Code", all.x = TRUE)

# Create a choropleth map for agricultural land in Africa
map_plot <- ggplot(merged_data) +
  geom_sf(aes(fill = Agricultural_Land_Percentage)) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Agricultural Land in Africa (2020)", fill = "Agricultural Land (%)") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    legend.title = element_text(size = 14),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )

# Display and save the map plot
ggsave("Agricultural_Land_Map_2020.png", plot = map_plot, width = 10, height = 8, dpi = 300)

# Step 10: Visualization 3 - Bar Chart with Randomly Selected Countries (Including South Africa, Kenya, Nigeria)
# Set seed for reproducibility and select 10 countries (3 required + 7 random)
required_countries <- c("South Africa", "Kenya", "Nigeria")
random_countries <- data_2020 %>%
  filter(!`Country Name` %in% required_countries) %>%
  sample_n(7) %>%
  pull(`Country Name`)
final_countries <- c(required_countries, random_countries)

# Filter the data for the selected 10 countries
data_final <- data_2020 %>% filter(`Country Name` %in% final_countries)

# Create a categorical variable for agricultural land in 2020
data_final$Category <- cut(
  data_final$Agricultural_Land_Percentage,
  breaks = c(0, 20, 50, 100),
  labels = c("Low", "Medium", "High")
)

# Create a bar plot with categories
bar_plot <- ggplot(data_final, aes(x = reorder(`Country Name`, Agricultural_Land_Percentage), 
                                   y = Agricultural_Land_Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.5) +
  labs(title = "Agricultural Land Categories in 2020 for Selected African Countries",
       x = "Country", y = "Agricultural Land (%)", fill = "Category") +
  scale_fill_manual(values = c("Low" = "#f1d70b", "Medium" = "#cf1ae7", "High" = "#e38809")) +
  theme_minimal(base_size = 15) +
  coord_flip() +  # Flip the coordinates to make the chart horizontal
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),
    legend.position = "top"
  )

# Display and save the bar plot
ggsave("Agricultural_Land_Categories_2020.png", plot = bar_plot, width = 10, height = 8, dpi = 300)
