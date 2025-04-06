library(dplyr)
library(tidyverse)
library(lubridate)

# Load and prepare data

# Want the full dataset? Visit the repo:  

# [DataForACoffee ☕](https://github.com/vitoroma101/DataForACoffee)  

# Load Shazam data from CSV
data <- read.csv("E:\\Documents\\30day\\25\\SyncedSongs.csv", stringsAsFactors = FALSE)

# Convert the 'date' column to datetime format
data$date <- ymd_hms(data$date)

# Extract hour and classify into Light Hours (6 AM - 6 PM) and Dark Hours (6 PM - 6 AM)
data <- data %>%
  mutate(
    hour = hour(date),  # Extract the hour
    light_dark = case_when(
      hour >= 6 & hour < 18 ~ "Light Hours",  # 6 AM - 6 PM
      hour >= 18 | hour < 6 ~ "Dark Hours"    # 6 PM - 6 AM
    ),
    light_dark = factor(light_dark, levels = c("Light Hours", "Dark Hours"))  # Order the categories
  )

# Summarize Shazam counts by light/dark period (2019-2025)
waffle_data <- data %>%
  group_by(light_dark) %>%
  summarise(count = n(), .groups = "drop")

# Calculate the total number of Shazams to normalize the data
total_shazams <- sum(waffle_data$count)

# Calculate the number of squares each category will represent in a 100-square grid
waffle_data <- waffle_data %>%
  mutate(
    proportion = count / total_shazams,  # Proportion of each category
    squares = round(proportion * 100)    # Number of squares (rounded to total 100)
  )

# Create a data frame for a 10x10 grid (100 squares)
grid <- expand.grid(x = 1:10, y = 1:10)  # 10 rows x 10 columns grid

# Assign categories to the squares based on proportions
# Order the squares from left to right, bottom to top
grid$category <- NA
light_squares <- waffle_data$squares[waffle_data$light_dark == "Light Hours"]
dark_squares <- waffle_data$squares[waffle_data$light_dark == "Dark Hours"]

# Assign "Light Hours" to the first 'light_squares' squares
grid$category[1:light_squares] <- "Light Hours"
# Assign "Dark Hours" to the next 'dark_squares' squares
grid$category[(light_squares + 1):(light_squares + dark_squares)] <- "Dark Hours"

# Define a color palette for light and dark hours
colors_light_dark <- c(
  "Light Hours" = "#FFF9C4",  # Light yellow for Light Hours
  "Dark Hours" = "#4A235A"    # Dark purple for Dark Hours
)

# Calculate the percentage of "Dark Hours" for the annotation
dark_percentage <- round(waffle_data$proportion[waffle_data$light_dark == "Dark Hours"] * 100)

# Construct the annotation text
annotation_text <- paste0(dark_percentage, "% of my music search\ntakes place at night")
print(annotation_text)  # Verify in the console that the text is generated correctly

# Create the waffle chart
p <- ggplot(grid, aes(x = x, y = y, fill = category)) +
  # Waffle chart grid
  geom_tile(color = "white", size = 0.5) +
  
  # Color palette
  scale_fill_manual(values = c(
    "Light Hours" = "#FFF9C4",
    "Dark Hours" = "#4A235A"
  ), na.value = "white") +
  
  # Adjust aspect ratio
  coord_fixed(ratio = 1, xlim = c(0, 16), expand = FALSE) +
  
  # Annotation line (black)
  geom_segment(
    aes(x = 10, xend = 12, y = 5, yend = 5), 
    color = "black", 
    size = 0.8
  ) +
  
  # Black dot at the start of the line (using annotate)
  annotate(
    "point",
    x = 10, 
    y = 5,
    shape = 16,       # Solid circle
    size = 1, 
    color = "black"   # Same color as the line
  ) +
  
  # Annotation text (75% large + text below)
  annotate(
    "text", 
    x = 12.5, 
    y = 5.2,          # Position of the 75%
    label = "75%", 
    hjust = 0, 
    vjust = 0.5, 
    size = 8, 
    color = "black", 
    fontface = "bold"
  ) +
  annotate(
    "text", 
    x = 12.5, 
    y = 4.7,          # Text below the 75%
    label = "of my music search\ntakes place at night", 
    hjust = 0, 
    vjust = 1, 
    size = 4, 
    color = "black", 
    lineheight = 0.9
  ) +
  
  # Titles and fonts
  labs(
    title = "What Time Do I Shazam the Most?",
    subtitle = "Light Hours: 6 AM–6 PM, Dark Hours: 6 PM–6 AM",
    caption = "Source: My Shazam data (2019-2025) | Created by: @micosapiens711"
  ) +
  
  # Visual theme
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0, 
      face = "bold", 
      size = 18, 
      color = "#1A237E"
    ),
    plot.subtitle = element_text(
      hjust = 0, 
      margin = margin(b = 15), 
      color = "#4A5568"
    ),
    plot.caption = element_text(
      hjust = 0.5, 
      margin = margin(t = 15), 
      color = "#718096"
    ),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 60, b = 20, l = 20, unit = "pt")
  )

# Display the chart
p

# Save the chart as a PNG with a wider width to accommodate the annotation
ggsave("bigorsmall_25.png", plot = p, width = 10, height = 8, dpi = 600)