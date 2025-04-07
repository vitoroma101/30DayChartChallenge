# Load required libraries
library(dplyr)      # For data manipulation
library(ggplot2)    # For creating the plot
library(lubridate)  # For handling dates and extracting components
library(forcats)    # For ordering factors

# Load and prepare data

# Want the full dataset? Visit the repo:  

# [DataForACoffee ☕](https://github.com/vitoroma101/DataForACoffee)  

# Load Shazam data from CSV

data <- read.csv("E:\\Documents\\30day\\25\\SyncedSongs.csv", stringsAsFactors = FALSE)

# Remove duplicates, keeping the first occurrence
data_no_duplicates <- data %>% 
  distinct(title, .keep_all = TRUE)

# Ensure 'date' is in date-time format
data_no_duplicates <- data_no_duplicates %>%
  mutate(date = ymd_hms(date))  # Convert to date-time format

# Calculate total Shazams per artist (globally)
global_shazams <- data_no_duplicates %>%
  group_by(artist) %>%
  summarise(total_shazams = n()) %>%
  arrange(desc(total_shazams))

# Create time ranges
data_with_ranges <- data_no_duplicates %>%
  mutate(hour = hour(date),  # Extract the hour
         time_range = case_when(
           hour >= 0 & hour < 6 ~ "00:00-06:00",
           hour >= 6 & hour < 12 ~ "06:00-12:00",
           hour >= 12 & hour < 18 ~ "12:00-18:00",
           hour >= 18 & hour <= 23 ~ "18:00-24:00",
           TRUE ~ NA_character_  # Handle potential NAs
         ))

# Calculate ranking of time ranges and select the top artist
hour_ranking <- data_with_ranges %>%
  filter(!is.na(time_range)) %>%  # Exclude NAs in time_range
  group_by(time_range, artist) %>%
  summarise(n_shazams = n(), .groups = "drop") %>%
  left_join(global_shazams, by = "artist") %>%  # Add global Shazams
  group_by(time_range) %>%
  arrange(time_range, desc(n_shazams), desc(total_shazams), artist) %>%  # Sort by Shazams in range, then global Shazams, then alphabetically
  summarise(n_shazams_total = sum(n_shazams),  # Total Shazams per range
            top_artist = first(artist),  # Select the first (prioritizing global Shazams)
            n_top_artist = first(n_shazams),  # Number of Shazams of the top artist in this range
            .groups = "drop") %>%
  arrange(desc(n_shazams_total))

# Ensure time_range is a factor with the correct order
hour_ranking <- hour_ranking %>%
  mutate(time_range = factor(time_range, 
                             levels = c("00:00-06:00", "06:00-12:00", "12:00-18:00", "18:00-24:00")))

# Create the bar plot for the ranking with adjusted labels
p <- ggplot(hour_ranking, aes(x = reorder(time_range, n_shazams_total), y = n_shazams_total, fill = time_range)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip() +  # Horizontal bars for better readability
  labs(title = "Ranking Music Discovery by Shazams in Time Ranges (2019-2025)",
       subtitle = "'Top Artist': in case of a tie, selected by shazams across all time ranges; if the tie persists, by alphabetical order",
       x = "Time Range",
       y = "Number of Shazams",
       fill = "Time Range",
       caption = "Source: My Shazam data\nCreated by: @micosapiens711") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        axis.text.y = element_text(size = 10, margin = margin(r = 10)),
        axis.text.x = element_text(size = 10),
        legend.position = "none",
        plot.margin = margin(t = 5, r = 80, b = 5, l = 5, unit = "pt")) +  # Increased right margin for more label space
  scale_fill_manual(values = c("00:00-06:00" = "#1A2526", 
                               "06:00-12:00" = "#5C6B73", 
                               "12:00-18:00" = "#C2A87D", 
                               "18:00-24:00" = "#4A2E5A")) +
  geom_text(aes(label = paste0(n_shazams_total, "\nTop: ", top_artist, " (", n_top_artist, ")")), 
            hjust = -0.2, size = 3.5, color = "black", lineheight = 0.8) +  # Adjusted hjust, smaller size, and lineheight for better spacing
  scale_y_continuous(limits = c(0, max(hour_ranking$n_shazams_total) * 1.3))  # Extend the y-axis to give more room for labels

# Display the plot
p

# Save the plot
ggsave("ranking_25.png", plot = p, width = 10, height = 6, dpi = 300)
