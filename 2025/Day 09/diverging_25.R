# Load required libraries
library(tidyverse)
library(lubridate)

# Want the full dataset? Visit the repo:  

# [DataForACoffee ☕](https://github.com/vitoroma101/DataForACoffee)  

# Set locale to English to ensure months are displayed in English
original_locale <- Sys.getlocale(category = "LC_TIME")  # Store the original locale
Sys.setlocale("LC_TIME", "en_US.UTF-8")  # Switch to English

# Read the data
data <- read.csv("E:\\Documents\\30day\\25\\SyncedSongs.csv")

# Process the data
data_filtered <- data %>%
  # Convert the 'date' column to datetime format
  mutate(datetime = ymd_hms(date, tz = "UTC"),
         # Adjust to Ecuador timezone (UTC-5)
         datetime_local = with_tz(datetime, tzone = "America/Guayaquil"),
         # Extract hour and month
         hour = hour(datetime_local),
         month = month(datetime_local, label = TRUE, abbr = FALSE),  # Months in English (January, February, ...)
         # Classify into Day (6 AM-6 PM) and Night (6 PM-6 AM)
         period = if_else(hour >= 6 & hour < 18, "Day", "Night")) %>%
  # Count the number of identifications by month and period
  group_by(month, period) %>%
  summarise(count = n(), .groups = "drop") %>%
  # Create divergent values: positive for Night (right), negative for Day (left)
  mutate(count_divergent = if_else(period == "Night", count, -count))

# Create the divergent bar chart
p <- ggplot(data_filtered, aes(x = count_divergent, y = month, fill = period)) +
  # Divergent bars
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  # Color scale
  scale_fill_manual(values = c("Night" = "#1A2526", "Day" = "#C2A87D"),
                    labels = c("Night" = "Night (6 PM-6 AM)", "Day" = "Day (6 AM-6 PM)"),
                    name = "Period") +
  # Vertical line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  # Adjust X axis to show counts (absolute values)
  scale_x_continuous(breaks = seq(-50, 50, by = 10),
                     labels = abs) +  # Show absolute values
  # Labels (all in English)
  labs(title = "Day vs. Night on Shazam: Monthly Trends (2019-2025)",
       subtitle = "Temporal patterns uncovered across 477 music identifications",
       x = "Number of Identifications",
       y = "",
       caption = "Source: My Shazam data | Created by: @micosapiens711") +
  # Custom theme (use "sans" to avoid warnings)
  theme_minimal(base_family = "sans") +
  theme(
    # Background and panel
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    # Title and subtitle
    plot.title = element_text(hjust = 0, size = 14, face = "bold", margin = margin(b = 10), color = "black"),
    plot.subtitle = element_text(hjust = 0, size = 12, margin = margin(b = 10), color = "grey30"),
    plot.caption = element_text(hjust = 1, size = 8, color = "grey50"),  # Caption on the right
    # Axes
    axis.title.x = element_text(size = 10, margin = margin(t = 10), color = "black"),
    axis.title.y = element_text(size = 10, margin = margin(r = 10), color = "black"),
    axis.text.x = element_text(size = 8, color = "black"),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.ticks = element_line(color = "black"),
    # Legend
    legend.position = "top",
    legend.text = element_text(size = 8, color = "black"),
    legend.title = element_text(size = 9, color = "black", face = "bold"),
    legend.background = element_rect(fill = "white", color = NA),
    # Margins
    plot.margin = unit(c(1, 1.5, 1, 1.5), "cm")
  )

# Display the plot
print(p)

# Save the plot
ggsave("diverging_25.png", plot = p, width = 7, height = 7, dpi = 300)

# Restore the original locale
Sys.setlocale("LC_TIME", original_locale)