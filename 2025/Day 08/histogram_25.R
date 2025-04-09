# Load libraries
library(tidyverse)
library(lubridate)

# Want the full dataset? Visit the repo:  

# [DataForACoffee ☕](https://github.com/vitoroma101/DataForACoffee)  

# Read Zepp data
heartmin <- read.csv("E:\\Documents\\30day\\25\\ZEPP\\HEARTRATE_AUTO\\HEARTRATE_AUTO_1744082351815.csv")

# Filter non-finite values immediately after loading
heartmin <- heartmin %>%
  filter(!is.na(heartRate) & !is.nan(heartRate) & is.finite(heartRate))

# Convert to datetime
heartmin <- heartmin %>%
  mutate(datetime = ymd_hm(paste(date, time)))

# Filter data for January 19, 2025 (entire day)
marathon_data <- heartmin %>%
  filter(date == "2025-01-19")

# Ensure heartRate is numeric
marathon_data <- marathon_data %>%
  mutate(heartRate = as.numeric(heartRate)) %>%
  filter(!is.na(heartRate) & is.finite(heartRate))

# Histogram
p <- ggplot(marathon_data, aes(x = heartRate)) +
  # Histogram
  geom_histogram(aes(fill = after_stat(x)), binwidth = 10, color = "black", alpha = 0.5) +
  # Density curve
  geom_density(aes(y = after_stat(count) * 10), color = "black", size = 1.2, alpha = 0.3) +
  # Color gradient
  scale_fill_gradient(low = "yellow", high = "red", name = "BPM") +
  # Adjust X-axis based on the actual range (without limits to avoid data removal)
  scale_x_continuous(breaks = seq(40, 200, by = 20)) +
  # Labels and style
  labs(title = "My Heart Rate on the Day I Ran a Marathon: January 19, 2025",
       subtitle = "320 Records, Including a 36-Minute 6K Event: 49 to 179 BPM",
       x = "Heart Rate (BPM)", y = "Frequency",
       caption = "Source: Amazfit GTS 2 via Zepp app (2022-2025) | Created by: @micosapiens711") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0, size = 12, face = "bold", margin = margin(b = 10)),
        plot.subtitle = element_text(hjust = 0, size = 10, margin = margin(b = 10)),
        axis.title.x = element_text(size = 10, margin = margin(t = 10)),
        axis.title.y = element_text(size = 10, margin = margin(r = 10)),
        legend.position = "none",
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "in"))

# Display the plot
print(p)

# Save the plot
ggsave("histogram_25.png", plot = p, width = 7, height = 5, dpi = 300)
