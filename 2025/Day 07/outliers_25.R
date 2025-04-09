# Load required libraries

library(ggplot2)
library(dplyr)
library(lubridate)

# Want the full dataset? Visit the repo:  

# [DataForACoffee ☕](https://github.com/vitoroma101/DataForACoffee)  

# Load Zepp data from csv
data <- read.csv("E:\\Documents\\30day\\25\\ZEPP\\HEARTRATE_AUTO\\HEARTRATE_AUTO_1744082351815.csv")

# Combine date and time into a single datetime column
data <- data %>%
  mutate(datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M", tz = "UTC"))

# Identify outliers using the IQR method
# Calculate Q1, Q3, and IQR
stats <- data %>%
  summarise(
    Q1 = quantile(heartRate, 0.25, na.rm = TRUE),
    Q3 = quantile(heartRate, 0.75, na.rm = TRUE)
  ) %>%
  mutate(
    IQR = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR
  )

# Mark outliers in the dataset
data <- data %>%
  mutate(
    is_outlier = heartRate < stats$lower_bound | heartRate > stats$upper_bound
  )

# Filter a subset of data for visualization (my birthday: October 7th)

data_subset <- data %>%
  filter(date == "2022-10-07")  # My birthday

# Create a line plot with highlighted outliers
p <- ggplot(data_subset, aes(x = datetime, y = heartRate)) +
  geom_line(color = "gray50", size = 0.5) +  # Line for heart rate trend
  geom_point(data = data_subset %>% filter(!is_outlier), 
             aes(color = "Normal"), size = 2, alpha = 0.5) +  # Normal points
  geom_point(data = data_subset %>% filter(is_outlier), 
             aes(color = "Outlier"), size = 3, shape = 18) +  # Highlighted outliers
  scale_color_manual(values = c("Normal" = "#00AFBB", "Outlier" = "#FC4E07")) +  # Colors
  labs(title = "My Heart Rate on My Birthday",  # Updated to my birthday
       subtitle = "Outliers: extreme moments from 914 recorded measurements",
       x = "Time",
       y = "Heart Rate (bpm)",
       color = "Data Type",
       caption = "Source: Amazfit GTS 2 via Zepp app (2022-2025) | Created by: @micosapiens711") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0, size = 12),
        legend.position = "top")
table(data_subset$is_outlier)

p

# Save the plot
ggsave("outliers_25.png", plot=p,width = 10, height = 6, dpi = 300)
