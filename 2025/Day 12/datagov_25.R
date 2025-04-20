# This script analyzes dog bite data from Data.gov for the #30DayChartChallenge

# Load libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(tidyr)

# Load raw data
data <- read.csv("E:\\Documents\\30day\\25\\DOHMH_Dog_Bite_Data.csv")

# Set English language for months
Sys.setlocale("LC_TIME", "en_US")

# Initial processing
gender_lines <- data %>%
  # Convert date and extract month
  mutate(
    DateOfBite = mdy(DateOfBite),
    Month = month(DateOfBite, label = TRUE, abbr = TRUE)
  ) %>%
  # Filter by breed and clean data
  filter(
    Breed == "Siberian Husky",
    !is.na(DateOfBite)
  ) %>%
  # Normalize gender
  mutate(
    Gender = case_when(
      Gender == "M" ~ "Male",
      Gender == "F" ~ "Female",
      TRUE ~ "Unknown/Other"
    )
  ) %>%
  # Count by month and gender
  count(Month, Gender, name = "Bites") %>%
  # Complete missing months
  complete(
    Month = month.abb,
    Gender,
    fill = list(Bites = 0)
  ) %>%
  # Order months chronologically
  mutate(Month = factor(Month, levels = month.abb))

# Chart with manual legend in empty area (November)
p <- ggplot() +
  # Main layers
  geom_area(data = filter(gender_lines, Gender == "Male"), 
            aes(x = Month, y = Bites, group = 1),
            fill = "#B0C4DE", alpha = 1) +
  geom_area(data = filter(gender_lines, Gender == "Female"), 
            aes(x = Month, y = Bites, group = 1),
            fill = "#8B7D6B", alpha = 1) +
  geom_line(data = filter(gender_lines, Gender == "Unknown/Other"), 
            aes(x = Month, y = Bites, group = 1),
            color = "#2E2E2E", linewidth = 1.2, linetype = "twodash") +
  
  # Legend in November (x = 11), in an empty area (y = 18-22)
  annotate("point", x = 11, y = 22, 
           size = 3, color = "#B0C4DE") +
  annotate("text", x = 11.2, y = 22, 
           label = "Male", size = 3.5, hjust = 0) +
  annotate("point", x = 11, y = 20, 
           size = 3, color = "#8B7D6B") +
  annotate("text", x = 11.2, y = 20, 
           label = "Female", size = 3.5, hjust = 0) +
  annotate("segment", x = 11, xend = 11.2, y = 18, yend = 18, 
           color = "#2E2E2E", linewidth = 0.8, linetype = "twodash") +
  annotate("text", x = 11.2, y = 18, 
           label = "Unknown", size = 3.5, hjust = 0) +
  
  # Final configuration
  labs(
    title = "Siberian Husky Bite Distribution by Gender (New York, 2015-2023)",
    subtitle = "441 registered reports, unexpectedly listed in the top 10 breeds by occurrence",
    caption = "Source: Data.gov | Created by: @micosapiens711"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9,
                               margin = margin(t = 3, b = 10)),    
    plot.margin = margin(t = 40, r = 40, b = 20, l = 20),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.2),
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(color = "gray40", size = 10, hjust = 0.5),
    plot.caption = element_text(color = "gray60", size = 8, hjust = 1)
  ) +
  coord_cartesian(clip = "off")

p

# Save the chart
ggsave("datagov_25.png", plot=p, width = 8, height = 6, dpi = 300)