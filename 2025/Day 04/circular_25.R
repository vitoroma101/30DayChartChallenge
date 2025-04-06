library(dplyr)
library(tidyverse)
library(lubridate)

# Load and prepare data

# Want the full dataset? Visit the repo:  

# [DataForACoffee ☕](https://github.com/vitoroma101/DataForACoffee)  

data <- read.csv("E:\\Documents\\30day\\25\\Day_03\\SyncedSongs.csv",  
                 stringsAsFactors = FALSE)   

data$date <- ymd_hms(data$date)
data$weekday <- wday(data$date, label = TRUE, week_start = 1, abbr = FALSE, locale = "en_US.UTF-8")

# Calculate frequencies and percentages
frequency_days <- data %>%
  group_by(weekday) %>%
  summarise(count = n()) %>%
  mutate(
    percentage = count/sum(count)*100,
    label = sprintf("%d\n(%.1f%%)", count, percentage),
    weekday = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday", 
                                         "Thursday", "Friday", "Saturday", "Sunday"))
  ) %>%
  arrange(weekday)

# Color palette
colors <- c(
  "Monday" = "#6BAED6", "Tuesday" = "#4292C6", "Wednesday" = "#2171B5",
  "Thursday" = "#08519C", "Friday" = "#08306B", "Saturday" = "#FDDD8E", 
  "Sunday" = "#FDC926"
)

# Create chart
p <- ggplot(frequency_days, aes(x = 2, y = count, fill = weekday)) +
  geom_col(color = "white", linewidth = 0.5, width = 1) +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), 
            color = "black", 
            size = 3.5,
            fontface = "bold") +
  scale_fill_manual(values = colors) +
  xlim(0.5, 2.5) +
  theme_void() +
  labs(
    title = "When Do I Shazam the Most?",
    subtitle = "Weekly pattern of music discoveries | 2019–2025",
    caption = "Source: My Shazam data | Created by: @micosapiens711",
    fill = "Day of the Week"
  ) +
  theme(
    plot.title = element_text(
      hjust = 0.5, 
      face = "bold", 
      size = 18, 
      color = "#1A237E",
      margin = margin(b = 5, t = 20)
    ),
    plot.subtitle = element_text(
      hjust = 0.5, 
      margin = margin(b = 30, t = 10),
      color = "#4A5568",
      size = 14
    ),
    plot.caption = element_text(
      hjust = 0.5,
      color = "#718096",  # Color original
      size = 10,          # Tamaño original
      margin = margin(t = 15)  # Margen original
    ),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    plot.margin = margin(1, 1, 1.5, 1, "cm")
  ) +
  annotate("text", x = 0.5, y = 0, 
           label = paste("Total\nShazams\n", sum(frequency_days$count)),
           size = 5, color = "#08306B", fontface = "bold")

p

ggsave("circular_25.png",plot = p, width = 10, height = 8, dpi = 300)
