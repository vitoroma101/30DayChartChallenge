# Load libraries
library(tidyverse)
library(lubridate)
library(viridis)  # For color gradients

# Want the full dataset? Visit the repo:  

# [DataForACoffee ☕](https://github.com/vitoroma101/DataForACoffee)  

# Load your data
data <- read.csv("E:\\Documents\\30day\\25\\ZEPP\\ACTIVITY_STAGE\\ACTIVITY_STAGE_1744082349844.csv")

# Convert 'date' to date format and extract components
data <- data %>% 
  mutate(
    date = as.Date(date, format = "%Y-%m-%d"),
    datetime = as.POSIXct(paste(date, start), format = "%Y-%m-%d %H:%M"),
    hour = hour(datetime),
    year = year(date)
  ) %>% 
  filter(year %in% 2022:2024)

# Create density plot with gradient fill
p <- ggplot(data, aes(x = hour)) +
  geom_density(
    aes(y = after_stat(scaled), fill = factor(year)), 
    alpha = 0.6, 
    color = NA, 
    adjust = 0.6  # Smoothing parameter
  ) +
  geom_line(
    aes(y = after_stat(scaled), color = factor(year)), 
    stat = "density", 
    linewidth = 0.8, 
    adjust = 0.6
  ) +
  scale_fill_viridis(
    discrete = TRUE, 
    option = "magma", 
    begin = 0.3, 
    end = 0.8,
    name = ""
  ) +
  scale_color_viridis(
    discrete = TRUE, 
    option = "magma", 
    begin = 0.3, 
    end = 0.8,
    name = ""
  ) +
  labs(
    title = "Circadian Rhythm of Activities",
    subtitle = "Normalized hourly distribution for annual comparison",
    x = "",
    y = "",
    caption = "Source: Amazfit GTS 2 via Zepp app (2022-2025) | Created by: @micosapiens711") +
  scale_x_continuous(
    breaks = seq(0, 23, 3),
    labels = function(x) sprintf("%02d:00", x)
  ) +
  facet_wrap(
    ~ year, 
    ncol = 1, 
    labeller = labeller(year = function(x) paste("", x))
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      hjust = 0, 
      face = "bold", 
      size = 18, 
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      hjust = 0, 
      color = "gray40", 
      margin = margin(b = 20)
    ),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.title = element_text(color = "gray30"),
    axis.text = element_text(color = "gray50"),
    strip.text = element_text(
      face = "bold", 
      size = 12, 
      color = "gray30"
    ),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

p

ggsave("multimodal_25.png", p, width = 9, height = 12, dpi = 300)
