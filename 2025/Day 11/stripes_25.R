# Load required libraries
library(dplyr)      # For data manipulation
library(ggplot2)    # For creating the plot
library(scales)     # For formatting numbers in the legend
library(lubridate)  # For handling dates

# Want the full dataset? Visit the repo:  

# [DataForACoffee ☕](https://github.com/vitoroma101/DataForACoffee)  

# Load data
data <- read.csv("E:\\Documents\\30day\\25\\ZEPP\\ACTIVITY\\ACTIVITY_1744082349491.csv")

# Filter out days with 0 steps to remove periods with no data
data_filtered <- data %>%
  filter(steps > 0) %>%  # Only keep days with steps > 0
  mutate(date = ymd(date))

# Create a daily dataset with total steps per day
data_daily <- data_filtered %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = TRUE), .groups = "drop") %>%
  mutate(y = 1)

# Create a continuous time index to compress the timeline (remove gaps)
data_daily <- data_daily %>%
  arrange(date) %>%  # Sort by date
  mutate(time_index = row_number())  # Create a continuous index for the x-axis

# Create the strip plot with a continuous gradient effect, using the compressed timeline
ggplot(data_daily, aes(x = time_index, y = y, fill = total_steps)) +
  geom_tile(width = 1, height = 1) +  # Width = 1 to create a continuous effect
  scale_fill_gradient(low = "white", high = "#1E88E5",  # Blue gradient
                      name = "Total Steps", 
                      labels = comma, 
                      breaks = pretty_breaks(n = 5),  # Automatically choose breaks for the legend
                      guide = guide_colorbar(direction = "horizontal",  # Horizontal gradient bar
                                             title.position = "top", 
                                             title.hjust = 0.5, 
                                             barwidth = 15,  # Width of the gradient bar
                                             barheight = 0.5)) +  # Height of the gradient bar
  scale_x_continuous(labels = NULL, expand = c(0.05, 0.05)) +  # Add padding to left and right margins
  labs(
    title = "Tracking My Steps: A Smartwatch Journey (2022-2025)",
    subtitle = "Note each day as a stripe with gaps intentionally excluded",  
    x = NULL,  # Remove x-axis label
    y = NULL,  # No label for y-axis
    caption = "Source: Amazfit GTS 2 via Zepp app (2022-2025) | Created by: @micosapiens711"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0),  # Smaller title
    plot.subtitle = element_text(size = 10, hjust = 0), 
    plot.caption = element_text(size = 8, color = "gray60", hjust = 1),  
    axis.text.y = element_blank(),  # Hide y-axis labels
    axis.ticks.y = element_blank(),  # Hide y-axis ticks
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(), # Required to remove minor grid lines and keep the plot clean
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",  # Place the legend at the bottom
    legend.title = element_text(size = 8),  # Smaller legend title
    legend.text = element_text(size = 8),  # Smaller legend text
    plot.margin = margin(l = 20, r = 20, t = 10, b = 10)  # Adjust left and right margins
  )

# Save the plot
ggsave("stripes_25.png", width = 12, height = 6, dpi = 600)
