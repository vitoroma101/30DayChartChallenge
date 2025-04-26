# Load libraries
library(tidyverse)    # Data manipulation and visualization
library(lubridate)    # Date handling
library(ggplot2)      # Plotting system
library(scales)       # Axis formatting
library(plotly)       # Interactive visualizations (optional)

# Load and prepare data

# Want the full dataset? Visit the repo:  

# [DataForACoffee ☕](https://github.com/vitoroma101/DataForACoffee)  

data <- read.csv("E:\\Documents\\30day\\25\\SyncedSongs.csv", stringsAsFactors = FALSE)

# Preprocess data
data <- data %>%
  mutate(
    date = ymd_hms(date),                   # Convert to datetime format
    date_day = as_date(date),                # Extract date component
    artist = str_trim(tolower(artist)),      # Normalize artist names
    artist = stringi::stri_trans_general(artist, "Latin-ASCII"),  # Remove accents
    artist = str_replace_all(artist, "&", "and")  # Standardize separators
  ) %>%
  filter(!is.na(artist) & artist != "") %>%  # Remove invalid entries
  filter(year(date_day) == 2020)             # Keep only 2020 data

# Create heatmap dataset
heatmap_data <- data %>%
  distinct(artist, date_day) %>%            # Unique artist-date combinations
  group_by(date_day) %>%
  summarise(n_artists = n_distinct(artist)) # Count artists per day

# Create full calendar structure 
start_date <- as_date("2019-12-29")  # First Sunday of 2020

all_days <- tibble(
  date_day = seq(as_date("2020-01-01"), as_date("2020-12-31"), by = "day")) %>%
  mutate(
    month = month(date_day, label = TRUE, abbr = TRUE, locale = "en_US.UTF-8"),  # English month names
    weekday_num = wday(date_day, week_start = 7) - 1,           # Sunday-based weekdays (0-6)
    days_since_start = as.numeric(date_day - start_date),       # Days from calendar start
    week = floor(days_since_start / 7) + 1,                     # Week number calculation
    weekday = case_when(                                        # Convert numbers to names
      weekday_num == 0 ~ "Sunday",
      weekday_num == 1 ~ "Monday",
      weekday_num == 2 ~ "Tuesday",
      weekday_num == 3 ~ "Wednesday",
      weekday_num == 4 ~ "Thursday",
      weekday_num == 5 ~ "Friday",
      weekday_num == 6 ~ "Saturday"
    )
  )

# Merge datasets and add tooltip text
heatmap_data_full <- all_days %>%
  left_join(heatmap_data, by = "date_day") %>%
  mutate(
    n_artists = replace_na(n_artists, 0), # Fill missing days with 0
    weekday = factor(weekday, levels = c("Saturday", "Friday", "Thursday", "Wednesday", 
                                         "Tuesday", "Monday", "Sunday")),  # Fix weekday order
    # Tooltip text (only for days with activity - can be removed if not using interactivity)
    tooltip_text = ifelse(
      n_artists > 0,
      paste0("Date: ", date_day, "\nDay: ", weekday, "\nArtists: ", n_artists),
      NA_character_
    )
  )

# Create month labels
month_labels <- heatmap_data_full %>%
  group_by(month) %>%
  summarise(week = min(week)) %>%  # First week of each month
  ungroup()

# Create heatmap
p <- ggplot(heatmap_data_full, aes(x = week, y = weekday, fill = n_artists, text = tooltip_text)) +
  geom_tile(
    color = "white",                 # Grid line color
    size = 0.7,                      # Grid line thickness
    width = 0.95,                    # Tile width (95% of cell)
    height = 0.95                    # Tile height (95% of cell)
  ) +
  scale_fill_gradient(
    low = "#EBEDF0",                 
    high = "#0D6A21",                
    name = "Artists",         # Legend title
    breaks = function(limits) seq(0, ceiling(limits[2]), 1),  # Integer breaks
    limits = c(0, ceiling(max(heatmap_data_full$n_artists)))  # Color scale limits
  ) +
  scale_x_continuous(
    breaks = month_labels$week,      # Month label positions
    labels = month_labels$month,     # Month abbreviations
    expand = c(0, 0),                # Remove padding
    limits = c(min(heatmap_data_full$week) - 0.5, max(heatmap_data_full$week) + 0.5),
    position = "top"                 # Month labels at top
  ) +
  scale_y_discrete(
    labels = c("Saturday" = "", "Friday" = "Fri", "Thursday" = "", 
               "Wednesday" = "Wed", "Tuesday" = "", "Monday" = "Mon", "Sunday" = "")  # Short weekday labels
  ) +
  coord_fixed(ratio = 1) + # Square aspect ratio
  labs(
    title = "My 2020 Music Relationships: A Map Journey",
    subtitle = "Unique artists per day across 43 active days",  # Customizable subtitle
    x = NULL,
    y = NULL,
    caption = "Source: My Shazam data | Created by: @micosapiens711"  # Data attribution
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8, margin = margin(b = 10)),
    axis.text.y = element_text(size = 8, margin = margin(r = 5)),
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 5)),  
    plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray40", margin = margin(b = 10)),  
    plot.caption = element_text(size = 8, hjust = 1, margin = margin(t = 15)),  
    panel.grid = element_blank(), # Remove all grid lines
    legend.position = "right",
    legend.title = element_text(size = 8, face = "bold"),  
    panel.spacing = unit(0, "lines"), # Remove spacing between panels
    plot.margin = margin(10, 10, 10, 10),
    plot.background = element_rect(fill = "#F5F6F5", color = NA)  
  )

# Display static plot
p


# Save high-resolution static version
ggsave("complicated_25.png",plot=p, width = 10, height = 6, dpi = 600)

# Create interactive version with plotly
p_interactive <- ggplotly(p, tooltip = "text")

# Display interactive plot
p_interactive
