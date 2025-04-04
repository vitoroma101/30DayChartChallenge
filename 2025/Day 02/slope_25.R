# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(ggrepel)

# Since the file is large, it is not displayed directly on GitHub. You should first download the file from the provided URL (https://github.com/vitoroma101/30DayChartChallenge/blob/main/2025/BVG_Acciones.xlsx) and then replace the path with its location on your PC.

# Load and process the data
data <- read_excel("E:\\Downloads\\BVG_Acciones.xlsx", skip = 1)

# 1. Filter data for 2019 and 2024
data <- data %>%
  mutate(Year = year(`FECHA NEGOCIACIÓN`)) %>%
  filter(Year %in% c(2019, 2024)) %>%
  filter(`FECHA NEGOCIACIÓN` <= as.POSIXct("2024-12-31 23:59:59"))

# 2. Identify companies with the highest transaction volume (total for 2019 and 2024)
companies_volume <- data %>%
  group_by(EMISOR) %>%
  summarise(Total_Volume = sum(`NÚMERO DE ACCIONES`, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Total_Volume))

# 3. Filter companies with a maximum price in 2019 under $5 and select the top 7 based on highest transaction volume
top_companies <- data %>%
  filter(Year == 2019) %>%  # Consider only 2019 for the maximum price
  group_by(EMISOR) %>%
  summarise(Max_2019 = max(PRECIO, na.rm = TRUE), .groups = "drop") %>%
  filter(Max_2019 < 5) %>%  # Filter for maximum price in 2019 < $5
  inner_join(companies_volume, by = "EMISOR") %>%  # Join with the volume data
  arrange(desc(Total_Volume)) %>%  # Sort by total volume
  slice_head(n = 7) %>%  # Select the top 7
  pull(EMISOR)

# 4. Filter the data to include only the selected top 7 companies
top_data <- data %>%
  filter(EMISOR %in% top_companies)

# 5. Select the highest recorded price per company and year (2019 and 2024)
slope_data <- top_data %>%
  group_by(EMISOR, Year) %>%
  summarise(Max_Price = max(PRECIO, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Year, values_from = Max_Price, names_prefix = "Price_") %>%
  # Filter companies that have data for both years (avoid NA)
  filter(!is.na(Price_2019) & !is.na(Price_2024)) %>%
  mutate(Trend = if_else(Price_2024 > Price_2019, "Increases", "Decreases"),
         Percent_Change = ((Price_2024 - Price_2019) / Price_2019) * 100)

# 6. Truncate company names to avoid overlap (maximum 20 characters)
# Note: If you want full names in the legend, you can skip this step or use EMISOR directly
slope_data <- slope_data %>%
  mutate(EMISOR_Short = stringr::str_trunc(EMISOR, width = 20, side = "right"))

# Calculate Y-axis limits dynamically
y_min <- min(c(slope_data$Price_2019, slope_data$Price_2024), na.rm = TRUE)
y_max <- max(c(slope_data$Price_2019, slope_data$Price_2024), na.rm = TRUE)
y_limits <- c(max(0, y_min * 0.9), y_max * 1.1)  # Adjust the range with a margin

# Create the slope chart
p <- ggplot(slope_data) +
  geom_segment(aes(x = 1, xend = 1.2, y = Price_2019, yend = Price_2024, 
                   group = EMISOR, color = EMISOR), 
               linewidth = 0.8, alpha = 0.9) +
  geom_point(aes(x = 1, y = Price_2019, color = EMISOR), size = 5) +
  geom_point(aes(x = 1.2, y = Price_2024, color = EMISOR), size = 5) +
  geom_text_repel(aes(x = 1, y = Price_2019, 
                      label = paste0("$", round(Price_2019, 2))), 
                  hjust = 1, nudge_x = -0.02, size = 4, direction = "y", 
                  segment.size = 0) +
  geom_text_repel(aes(x = 1.2, y = Price_2024, 
                      label = paste0("$", round(Price_2024, 2))), 
                  hjust = 0, nudge_x = 0.02, size = 4, direction = "y", 
                  segment.size = 0) +
  scale_x_continuous(breaks = c(1, 1.2), labels = c("2019", "2024")) +
  scale_y_continuous(
    name = "Highest Recorded Price (USD)",
    limits = y_limits,
    breaks = seq(0, ceiling(y_max * 1.1), by = 0.5),
    sec.axis = dup_axis(name = NULL, labels = NULL)  # No labels on the right Y-axis
  ) +
  # MAIN CHANGE: Color palette for 7 companies
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",
                                "#66A61E", "#FF7F00", "#6A3D9A")) + # 2 colors added
  labs(
    title = "Top Affordable Stocks in Ecuador: Comparing 2019 vs 2024",
    subtitle = "7 Companies selected by overall transaction volume with peak prices under $5 pre-pandemic",
    x = NULL,
    color = NULL,
    caption = "Source: Bolsa de Valores de Guayaquil (BVG) | Created by: @micosapiens711"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.title.y.left = element_text(size = 12, face = "bold"),
    axis.line.y = element_line(color = "black", linewidth = 0.5),  # Vertical lines for both Y-axes
    # Optional adjustments for better visualization
    legend.text = element_text(size = 9),  # Reduce legend text size if needed
    legend.key.size = unit(0.4, "cm"),     # Adjust legend spacing
    plot.margin = margin(t = 15, r = 25, b = 15, l = 15, unit = "pt"), # Add more margin
    plot.caption = element_text(hjust = 0, size = 10),  # Align caption to the left
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),  # Align title to the left
    plot.subtitle = element_text(hjust = 0, size = 12)  # Align subtitle to the left
  )

# Display the plot
p

# Save the plot with a wider width to accommodate the legend
ggsave("slope_25.png", plot = p, width = 10, height = 8, dpi = 300)