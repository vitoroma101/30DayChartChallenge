# Load required libraries

library(readxl)
library(dplyr)
library(ggplot2)
library(ggforce)  # For creating shaded circles
library(lubridate)  # For handling dates

# Since the file is large, it is not displayed directly on GitHub. You should first download the file from the provided URL (https://github.com/vitoroma101/30DayChartChallenge/blob/bfbd398d93cb8c5a4709e7357db67e5fd7eb54e6/2025/mdi_personasdesaparecidas_pm_2017-2024.xlsx) and then replace the path with its location on your PC.

# Load data
data <- read_excel("E:\\Documents\\30day\\25\\mdi_personasdesaparecidas_pm_2017-2024.xlsx")

# Filter 2024 data and calculate fractions of under-18 disappearances by province
data_2024 <- data %>%
  filter(year(`Fecha Desaparición`) == 2024) %>%  # Filter for 2024 only
  mutate(Under_18 = `Edad Aprox.` < 18) %>%  # Create a column to identify under-18 individuals
  group_by(Provincia, Under_18) %>%
  summarise(Count = n()) %>%  # Count disappearances by province and age group
  ungroup() %>%
  group_by(Provincia) %>%
  mutate(Total = sum(Count),  # Total disappearances per province
         Fraction = Count / Total,  # Fraction of under-18 disappearances per province
         Percentage = Fraction * 100) %>%  # Calculate percentage
  ungroup() %>%
  filter(Under_18 == TRUE) %>%  # Filter for under-18 only
  select(Provincia, Fraction, Percentage, Count, Total)

# Convert province names to title case (first letter uppercase, rest lowercase)
data_2024 <- data_2024 %>%
  mutate(Provincia = tools::toTitleCase(tolower(Provincia)))

# Sort provinces by Fraction in descending order, assign positions, and calculate radius
data_2024 <- data_2024 %>%
  arrange(desc(Fraction)) %>%  # Sort by fraction in descending order
  mutate(
    row = rep(6:1, each = 4)[1:24],  # Rows: 6 (top) to 1 (bottom)
    col = rep(1:4, times = 6)[1:24],  # Columns: 1 (left) to 4 (right)
    r = 0.2 * sqrt(Total / mean(Total))  # Calculate radius explicitly
  )

# Split Data for Full and Partial Circles
data_partial <- data_2024 %>% filter(Fraction < 1)  # Provinces with Fraction < 1
data_full <- data_2024 %>% filter(Fraction == 1)    # Provinces with Fraction == 1 (e.g., Galápagos)

# Create the visualization with shaded circles
p <- ggplot(data_2024) +
  # Background full circle for all provinces, using the calculated radius
  geom_arc_bar(aes(x0 = col, y0 = row, r0 = 0, r = r, start = 0, end = 2 * pi),
               fill = "#2A3A4A", color = "black", size = 0.5, alpha = 0.3) +
  # Shaded circle for provinces with Fraction < 1, using geom_arc_bar
  geom_arc_bar(data = data_partial, 
               aes(x0 = col, y0 = row, r0 = 0, r = r, start = 0, end = 2 * pi * Fraction, fill = Fraction),
               color = "black", size = 0.5) +
  # Full circle for provinces with Fraction == 1, using geom_circle
  geom_circle(data = data_full, 
              aes(x0 = col, y0 = row, r = r, fill = Fraction),
              color = "black", size = 0.5) +
  # Labels with province names only (no percentages), positioned above the circles
  geom_text(aes(x = col, y = row + 0.3, label = Provincia),
            size = 2.5, color = "white", lineheight = 0.8) +  # Move labels above the circle
  # Color scale for shading
  scale_fill_gradient(low = "gray50", high = "yellow", name = "Fraction", labels = scales::percent) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(
      color = "white", 
      size = 16, 
      hjust = 0, 
      face = "bold",
      margin = margin(t = 10, b = 10)  # Space around the title (top: 10 points, bottom: 10 points)
    ),
    plot.subtitle = element_text(color = "white", size = 10, hjust = 0),
    plot.caption = element_text(color = "white", size = 8, hjust = 0.5, 
                                margin = margin(t = 10, b = 0, unit = "pt")),  # Adjust caption margin
    plot.margin = margin(t = 10, r = 10, b = 20, l = 10, unit = "pt"),  # Add bottom margin to the plot
    legend.position = "none"
  ) +
  labs(
    title = "Disappearances of Minors Under 18 in Ecuador 2024",
    subtitle = "Fraction of under-18 disappearances by province from highest to lowest.\n(Circle size proportional to the total number of cases).\n\nOn average, the proportion was 58.9%, ranging from 100% in Galápagos to 44.7% in\nGuayas, with the top row above 70% and the bottom four below 50%.",
    caption = "Source: Datos Abiertos Ecuador | Created by: @micosapiens711"
  ) +
  coord_fixed(xlim = c(0.5, 4.5), ylim = c(0.5, 6.5))

# Display the plot
p

# Save the plot
ggsave("fractions_25.png", plot = p, width = 8, height = 10, dpi = 300)

# Calculate the average and range of Fraction
fraction_avg <- mean(data_2024$Fraction) * 100  # Average in percentage
fraction_range <- range(data_2024$Fraction) * 100  # Range (minimum and maximum) in percentage
