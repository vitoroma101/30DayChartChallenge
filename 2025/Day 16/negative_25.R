# Load libraries
library(tidyverse)

# Load datasets
activity <- read.csv("E:\\Documents\\30day\\25\\ZEPP\\ACTIVITY_stdirdcal\\ACTIVITY_1744082349491.csv")
sleep <- read.csv("E:\\Documents\\30day\\25\\ZEPP\\SLEEP\\SLEEP_1744082350735.csv")

# Combine datasets by the 'date' column
combined_data <- inner_join(activity, sleep, by = "date")

# --- EDA: Filter Decision Analysis ---
# Statistical summary of shallowSleepTime in combined_data
# Finding: Median and Q1 are 0, indicating many days without shallow sleep; Q3 is 331.25, suggesting non-zero values are high, prompting exploration of intermediate ranges.
shallow_summary <- combined_data %>%
  summarise(
    min_shallow = min(shallowSleepTime),
    max_shallow = max(shallowSleepTime),
    mean_shallow = mean(shallowSleepTime),
    median_shallow = median(shallowSleepTime),
    q1_shallow = quantile(shallowSleepTime, 0.25),
    q3_shallow = quantile(shallowSleepTime, 0.75)
  )
print(shallow_summary)

# Count days with low shallowSleepTime
# Threshold: 120 minutes was chosen due to a jump in the distribution (297 days with 0, 17 days between 1-120, then values > 120); 120 minutes (2 hours) is a reasonable limit for "low" shallow sleep.
# Finding: 297 out of 528 days have 0 minutes; a > 0 filter removes 56% of the data but ensures valid data.
shallow_low_counts <- combined_data %>%
  summarise(
    count_zero = sum(shallowSleepTime == 0),
    count_leq_5 = sum(shallowSleepTime <= 5),
    count_leq_30 = sum(shallowSleepTime <= 30),
    count_leq_60 = sum(shallowSleepTime <= 60),
    count_leq_120 = sum(shallowSleepTime <= 120)
  )
print(shallow_low_counts)

# Show details of days with shallowSleepTime <= 120
# Finding: 17 days have between 1-120 minutes, suggesting they are not errors and the > 0 filter is sufficient.
shallow_low_details <- combined_data %>%
  filter(shallowSleepTime <= 120) %>%
  select(date, shallowSleepTime, steps)
print(shallow_low_details)

# Histogram to visualize the distribution of shallowSleepTime
# Finding: The distribution has a massive peak at 0 (297 days), reinforcing that the > 0 filter is necessary.
ggplot(combined_data, aes(x = shallowSleepTime)) +
  geom_histogram(binwidth = 30, fill = "#4682B4", color = "black") +
  labs(title = "Distribution of Shallow Sleep",
       x = "Shallow Sleep (minutes)",
       y = "Frequency",
       caption = "Source: My Zepp Data | Created by: @micosapiens711") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(color = "gray50"),
    panel.grid.minor = element_blank()
  )

# Filter shallowSleepTime > 0 and create new dataset
# Finding: Reduces the dataset from 528 to 231 days.
newdata <- combined_data %>%
  filter(shallowSleepTime > 0)

# Statistical summary of steps
# Finding: 2 days have ≤ 5 steps, suggesting a > 5 filter to remove outliers.
steps_summary <- newdata %>%
  summarise(
    min_steps = min(steps),
    max_steps = max(steps),
    mean_steps = mean(steps),
    median_steps = median(steps),
    q1_steps = quantile(steps, 0.25),
    q3_steps = quantile(steps, 0.75)
  )
print(steps_summary)

# Count days with low steps
# Threshold: 5 steps is a very low threshold (only 2 days ≤ 5), indicating days without device usage.
# Finding: 2 out of 231 days have ≤ 5 steps, suggesting a > 5 filter to remove outliers.
steps_low_counts <- newdata %>%
  summarise(
    count_zero = sum(steps == 0),
    count_leq_5 = sum(steps <= 5),
    count_leq_100 = sum(steps <= 100)
  )
print(steps_low_counts)

# Show details of days with steps <= 5
# Finding: Only 2 days have ≤ 5 steps (2022-10-12: 5 steps, 2024-01-25: 0 steps), confirming they are outliers and the > 5 filter is appropriate.
steps_low_details <- newdata %>%
  filter(steps <= 5) %>%
  select(date, steps, shallowSleepTime)
print(steps_low_details)

# Histogram to visualize the distribution of steps
# Finding: The distribution shows few days near 0 (only 2 ≤ 5 steps); the > 5 filter leaves 229 days.
ggplot(newdata, aes(x = steps)) +
  geom_histogram(binwidth = 1000, fill = "#4682B4", color = "black") +
  labs(title = "Distribution of Daily Steps",
       x = "Steps",
       y = "Frequency",
       caption = "Source: My Zepp Data | Created by: @micosapiens711") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(color = "gray50"),
    panel.grid.minor = element_blank()
  )

# ------------------------------------

# Filter days with valid shallowSleepTime and steps values
valid_data <- combined_data %>% 
  filter(shallowSleepTime > 0, steps > 5)

# Calculate correlation between shallowSleepTime and steps
correlation <- cor.test(valid_data$shallowSleepTime, valid_data$steps, method = "pearson")
print(correlation)

# Enhanced scatter plot for shallowSleepTime vs. steps
p <- ggplot(valid_data, aes(x = shallowSleepTime, y = steps)) +
  geom_point(color = "#4682B4", alpha = 0.5, size = 2) +  # Blue semi-transparent points
  geom_smooth(method = "lm", formula = y ~ x, color = "#4B4B4B", se = FALSE, size = 1.2) +  # Dark gray trend line
  # Highlight extreme days (e.g., shallowSleepTime > 600 or steps < 3000)
  geom_point(data = valid_data[valid_data$shallowSleepTime > 600 | valid_data$steps < 3000, ],
             aes(x = shallowSleepTime, y = steps), color = "#800020", size = 3, shape = 21, fill = "#800020") +
  # Reference lines for normal averages
  geom_vline(xintercept = 300, linetype = "dashed", color = "#808080", size = 0.5) +  # Normal average for shallow sleep (~300 min)
  geom_hline(yintercept = 10000, linetype = "dashed", color = "#808080", size = 0.5) +  # Approximate average steps
  # Add correlation and significance as an annotation inside the plot
  annotate("text", x = 600, y = 22000, 
           label = "Correlation: -0.31 (p < 0.001)", 
           color = "black", size = 4, hjust = 1, vjust = 1) +
  # Labels and styling (in English)
  labs(title = "Exploring Sleep and Steps: A Relationship Across 2022-2025",
       subtitle = "Reference Lines: Shallow Sleep = 300, Steps = 10,000 | Extremes: Shallow Sleep > 600 or Steps < 3,000 | 229 Observations",
       x = "Shallow Sleep (minutes)",
       y = "Daily Steps",
       caption = "Source: Amazfit GTS 2 via Zepp app | Created by: @micosapiens711") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(hjust = 0, color = "gray40"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(color = "gray50"),
    plot.background = element_rect(fill = "#F5F5F5", color = NA),  # Light gray background
    panel.background = element_rect(fill = "#F5F5F5", color = NA)   # Same background for the panel
  )

p

# Save the plot
ggsave("negative_25.png", plot = p, width = 11, height = 7, dpi = 300)