# ---------------------------
# Load libraries
# ---------------------------
library(tidyverse)
library(factoextra)  # For clustering visualization
library(ggplot2)
library(ggrepel)     # For non-overlapping labels

# ---------------------------
# Import and clean data
# ---------------------------

# Download the dataset from Kaggle: "Bayer Leverkusen 2023/2024 Squad" by keremkarayaz

# Load data 
data <- read.csv(
  "E:\\Documents\\30day\\25\\Leverkusen\\Bayer_Leverkusen_Squad.csv",
  sep = ";",
  header = TRUE,
  stringsAsFactors = FALSE
)

# Fix encoding for player names (e.g., accents, special characters)
data$Name <- iconv(data$Name, from = "ISO-8859-1", to = "UTF-8")

# Manually correct the problematic names
data$Name[data$Name == "Adam Hlo?ek"] <- "Adam Hložek"
data$Name[data$Name == "Luká\u009a Hrádeck?"] <- "Lukáš Hrádecký"
data$Name[data$Name == "Josip Stani\u009ai?"] <- "Josip Stanišić"

# Verify the corrected names
print(table(data$Name))

# Filter players with actual playing time
data_filtered <- data[data$Minutes > 0, ]

# ---------------------------
# Prepare variables for clustering
# ---------------------------

# Select key metrics reflecting player contribution
vars <- c("Minutes", "StartXI", "Age", "Yellow_Card")
data_cluster <- data_filtered[, vars]

# Standardize data to equalize variable scales
data_scaled <- scale(data_cluster)

# ---------------------------
# Determine optimal clusters
# ---------------------------

# Elbow method (look for the "bend" in the curve)
fviz_nbclust(data_scaled, kmeans, method = "wss") + 
  geom_vline(xintercept = 3, linetype = 2, color = "red")  

# Silhouette method (higher values = better separation)
fviz_nbclust(data_scaled, kmeans, method = "silhouette")

# ---------------------------
# Perform PCA to interpret dimensions
# ---------------------------

# PCA helps reduce dimensionality and understand variable contributions
pca_result <- prcomp(data_scaled, scale. = FALSE)  # Data already scaled
loadings <- pca_result$rotation

# Print variable contributions to PC1 and PC2 (used for axis labels)
print(loadings[, 1:2])

# ---------------------------
# K-means clustering (k=3)
# ---------------------------

set.seed(123)  # For reproducibility
km_result <- kmeans(data_scaled, centers = 3, nstart = 25)
data_filtered$Cluster <- as.factor(km_result$cluster)

# ---------------------------
# Visualize clusters
# ---------------------------

# Use Bayer Leverkusen's official colors for branding
club_colors <- c("#E32221", "#1A171B", "#F3E500")  # Red, Black, Yellow

# Base cluster plot
p <- fviz_cluster(
  km_result,
  data = data_scaled,
  geom = "point",
  ellipse.type = "norm",
  ellipse.level = 0.7,
  ggtheme = theme_minimal(),
  palette = club_colors,
  show.clust.cent = TRUE
) 

# Add labels, highlight Hincapié, and styling
final_plot <- p + 
  geom_text_repel(
    aes(label = data_filtered$Name),
    size = 3,
    force = 0.5,
    max.overlaps = 20,
    box.padding = 0.4,
    segment.color = "grey50",
    color = "#333333"
  ) +
  # Highlight Hincapié with a larger point and a different color.
  
    geom_point(
    data = data_filtered %>% filter(grepl("Hincapié", Name, ignore.case = TRUE)),
    aes(x = pca_result$x[rownames(data_filtered)[grepl("Hincapié", data_filtered$Name, ignore.case = TRUE)], 1], 
        y = pca_result$x[rownames(data_filtered)[grepl("Hincapié", data_filtered$Name, ignore.case = TRUE)], 2]),
    color = "#2ECC71", 
    size = 5, 
    shape = 18) +
  labs(
    title = "Player Contribution Clusters: Bayer 04 Leverkusen 2023-2024 Championship Season",
    subtitle = "Using 3 Groups to Gain In-depth Understanding",  
    caption = "Source: Bayer Leverkusen 2023/2024 Squad Dataset by keremkarayaz on Kaggle\nCreated by: @micosapiens711",
    x = "Dimension 1: Participation (70.3% variance)",  # Directly from PCA
    y = "Dimension 2: Age & Discipline (29.7% variance)" 
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "#E32221", size = 16),
    plot.subtitle = element_text(hjust = 0.5, color = "#333333", size = 12),  # Non-redundant
    plot.caption = element_text(color = "#666666", size = 8, hjust = 1),
    axis.title = element_text(color = "#333333", size = 10),
  )

final_plot

ggsave("clusters_25.png", plot = final_plot, width = 10, height = 6, dpi = 300)
