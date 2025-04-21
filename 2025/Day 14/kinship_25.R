# Load the required libraries
library(smacof)    # For MDS
library(ggplot2)   # For visualization
library(ggrepel)   # For geom_text_repel()

# Load the Ekman dataset
data(ekman)

# Convert to a similarity matrix
sim_matrix <- as.matrix(ekman)

# The colors are labeled by their wavelength (in nm)
colors <- attr(ekman, "Labels")
print(colors)

# Convert the similarity matrix to dissimilarities (MDS works with dissimilarities)
diss <- 1 - sim_matrix

# Perform the MDS analysis (multidimensional scaling)
# We use smacofSym for a classical (metric) MDS in 2 dimensions
mds_result <- smacofSym(diss, ndim = 2, type = "ratio")

# Display the basic details of the MDS analysis
mds_result

# Extract the 2D coordinates of the points (colors)
coords <- as.data.frame(mds_result$conf)
colnames(coords) <- c("x", "y")

# Add the labels (wavelengths) and convert them to numeric for the color gradient
coords$wavelength <- colors
coords$wavelength_num <- as.numeric(colors)

# Visualize the MDS result with ggplot2
p <- ggplot(coords, aes(x = x, y = y)) +
  # Points (nodes) colored according to the wavelength
  geom_point(aes(color = wavelength_num), size = 5, shape = 16) +
  # Color gradient adjusted to the specific wavelengths
  scale_color_gradientn(
    colors = c("purple", "blue", "turquoise", "green", "yellow", "red"),
    values = scales::rescale(c(434, 465, 490, 504, 584, 674), to = c(0, 1)),
    name = "Wavelength (nm)"
  ) +
  # Labels for the points (wavelengths) using geom_text_repel, without connecting lines
  geom_text_repel(aes(label = wavelength), size = 4, 
                  segment.size = 0, segment.color = NA, segment.alpha = 0, 
                  max.overlaps = Inf, force = 1, direction = "both") +
  # Add the stress value in the top right corner inside the square
  annotate("text", x = 0.9, y = 0.9, label = "Stress-1 = 0.131", size = 4, hjust = 1) +
  # Add D1 and D2 axes (MDS dimensions)
  labs(x = "D1", y = "D2") +
  # Define the axis ticks (scale: -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6)
  scale_x_continuous(breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-1, 1)) +
  scale_y_continuous(breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6), limits = c(-1, 1)) +
  # Expand the limits to ensure labels are visible
  expand_limits(y = c(-1, 1), x = c(-1, 1)) +
  # Theme with white background and black border
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_rect(fill = "white", colour = NA),  # White background
        panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Black border
        axis.title = element_text(size = 12),  # Size of axis labels
        axis.text = element_text(size = 10),   # Size of axis ticks
        plot.caption = element_text(hjust = 1),  # Align caption to the right
        plot.title = element_text(hjust = 0.5),  # Center the title
        plot.subtitle = element_text(hjust = 0.5),  # Center the subtitle
        legend.position = "right") +  # Move the legend to the right
  labs(title = "Perceptual Similarity of Colors via MDS",
       subtitle = "*Color gradient mimics the visible spectrum",
       caption = "Source: Ekman Dataset (1954) | Created by: @micosapiens711") +
  # Ensure the plot is a perfect square with equal scales
  coord_fixed(ratio = 1, xlim = c(-1, 1), ylim = c(-1, 1), expand = FALSE)

p

# Save the plot
ggsave("kinship_25.png", plot = p, width = 8, height = 8, dpi = 300)
