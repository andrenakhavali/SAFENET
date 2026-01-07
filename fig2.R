# Load required libraries
library(ggplot2)
library(dplyr)
library(patchwork)

# Create a clean, minimal theme
theme_clean <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16,
                                margin = margin(b = 10)),
      plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40"),
      axis.title = element_text(size = 10, face = "bold"),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(20, 20, 20, 20),
      legend.position = "none"
    )
}



# Create a minimalist version
minimalist_plot <- ggplot() +
  # Draw the 2x2 grid
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 0), color = "black", linewidth = 1) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1), color = "black", linewidth = 1) +
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 1), color = "black", linewidth = 1) +
  geom_segment(aes(x = 0, xend = 1, y = 1, yend = 1), color = "black", linewidth = 1) +
  geom_segment(aes(x = 0.5, xend = 0.5, y = 0, yend = 1), color = "gray80", linewidth = 0.5) +
  geom_segment(aes(x = 0, xend = 1, y = 0.5, yend = 0.5), color = "gray80", linewidth = 0.5) +

  # Add SSP labels
  annotate("text", x = 0.25, y = 0.75, label = "SSP5",
           size = 6, fontface = "bold", color = "#2E8B57") +
  annotate("text", x = 0.25, y = 0.65, label = "CONVENTIONAL\nDEVELOPMENT",
           size = 4, color = "#2E8B57") +

  annotate("text", x = 0.75, y = 0.75, label = "SSP3",
           size = 5, color = "gray60") +
  annotate("text", x = 0.75, y = 0.65, label = "FRAGMENTATION",
           size = 3.5, color = "gray60") +

  annotate("text", x = 0.25, y = 0.25, label = "SSP1",
           size = 6, fontface = "bold", color = "#4B0082") +
  annotate("text", x = 0.25, y = 0.15, label = "SUSTAINABILITY",
           size = 4, color = "#4B0082", lineheight = 0.9) +

  annotate("text", x = 0.75, y = 0.25, label = "SSP4",
           size = 5, color = "gray60") +
  annotate("text", x = 0.75, y = 0.15, label = "INEQUALITY",
           size = 3.5, color = "gray60") +

  annotate("text", x = 0.5, y = 0.5, label = "SSP2",
           size = 6, fontface = "bold", color = "#4682B4") +
  annotate("text", x = 0.5, y = 0.4, label = "CONTINUATION",
           size = 4, color = "#4682B4") +

  # Add axes with arrows
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 0),
               color = "black", linewidth = 1.2,
               arrow = arrow(type = "closed", length = unit(0.25, "cm"))) +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1),
               color = "black", linewidth = 1.2,
               arrow = arrow(type = "closed", length = unit(0.25, "cm"))) +

  # Axis labels
  annotate("text", x = 0.5, y = -0.07,
           label = "Increasing adaptation challenges",
           size = 4.5, fontface = "bold") +
  annotate("text", x = -0.07, y = 0.5,
           label = "Increasing mitigation challenges",
           size = 4.5, fontface = "bold", angle = 90) +

  # Citation
  annotate("text", x = 0.95, y = -0.05,
           label = "Adopted from: O'Neill et al. 2014",
           size = 3.0, fontface = "italic", color = "gray50") +

  # Scale
  scale_x_continuous(limits = c(-0.1, 1.1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.1, 1.1), expand = c(0, 0)) +
  coord_fixed(ratio = 1) +

  # Title
  labs(
    title = "SSP Framework Visualization",
  ) +

  # Theme
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18,
                              margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "gray40",
                                 margin = margin(b = 20)),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Display minimalist version
print(minimalist_plot)
ggsave("ssp_framework_minimalist.png", plot = minimalist_plot,
       width = 10, height = 9, dpi = 300, bg = "white")
