# EURO-CORDEX Climate Projections
# Clear workspace
rm(list = ls())
gc()

# Load required libraries
library(terra)
library(ggplot2)
library(patchwork)
library(RColorBrewer)
library(viridis)

# Set working directory
setwd("H:/EU-CORDEX/EU-CORDEX/comparison")

# Create output directory
if (!dir.exists("corrected_colors")) dir.create("corrected_colors")

# Define variables with CORRECTED color schemes
variables <- list(
  pr = list(
    name = "Precipitation",
    hist_unit = "mm/month",
    change_unit = "% change",
    hist_palette = "Blues",      # Blue gradient for historical
    hist_direction = 1,          # Lighter to darker blue
    change_palette = "BrBG",     # Brown-Blue-Green for changes
    change_direction = 1         # Brown (negative) to Blue (positive)
  ),
  rsds = list(
    name = "Solar Radiation",
    hist_unit = "W/m²",
    change_unit = "ΔW/m²",
    hist_palette = "YlOrRd",     # Yellow-Orange-Red for intensity
    hist_direction = 1,          # Lighter to darker
    change_palette = "RdYlBu",   # Red-Yellow-Blue for changes
    change_direction = -1        # Blue (negative) to Red (positive)
  ),
  tas = list(
    name = "Temperature",
    hist_unit = "°C",
    change_unit = "Δ°C",
    hist_palette = "RdBu",       # Red-Blue for temperature
    hist_direction = -1,         # Blue (cold) to Red (hot)
    change_palette = "RdBu",     # Red-Blue for changes
    change_direction = -1
  ),
  tasmax = list(
    name = "Maximum Temperature",
    hist_unit = "°C",
    change_unit = "Δ°C",
    hist_palette = "RdBu",
    hist_direction = -1,
    change_palette = "RdBu",
    change_direction = -1
  ),
  tasmin = list(
    name = "Minimum Temperature",
    hist_unit = "°C",
    change_unit = "Δ°C",
    hist_palette = "RdBu",
    hist_direction = -1,
    change_palette = "RdBu",
    change_direction = -1
  )
)

# Define scenarios
scenarios <- list(
  historical = list(
    code = "historical",
    label = "Historical Baseline\n(1991-2005)",
    time_period = "1991-2005"
  ),
  rcp26 = list(
    code = "rcp26",
    label = "RCP2.6\nRelative Change\n(2081-2100)",
    time_period = "2081-2100\n(Relative to 1991-2005)"
  ),
  rcp45 = list(
    code = "rcp45",
    label = "RCP4.5\nRelative Change\n(2081-2100)",
    time_period = "2081-2100\n(Relative to 1991-2005)"
  ),
  rcp85 = list(
    code = "rcp85",
    label = "RCP8.5\nRelative Change\n(2081-2100)",
    time_period = "2081-2100\n(Relative to 1991-2005)"
  )
)

# Function to load raster data
load_raster_data <- function(var, scen) {
  # Determine filename
  if (scen == "historical") {
    filename <- paste0(var, "_historical_baseline_1991_2005.tif")
  } else {
    if (var == "pr") {
      filename <- paste0(var, "_", scen, "_change_relative.tif")
    } else {
      filename <- paste0(var, "_", scen, "_change_absolute.tif")
    }
  }

  if (!file.exists(filename)) {
    cat("File not found:", filename, "\n")
    return(NULL)
  }

  # Load raster
  r <- rast(filename)

  # Flip for RCP4.5 and RCP8.5
  if (scen %in% c("rcp45", "rcp85")) {
    r <- flip(r, direction = "vertical")
  }

  # Convert to dataframe
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  colnames(df)[3] <- "value"

  if (nrow(df) == 0) {
    cat("No data\n")
    return(NULL)
  }

  # Convert precipitation relative change to percentage
  if (var == "pr" && scen != "historical" && grepl("change_relative", filename)) {
    df$value <- df$value * 100
  }

  return(df)
}

# Function to create a single plot with CORRECTED colors
create_corrected_plot <- function(var, scen, show_legend = FALSE) {
  var_info <- variables[[var]]
  scen_info <- scenarios[[scen]]

  # Load data
  df <- load_raster_data(var, scen)
  if (is.null(df) || nrow(df) == 0) {
    return(ggplot() +
             theme_void() +
             labs(title = scen_info$label) +
             theme(plot.background = element_rect(fill = "white", color = NA)))
  }

  # Determine if historical or change
  is_historical <- scen == "historical"

  if (is_historical) {
    # Historical: use absolute values
    if (var == "pr") {
      # Precipitation historical: Blues palette (darker blue = more precipitation)
      color_scale <- scale_fill_distiller(
        palette = var_info$hist_palette,
        direction = var_info$hist_direction,
        name = var_info$hist_unit,
        guide = if(show_legend) guide_colorbar(
          barwidth = unit(2, "cm"),
          barheight = unit(0.3, "cm"),
          title.position = "top",
          title.hjust = 0.5
        ) else "none"
      )
    } else if (var == "rsds") {
      # Solar radiation historical: YlOrRd palette
      color_scale <- scale_fill_distiller(
        palette = var_info$hist_palette,
        direction = var_info$hist_direction,
        name = var_info$hist_unit,
        guide = if(show_legend) guide_colorbar(
          barwidth = unit(2, "cm"),
          barheight = unit(0.3, "cm"),
          title.position = "top",
          title.hjust = 0.5
        ) else "none"
      )
    } else {
      # Temperature variables: RdBu palette
      color_scale <- scale_fill_distiller(
        palette = var_info$hist_palette,
        direction = var_info$hist_direction,
        name = var_info$hist_unit,
        guide = if(show_legend) guide_colorbar(
          barwidth = unit(2, "cm"),
          barheight = unit(0.3, "cm"),
          title.position = "top",
          title.hjust = 0.5
        ) else "none"
      )
    }
  } else {
    # Change maps: use diverging scale centered at 0
    if (var == "pr") {
      # Precipitation change: BrBG palette (Brown-Blue-Green)
      max_abs <- max(abs(df$value), na.rm = TRUE)
      color_scale <- scale_fill_distiller(
        palette = var_info$change_palette,
        limits = c(-max_abs, max_abs),
        direction = var_info$change_direction,
        name = var_info$change_unit,
        guide = if(show_legend) guide_colorbar(
          barwidth = unit(2, "cm"),
          barheight = unit(0.3, "cm"),
          title.position = "top",
          title.hjust = 0.5
        ) else "none"
      )
    } else if (var == "rsds") {
      # Solar radiation change: RdYlBu palette
      max_abs <- max(abs(df$value), na.rm = TRUE)
      color_scale <- scale_fill_distiller(
        palette = var_info$change_palette,
        limits = c(-max_abs, max_abs),
        direction = var_info$change_direction,
        name = var_info$change_unit,
        guide = if(show_legend) guide_colorbar(
          barwidth = unit(2, "cm"),
          barheight = unit(0.3, "cm"),
          title.position = "top",
          title.hjust = 0.5
        ) else "none"
      )
    } else {
      # Temperature changes: RdBu palette
      max_abs <- max(abs(df$value), na.rm = TRUE)
      color_scale <- scale_fill_distiller(
        palette = var_info$change_palette,
        limits = c(-max_abs, max_abs),
        direction = var_info$change_direction,
        name = var_info$change_unit,
        guide = if(show_legend) guide_colorbar(
          barwidth = unit(2, "cm"),
          barheight = unit(0.3, "cm"),
          title.position = "top",
          title.hjust = 0.5
        ) else "none"
      )
    }
  }

  # Create the plot with WHITE BACKGROUND
  p <- ggplot(df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    color_scale +
    labs(title = scen_info$label) +
    theme_minimal() +
    theme(
      plot.title = element_text(
        size = 8,
        hjust = 0.5,
        face = "bold",
        margin = margin(b = 3)
      ),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),  # White background
      plot.background = element_rect(fill = "white", color = NA),   # White background
      plot.margin = margin(2, 2, 2, 2),
      legend.position = if(show_legend) "bottom" else "none"
    )

  if (show_legend) {
    p <- p + theme(
      legend.position = "bottom",
      legend.key.width = unit(2, "cm"),
      legend.key.height = unit(0.3, "cm"),
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 7)
    )
  }

  return(p)
}

# Create all individual plots
cat("Creating plots with corrected color gradients...\n")
all_plots <- list()

for (var_code in names(variables)) {
  cat("\nVariable:", variables[[var_code]]$name, "\n")

  for (scen_code in names(scenarios)) {
    plot_name <- paste(var_code, scen_code, sep = "_")
    cat("  ", scenarios[[scen_code]]$label, "...")

    # Create plot WITHOUT legend (we'll add at bottom)
    all_plots[[plot_name]] <- create_corrected_plot(var_code, scen_code, show_legend = FALSE)
    cat(" ✓\n")
  }
}

# Create color bars for each variable
cat("\nCreating color bars for each variable...\n")

create_variable_colorbar <- function(var_code, for_change = FALSE) {
  var_info <- variables[[var_code]]

  # Create a dummy dataframe for the color bar
  if (for_change) {
    # For change maps: symmetric around 0
    df <- data.frame(
      x = seq(-5, 5, length.out = 100),
      y = 0
    )
    palette <- var_info$change_palette
    direction <- var_info$change_direction
    title <- paste(var_info$name, "- Change")
    label <- var_info$change_unit
  } else {
    # For historical maps: sequential
    df <- data.frame(
      x = seq(0, 10, length.out = 100),
      y = 0
    )
    palette <- var_info$hist_palette
    direction <- var_info$hist_direction
    title <- paste(var_info$name, "- Historical")
    label <- var_info$hist_unit
  }

  # Create color bar
  p <- ggplot(df, aes(x = x, y = y, fill = x)) +
    geom_tile() +
    scale_fill_distiller(
      palette = palette,
      direction = direction,
      name = label,
      guide = guide_colorbar(
        barwidth = unit(3, "cm"),
        barheight = unit(0.4, "cm"),
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    labs(title = title) +
    theme_void() +
    theme(
      plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(size = 8, face = "bold"),
      plot.background = element_rect(fill = "white", color = NA)
    )

  return(p)
}

# Create all color bars
color_bars <- list()
for (var_code in names(variables)) {
  color_bars[[paste0(var_code, "_hist")]] <- create_variable_colorbar(var_code, for_change = FALSE)
  color_bars[[paste0(var_code, "_change")]] <- create_variable_colorbar(var_code, for_change = TRUE)
}

# Create each row with its color bars
cat("\nAssembling rows with color bars...\n")

create_variable_row <- function(var_code) {
  var_info <- variables[[var_code]]

  # Get the 4 plots for this variable
  historical_plot <- all_plots[[paste(var_code, "historical", sep = "_")]]
  rcp26_plot <- all_plots[[paste(var_code, "rcp26", sep = "_")]]
  rcp45_plot <- all_plots[[paste(var_code, "rcp45", sep = "_")]]
  rcp85_plot <- all_plots[[paste(var_code, "rcp85", sep = "_")]]

  # Create the 4-plot row
  plot_row <- (historical_plot + rcp26_plot + rcp45_plot + rcp85_plot) +
    plot_layout(nrow = 1)

  # Add historical color bar at bottom
  row_with_colorbar <- plot_row / color_bars[[paste0(var_code, "_hist")]] /
    color_bars[[paste0(var_code, "_change")]] +
    plot_layout(heights = c(4, 0.5, 0.5))

  # Add variable label on the left
  variable_label <- ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = var_info$name,
             angle = 90,
             size = 4.5,
             fontface = "bold",
             color = "black") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA)
    )

  # Combine label with the row
  (variable_label | row_with_colorbar) +
    plot_layout(widths = c(0.08, 1))
}

# Create all rows
enhanced_rows <- list()
for (var_code in names(variables)) {
  cat("  Row:", variables[[var_code]]$name, "\n")
  enhanced_rows[[var_code]] <- create_variable_row(var_code)
}

# Create scenario header
scenario_header <- ggplot() +
  annotate("text", x = 1, y = 0.7,
           label = "Historical Baseline\n(1991-2005)",
           size = 4, fontface = "bold", color = "black") +
  annotate("text", x = 2, y = 0.7,
           label = "RCP2.6\nRelative Change\n(2081-2100)",
           size = 4, fontface = "bold", color = "black") +
  annotate("text", x = 3, y = 0.7,
           label = "RCP4.5\nRelative Change\n(2081-2100)",
           size = 4, fontface = "bold", color = "black") +
  annotate("text", x = 4, y = 0.7,
           label = "RCP8.5\nRelative Change\n(2081-2100)",
           size = 4, fontface = "bold", color = "black") +
  xlim(0.5, 4.5) +
  ylim(0, 1) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )

# Combine everything
final_grid <- scenario_header
for (var_code in names(variables)) {
  final_grid <- final_grid / enhanced_rows[[var_code]]
}

# Add main title
final_plot <- final_grid +
  plot_layout(heights = c(0.1, rep(1, length(variables)))) +
  plot_annotation(
    title = "EURO-CORDEX Climate Projections for Europe",
    subtitle = "Historical Baseline (1991-2005) and Future Changes (2081-2100) under Different RCP Scenarios",
    caption = "Data: EURO-CORDEX | Precipitation: Blue gradient (darker = more precipitation) | All plots with white background",
    theme = theme(
      plot.title = element_text(
        size = 18,
        face = "bold",
        hjust = 0.5,
        color = "black",
        margin = margin(b = 10, t = 10)
      ),
      plot.subtitle = element_text(
        size = 12,
        hjust = 0.5,
        color = "gray40",
        margin = margin(b = 20)
      ),
      plot.caption = element_text(
        size = 10,
        hjust = 0.5,
        color = "gray60",
        margin = margin(t = 20)
      ),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

# Save the final plot
cat("\nSaving final plot...\n")
ggsave(
  "corrected_colors/EURO_CORDEX_CORRECTED_COLORS.png",
  plot = final_plot,
  width = 16,
  height = 22,
  dpi = 300,
  bg = "white"
)

cat("✅ Final plot saved: corrected_colors/EURO_CORDEX_CORRECTED_COLORS.png\n")

# Also save individual rows
for (var_code in names(variables)) {
  ggsave(
    paste0("corrected_colors/", var_code, "_CORRECTED_ROW.png"),
    plot = enhanced_rows[[var_code]],
    width = 16,
    height = 5,
    dpi = 300,
    bg = "white"
  )
}
