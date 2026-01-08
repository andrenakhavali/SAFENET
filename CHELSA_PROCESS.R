# Load required packages
library(tidyverse)
library(data.table)
library(ncdf4)
library(ggplot2)
library(patchwork)
library(viridis)
library(gridExtra)
library(ggridges)
library(ggh4x)
library(scales)
library(lubridate)

# Set paths and variables
base_path <- "H:/CHELSA_EU/"
variables <- c("pr", "tas", "tasmin", "tasmax")
variable_names <- c("Precipitation", "Mean Temperature", "Min Temperature", "Max Temperature")
units <- c("mm/day", "°C", "°C", "°C")

# Function to efficiently process netCDF files WITHOUT parallel processing
process_variable_fast <- function(var, base_path) {
  cat("Processing", var, "...\n")

  # Construct the full path
  var_path <- file.path(base_path, var)

  # List all files for the variable
  files <- list.files(
    path = var_path,
    pattern = paste0("chelsa_EU_", var, "_300arcsec_daily.*\\.nc$"),
    full.names = TRUE
  )

  if (length(files) == 0) {
    stop(paste("No files found for variable:", var, "at path:", var_path))
  }

  # Extract years from filenames
  years <- as.numeric(str_extract(files, "(?<=daily)\\d{4}(?=\\d{2}\\.nc$)"))

  # Function to extract spatial mean from single file
  extract_spatial_mean <- function(file) {
    nc <- nc_open(file)

    # Check if variable exists in the file
    if (!(var %in% names(nc$var))) {
      # Try to find the variable name
      var_names <- names(nc$var)
      if (length(var_names) > 0) {
        # Use the first variable if the expected one doesn't exist
        actual_var <- var_names[1]
        cat(paste("Using variable", actual_var, "instead of", var, "\n"))
        data_array <- ncvar_get(nc, actual_var)
      } else {
        nc_close(nc)
        return(NA_real_)
      }
    } else {
      data_array <- ncvar_get(nc, var)
    }

    # Calculate spatial mean (ignoring NAs)
    spatial_mean <- mean(data_array, na.rm = TRUE)

    nc_close(nc)
    return(spatial_mean)
  }

  # Process files sequentially (faster for I/O bound operations)
  spatial_means <- sapply(files, extract_spatial_mean)

  # Remove any NA values
  valid_idx <- !is.na(spatial_means)
  spatial_means <- spatial_means[valid_idx]
  years <- years[valid_idx]

  if (length(spatial_means) == 0) {
    return(data.table(year = numeric(), value = numeric(), variable = character()))
  }

  # Convert to annual means (for temperature) or annual totals (for precipitation)
  if (var == "pr") {
    # For precipitation: convert from kg m-2 s-1 to mm/day
    # 1 kg/m²/s = 86400 mm/day
    annual_values <- spatial_means * 86400
  } else {
    # For temperature: convert from K to °C
    annual_values <- spatial_means - 273.15
  }

  return(data.table(year = years, value = annual_values, variable = var))
}

# Process all variables sequentially
cat("Starting data processing...\n")
start_time <- Sys.time()

# Process each variable
results_list <- list()
for (var in variables) {
  cat(paste("\nProcessing", var, "...\n"))
  var_data <- process_variable_fast(var, base_path)
  if (nrow(var_data) > 0) {
    results_list[[var]] <- var_data
  }
}

# Combine all results
if (length(results_list) > 0) {
  climate_data <- rbindlist(results_list)

  end_time <- Sys.time()
  cat(paste("\nProcessing completed in", round(end_time - start_time, 2), "seconds\n"))

  # Calculate annual means (some files might be monthly, but we have annual files)
  # Let's ensure we have one value per year per variable
  climate_data <- climate_data[, .(value = mean(value, na.rm = TRUE)), by = .(year, variable)]

  # Calculate trends and anomalies
  setorder(climate_data, variable, year)

  # Use safe trend calculation
  calculate_trend <- function(years, values) {
    if (length(values) > 1) {
      model <- lm(values ~ years)
      return(predict(model))
    } else {
      return(rep(NA_real_, length(values)))
    }
  }

  climate_data[, trend := calculate_trend(year, value), by = variable]
  climate_data[, anomaly := value - mean(value, na.rm = TRUE), by = variable]

  # Add metadata
  climate_data[, variable_name := factor(variable,
                                         levels = variables,
                                         labels = variable_names)]
  climate_data[, unit := factor(variable,
                                levels = variables,
                                labels = units)]

  # Calculate summary statistics
  summary_stats <- climate_data[, {
    if (.N > 1) {
      model <- lm(value ~ year)
      coefs <- coef(model)
      p_val <- if (.N > 2) {
        summary(model)$coefficients[2, 4]
      } else {
        NA_real_
      }
      .(
        mean_value = mean(value, na.rm = TRUE),
        trend_per_decade = 10 * coefs[2],
        p_value = p_val,
        first_year = first(year),
        last_year = last(year),
        change = last(value) - first(value)
      )
    } else {
      .(
        mean_value = value,
        trend_per_decade = NA_real_,
        p_value = NA_real_,
        first_year = year,
        last_year = year,
        change = 0
      )
    }
  }, by = variable_name]

  # CREATIVE MULTI-PANEL PLOT
  theme_custom <- function(base_size = 12) {
    theme_minimal(base_size = base_size) +
      theme(
        plot.background = element_rect(fill = "#f0f0f0", color = NA),
        panel.background = element_rect(fill = "white", color = "grey80"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
        panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
        strip.background = element_rect(fill = "#2c3e50", color = NA),
        strip.text = element_text(color = "white", face = "bold", size = 10),
        legend.background = element_rect(fill = "white", color = "grey70"),
        legend.key = element_rect(fill = "white"),
        plot.title = element_text(face = "bold", hjust = 0, size = 18, color = "#2c3e50"),
        plot.subtitle = element_text(color = "#7f8c8d", hjust = 0, size = 12),
        plot.caption = element_text(color = "#95a5a6", size = 9, hjust = 1),
        axis.title = element_text(face = "bold", color = "#2c3e50"),
        axis.text = element_text(color = "#34495e"),
        plot.margin = margin(20, 20, 20, 20)
      )
  }

  # Create a palette
  climate_palette <- c(
    "#3498db",  # Precipitation - blue
    "#e74c3c",  # Mean Temp - red
    "#f39c12",  # Min Temp - orange
    "#9b59b6"   # Max Temp - purple
  )

  names(climate_palette) <- variable_names

  # Panel 1: Time series with trends and confidence intervals
  p1 <- ggplot(climate_data, aes(x = year, y = value, color = variable_name)) +
    geom_line(alpha = 0.6, linewidth = 0.8) +
    geom_point(size = 1.5, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.2, linewidth = 1,
                aes(fill = variable_name)) +
    scale_color_manual(values = climate_palette) +
    scale_fill_manual(values = climate_palette) +
    labs(x = "Year", y = "", color = "Variable",
         title = "Historical Climate Trends (1979-2017)") +
    facet_wrap2(~ variable_name, scales = "free_y", ncol = 2,
                labeller = labeller(variable_name = label_wrap_gen(20))) +
    theme_custom() +
    theme(legend.position = "none",
          strip.background = element_rect(fill = "#34495e"),
          strip.text = element_text(color = "white"))

  # Panel 2: Ridgeline plot of distributions
  p2 <- ggplot(climate_data, aes(x = value, y = variable_name, fill = variable_name)) +
    geom_density_ridges(
      alpha = 0.8,
      scale = 0.9,
      quantile_lines = TRUE,
      quantiles = 4,
      jittered_points = TRUE,
      point_shape = "|",
      point_size = 2,
      point_alpha = 0.3,
      position = position_raincloud(width = 0.1)
    ) +
    scale_fill_manual(values = climate_palette) +
    labs(x = "Value", y = "", title = "Distribution Density") +
    theme_custom() +
    theme(legend.position = "none",
          panel.grid.major.x = element_blank())

  # Panel 3: Change from first to last year with arrows
  change_data <- climate_data[, .(
    first = first(value),
    last = last(value),
    mean = mean(value)
  ), by = variable_name]

  p3 <- ggplot(change_data) +
    geom_segment(aes(x = variable_name, xend = variable_name,
                     y = first, yend = last, color = variable_name),
                 linewidth = 2, alpha = 0.8,
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
    geom_point(aes(x = variable_name, y = first, color = variable_name),
               size = 4, shape = 21, fill = "white") +
    geom_point(aes(x = variable_name, y = last, color = variable_name),
               size = 4, shape = 19) +
    geom_text(aes(x = variable_name, y = first,
                  label = sprintf("%.1f", first)),
              vjust = 1.5, size = 3, color = "#2c3e50") +
    geom_text(aes(x = variable_name, y = last,
                  label = sprintf("%.1f", last)),
              vjust = -1, size = 3, color = "#2c3e50") +
    scale_color_manual(values = climate_palette) +
    labs(x = "", y = "Value",
         title = "Change from 1979 to 2017",
         subtitle = "○ = First year ● = Last year") +
    theme_custom() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 0, hjust = 0.5))

  # Panel 4: Trend magnitude with significance stars
  p4_data <- copy(summary_stats)
  p4_data[, significance := cut(p_value,
                                breaks = c(0, 0.001, 0.01, 0.05, 1),
                                labels = c("***", "**", "*", "ns"),
                                include.lowest = TRUE)]
  p4_data[, label := sprintf("%+.3f/decade %s", trend_per_decade, significance)]

  p4 <- ggplot(p4_data, aes(x = reorder(variable_name, abs(trend_per_decade)),
                            y = trend_per_decade,
                            fill = trend_per_decade > 0)) +
    geom_col(width = 0.6, alpha = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
    geom_text(aes(label = label, y = ifelse(trend_per_decade >= 0,
                                            trend_per_decade + max(abs(trend_per_decade))*0.05,
                                            trend_per_decade - max(abs(trend_per_decade))*0.05)),
              hjust = ifelse(p4_data$trend_per_decade >= 0, 0, 1),
              size = 3, color = "black") +
    scale_fill_manual(values = c("FALSE" = "#3498db", "TRUE" = "#e74c3c")) +
    coord_flip() +
    labs(x = "", y = "Trend per decade",
         title = "Decadal Trend Magnitude",
         subtitle = "Red = Warming/Increase, Blue = Cooling/Decrease") +
    theme_custom() +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank())

  # Combine all panels
  final_plot <- (p1 | (p2 / p3)) / p4 +
    plot_annotation(
      title = "CHELSA Climate Data Analysis: 1979-2017",
      subtitle = paste("European Climate Trends: Precipitation and Temperature Variables\n",
                       "Data Source: CHELSA High Resolution Climate Data"),
      caption = paste("Generated on", format(Sys.Date(), "%B %d, %Y"),
                      "| Units: Precipitation (mm/day), Temperature (°C)"),
      theme = theme(
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5,
                                  color = "#2c3e50", margin = margin(b = 10)),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#7f8c8d",
                                     margin = margin(b = 20)),
        plot.caption = element_text(size = 9, hjust = 1, color = "#95a5a6",
                                    margin = margin(t = 10)),
        plot.background = element_rect(fill = "#f8f9fa", color = NA),
        plot.margin = margin(20, 30, 20, 30)
      )
    ) +
    plot_layout(heights = c(2, 1.2))

  # Display the plot
  print(final_plot)

  # Save high-resolution output
  ggsave("CHELSA_Climate_Trends_1979-2017.png",
         plot = final_plot,
         width = 18,
         height = 16,
         dpi = 300,
         bg = "white")

  cat("\nPlot saved as 'CHELSA_Climate_Trends_1979-2017.png'\n")

  # Create an interactive summary table
  cat("\n" + strrep("=", 60) + "\n")
  cat("CLIMATE CHANGE SUMMARY (1979-2017)\n")
  cat(strrep("=", 60) + "\n\n")

  print(summary_stats[, .(
    Variable = variable_name,
    Mean = sprintf("%.2f", mean_value),
    `Trend/Decade` = sprintf("%+.3f", trend_per_decade),
    `Total Change` = sprintf("%+.2f", change),
    `P-value` = ifelse(is.na(p_value), "NA",
                       ifelse(p_value < 0.001, "<0.001",
                              sprintf("%.3f", p_value))),
    Significance = ifelse(is.na(p_value), "NA",
                          ifelse(p_value < 0.001, "***",
                                 ifelse(p_value < 0.01, "**",
                                        ifelse(p_value < 0.05, "*",
                                               ifelse(p_value < 0.1, ".", "ns")))))
  )])

  # Additional creative visualization: Heatmap of annual values
  p_heatmap <- ggplot(climate_data, aes(x = year, y = variable_name, fill = value)) +
    geom_tile(color = "white", linewidth = 0.8) +
    scale_fill_viridis_c(option = "plasma", name = "Value",
                         guide = guide_colorbar(barwidth = 12, barheight = 0.8)) +
    scale_x_continuous(breaks = seq(1980, 2015, 5)) +
    labs(x = "Year", y = "",
         title = "Annual Values Heatmap",
         subtitle = "Darker colors indicate higher values") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5)
    )

  print(p_heatmap)

  # Save heatmap
  ggsave("CHELSA_Climate_Heatmap_1979-2017.png",
         plot = p_heatmap,
         width = 16, height = 6, dpi = 300, bg = "white")

  cat("\nHeatmap saved as 'CHELSA_Climate_Heatmap_1979-2017.png'\n")

} else {
  cat("ERROR: No data was processed. Please check your file paths and patterns.\n")
  cat("Expected pattern: chelsa_EU_[variable]_300arcsec_dailyYYYYMM.nc\n")
  cat("Base path:", base_path, "\n")
  cat("Variables to process:", paste(variables, collapse = ", "), "\n")
}
