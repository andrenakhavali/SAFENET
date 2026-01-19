# Complete R Script for CHELSA vs EU-CORDEX Analysis
# =================================================

# Load required libraries
library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(sf)
library(patchwork)
library(viridis)
library(readr)
library(stringr)
library(tools)

# Set working directory and paths
base_path <- "H:/EU-CORDEX/output/annual_historical_1980plus"
chel_path <- file.path(base_path, "CHELSA")
eucordex_path <- file.path(base_path, "EU-CORDEX")

# Create output directories
output_dirs <- c("analysis_report", "maps", "difference_maps", "time_series",
                 "tables", "raster_outputs", "plots", "side_by_side_maps")
for(dir in output_dirs) {
  dir_path <- file.path(base_path, dir)
  if(!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    cat("Created directory:", dir_path, "\n")
  }
}

# List all available files
list_available_files <- function() {
  cat("=== Searching for files ===\n")
  cat("CHELSA PR path:", file.path(chel_path, "pr"), "\n")
  cat("CHELSA TAS path:", file.path(chel_path, "tas"), "\n")
  cat("EU-CORDEX PR path:", file.path(eucordex_path, "pr"), "\n")
  cat("EU-CORDEX TAS path:", file.path(eucordex_path, "tas"), "\n\n")

  # List all files in directories
  chel_pr_files <- list.files(file.path(chel_path, "pr"),
                              pattern = "\\.tif$", full.names = TRUE)
  chel_tas_files <- list.files(file.path(chel_path, "tas"),
                               pattern = "\\.tif$", full.names = TRUE)
  eucordex_pr_files <- list.files(file.path(eucordex_path, "pr"),
                                  pattern = "\\.tif$", full.names = TRUE)
  eucordex_tas_files <- list.files(file.path(eucordex_path, "tas"),
                                   pattern = "\\.tif$", full.names = TRUE)

  # Check if directories exist
  if(length(chel_pr_files) == 0) {
    cat("WARNING: No CHELSA PR files found in:", file.path(chel_path, "pr"), "\n")
  }
  if(length(chel_tas_files) == 0) {
    cat("WARNING: No CHELSA TAS files found in:", file.path(chel_path, "tas"), "\n")
  }
  if(length(eucordex_pr_files) == 0) {
    cat("WARNING: No EU-CORDEX PR files found in:", file.path(eucordex_path, "pr"), "\n")
  }
  if(length(eucordex_tas_files) == 0) {
    cat("WARNING: No EU-CORDEX TAS files found in:", file.path(eucordex_path, "tas"), "\n")
  }

  cat("\n=== File Summary ===\n")
  cat("CHELSA PR files:", length(chel_pr_files), "\n")
  if(length(chel_pr_files) > 0) {
    cat("Sample CHELSA PR files:", basename(head(chel_pr_files, 3)), "\n")
  }

  cat("CHELSA TAS files:", length(chel_tas_files), "\n")
  if(length(chel_tas_files) > 0) {
    cat("Sample CHELSA TAS files:", basename(head(chel_tas_files, 3)), "\n")
  }

  cat("EU-CORDEX PR files:", length(eucordex_pr_files), "\n")
  if(length(eucordex_pr_files) > 0) {
    cat("Sample EU-CORDEX PR files:", basename(head(eucordex_pr_files, 3)), "\n")
  }

  cat("EU-CORDEX TAS files:", length(eucordex_tas_files), "\n")
  if(length(eucordex_tas_files) > 0) {
    cat("Sample EU-CORDEX TAS files:", basename(head(eucordex_tas_files, 3)), "\n")
  }

  return(list(
    chel_pr = chel_pr_files,
    chel_tas = chel_tas_files,
    eucordex_pr = eucordex_pr_files,
    eucordex_tas = eucordex_tas_files
  ))
}

# Parse year from filename
parse_year <- function(filename) {
  # Extract year from filename (assuming format like pr_historical_annual_1980.tif)
  filename_base <- basename(filename)
  year <- str_extract(filename_base, "\\d{4}")
  if(is.na(year)) {
    year <- str_extract(filename_base, "(?<=_)\\d{4}(?=\\.)")
  }
  if(is.na(year)) {
    year <- str_extract(filename_base, "\\d{4}")
  }
  return(as.numeric(year))
}

# Filter files to only include years up to 2005
filter_files_to_2005 <- function(files_list) {
  cat("\n=== Filtering files to years <= 2005 ===\n")

  filtered_list <- list()

  for(data_type in names(files_list)) {
    files <- files_list[[data_type]]
    if(length(files) > 0) {
      years <- sapply(files, parse_year)
      # Keep only files with years <= 2005
      keep_indices <- which(years <= 2005)
      filtered_files <- files[keep_indices]
      filtered_list[[data_type]] <- filtered_files

      cat("Filtered", data_type, "from", length(files), "to", length(filtered_files), "files (<= 2005)\n")
      if(length(filtered_files) > 0) {
        filtered_years <- sapply(filtered_files, parse_year)
        cat("Year range for", data_type, ":", paste(range(filtered_years), collapse = "-"), "\n")
      }
    } else {
      filtered_list[[data_type]] <- files
    }
  }

  return(filtered_list)
}

# Unit conversion functions
convert_units <- function(raster, variable, dataset) {
  cat("  Converting units for", dataset, variable, "...")

  if(variable == "pr") {
    # Convert precipitation from kg m⁻² s⁻¹ to mm/year
    # 1 kg m⁻² s⁻¹ = 1 mm/s (since 1 kg of water over 1 m² = 1 mm depth)
    # To convert to mm/year: multiply by seconds in a year
    seconds_per_year <- 86400
    raster_converted <- raster * seconds_per_year
    cat(" PR: kg m⁻² s⁻¹ to mm/year\n")
  } else if(variable == "tas") {
    # Convert temperature from Kelvin to Celsius
    raster_converted <- raster - 273.15
    cat(" TAS: K to °C\n")
  } else {
    raster_converted <- raster
    cat(" No conversion needed\n")
  }

  return(raster_converted)
}

# Load all data into a structured format
load_all_data <- function(files_list) {
  all_data <- list()

  # Function to load and process raster files
  load_raster_stack <- function(files, dataset, variable) {
    if(length(files) == 0) {
      cat("No files found for", dataset, variable, "\n")
      return(NULL)
    }

    years <- sapply(files, parse_year)
    sorted_idx <- order(years)
    files <- files[sorted_idx]
    years <- years[sorted_idx]

    # Filter to years <= 2005
    keep_idx <- years <= 2005
    files <- files[keep_idx]
    years <- years[keep_idx]

    if(length(files) == 0) {
      cat("No files for", dataset, variable, "with years <= 2005\n")
      return(NULL)
    }

    cat("Loading", length(files), "files for", dataset, variable,
        "Years:", paste(range(years), collapse = "-"), "\n")

    # Create raster stack
    rstack <- rast()
    for(i in seq_along(files)) {
      tryCatch({
        r <- rast(files[i])

        # Convert units
        r <- convert_units(r, variable, dataset)

        names(r) <- paste0(variable, "_", dataset, "_", years[i])
        add(rstack) <- r

        if(i %% 10 == 0) {
          cat("  Loaded", i, "of", length(files), "files\n")
        }
      }, error = function(e) {
        cat("Error loading file:", basename(files[i]), "-", e$message, "\n")
      })
    }

    names(rstack) <- paste0(variable, "_", dataset, "_", years)

    cat("Successfully loaded", nlyr(rstack), "layers for", dataset, variable, "\n")

    return(list(
      stack = rstack,
      years = years,
      dataset = dataset,
      variable = variable
    ))
  }

  # Load all data
  cat("\n=== Loading data (years <= 2005) ===\n")
  all_data$chel_pr <- load_raster_stack(files_list$chel_pr, "CHELSA", "pr")
  all_data$chel_tas <- load_raster_stack(files_list$chel_tas, "CHELSA", "tas")
  all_data$eucordex_pr <- load_raster_stack(files_list$eucordex_pr, "EU_CORDEX", "pr")
  all_data$eucordex_tas <- load_raster_stack(files_list$eucordex_tas, "EU_CORDEX", "tas")

  return(all_data)
}

# Create summary statistics CSV
create_summary_csv <- function(all_data) {
  cat("\n=== Creating summary statistics ===\n")
  summary_list <- list()

  for(data_name in names(all_data)) {
    if(!is.null(all_data[[data_name]])) {
      data_obj <- all_data[[data_name]]
      rstack <- data_obj$stack
      dataset <- data_obj$dataset
      variable <- data_obj$variable

      cat("Processing", dataset, variable, "-", nlyr(rstack), "layers\n")

      # Calculate statistics for each year
      for(i in 1:nlyr(rstack)) {
        r <- rstack[[i]]
        year <- data_obj$years[i]

        # Calculate statistics
        vals <- values(r)
        vals <- vals[!is.na(vals)]

        if(length(vals) > 0) {
          stats_df <- data.frame(
            Year = year,
            Dataset = dataset,
            Variable = variable,
            Mean = mean(vals, na.rm = TRUE),
            Median = median(vals, na.rm = TRUE),
            SD = sd(vals, na.rm = TRUE),
            Min = min(vals, na.rm = TRUE),
            Max = max(vals, na.rm = TRUE),
            Q25 = quantile(vals, 0.25, na.rm = TRUE),
            Q75 = quantile(vals, 0.75, na.rm = TRUE),
            CellCount = length(vals)
          )

          summary_list[[paste0(data_name, "_", year)]] <- stats_df
        }
      }
    }
  }

  # Combine all statistics
  summary_df <- bind_rows(summary_list)

  # Save to CSV
  write_csv(summary_df, file.path(base_path, "tables", "summary_statistics_all_datasets.csv"))
  cat("\nSummary statistics saved to:", file.path(base_path, "tables", "summary_statistics_all_datasets.csv"), "\n")

  return(summary_df)
}

# Create mismatch analysis CSV
create_mismatch_analysis <- function(all_data) {
  cat("\n=== Creating mismatch analysis ===\n")
  mismatch_list <- list()
  diff_layers <- list(pr = list(), tas = list())

  # Compare CHELSA vs EU-CORDEX for each variable
  for(variable in c("pr", "tas")) {
    chel_key <- paste0("chel_", variable)
    eucordex_key <- paste0("eucordex_", variable)

    if(!is.null(all_data[[chel_key]]) && !is.null(all_data[[eucordex_key]])) {
      chel_stack <- all_data[[chel_key]]$stack
      eucordex_stack <- all_data[[eucordex_key]]$stack
      years <- intersect(all_data[[chel_key]]$years, all_data[[eucordex_key]]$years)

      # Filter to years <= 2005
      years <- years[years <= 2005]

      cat("Comparing", variable, "- Common years (<= 2005):", paste(years, collapse = ", "), "\n")

      for(year in years) {
        chel_idx <- which(all_data[[chel_key]]$years == year)
        eucordex_idx <- which(all_data[[eucordex_key]]$years == year)

        if(length(chel_idx) > 0 && length(eucordex_idx) > 0) {
          chel_layer <- chel_stack[[chel_idx]]
          eucordex_layer <- eucordex_stack[[eucordex_idx]]

          cat("  Year", year, ": Resampling to match grids...")

          # Ensure same extent and resolution
          eucordex_layer_resampled <- resample(eucordex_layer, chel_layer, method = "bilinear")

          # Calculate difference
          diff_layer <- chel_layer - eucordex_layer_resampled
          names(diff_layer) <- paste0("diff_", variable, "_", year)

          # Store difference raster
          diff_layers[[variable]][[as.character(year)]] <- diff_layer

          # Calculate mismatch statistics
          chel_vals <- values(chel_layer) %>% na.omit()
          eucordex_vals <- values(eucordex_layer_resampled) %>% na.omit()
          diff_vals <- values(diff_layer) %>% na.omit()

          if(length(chel_vals) > 0 && length(eucordex_vals) > 0 && length(diff_vals) > 0) {
            # Calculate correlation (ensure same length)
            min_length <- min(length(chel_vals), length(eucordex_vals))
            correlation_val <- ifelse(min_length > 1,
                                      cor(chel_vals[1:min_length], eucordex_vals[1:min_length]),
                                      NA)

            mismatch_df <- data.frame(
              Year = year,
              Variable = variable,
              Dataset1 = "CHELSA",
              Dataset2 = "EU_CORDEX",
              Mean_Difference = mean(diff_vals, na.rm = TRUE),
              Abs_Mean_Difference = mean(abs(diff_vals), na.rm = TRUE),
              SD_Difference = sd(diff_vals, na.rm = TRUE),
              RMSE = sqrt(mean(diff_vals^2, na.rm = TRUE)),
              Correlation = correlation_val,
              Percent_Mismatch = ifelse(length(chel_vals) > 0,
                                        (sum(abs(diff_vals) > (0.1 * mean(chel_vals, na.rm = TRUE)), na.rm = TRUE) / length(diff_vals)) * 100,
                                        NA),
              Max_Positive_Diff = max(diff_vals, na.rm = TRUE),
              Max_Negative_Diff = min(diff_vals, na.rm = TRUE),
              CHELSA_Mean = mean(chel_vals, na.rm = TRUE),
              EU_CORDEX_Mean = mean(eucordex_vals, na.rm = TRUE)
            )

            mismatch_list[[paste0(variable, "_", year)]] <- mismatch_df
            cat(" Done\n")
          } else {
            cat(" Warning: No valid data for comparison\n")
          }
        }
      }
    }
  }

  # Combine all mismatch statistics
  if(length(mismatch_list) > 0) {
    mismatch_df <- bind_rows(mismatch_list)

    # Save to CSV
    write_csv(mismatch_df, file.path(base_path, "tables", "mismatch_analysis_chelsa_eucordex.csv"))
    cat("\nMismatch analysis saved to:", file.path(base_path, "tables", "mismatch_analysis_chelsa_eucordex.csv"), "\n")
  } else {
    mismatch_df <- data.frame()
    cat("\nNo mismatch data to save\n")
  }

  return(list(mismatch_df = mismatch_df, diff_layers = diff_layers))
}

# Create maps
create_maps <- function(all_data) {
  cat("\n=== Creating maps ===\n")
  map_dir <- file.path(base_path, "maps")

  # Function to create a single map
  create_single_map <- function(raster, title, filename, variable) {
    df <- as.data.frame(raster, xy = TRUE, na.rm = TRUE)
    colnames(df) <- c("x", "y", "value")

    if(nrow(df) == 0) {
      cat("Warning: No data for", filename, "\n")
      return(NULL)
    }

    # Define color scale based on variable
    if(variable == "pr") {
      fill_title <- "Precipitation (mm/year)"
      color_scheme <- scale_fill_viridis_c(
        name = fill_title,
        option = "viridis"
      )
    } else if(variable == "tas") {
      fill_title <- "Temperature (°C)"
      color_scheme <- scale_fill_gradientn(
        name = fill_title,
        colors = c("blue", "cyan", "green", "yellow", "red")
      )
    } else {
      fill_title <- "Value"
      color_scheme <- scale_fill_viridis_c(name = fill_title)
    }

    p <- ggplot() +
      geom_raster(data = df, aes(x = x, y = y, fill = value)) +
      color_scheme +
      coord_sf() +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.title = element_text(size = 10),
        legend.position = "right",
        legend.title = element_text(size = 9)
      ) +
      labs(
        title = title,
        x = "Longitude",
        y = "Latitude"
      )

    ggsave(file.path(map_dir, filename), p,
           width = 10, height = 8, dpi = 300, bg = "white")

    return(p)
  }

  # Create maps for each dataset and variable
  for(data_name in names(all_data)) {
    if(!is.null(all_data[[data_name]])) {
      data_obj <- all_data[[data_name]]
      variable <- data_obj$variable
      dataset <- data_obj$dataset

      cat("Creating maps for", dataset, variable, "\n")

      # Create mean map
      mean_raster <- mean(data_obj$stack, na.rm = TRUE)
      if(nlyr(mean_raster) > 0) {
        create_single_map(
          raster = mean_raster,
          title = paste(dataset, toupper(variable), "- Mean (1980-2005)"),
          filename = paste0("map_", dataset, "_", variable, "_mean.png"),
          variable = variable
        )
      }

      # Create individual year maps
      for(i in 1:min(5, nlyr(data_obj$stack))) {  # First 5 years
        year <- data_obj$years[i]
        create_single_map(
          raster = data_obj$stack[[i]],
          title = paste(dataset, toupper(variable), "-", year),
          filename = paste0("map_", dataset, "_", variable, "_", year, ".png"),
          variable = variable
        )
      }
    }
  }

  cat("All maps saved to:", map_dir, "\n")
}

# Create side-by-side comparison maps
create_side_by_side_maps <- function(all_data) {
  cat("\n=== Creating side-by-side comparison maps ===\n")
  side_by_side_dir <- file.path(base_path, "side_by_side_maps")

  # Create maps for TAS
  if(!is.null(all_data$chel_tas) && !is.null(all_data$eucordex_tas)) {
    cat("Creating side-by-side TAS maps...\n")

    # Calculate mean for TAS
    chel_tas_mean <- mean(all_data$chel_tas$stack, na.rm = TRUE)
    eucordex_tas_mean <- mean(all_data$eucordex_tas$stack, na.rm = TRUE)

    # Convert to dataframes
    chel_tas_df <- as.data.frame(chel_tas_mean, xy = TRUE, na.rm = TRUE)
    colnames(chel_tas_df) <- c("x", "y", "value")
    chel_tas_df$Dataset <- "CHELSA"

    eucordex_tas_df <- as.data.frame(eucordex_tas_mean, xy = TRUE, na.rm = TRUE)
    colnames(eucordex_tas_df) <- c("x", "y", "value")
    eucordex_tas_df$Dataset <- "EU-CORDEX"

    # Combine data
    tas_df <- bind_rows(chel_tas_df, eucordex_tas_df)

    if(nrow(tas_df) > 0) {
      # Create side-by-side TAS plot
      p_tas <- ggplot(tas_df, aes(x = x, y = y, fill = value)) +
        geom_raster() +
        facet_wrap(~Dataset, nrow = 1) +
        scale_fill_gradientn(
          name = "Temperature (°C)",
          colors = c("blue", "cyan", "green", "yellow", "red")
        ) +
        coord_sf() +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          strip.text = element_text(size = 12, face = "bold"),
          legend.position = "right"
        ) +
        labs(
          title = "Mean Temperature (TAS) 1980-2005",
          x = "Longitude",
          y = "Latitude"
        )

      ggsave(file.path(side_by_side_dir, "side_by_side_tas_mean.png"),
             p_tas, width = 16, height = 8, dpi = 300, bg = "white")
      cat("Saved side-by-side TAS map to:", file.path(side_by_side_dir, "side_by_side_tas_mean.png"), "\n")
    }
  }

  # Create maps for PR
  if(!is.null(all_data$chel_pr) && !is.null(all_data$eucordex_pr)) {
    cat("Creating side-by-side PR maps...\n")

    # Calculate mean for PR
    chel_pr_mean <- mean(all_data$chel_pr$stack, na.rm = TRUE)
    eucordex_pr_mean <- mean(all_data$eucordex_pr$stack, na.rm = TRUE)

    # Convert to dataframes
    chel_pr_df <- as.data.frame(chel_pr_mean, xy = TRUE, na.rm = TRUE)
    colnames(chel_pr_df) <- c("x", "y", "value")
    chel_pr_df$Dataset <- "CHELSA"

    eucordex_pr_df <- as.data.frame(eucordex_pr_mean, xy = TRUE, na.rm = TRUE)
    colnames(eucordex_pr_df) <- c("x", "y", "value")
    eucordex_pr_df$Dataset <- "EU-CORDEX"

    # Combine data
    pr_df <- bind_rows(chel_pr_df, eucordex_pr_df)

    if(nrow(pr_df) > 0) {
      # Create side-by-side PR plot
      p_pr <- ggplot(pr_df, aes(x = x, y = y, fill = value)) +
        geom_raster() +
        facet_wrap(~Dataset, nrow = 1) +
        scale_fill_viridis_c(
          name = "Precipitation (mm/year)",
          option = "viridis"
        ) +
        coord_sf() +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          strip.text = element_text(size = 12, face = "bold"),
          legend.position = "right"
        ) +
        labs(
          title = "Mean Precipitation (PR) 1980-2005",
          x = "Longitude",
          y = "Latitude"
        )

      ggsave(file.path(side_by_side_dir, "side_by_side_pr_mean.png"),
             p_pr, width = 16, height = 8, dpi = 300, bg = "white")
      cat("Saved side-by-side PR map to:", file.path(side_by_side_dir, "side_by_side_pr_mean.png"), "\n")
    }
  }

  # Create combined plot with both TAS and PR
  if(!is.null(all_data$chel_tas) && !is.null(all_data$eucordex_tas) &&
     !is.null(all_data$chel_pr) && !is.null(all_data$eucordex_pr)) {
    cat("Creating combined comparison plot...\n")

    # Create TAS plots
    p_tas_chel <- ggplot(chel_tas_df, aes(x = x, y = y, fill = value)) +
      geom_raster() +
      scale_fill_gradientn(
        name = "Temperature (°C)",
        colors = c("blue", "cyan", "green", "yellow", "red")
      ) +
      coord_sf() +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 12),
        legend.position = "right"
      ) +
      labs(
        title = "CHELSA TAS",
        x = "Longitude",
        y = "Latitude"
      )

    p_tas_eucordex <- ggplot(eucordex_tas_df, aes(x = x, y = y, fill = value)) +
      geom_raster() +
      scale_fill_gradientn(
        name = "Temperature (°C)",
        colors = c("blue", "cyan", "green", "yellow", "red")
      ) +
      coord_sf() +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 12),
        legend.position = "right"
      ) +
      labs(
        title = "EU-CORDEX TAS",
        x = "Longitude",
        y = "Latitude"
      )

    # Create PR plots
    p_pr_chel <- ggplot(chel_pr_df, aes(x = x, y = y, fill = value)) +
      geom_raster() +
      scale_fill_viridis_c(
        name = "Precipitation (mm/year)",
        option = "viridis"
      ) +
      coord_sf() +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 12),
        legend.position = "right"
      ) +
      labs(
        title = "CHELSA PR",
        x = "Longitude",
        y = "Latitude"
      )

    p_pr_eucordex <- ggplot(eucordex_pr_df, aes(x = x, y = y, fill = value)) +
      geom_raster() +
      scale_fill_viridis_c(
        name = "Precipitation (mm/year)",
        option = "viridis"
      ) +
      coord_sf() +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 12),
        legend.position = "right"
      ) +
      labs(
        title = "EU-CORDEX PR",
        x = "Longitude",
        y = "Latitude"
      )

    # Combine all plots
    combined_plot <- (p_tas_chel + p_tas_eucordex) / (p_pr_chel + p_pr_eucordex) +
      plot_annotation(
        title = "CHELSA vs EU-CORDEX Comparison (1980-2005)",
        theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
      )

    ggsave(file.path(side_by_side_dir, "combined_side_by_side_comparison.png"),
           combined_plot, width = 16, height = 12, dpi = 300, bg = "white")
    cat("Saved combined comparison plot to:", file.path(side_by_side_dir, "combined_side_by_side_comparison.png"), "\n")
  }

  cat("Side-by-side maps saved to:", side_by_side_dir, "\n")
}

# Create difference maps
create_difference_maps <- function(diff_layers) {
  cat("\n=== Creating difference maps ===\n")
  diff_dir <- file.path(base_path, "difference_maps")

  for(variable in names(diff_layers)) {
    for(year_name in names(diff_layers[[variable]])) {
      diff_layer <- diff_layers[[variable]][[year_name]]

      if(!is.null(diff_layer)) {
        # Create difference map
        df <- as.data.frame(diff_layer, xy = TRUE, na.rm = TRUE)
        colnames(df) <- c("x", "y", "value")

        if(nrow(df) > 0) {
          # Determine appropriate color scale
          max_abs <- max(abs(range(df$value, na.rm = TRUE)))

          p <- ggplot() +
            geom_raster(data = df, aes(x = x, y = y, fill = value)) +
            scale_fill_gradient2(
              name = ifelse(variable == "pr", "Diff (mm/year)", "Diff (°C)"),
              low = "blue",
              mid = "white",
              high = "red",
              midpoint = 0,
              limits = c(-max_abs, max_abs)
            ) +
            coord_sf() +
            theme_minimal() +
            theme(
              plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
              legend.position = "right"
            ) +
            labs(
              title = paste("Difference:", toupper(variable), "CHELSA - EU-CORDEX (", year_name, ")"),
              x = "Longitude",
              y = "Latitude"
            )

          filename <- paste0("diff_", variable, "_", year_name, ".png")
          ggsave(file.path(diff_dir, filename),
                 p, width = 10, height = 8, dpi = 300, bg = "white")

          # Save difference raster
          writeRaster(diff_layer,
                      file.path(base_path, "raster_outputs", paste0("diff_", variable, "_", year_name, ".tif")),
                      overwrite = TRUE)
        }
      }
    }
  }

  cat("Difference maps saved to:", diff_dir, "\n")
}

# Create time series line plots
create_time_series_plots <- function(summary_df, mismatch_df) {
  cat("\n=== Creating time series plots ===\n")
  ts_dir <- file.path(base_path, "time_series")

  if(nrow(summary_df) == 0) {
    cat("No summary data for time series plots\n")
    return()
  }

  # Filter data to years <= 2005
  summary_df <- summary_df %>% filter(Year <= 2005)
  if(nrow(mismatch_df) > 0) {
    mismatch_df <- mismatch_df %>% filter(Year <= 2005)
  }

  # Line plot for each variable
  for(variable in c("pr", "tas")) {
    # Filter data for the variable
    var_summary <- summary_df %>% filter(Variable == variable)

    if(nrow(var_summary) > 0) {
      # Create time series plot
      p1 <- ggplot(var_summary, aes(x = Year, y = Mean, color = Dataset, group = Dataset)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        geom_ribbon(aes(ymin = Mean - SD, ymax = Mean + SD, fill = Dataset),
                    alpha = 0.2) +
        labs(
          title = paste("Time Series:", toupper(variable), "Mean ± SD (1980-2005)"),
          y = ifelse(variable == "pr", "Precipitation (mm/year)", "Temperature (°C)"),
          x = "Year"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.position = "bottom"
        ) +
        scale_color_brewer(palette = "Set1") +
        scale_fill_brewer(palette = "Set1")

      # Boxplot comparison
      p2 <- ggplot(var_summary, aes(x = Dataset, y = Mean, fill = Dataset)) +
        geom_boxplot(alpha = 0.7) +
        geom_jitter(width = 0.2, alpha = 0.5) +
        labs(
          title = paste("Distribution Comparison:", toupper(variable), "(1980-2005)"),
          y = ifelse(variable == "pr", "Precipitation (mm/year)", "Temperature (°C)")
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        scale_fill_brewer(palette = "Set1")

      # Mismatch time series if available
      if(nrow(mismatch_df) > 0) {
        var_mismatch <- mismatch_df %>% filter(Variable == variable)
        if(nrow(var_mismatch) > 0) {
          p3 <- ggplot(var_mismatch, aes(x = Year, y = Mean_Difference)) +
            geom_line(color = "darkred", size = 1) +
            geom_point(color = "darkred", size = 2) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
            geom_ribbon(aes(ymin = Mean_Difference - SD_Difference,
                            ymax = Mean_Difference + SD_Difference),
                        alpha = 0.2, fill = "red") +
            labs(
              title = paste("Mismatch Time Series:", toupper(variable), "(1980-2005)"),
              y = ifelse(variable == "pr", "Difference (mm/year)", "Difference (°C)"),
              subtitle = "CHELSA - EU-CORDEX"
            ) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

          # Combine plots
          combined_plot <- (p1 + p2) / p3 +
            plot_annotation(
              title = paste("Comprehensive Analysis:", toupper(variable), "(1980-2005)"),
              theme = theme(plot.title = element_text(size = 16, face = "bold"))
            )
        } else {
          combined_plot <- p1 + p2 +
            plot_annotation(
              title = paste("Comprehensive Analysis:", toupper(variable), "(1980-2005)"),
              theme = theme(plot.title = element_text(size = 16, face = "bold"))
            )
        }
      } else {
        combined_plot <- p1 + p2 +
          plot_annotation(
            title = paste("Comprehensive Analysis:", toupper(variable), "(1980-2005)"),
            theme = theme(plot.title = element_text(size = 16, face = "bold"))
          )
      }

      ggsave(file.path(ts_dir, paste0("timeseries_", variable, ".png")),
             combined_plot, width = 14, height = 10, dpi = 300, bg = "white")
    }
  }

  cat("Time series plots saved to:", ts_dir, "\n")
}

# Create comprehensive comparison report
create_comprehensive_report <- function(summary_df, mismatch_df) {
  cat("\n=== Creating comprehensive report ===\n")
  report_dir <- file.path(base_path, "analysis_report")

  if(nrow(summary_df) == 0) {
    cat("No data for comprehensive report\n")
    return(NULL)
  }

  # Filter data to years <= 2005
  summary_df <- summary_df %>% filter(Year <= 2005)
  if(nrow(mismatch_df) > 0) {
    mismatch_df <- mismatch_df %>% filter(Year <= 2005)
  }

  # Generate summary statistics table
  summary_stats <- summary_df %>%
    group_by(Dataset, Variable) %>%
    summarise(
      Mean_Overall = mean(Mean, na.rm = TRUE),
      SD_Overall = sd(Mean, na.rm = TRUE),
      Min_Overall = min(Min, na.rm = TRUE),
      Max_Overall = max(Max, na.rm = TRUE),
      Trend = ifelse(n() > 1,
                     coef(lm(Mean ~ Year))[2], NA),
      N_Years = n(),
      Year_Range = paste(min(Year), "-", max(Year)),
      .groups = "drop"
    )

  write_csv(summary_stats, file.path(report_dir, "overall_summary_statistics_1980_2005.csv"))

  # Create trend analysis
  trend_analysis <- summary_df %>%
    group_by(Dataset, Variable) %>%
    filter(n() > 1) %>%
    do({
      mod <- lm(Mean ~ Year, data = .)
      data.frame(
        Intercept = coef(mod)[1],
        Slope = coef(mod)[2],
        R_squared = summary(mod)$r.squared,
        P_value = ifelse(!is.na(summary(mod)$coefficients[2,4]),
                         summary(mod)$coefficients[2,4], NA)
      )
    })

  if(nrow(trend_analysis) > 0) {
    write_csv(trend_analysis, file.path(report_dir, "trend_analysis_1980_2005.csv"))
  }

  # Create trend plot
  p_trend <- ggplot(summary_df, aes(x = Year, y = Mean, color = Dataset)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~Variable, scales = "free_y", ncol = 1) +
    labs(
      title = "Trend Analysis Over Time (1980-2005)",
      y = "Value",
      x = "Year"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    ) +
    scale_color_brewer(palette = "Set1")

  ggsave(file.path(report_dir, "trend_analysis_plot_1980_2005.png"),
         p_trend, width = 12, height = 8, dpi = 300, bg = "white")

  # Create summary statistics by dataset
  if(nrow(mismatch_df) > 0) {
    mismatch_summary <- mismatch_df %>%
      group_by(Variable) %>%
      summarise(
        Mean_Difference = mean(Mean_Difference, na.rm = TRUE),
        Mean_Abs_Difference = mean(Abs_Mean_Difference, na.rm = TRUE),
        Mean_RMSE = mean(RMSE, na.rm = TRUE),
        Mean_Correlation = mean(Correlation, na.rm = TRUE),
        N_Years = n(),
        Year_Range = paste(min(Year), "-", max(Year)),
        .groups = "drop"
      )

    write_csv(mismatch_summary, file.path(report_dir, "mismatch_summary_1980_2005.csv"))
  }

  cat("Comprehensive report saved to:", report_dir, "\n")

  return(list(
    summary_stats = summary_stats,
    trend_analysis = if(exists("trend_analysis")) trend_analysis else NULL
  ))
}

# Create annual difference time series
create_annual_difference_plots <- function(mismatch_df) {
  cat("\n=== Creating annual difference plots ===\n")
  ts_dir <- file.path(base_path, "time_series")

  if(nrow(mismatch_df) > 0) {
    # Filter to years <= 2005
    mismatch_df <- mismatch_df %>% filter(Year <= 2005)

    for(variable in c("pr", "tas")) {
      var_data <- mismatch_df %>% filter(Variable == variable)

      if(nrow(var_data) > 0) {
        # Create annual difference plot
        p <- ggplot(var_data, aes(x = Year)) +
          geom_line(aes(y = Mean_Difference, color = "Mean Difference"), size = 1) +
          geom_ribbon(aes(ymin = Mean_Difference - SD_Difference,
                          ymax = Mean_Difference + SD_Difference),
                      alpha = 0.2, fill = "blue") +
          geom_line(aes(y = Abs_Mean_Difference, color = "Absolute Difference"), size = 1, linetype = "dashed") +
          geom_line(aes(y = RMSE, color = "RMSE"), size = 1, linetype = "dotted") +
          geom_hline(yintercept = 0, linetype = "solid", color = "gray50") +
          labs(
            title = paste("Annual Differences:", toupper(variable), "(CHELSA - EU-CORDEX) 1980-2005"),
            y = ifelse(variable == "pr", "Difference (mm/year)", "Difference (°C)"),
            x = "Year",
            color = "Metric"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.position = "bottom"
          ) +
          scale_color_manual(values = c(
            "Mean Difference" = "blue",
            "Absolute Difference" = "red",
            "RMSE" = "green"
          ))

        ggsave(file.path(ts_dir, paste0("annual_differences_", variable, "_1980_2005.png")),
               p, width = 12, height = 6, dpi = 300, bg = "white")
      }
    }
  }
}

# Main execution function
main_analysis <- function() {
  cat("\n")
  cat(rep("=", 60), "\n", sep = "")
  cat("STARTING COMPREHENSIVE CHELSA vs EU-CORDEX ANALYSIS (1980-2005)\n")
  cat("Note: Converting PR from kg m⁻² s⁻¹ to mm/year and TAS from K to °C\n")
  cat(rep("=", 60), "\n\n", sep = "")

  # Step 1: List and load all files
  cat("STEP 1: Loading all data files...\n")
  files_list <- list_available_files()

  # Filter files to only include years up to 2005
  files_list <- filter_files_to_2005(files_list)

  all_data <- load_all_data(files_list)

  # Check if any data was loaded
  if(all(sapply(all_data, is.null))) {
    cat("ERROR: No data loaded. Check file paths and try again.\n")
    return(NULL)
  }

  # Step 2: Create summary CSV
  cat("\nSTEP 2: Creating summary statistics CSV...\n")
  summary_df <- create_summary_csv(all_data)

  # Step 3: Create mismatch analysis CSV
  cat("\nSTEP 3: Creating mismatch analysis CSV...\n")
  mismatch_results <- create_mismatch_analysis(all_data)
  mismatch_df <- mismatch_results$mismatch_df
  diff_layers <- mismatch_results$diff_layers

  # Step 4: Create maps
  cat("\nSTEP 4: Creating maps...\n")
  create_maps(all_data)

  # Step 4b: Create side-by-side comparison maps
  cat("\nSTEP 4b: Creating side-by-side comparison maps...\n")
  create_side_by_side_maps(all_data)

  # Step 5: Create difference maps
  cat("\nSTEP 5: Creating difference maps...\n")
  create_difference_maps(diff_layers)

  # Step 6: Create time series plots
  cat("\nSTEP 6: Creating time series plots...\n")
  create_time_series_plots(summary_df, mismatch_df)

  # Step 7: Create annual difference plots
  cat("\nSTEP 7: Creating annual difference plots...\n")
  create_annual_difference_plots(mismatch_df)

  # Step 8: Create comprehensive report
  cat("\nSTEP 8: Creating comprehensive report...\n")
  report_results <- create_comprehensive_report(summary_df, mismatch_df)

  # Step 9: Print summary
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("ANALYSIS COMPLETE! (1980-2005)\n")
  cat(rep("=", 60), "\n\n", sep = "")

  cat("GENERATED FILES:\n")
  cat("1. tables/summary_statistics_all_datasets.csv\n")
  cat("2. tables/mismatch_analysis_chelsa_eucordex.csv\n")
  cat("3. maps/ - All individual and mean maps\n")
  cat("4. side_by_side_maps/ - Side-by-side comparison maps\n")
  cat("5. difference_maps/ - All difference maps\n")
  cat("6. time_series/ - Time series and difference plots\n")
  cat("7. raster_outputs/ - Difference raster files (.tif)\n")
  cat("8. analysis_report/ - Comprehensive analysis reports\n")
  cat("9. plots/ - Additional plots\n\n")

  # Print key findings
  cat("KEY FINDINGS (1980-2005):\n")
  if(nrow(summary_df) > 0) {
    for(variable in c("pr", "tas")) {
      var_summary <- summary_df %>% filter(Variable == variable)
      if(nrow(var_summary) > 0) {
        cat(sprintf("\n%s ANALYSIS:\n", toupper(variable)))

        for(dataset in unique(var_summary$Dataset)) {
          dataset_data <- var_summary %>% filter(Dataset == dataset)
          if(nrow(dataset_data) > 0) {
            unit <- ifelse(variable == "pr", "mm/year", "°C")
            cat(sprintf("  %s: Mean = %.2f %s, SD = %.2f %s\n",
                        dataset,
                        mean(dataset_data$Mean, na.rm = TRUE), unit,
                        mean(dataset_data$SD, na.rm = TRUE), unit))
          }
        }
      }
    }
  }

  if(nrow(mismatch_df) > 0) {
    cat("\nDIFFERENCES (CHELSA - EU-CORDEX) 1980-2005:\n")
    for(variable in c("pr", "tas")) {
      var_mismatch <- mismatch_df %>% filter(Variable == variable)
      if(nrow(var_mismatch) > 0) {
        avg_diff <- mean(var_mismatch$Mean_Difference, na.rm = TRUE)
        avg_abs_diff <- mean(var_mismatch$Abs_Mean_Difference, na.rm = TRUE)
        unit <- ifelse(variable == "pr", "mm/year", "°C")

        cat(sprintf("\n  %s:\n", toupper(variable)))
        cat(sprintf("    Average difference: %.2f %s\n", avg_diff, unit))
        cat(sprintf("    Average absolute difference: %.2f %s\n", avg_abs_diff, unit))
        cat(sprintf("    Average correlation: %.3f\n",
                    mean(var_mismatch$Correlation, na.rm = TRUE)))
      }
    }
  }

  return(list(
    all_data = all_data,
    summary = summary_df,
    mismatch = mismatch_df,
    diff_layers = diff_layers,
    report = report_results
  ))
}

# =================================================
# RUN THE ANALYSIS
# =================================================

# Install missing packages if needed
required_packages <- c("terra", "ggplot2", "dplyr", "tidyr", "purrr",
                       "sf", "patchwork", "viridis", "readr", "stringr", "tools")
for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Run the complete analysis
results <- main_analysis()

# Save workspace for later use
save.image(file.path(base_path, "complete_analysis_workspace_1980_2005.RData"))
cat("\nWorkspace saved to: complete_analysis_workspace_1980_2005.RData\n")

# Print session info
cat("\nSESSION INFO:\n")
sessionInfo()
