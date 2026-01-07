# ============================================================================
# EU-CORDEX CLIMATE PROJECTION ANALYSIS
# ============================================================================
# This script processes all variables for all scenarios and exports NetCDF files
# with proper orientation and coordinate systems
# ============================================================================

# Load required libraries
library(terra)
library(sf)
library(rnaturalearth)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
library(ncdf4)

# Set main directory
main_dir <- "H:/EU-CORDEX"
setwd(main_dir)

cat("======================================================================\n")
cat("EU-CORDEX CLIMATE PROJECTION ANALYSIS - COMPLETE PROCESSING\n")
cat("Starting at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("======================================================================\n\n")

# ============================================================================
# CONFIGURATION
# ============================================================================

# Base path
base_path <- "EU-CORDEX"

# Define scenarios with proper time periods
scenarios <- list(
  historical = list(
    name = "historical",
    folder = "historical",
    file_scenario = "historical",
    baseline_periods = c("19910101-19951231", "19960101-20001231",
                         "20010101-20051231")
  ),
  rcp26 = list(
    name = "rcp26",
    folder = "rcp2p6",
    file_scenario = "rcp26",
    baseline_periods = c("19910101-19951231", "19960101-20001231",
                         "20010101-20051231"),
    future_periods = c("20810101-20851231", "20860101-20901231",
                       "20910101-20951231", "20960101-21001231")
  ),
  rcp45 = list(
    name = "rcp45",
    folder = "rcp4p5",
    file_scenario = "rcp45",
    baseline_periods = c("19910101-19951231", "19960101-20001231",
                         "20010101-20051231"),
    future_periods = c("20810101-20851231", "20860101-20901231",
                       "20910101-20951231", "20960101-21001231")
  ),
  rcp85 = list(
    name = "rcp85",
    folder = "rcp8p5",
    file_scenario = "rcp85",
    baseline_periods = c("19910101-19951231", "19960101-20001231",
                         "20010101-20051231"),
    future_periods = c("20810101-20851231", "20860101-20901231",
                       "20910101-20951231", "20960101-21001231")
  )
)

# Variables to process
variables <- c("pr", "tas", "tasmin", "tasmax", "rsds")

# RCM models for each scenario
rcm_models <- list(
  historical = list(
    pr = "MOHC-HadREM3-GA7-05",
    tas = "MOHC-HadREM3-GA7-05",
    tasmin = "MOHC-HadREM3-GA7-05",
    tasmax = "MOHC-HadREM3-GA7-05",
    rsds = "MOHC-HadREM3-GA7-05"
  ),
  rcp26 = list(
    pr = "KNMI-RACMO22E",
    tas = "KNMI-RACMO22E",
    tasmin = "KNMI-RACMO22E",
    tasmax = "KNMI-RACMO22E",
    rsds = "KNMI-RACMO22E"
  ),
  rcp45 = list(
    pr = "MPI-CSC-REMO2009",
    tas = "CLMcom-CCLM4-8-17",
    tasmin = "MPI-CSC-REMO2009",
    tasmax = "MPI-CSC-REMO2009",
    rsds = "MPI-CSC-REMO2009"
  ),
  rcp85 = list(
    pr = "MOHC-HadREM3-GA7-05",
    tas = "MOHC-HadREM3-GA7-05",
    tasmin = "MOHC-HadREM3-GA7-05",
    tasmax = "MOHC-HadREM3-GA7-05",
    rsds = "MOHC-HadREM3-GA7-05"
  )
)

# ============================================================================
# OUTPUT DIRECTORY SETUP
# ============================================================================

# Create timestamp for output directory
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
output_dir <- file.path(main_dir, paste0("FINAL_ANALYSIS_", timestamp))

# Create subdirectories
dir.create(output_dir, recursive = TRUE)
dir.create(file.path(output_dir, "netcdf"), recursive = TRUE)
dir.create(file.path(output_dir, "plots"), recursive = TRUE)
dir.create(file.path(output_dir, "statistics"), recursive = TRUE)
dir.create(file.path(output_dir, "geotiff"), recursive = TRUE)

cat("Output directory:", output_dir, "\n")
cat("NetCDF outputs will be saved in:", file.path(output_dir, "netcdf"), "\n")
cat("Plots will be saved in:", file.path(output_dir, "plots"), "\n\n")

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Function to construct file path with ABSOLUTE path
construct_path <- function(var, scenario_name, period, rcm_model) {
  scenario_info <- scenarios[[scenario_name]]
  path <- file.path(main_dir, base_path, scenario_info$folder, var,
                    paste0(var, "_EUR-11_MPI-M-MPI-ESM-LR_",
                           scenario_info$file_scenario,
                           "_r1i1p1_", rcm_model,
                           "_v1_day_", period, ".nc"))
  return(path)
}

# Function to fix map orientation (flip if needed)
fix_orientation <- function(raster_obj) {
  # Check if the raster needs to be flipped
  # EURO-CORDEX data often has Y coordinates in descending order
  y_coords <- yFromRow(raster_obj, 1:nrow(raster_obj))

  if (is.unsorted(y_coords)) {
    cat("  Fixing orientation: flipping vertically\n")
    return(flip(raster_obj, direction = "vertical"))
  }
  return(raster_obj)
}

# Function to calculate mean for a set of periods with orientation fix
calculate_period_mean <- function(var, periods, scenario_name, rcm_model) {
  cat("  Processing", length(periods), "period(s) for", var, "in", scenario_name, "\n")

  rasters <- list()

  for(period in periods) {
    path <- construct_path(var, scenario_name, period, rcm_model)

    cat("    Checking:", basename(path), "\n")

    if(!file.exists(path)) {
      cat("    ✗ File not found\n")
      next
    }

    tryCatch({
      r <- rast(path)
      cat("    ✓ Loaded:", basename(path), "(", nlyr(r), "layers, ",
          dim(r)[1], "x", dim(r)[2], ")\n")

      # Fix orientation
      r <- fix_orientation(r)

      # Calculate mean for this period
      r_mean <- mean(r, na.rm = TRUE)
      cat("    ✓ Calculated mean\n")

      # Unit conversions
      if(var == "pr") {
        r_mean <- r_mean * 86400  # kg/m²/s to mm/day
        cat("    ✓ Converted to mm/day\n")
      } else if(var %in% c("tas", "tasmin", "tasmax")) {
        r_mean <- r_mean - 273.15  # K to °C
        cat("    ✓ Converted to °C\n")
      }

      rasters <- c(rasters, r_mean)

    }, error = function(e) {
      cat("    ✗ Error processing:", e$message, "\n")
    })
  }

  if(length(rasters) == 0) {
    cat("    ✗ No valid rasters\n")
    return(NULL)
  }

  # Combine all periods
  if(length(rasters) > 1) {
    combined <- rast(rasters)
    result <- mean(combined, na.rm = TRUE)
    cat("    ✓ Combined", length(rasters), "periods\n")
  } else {
    result <- rasters[[1]]
  }

  return(result)
}

# Function to save raster as NetCDF with proper metadata
save_as_netcdf <- function(raster_obj, filename, var_name, scenario,
                           time_period, description, units) {

  output_path <- file.path(output_dir, "netcdf", filename)

  # Set variable name
  names(raster_obj) <- var_name

  # Create metadata
  metadata <- list(
    title = paste("EURO-CORDEX Climate Projection:", description),
    institution = "Processed by EU-CORDEX Analysis Script",
    source = "EURO-CORDEX data from ESGF",
    references = "Jacob et al. (2014), Regional climate modeling",
    Conventions = "CF-1.7",
    scenario = scenario,
    variable = var_name,
    time_period = time_period,
    units = units,
    processing_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    note = "Map orientation has been corrected (flipped vertically if needed)"
  )

  # Write to NetCDF
  writeCDF(raster_obj, output_path,
           varname = var_name,
           longname = description,
           unit = units,
           overwrite = TRUE,
           zname = "time",
           prec = "float",
           compression = 5)

  cat("    ✓ Saved NetCDF:", basename(output_path), "\n")
  return(output_path)
}

# Function to save as GeoTIFF
save_as_geotiff <- function(raster_obj, filename) {
  output_path <- file.path(output_dir, "geotiff", filename)
  writeRaster(raster_obj, output_path, overwrite = TRUE)
  cat("    ✓ Saved GeoTIFF:", basename(output_path), "\n")
  return(output_path)
}

# Function to process a variable for a scenario
process_scenario_variable <- function(var, scenario_name) {
  scenario_info <- scenarios[[scenario_name]]

  cat("\n")
  cat(paste(rep("-", 60), collapse = ""), "\n")
  cat("Processing:", scenario_name, "-", var, "\n")
  cat(paste(rep("-", 60), collapse = ""), "\n")

  if(scenario_name == "historical") {
    # Historical baseline only
    rcm_model <- rcm_models[["historical"]][[var]]
    baseline <- calculate_period_mean(var, scenario_info$baseline_periods,
                                      scenario_name, rcm_model)

    if(is.null(baseline)) {
      cat("  ✗ Failed to calculate baseline\n")
      return(NULL)
    }

    # Save outputs
    base_desc <- paste("Historical baseline", var, "1991-2005")
    base_units <- ifelse(var == "pr", "mm/day",
                         ifelse(var %in% c("tas", "tasmin", "tasmax"), "°C", "W/m²"))

    # Save NetCDF
    nc_filename <- paste0(var, "_historical_baseline_1991_2005.nc")
    save_as_netcdf(baseline, nc_filename, var, "historical",
                   "1991-2005", base_desc, base_units)

    # Save GeoTIFF
    tif_filename <- paste0(var, "_historical_baseline_1991_2005.tif")
    save_as_geotiff(baseline, tif_filename)

    return(list(baseline = baseline, future = NULL,
                absolute = NULL, relative = NULL))
  } else {
    # RCP scenarios: baseline (from historical) and future
    cat("  Baseline (historical):\n")
    baseline_rcm <- rcm_models[["historical"]][[var]]
    baseline <- calculate_period_mean(var, scenario_info$baseline_periods,
                                      "historical", baseline_rcm)

    cat("  Future (", scenario_name, "):\n")
    future_rcm <- rcm_models[[scenario_name]][[var]]
    future <- calculate_period_mean(var, scenario_info$future_periods,
                                    scenario_name, future_rcm)

    if(is.null(baseline) || is.null(future)) {
      cat("  ✗ Missing data\n")
      return(NULL)
    }

    # Calculate difference
    absolute_diff <- future - baseline
    cat("  ✓ Calculated absolute difference\n")

    # For precipitation, also calculate relative change
    if(var == "pr") {
      # Avoid division by zero
      baseline_adj <- baseline
      baseline_adj[baseline_adj == 0] <- 0.01
      relative_diff <- (absolute_diff / baseline_adj) * 100
      cat("  ✓ Calculated relative difference (%)\n")
    } else {
      relative_diff <- NULL
    }

    # Save all outputs
    prefix <- paste0(var, "_", scenario_name)

    # 1. Baseline NetCDF
    base_desc <- paste("Historical baseline", var, "1991-2005 for", scenario_name)
    base_units <- ifelse(var == "pr", "mm/day",
                         ifelse(var %in% c("tas", "tasmin", "tasmax"), "°C", "W/m²"))
    save_as_netcdf(baseline, paste0(prefix, "_baseline_1991_2005.nc"),
                   var, scenario_name, "1991-2005", base_desc, base_units)

    # 2. Future NetCDF
    future_desc <- paste("Future", var, "2081-2100 for", scenario_name)
    save_as_netcdf(future, paste0(prefix, "_future_2081_2100.nc"),
                   var, scenario_name, "2081-2100", future_desc, base_units)

    # 3. Absolute change NetCDF
    abs_desc <- paste("Absolute change in", var, "2081-2100 vs 1991-2005 for", scenario_name)
    abs_units <- base_units
    save_as_netcdf(absolute_diff, paste0(prefix, "_change_absolute.nc"),
                   var, scenario_name, "2081-2100 vs 1991-2005", abs_desc, abs_units)

    # 4. Relative change NetCDF (for precipitation only)
    if(var == "pr" && !is.null(relative_diff)) {
      rel_desc <- paste("Relative change in precipitation (%) 2081-2100 vs 1991-2005 for", scenario_name)
      save_as_netcdf(relative_diff, paste0(prefix, "_change_relative.nc"),
                     "pr_rel", scenario_name, "2081-2100 vs 1991-2005", rel_desc, "%")
    }

    # Save GeoTIFFs
    save_as_geotiff(baseline, paste0(prefix, "_baseline_1991_2005.tif"))
    save_as_geotiff(future, paste0(prefix, "_future_2081_2100.tif"))
    save_as_geotiff(absolute_diff, paste0(prefix, "_change_absolute.tif"))
    if(var == "pr" && !is.null(relative_diff)) {
      save_as_geotiff(relative_diff, paste0(prefix, "_change_relative.tif"))
    }

    return(list(baseline = baseline, future = future,
                absolute = absolute_diff, relative = relative_diff))
  }
}

# ============================================================================
# SIMPLIFIED MAIN PROCESSING
# ============================================================================

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("STARTING PROCESSING\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

# Initialize results storage
results <- list()
processed_count <- 0
error_count <- 0
rcp_scenarios <- c("rcp26", "rcp45", "rcp85")

# Process historical baseline
cat("\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("PROCESSING HISTORICAL BASELINE (1991-2005)\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

results[["historical"]] <- list()

for(var in variables) {
  cat("\n--- Processing historical", var, "---\n")
  processed <- process_scenario_variable(var, "historical")

  if(!is.null(processed)) {
    results[["historical"]][[var]] <- processed
    processed_count <- processed_count + 1
    cat("  ✓ Successfully processed\n")
  } else {
    error_count <- error_count + 1
    cat("  ✗ Failed\n")
  }
}

# Process RCP scenarios
for(scenario in rcp_scenarios) {
  cat("\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("PROCESSING", toupper(scenario), "SCENARIO\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")

  results[[scenario]] <- list()

  for(var in variables) {
    cat("\n--- Processing", scenario, var, "---\n")
    processed <- process_scenario_variable(var, scenario)

    if(!is.null(processed)) {
      results[[scenario]][[var]] <- processed
      processed_count <- processed_count + 1
      cat("  ✓ Successfully processed\n")
    } else {
      error_count <- error_count + 1
      cat("  ✗ Failed\n")
    }
  }
}

# ============================================================================
# CREATE SUMMARY STATISTICS
# ============================================================================

cat("\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("CREATING SUMMARY STATISTICS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

summary_data <- data.frame()

for(scenario in rcp_scenarios) {
  for(var in variables) {
    if(!is.null(results[[scenario]][[var]])) {
      diff_raster <- results[[scenario]][[var]]$absolute

      stats <- data.frame(
        Scenario = scenario,
        Variable = var,
        Units = ifelse(var == "pr", "mm/day",
                       ifelse(var %in% c("tas", "tasmin", "tasmax"), "°C", "W/m²")),
        Mean = global(diff_raster, "mean", na.rm = TRUE)[1,1],
        SD = global(diff_raster, "sd", na.rm = TRUE)[1,1],
        Min = global(diff_raster, "min", na.rm = TRUE)[1,1],
        Max = global(diff_raster, "max", na.rm = TRUE)[1,1]
      )

      # For precipitation, add relative change statistics
      if(var == "pr") {
        rel_diff_raster <- results[[scenario]][[var]]$relative
        rel_stats <- data.frame(
          Scenario = scenario,
          Variable = "pr_rel",
          Units = "%",
          Mean = global(rel_diff_raster, "mean", na.rm = TRUE)[1,1],
          SD = global(rel_diff_raster, "sd", na.rm = TRUE)[1,1],
          Min = global(rel_diff_raster, "min", na.rm = TRUE)[1,1],
          Max = global(rel_diff_raster, "max", na.rm = TRUE)[1,1]
        )
        summary_data <- rbind(summary_data, stats, rel_stats)
      } else {
        summary_data <- rbind(summary_data, stats)
      }
    }
  }
}

# Save summary to CSV
summary_file <- file.path(output_dir, "statistics", "projected_changes_summary.csv")
write.csv(summary_data, summary_file, row.names = FALSE)
cat("\n✓ Summary statistics saved to: projected_changes_summary.csv\n")

# ============================================================================
# CREATE SIMPLE VISUALIZATIONS
# ============================================================================

cat("\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("CREATING VISUALIZATIONS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Create a simple summary plot
png(file.path(output_dir, "plots", "climate_changes_summary.png"),
    width = 2000, height = 1600, res = 150)

# Layout: 5 rows (variables) x 4 columns (historical baseline + 3 RCP changes)
par(mfrow = c(5, 4), mar = c(2, 2, 3, 1))

# Variable names
var_names <- c("Precipitation", "Mean Temp", "Min Temp", "Max Temp", "Radiation")
units <- c("mm/day", "°C", "°C", "°C", "W/m²")

for(var_idx in 1:length(variables)) {
  var <- variables[var_idx]
  var_name <- var_names[var_idx]
  unit <- units[var_idx]

  # Column 1: Historical baseline
  if(!is.null(results[["historical"]][[var]])) {
    plot(results[["historical"]][[var]]$baseline,
         main = paste(var_name, "\nHistorical Baseline"))
  } else {
    plot.new()
    text(0.5, 0.5, "No data", col = "red")
  }

  # Columns 2-4: RCP scenarios absolute changes
  for(scenario in rcp_scenarios) {
    if(!is.null(results[[scenario]][[var]])) {
      plot(results[[scenario]][[var]]$absolute,
           main = paste(var_name, "\n", toupper(scenario), "Change"))
    } else {
      plot.new()
      text(0.5, 0.5, "No data", col = "red")
    }
  }
}

dev.off()
cat("✓ Created summary visualization: climate_changes_summary.png\n")

# ============================================================================
# CREATE FINAL REPORT
# ============================================================================

cat("\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("CREATING FINAL REPORT\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Create processing report
sink(file.path(output_dir, "processing_report.txt"))
cat("EURO-CORDEX CLIMATE PROJECTION ANALYSIS - FINAL REPORT\n")
cat("=======================================================\n\n")
cat("Analysis completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Output directory:", output_dir, "\n\n")

cat("PROCESSING SUMMARY\n")
cat("------------------\n")
cat("Processed scenarios: historical, rcp26, rcp45, rcp85\n")
cat("Processed variables:", paste(variables, collapse = ", "), "\n")
cat("Successfully processed:", processed_count, "variable-scenario combinations\n")
cat("Errors:", error_count, "\n\n")

cat("OUTPUT FILES\n")
cat("------------\n")
cat("NetCDF files created in:", file.path(output_dir, "netcdf"), "\n")
cat("GeoTIFF files created in:", file.path(output_dir, "geotiff"), "\n")
cat("Plots created in:", file.path(output_dir, "plots"), "\n")
cat("Statistics saved in:", file.path(output_dir, "statistics"), "\n\n")

cat("PROJECTED CHANGES SUMMARY (2081-2100 vs 1991-2005)\n")
cat("--------------------------------------------------\n")
print(summary_data, row.names = FALSE)

cat("\nNOTES\n")
cat("-----\n")
cat("1. Map orientation has been automatically corrected\n")
cat("2. Precipitation converted from kg/m²/s to mm/day\n")
cat("3. Temperature converted from Kelvin to Celsius\n")
cat("4. Relative precipitation changes calculated as percentage\n")

cat("\n--- END OF REPORT ---\n")
sink()

cat("✓ Created processing report\n")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("✅ PROCESSING COMPLETE!\n")
cat(paste(rep("=", 80), collapse = ""), "\n")

cat("\nOutput directory:", output_dir, "\n")
cat("Total NetCDF files created:", length(list.files(file.path(output_dir, "netcdf"), pattern = "\\.nc$")), "\n")
cat("Total GeoTIFF files created:", length(list.files(file.path(output_dir, "geotiff"), pattern = "\\.tif$")), "\n")
cat("Analysis completed at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
