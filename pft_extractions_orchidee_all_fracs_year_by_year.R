# Load required libraries
library(terra)

# Define helper functions
get_hist_chunks <- function() {
  base_dir <- "H:/SAFENET/Hoffmann et al 2021 Data"
  dir_path <- file.path(base_dir, "LUC_hist_EU_afts_v1.1_1-7")
  files <- list.files(dir_path, pattern = "LUCAS_LUC_v1.1_historical_Europe_0.1deg_\\d{4}_\\d{4}\\.nc$", full.names = TRUE)
  yrs <- sub(".*_(\\d{4})_(\\d{4})\\.nc$", "\\1_\\2", basename(files))
  df <- data.frame(file = files, years = yrs, stringsAsFactors = FALSE)
  sp <- strsplit(df$years, "_")
  df$start_year <- as.integer(sapply(sp, `[`, 1))
  df$end_year <- as.integer(sapply(sp, `[`, 2))
  df[order(df$start_year), ]
}

get_fut_chunks_for_ssp <- function(ssp_code) {
  base_dir <- "H:/SAFENET/Hoffmann et al 2021 Data"
  dir_path <- file.path(base_dir, sprintf("LUC_future_EU_ssp%s_v1.1_1-9", ssp_code))
  files <- list.files(dir_path, pattern = paste0("ssp", ssp_code, ".*\\.nc$"), full.names = TRUE)
  yrs <- sub(".*_(\\d{4}_\\d{4})\\.nc$", "\\1", basename(files))
  df <- data.frame(file = files, years = yrs, stringsAsFactors = FALSE)
  sp <- strsplit(df$years, "_")
  df$start_year <- as.integer(sapply(sp, `[`, 1))
  df$end_year <- as.integer(sapply(sp, `[`, 2))
  df[order(df$start_year), ]
}

read_all_pfts_for_year <- function(year, chunks) {
  row <- chunks[chunks$start_year <= year & chunks$end_year >= year, ]
  if (nrow(row) == 0) stop("No data found for year ", year)
  nc <- rast(row$file[1])
  idx <- year - row$start_year[1] + 1
  pfts <- grep(paste0("landCoverFrac_lctype=\\d+_", idx, "$"), names(nc), value = TRUE)
  if (length(pfts) == 0) {
    pfts <- grep(paste0("landCoverFrac_lctype=\\d+.*_", idx, "$"), names(nc), value = TRUE)
  }
  if (length(pfts) == 0) stop("No PFT variables found for year ", year, " in file ", row$file[1])
  pft_numbers <- as.integer(sub("landCoverFrac_lctype=(\\d+).*", "\\1", pfts))
  pfts_ordered <- pfts[order(pft_numbers)]
  nc[[pfts_ordered]]
}

# Create output directory
out_dir <- "E:/SAFENET/PFT_fractions"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Define original Hoffmann PFT names
hoffmann_pft_names <- c(
  "TropBroadEvergreen", "TropDeciduous", "TempBroadEvergreen", "TempDeciduous",
  "EvergreenConifer", "DeciduousConifer", "ConiferShrubs", "DeciduousShrubs",
  "C3Grass", "C4Grass", "Tundra", "Swamp",
  "NonIrrCrops", "IrrCrops", "Urban", "Bare"
)

# Process historical data (1950-2015)
cat("=== PROCESSING HISTORICAL DATA (1950-2015) ===\n")
hist_chunks <- get_hist_chunks()
hist_years <- 1950:2015

# Create subdirectory for historical data
hist_dir <- file.path(out_dir, "historical")
dir.create(hist_dir, recursive = TRUE, showWarnings = FALSE)

# Process and save each historical year individually
for (i in seq_along(hist_years)) {
  yr <- hist_years[i]
  cat("Processing historical year", yr, "(", i, "/", length(hist_years), ")...\n")

  # Read PFT data for this year
  pft_stack <- read_all_pfts_for_year(yr, hist_chunks)
  names(pft_stack) <- hoffmann_pft_names

  # Save as NetCDF for this year
  output_file <- file.path(hist_dir, paste0("pft_fractions_", yr, ".nc"))

  writeCDF(pft_stack,
           filename = output_file,
           varname = "landCoverFrac",
           longname = paste("Hoffmann PFT fractions for year", yr),
           unit = "fraction",
           overwrite = TRUE)

  cat("✓ Saved:", basename(output_file), "\n")
}

# Process future scenarios
cat("\n=== PROCESSING FUTURE SCENARIOS ===\n")
ssp_codes <- c("119", "126", "245", "370", "585")
fut_years <- 2016:2100

for (ssp in ssp_codes) {
  cat("Processing SSP", ssp, "...\n")

  # Create subdirectory for this SSP
  ssp_dir <- file.path(out_dir, paste0("ssp", ssp))
  dir.create(ssp_dir, recursive = TRUE, showWarnings = FALSE)

  # Get future chunks
  chunks <- get_fut_chunks_for_ssp(ssp)

  # Process and save each future year individually
  for (i in seq_along(fut_years)) {
    yr <- fut_years[i]
    cat("Processing SSP", ssp, "year", yr, "(", i, "/", length(fut_years), ")...\n")

    # Read PFT data for this year
    pft_stack <- read_all_pfts_for_year(yr, chunks)
    names(pft_stack) <- hoffmann_pft_names

    # Save as NetCDF for this year
    output_file <- file.path(ssp_dir, paste0("pft_fractions_ssp", ssp, "_", yr, ".nc"))

    writeCDF(pft_stack,
             filename = output_file,
             varname = "landCoverFrac",
             longname = paste("Hoffmann PFT fractions for SSP", ssp, "year", yr),
             unit = "fraction",
             overwrite = TRUE)

    cat("✓ Saved:", basename(output_file), "\n")
  }
}

# Final summary
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("PROCESSING COMPLETE\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Count created files
hist_files <- list.files(hist_dir, pattern = "\\.nc$", full.names = TRUE)
cat("Historical files created:", length(hist_files), "/", length(hist_years), "\n")

for (ssp in ssp_codes) {
  ssp_dir <- file.path(out_dir, paste0("ssp", ssp))
  ssp_files <- list.files(ssp_dir, pattern = "\\.nc$", full.names = TRUE)
  cat("SSP", ssp, "files created:", length(ssp_files), "/", length(fut_years), "\n")
}

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("FILE STRUCTURE:\n")
cat("Output directory:", out_dir, "\n")
cat("├── historical/\n")
cat("│   ├── pft_fractions_1950.nc (16 PFT layers)\n")
cat("│   ├── pft_fractions_1951.nc (16 PFT layers)\n")
cat("│   └── ... (1950-2015)\n")
for (ssp in ssp_codes) {
  cat("├── ssp", ssp, "/\n", sep = "")
  cat("│   ├── pft_fractions_ssp", ssp, "_2016.nc (16 PFT layers)\n", sep = "")
  cat("│   ├── pft_fractions_ssp", ssp, "_2017.nc (16 PFT layers)\n", sep = "")
  cat("│   └── ... (2016-2100)\n")
}

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("DATA SUMMARY:\n")
cat("Each NetCDF file contains 16 layers (one for each PFT):\n")
for (i in 1:16) {
  cat("  - Layer", i, ":", hoffmann_pft_names[i], "\n")
}
cat("\nFile format: NetCDF with CF-compliant metadata\n")
cat("Variable name: landCoverFrac\n")
cat("Units: fraction (0-1)\n")

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("SUCCESS! Yearly PFT fraction files created.\n")
cat("Ready for use with ORCHIDEE model.\n")
