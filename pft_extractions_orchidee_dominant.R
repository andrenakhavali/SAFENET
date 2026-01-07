# Load required libraries
library(terra)
library(ncdf4)

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
    # Try alternative pattern if first doesn't work
    pfts <- grep(paste0("landCoverFrac_lctype=\\d+.*_", idx, "$"), names(nc), value = TRUE)
  }
  if (length(pfts) == 0) stop("No PFT variables found for year ", year, " in file ", row$file[1])
  pft_numbers <- as.integer(sub("landCoverFrac_lctype=(\\d+).*", "\\1", pfts))
  pfts_ordered <- pfts[order(pft_numbers)]
  nc[[pfts_ordered]]
}

add_pft_attributes <- function(nc_file) {
  nc <- nc_open(nc_file, write = TRUE)
  if (!"dominant_pft" %in% names(nc$var)) {
    warning("Variable 'dominant_pft' not found in ", nc_file)
    nc_close(nc)
    return()
  }

  pft_names <- c(
    "TropBroadEvergreen", "TropDeciduous", "TempBroadEvergreen", "TempDeciduous",
    "EvergreenConifer", "DeciduousConifer", "ConiferShrubs", "DeciduousShrubs",
    "C3Grass", "C4Grass", "Tundra", "Swamp",
    "NonIrrCrops", "IrrCrops", "Urban", "Bare"
  )

  # Add flag values and meanings
  ncatt_put(nc, "dominant_pft", "flag_values", 1:16, prec = "integer")
  ncatt_put(nc, "dominant_pft", "flag_meanings", paste(pft_names, collapse = " "))

  # Add grid mapping information
  if ("crs" %in% names(nc$var)) {
    ncatt_put(nc, "dominant_pft", "grid_mapping", "crs")
    ncatt_put(nc, "crs", "crs_wkt", 'GEOGCRS["WGS 84 (CRS84)", DATUM["World Geodetic System 1984", ELLIPSOID["WGS 84",6378137,298.257223563, LENGTHUNIT["metre",1]]], PRIMEM["Greenwich",0, ANGLEUNIT["degree",0.0174532925199433]], CS[ellipsoidal,2], AXIS["geodetic longitude (Lon)",east, ORDER[1], ANGLEUNIT["degree",0.0174532925199433]], AXIS["geodetic latitude (Lat)",north, ORDER[2], ANGLEUNIT["degree",0.0174532925199433]], USAGE[SCOPE["unknown"], AREA["World"], BBOX[-90,-180,90,180]], ID["OGC","CRS84"]]')
    ncatt_put(nc, "crs", "spatial_ref", 'GEOGCRS["WGS 84 (CRS84)", DATUM["World Geodetic System 1984", ELLIPSOID["WGS 84",6378137,298.257223563, LENGTHUNIT["metre",1]]], PRIMEM["Greenwich",0, ANGLEUNIT["degree",0.0174532925199433]], CS[ellipsoidal,2], AXIS["geodetic longitude (Lon)",east, ORDER[1], ANGLEUNIT["degree",0.0174532925199433]], AXIS["geodetic latitude (Lat)",north, ORDER[2], ANGLEUNIT["degree",0.0174532925199433]], USAGE[SCOPE["unknown"], AREA["World"], BBOX[-90,-180,90,180]], ID["OGC","CRS84"]]')
    ncatt_put(nc, "crs", "proj4", "+proj=longlat +datum=WGS84 +no_defs")
    ncatt_put(nc, "crs", "code", "OGC:CRS84")
  }

  # Add global attributes
  ncatt_put(nc, 0, "title", "Dominant Plant Functional Types")
  ncatt_put(nc, 0, "source", "Hoffmann et al. 2021 Data")
  ncatt_put(nc, 0, "institution", "IIASA")
  ncatt_put(nc, 0, "created_with", paste0("R terra package v", packageVersion("terra")))
  ncatt_put(nc, 0, "Conventions", "CF-1.4")
  ncatt_put(nc, 0, "created_date", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

  nc_close(nc)
}

# Create output directory
out_dir <- "H:/SAFENET/PFT"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Define PFT names
pft_names <- c(
  "TropBroadEvergreen", "TropDeciduous", "TempBroadEvergreen", "TempDeciduous",
  "EvergreenConifer", "DeciduousConifer", "ConiferShrubs", "DeciduousShrubs",
  "C3Grass", "C4Grass", "Tundra", "Swamp",
  "NonIrrCrops", "IrrCrops", "Urban", "Bare"
)

# Process historical data (1950-2015)
hist_chunks <- get_hist_chunks()
hist_years <- 1950:2015

# Create template from first year
first_stack <- read_all_pfts_for_year(1950, hist_chunks)
dom_stack_hist <- rast(nlyr = length(hist_years),
                       extent = ext(first_stack),
                       crs = crs(first_stack),
                       resolution = res(first_stack))
time(dom_stack_hist) <- as.Date(paste0(hist_years, "-01-01"))
names(dom_stack_hist) <- paste0("Year_", hist_years)

# Process each historical year
for (i in seq_along(hist_years)) {
  yr <- hist_years[i]
  cat("Processing historical year", yr, "\n")
  pft_stack <- read_all_pfts_for_year(yr, hist_chunks)
  dom_layer <- which.max(pft_stack)
  dom_stack_hist[[i]] <- dom_layer
}

# Save historical output with better metadata
out_file_hist <- file.path(out_dir, "dominant_pft_historical_1950-2015.nc")
writeCDF(dom_stack_hist, out_file_hist, varname = "dominant_pft",
         longname = "Dominant Plant Functional Type index (1-16)",
         unit = "index", overwrite = TRUE,
         missval = -1.175494e-38, # Standard missing value
         prec = "float")
add_pft_attributes(out_file_hist)

# Process future scenarios
ssp_codes <- c("119", "126", "245", "370", "585")
fut_years <- 2016:2100

for (ssp in ssp_codes) {
  chunks <- get_fut_chunks_for_ssp(ssp)

  # Create template from first year
  first_stack <- read_all_pfts_for_year(2016, chunks)
  dom_stack_fut <- rast(nlyr = length(fut_years),
                        extent = ext(first_stack),
                        crs = crs(first_stack),
                        resolution = res(first_stack))
  time(dom_stack_fut) <- as.Date(paste0(fut_years, "-01-01"))
  names(dom_stack_fut) <- paste0("Year_", fut_years)

  # Process each future year
  for (i in seq_along(fut_years)) {
    yr <- fut_years[i]
    cat("Processing SSP", ssp, "year", yr, "\n")
    pft_stack <- read_all_pfts_for_year(yr, chunks)
    dom_layer <- which.max(pft_stack)
    dom_stack_fut[[i]] <- dom_layer
  }

  # Save output with better metadata
  out_file_ssp <- file.path(out_dir, paste0("dominant_pft_ssp", ssp, "_2016-2100.nc"))
  writeCDF(dom_stack_fut, out_file_ssp, varname = "dominant_pft",
           longname = "Dominant Plant Functional Type index (1-16)",
           unit = "index", overwrite = TRUE,
           missval = -1.175494e-38, # Standard missing value
           prec = "float")
  add_pft_attributes(out_file_ssp)
}

message("Processing complete! Files saved in: ", out_dir)
