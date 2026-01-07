library(data.table)

# 1) Load the original file
src_path <- "H:/lightning_density_rasters/FOREST_NAVIGATOR_isric_grd5_soil_data.csv"
x <- fread(src_path)

# 2) Basic checks (fail fast with clear errors)
stopifnot(all(c("ID", "PlgId", "longitude", "latitude") %in% names(x)))

# 3) Swap longitude/latitude (CSV had them flipped for your EU use case)
tmp <- x$longitude
x$longitude <- x$latitude
x$latitude  <- tmp
rm(tmp)

# 4) Drop ID & PlgId
x[, c("ID", "PlgId") := NULL]

# 5) (Optional) put coords first for convenience
setcolorder(x, c("longitude", "latitude", setdiff(names(x), c("longitude", "latitude"))))

# 6) Write corrected file
out_path <- "H:/Safenet/ISRIC_soil_data.csv"
fwrite(x, out_path)

# 7) Quick confirmation of extents after the swap
rng_lon <- range(x$longitude, na.rm = TRUE)
rng_lat <- range(x$latitude,  na.rm = TRUE)
message(
  "Saved: ", out_path, "\n",
  "Post-swap extents — lon: [", sprintf("%.2f", rng_lon[1]), ", ", sprintf("%.2f", rng_lon[2]),
  "]  lat: [", sprintf("%.2f", rng_lat[1]), ", ", sprintf("%.2f", rng_lat[2]), "]"
)


# Create true multi-variable NetCDF from ISRIC soil data ----------------
library(data.table)
library(terra)
library(ncdf4)

# 1) Load data ----------------------------------------------------------
csv_path <- "H:/Safenet/ISRIC_soil_data.csv"
dt <- fread(csv_path)
dt <- dt[!is.na(longitude) & !is.na(latitude)]

# 2) Define grid and variables -----------------------------------------
lon_vals <- sort(unique(dt$longitude))
lat_vals <- sort(unique(dt$latitude))
nx <- length(lon_vals)
ny <- length(lat_vals)

# Confirm regular grid
dx <- diff(lon_vals)[1]
dy <- diff(lat_vals)[1]
message("Grid size: ", nx, " x ", ny, "  |  Δlon=", dx, ", Δlat=", dy)

# 3) Reshape each variable to matrix (longitude x latitude)
var_names <- setdiff(names(dt), c("longitude", "latitude"))
var_list <- list()

for (v in var_names) {
  tmp <- dt[, .(longitude, latitude, val = get(v))]
  m <- matrix(NA_real_, nrow = nx, ncol = ny)
  for (i in seq_len(nrow(tmp))) {
    ix <- match(tmp$longitude[i], lon_vals)
    iy <- match(tmp$latitude[i], lat_vals)
    m[ix, iy] <- tmp$val[i]
  }
  var_list[[v]] <- m
}

# 4) Define NetCDF dimensions ------------------------------------------
lon_dim <- ncdim_def("lon", "degrees_east", vals = lon_vals)
lat_dim <- ncdim_def("lat", "degrees_north", vals = lat_vals)

# 5) Define each variable ----------------------------------------------
nc_vars <- lapply(names(var_list), function(v) {
  ncvar_def(
    name  = v,
    units = switch(v,
                   clay   = "%",
                   silt   = "%",
                   sand   = "%",
                   soC    = "g/kg",
                   N      = "g/kg",
                   AWC    = "mm",
                   pHiHXO = "pH",
                   pHiKCl = "pH",
                   "unknown"),
    dim   = list(lon_dim, lat_dim),
    missval = NA,
    longname = v,
    prec  = "float"
  )
})

# 6) Create and write NetCDF -------------------------------------------
nc_out <- "H:/Safenet/ISRIC_soil_Safenet.nc"
nc <- nc_create(nc_out, nc_vars)

for (v in names(var_list)) {
  ncvar_put(nc, v, var_list[[v]])
}

# Add global attributes
ncatt_put(nc, 0, "title", "ISRIC Soil Data")
ncatt_put(nc, 0, "source", "ISRIC soil data for SAFENET project")
ncatt_put(nc, 0, "crs", "EPSG:4326")
ncatt_put(nc, 0, "Conventions", "CF-1.8")

nc_close(nc)

message("✅ Multi-variable NetCDF written to: ", nc_out)



# HWSD2 -> clean CSV -> multi-variable NetCDF ----------------------------
library(data.table)
library(ncdf4)

# --- 1) Load ------------------------------------------------------------
src_path <- "H:/Condor_run_Chelsa_ECA/tests/HWSD2_soil_extract/FOREST_NAVIGATOR_hwsd2_grd5_soil_data.csv"
dt <- fread(src_path)

# --- 2) Drop ID-like columns, standardize coord names -------------------
id_like <- intersect(names(dt), c("ID", "PlgId", "V1", "V2", "index"))
if (length(id_like)) dt[, (id_like) := NULL]

# Ensure we have longitude/latitude columns (your file already has these)
stopifnot(all(c("longitude", "latitude") %in% names(dt)))

# --- 3) Auto-detect lon/lat swap (fix if needed) ------------------------
# Expect EU-ish bounds: lon ≈ [-25, 40], lat ≈ [34, 72]
rng_lon <- range(dt$longitude, na.rm = TRUE)
rng_lat <- range(dt$latitude,  na.rm = TRUE)
if (rng_lon[1] > 0 && rng_lon[2] > 60 && rng_lat[1] < 0) {
  tmp <- dt$longitude
  dt$longitude <- dt$latitude
  dt$latitude  <- tmp
}

# Keep only rows with valid coords
dt <- dt[!is.na(longitude) & !is.na(latitude)]

# Put coords first for convenience
setcolorder(dt, c("longitude", "latitude", setdiff(names(dt), c("longitude", "latitude"))))

# --- 4) Write clean CSV -------------------------------------------------
csv_out <- "H:/Safenet/HWSD2_soil_data.csv"
fwrite(dt, csv_out)

# --- 5) Build true multi-variable NetCDF -------------------------------
# Grid vectors (sorted unique coords)
lon_vals <- sort(unique(dt$longitude))
lat_vals <- sort(unique(dt$latitude))
nx <- length(lon_vals); ny <- length(lat_vals)

# Dimensions
lon_dim <- ncdim_def("lon", "degrees_east",  vals = lon_vals)
lat_dim <- ncdim_def("lat", "degrees_north", vals = lat_vals)

# Data columns (everything except coords)
data_cols <- setdiff(names(dt), c("longitude", "latitude"))
stopifnot(length(data_cols) > 0)

# Helper: units by variable name (fallback "unknown")
var_units <- function(v) {
  switch(tolower(v),
         clay   = "%",
         silt   = "%",
         sand   = "%",
         soc    = "g/kg",
         n      = "g/kg",
         awc    = "mm")
}

# Define NC variables
nc_vars <- lapply(data_cols, function(v) {
  ncvar_def(
    name     = v,
    units    = var_units(v),
    dim      = list(lon_dim, lat_dim),
    missval  = NA_real_,
    longname = v,
    prec     = "float"
  )
})

# Create file
nc_out <- "H:/Safenet/HWSD2_soil_Safenet.nc"
nc <- nc_create(nc_out, nc_vars)

# Efficient write: fill matrices via vectorized indexing
for (v in data_cols) {
  tmp <- dt[, .(longitude, latitude, val = get(v))]
  # Map to grid indices
  ix <- match(tmp$longitude, lon_vals)
  iy <- match(tmp$latitude,  lat_vals)
  # Pre-allocated matrix [lon, lat]
  m <- matrix(NA_real_, nrow = nx, ncol = ny)
  m[cbind(ix, iy)] <- tmp$val
  # Write to NetCDF variable
  ncvar_put(nc, v, m)
}

# Global metadata
ncatt_put(nc, 0, "title",       "HWSD2 Soil Data ")
ncatt_put(nc, 0, "source",      "HWSD2 Soil Data for Safenet proejct ")
ncatt_put(nc, 0, "crs",         "EPSG:4326")
ncatt_put(nc, 0, "Conventions", "CF-1.8")

nc_close(nc)

message(
  "Clean CSV: ", csv_out, "\n",
  "NetCDF:    ", nc_out,  "\n",
  "Grid: nx=", nx, " (lon), ny=", ny, " (lat)"
)



# Plot all variables from a NetCDF in one window -------------------------
library(terra)
library(ncdf4)
library(viridisLite)

# Helper: read all 2D lon/lat variables from a NetCDF into one SpatRaster
read_all_vars_rast <- function(nc_path) {
  # Try fast path: if rast() already returns multiple layers, use it
  r_try <- try(rast(nc_path), silent = TRUE)
  if (!inherits(r_try, "try-error") && nlyr(r_try) > 1) {
    return(r_try)
  }

  # Fallback: list vars with ncdf4, load each as file:var, then stack
  nc <- nc_open(nc_path)
  on.exit(nc_close(nc), add = TRUE)

  # keep 2D variables on lon/lat (or longitude/latitude)
  is_lon <- function(x) grepl("^lon(gitude)?$", x, ignore.case = TRUE)
  is_lat <- function(x) grepl("^lat(itude)?$", x, ignore.case = TRUE)

  vars <- names(nc$var)
  vars2d <- vapply(vars, function(v) {
    dn <- vapply(nc$var[[v]]$dim, `[[`, "", "name")
    length(dn) == 2 && any(is_lon(dn)) && any(is_lat(dn))
  }, logical(1))

  v_ok <- vars[vars2d]
  if (!length(v_ok)) stop("No 2D lon/lat variables found in: ", nc_path)

  r_list <- lapply(v_ok, function(v) {
    r <- rast(paste0(nc_path, ":", v))
    names(r) <- v
    r
  })
  do.call(c, r_list)
}

# Helper: multi-panel plot for a SpatRaster (one layer per panel)
plot_all_layers <- function(r, ncol = 3, palette = viridisLite::magma(256),
                            crop_europe = TRUE, add_axes = TRUE) {
  if (crop_europe) {
    # bbox for Europe (lon: -25..52, lat: 34..81.5 as in your HWSD2 example)
    r <- crop(r, ext(-25, 52, 34, 81.5))
  }

  nl <- nlyr(r)
  nrow <- ceiling(nl / ncol)

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  par(mfrow = c(nrow, ncol), mar = c(3, 3, 3, 2))

  u <- try(units(r), silent = TRUE)
  has_units <- !inherits(u, "try-error") && length(u) == nl

  for (i in seq_len(nl)) {
    ttl <- names(r)[i]
    if (has_units && nzchar(u[i])) ttl <- paste0(ttl, " [", u[i], "]")
    plot(r[[i]], col = palette, main = ttl, axes = add_axes)
  }

  invisible(r)
}

# ---------------- HWSD2: read with rast() and plot ----------------------
hwsd_nc <- "H:/Safenet/HWSD2_soil_Safenet.nc"   # adjust if different
x_hwsd  <- read_all_vars_rast(hwsd_nc)


plot_all_layers(x_hwsd, ncol = 3)

# ---------------- ISRIC: read with rast() and plot ----------------------
isric_nc <- "H:/Safenet/ISRIC_soil_Safenet.nc"
x_isric  <- read_all_vars_rast(isric_nc)

plot_all_layers(x_isric, ncol = 3)

