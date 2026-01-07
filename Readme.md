# SAFENET – Repository (EU-CORDEX, PFT/Land Use, Soils, Figures)

This repository contains the R scripts used for the **SAFENET** workflow to:
1) process **EU-CORDEX** climate projections,  
2) derive **land use / PFT** fractions and forest indicators from Hoffmann et al. products (ORCHIDEE-compatible),  
3) extract and package **soil** inputs (ISRIC / HWSD2), and  
4) generate SAFENET **deliverable figures**.

> **Important:** Several scripts use **hard-coded absolute paths** (e.g., `H:/...`, `E:/...`). Before running, update path variables near the top of each script (or refactor to a single `ROOT_DIR`).

---

## Repository scripts

### EU-CORDEX climate processing & plotting
- **`EU_CORDEX_PROCESS.R`**
  - Processes EU-CORDEX daily NetCDF for multiple variables/scenarios.
  - Produces baseline (1991–2005), future (2081–2100), and change layers (absolute; and **relative for precipitation**).
  - Unit conversions:
    - `pr`: kg m⁻² s⁻¹ → mm/day (× 86400)
    - `tas`, `tasmin`, `tasmax`: K → °C (− 273.15)
  - Exports GeoTIFF + NetCDF, summary stats (CSV), diagnostic plots, and a processing report.

- **`EU_CORDEX_PLOTTING.R`**
  - Produces publication-style panels from processed GeoTIFF outputs.
  - Assumes a “comparison” directory containing files named like:
    - `pr_historical_baseline_1991_2005.tif`
    - `tas_rcp45_change_absolute.tif`
    - `pr_rcp85_change_relative.tif`
  - Saves figures to a plotting output folder (e.g., `corrected_colors/`).

---

### Land use / PFT fractions (Hoffmann et al. 2021) & ORCHIDEE-format products
- **`LUC_fractions.R`**
  - Aggregates historical (1950–2015) and future (2016–2100) land-cover fractions over Europe.
  - Re-bins 16 PFTs into 7 categories: `Urban`, `Crops`, `Swamp`, `Tundra`, `Grass`, `Shrubs`, `Forest` (drops `Bare` and rescales to sum to 1).
  - Writes long/wide CSVs, summary stats, and stacked-area plots (PNG/PDF).

- **`pft_extractions_orchidee_dominant.R`**
  - Builds **dominant PFT** (index 1–16) NetCDF stacks for:
    - `historical_1950–2015`
    - `ssp119`, `ssp126`, `ssp245`, `ssp370`, `ssp585` (2016–2100)
  - Adds CF-style flag metadata.

- **`pft_extractions_orchidee_all_fracs_year_by_year.R`**
  - Exports **year-by-year** NetCDF files containing the **full 16-layer PFT fraction stack** for:
    - each historical year 1950–2015
    - each SSP year 2016–2100 (ssp119/126/245/370/585)

- **`PFT_FOREST_DATA_PROCESS.R`**
  - Uses dominant PFT outputs to compute forest indicators (area by forest PFT type, total forest area) for:
    - **EU27+UK**
    - **Europe (continent)**
  - Produces time series plots and deliverable CSV exports (e.g., total forest, afforestation, PFT areas).

---

### Soil extraction & packaging (ISRIC / HWSD2)
- **`hwsd_soil_extraction.R`**
  - Extracts HWSD2-derived soil properties over the target grid (e.g., GLOBIOM/BOKU points).
  - Produces consolidated text/CSV-style outputs (e.g., `soil_data_hwsd.txt`).

- **`isric_soil_extraction.R`**
  - Extracts ISRIC soil variables (multi-depth handling, optional depth-weighting).
  - Writes consolidated outputs (CSV-like) and may include intermediate raster logic.

- **`soil_data_processing.R`**
  - Utility processing for SAFENET soil inputs:
    1. cleans/repairs ISRIC CSV (including lon/lat swap if needed),
    2. builds ISRIC multi-variable NetCDF,
    3. cleans HWSD2 CSV and builds HWSD2 NetCDF,
    4. quick multi-panel plotting for QA.

---

### SAFENET deliverable figures / visuals
- **`fig1.R`** – Global temperature time series + SSP projections (e.g., `global_temperature_final_corrected.png`)
- **`fig2.R`** – SSP framework schematic (e.g., `ssp_framework_minimalist.png`)
- **`fig4.R`** – CO₂ concentration trajectories (plot creation; add `ggsave()` if needed)
- **`simulation_extend.R`** – SAFENET Europe modelling extent map 

---

## Recommended run order

A typical end-to-end sequence (adjust to your needs):

1. **EU-CORDEX processing**  
   Run `EU_CORDEX_PROCESS.R` to generate baseline/future/change outputs (NetCDF + GeoTIFF).

2. **EU-CORDEX plotting**  
   Ensure GeoTIFFs are located in the directory expected by `EU_CORDEX_PLOTTING.R` (or edit the path in the script), then run `EU_CORDEX_PLOTTING.R`.

3. **Land cover fractions**  
   Run `LUC_fractions.R` to generate Europe-wide category fractions (1950–2100) + CSV + plots.

4. **PFT products**  
   Run `pft_extractions_orchidee_dominant.R` (dominant PFT stacks).  
   Optional: `pft_extractions_orchidee_all_fracs_year_by_year.R` if you need per-year full fraction files.

5. **Forest indicators**  
   Run `PFT_FOREST_DATA_PROCESS.R` to compute and export forest-area and afforestation indicators.

6. **Soils**  
   Run `hwsd_soil_extraction.R` and/or `isric_soil_extraction.R`, then `soil_data_processing.R` to standardize and package NetCDF soil inputs.

7. **Static figures**  
   Run `fig1.R`, `fig2.R`, `fig4.R`, `simulation_extend.R` as required for deliverables.

---

## Requirements

Recommended environment:
- **R ≥ 4.2**
- Common packages used across scripts:
  - `terra`, `sf`, `ncdf4`, `rnaturalearth`
  - `ggplot2`, `patchwork`, `RColorBrewer`, `viridis`, `viridisLite`
  - `dplyr`, `tidyr`, `tidyverse`, `data.table`, `reshape2`, `gridExtra`

Install example:

```r
install.packages(c(
  "terra","sf","ncdf4","rnaturalearth",
  "ggplot2","patchwork","RColorBrewer","viridis","viridisLite",
  "dplyr","tidyr","tidyverse","data.table","reshape2","gridExtra"
))
```

---

## Notes

- **Path portability:** Consider defining a single `ROOT_DIR` and replacing hard-coded drive paths with `file.path(ROOT_DIR, ...)`.
- **Performance:** Year-by-year PFT exports (1950–2100 × multiple SSPs) are I/O heavy; prefer fast local storage.
- **Units:** Ensure conversions are applied once (the processing script already converts key climate variables).

---
---

## Creator / attribution

- **Creator:** Dr. Andre Nakhavali  
- **Institute:** IIASA (International Institute for Applied Systems Analysis)  
- **Project:** SAFENET  
  - Funded by the European Union under the Horizon Europe Programme  
  - **Grant Agreement No. 101181981 (SafeNet)**


## Included files (as provided)

- `EU_CORDEX_PROCESS.R`
- `EU_CORDEX_PLOTTING.R`
- `LUC_fractions.R`
- `pft_extractions_orchidee_dominant.R`
- `pft_extractions_orchidee_all_fracs_year_by_year.R`
- `PFT_FOREST_DATA_PROCESS.R`
- `hwsd_soil_extraction.R`
- `isric_soil_extraction.R`
- `soil_data_processing.R`
- `simulation_extend.R`
- `fig1.R`
- `fig2.R`
- `fig4.R`
