# Full Script: Historical (1950–2015) + Future (2016–2100) Aggregation,

# 1) LOAD LIBRARIES
library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)

# 2) DEFINE PFT IDs AND ORIGINAL FULL NAMES
pft_ids   <- 1:16
pft_names <- c(
  "Tropical broadleaf evergreen trees",  # PFT 1
  "Tropical deciduous trees",            # PFT 2
  "Temperate broadleaf evergreen trees",  # PFT 3
  "Temperate deciduous trees",            # PFT 4
  "Evergreen coniferous trees",           # PFT 5
  "Deciduous coniferous trees",           # PFT 6
  "Coniferous shrubs",                    # PFT 7
  "Deciduous shrubs",                     # PFT 8
  "C3 grass",                              # PFT 9
  "C4 grass",                              # PFT 10
  "Tundra",                                # PFT 11
  "Swamp",                                 # PFT 12
  "Non-irrigated crops",                   # PFT 13
  "Irrigated crops",                       # PFT 14
  "Urban",                                 # PFT 15
  "Bare"                                   # PFT 16
)

# 3) HISTORICAL FILES (1950–2015)
hist_files  <- c(
  "H:/SAFENET/Hoffmann et al 2021 Data/LUC_hist_EU_afts_v1.1_1-7/LUCAS_LUC_v1.1_historical_Europe_0.1deg_1950_1959.nc",
  "H:/SAFENET/Hoffmann et al 2021 Data/LUC_hist_EU_afts_v1.1_1-7/LUCAS_LUC_v1.1_historical_Europe_0.1deg_1960_1969.nc",
  "H:/SAFENET/Hoffmann et al 2021 Data/LUC_hist_EU_afts_v1.1_1-7/LUCAS_LUC_v1.1_historical_Europe_0.1deg_1970_1979.nc",
  "H:/SAFENET/Hoffmann et al 2021 Data/LUC_hist_EU_afts_v1.1_1-7/LUCAS_LUC_v1.1_historical_Europe_0.1deg_1980_1989.nc",
  "H:/SAFENET/Hoffmann et al 2021 Data/LUC_hist_EU_afts_v1.1_1-7/LUCAS_LUC_v1.1_historical_Europe_0.1deg_1990_1999.nc",
  "H:/SAFENET/Hoffmann et al 2021 Data/LUC_hist_EU_afts_v1.1_1-7/LUCAS_LUC_v1.1_historical_Europe_0.1deg_2000_2009.nc",
  "H:/SAFENET/Hoffmann et al 2021 Data/LUC_hist_EU_afts_v1.1_1-7/LUCAS_LUC_v1.1_historical_Europe_0.1deg_2010_2015.nc"
)
start_years <- c(1950, 1960, 1970, 1980, 1990, 2000, 2010)
end_years   <- c(1959, 1969, 1979, 1989, 1999, 2009, 2015)

# 4) READ & AGGREGATE HISTORICAL INTO PFT AREAS
hist_rows <- list()

for (i in seq_along(hist_files)) {
  file_path <- hist_files[i]
  sy <- start_years[i]; ey <- end_years[i]
  if (!file.exists(file_path)) stop("File not found: ", file_path)
  r <- rast(file_path)
  cell_area_km2 <- terra::cellSize(r[[1]], unit = "km")

  for (yr in sy:ey) {
    idx <- yr - sy + 1
    for (pid in pft_ids) {
      patt <- paste0("landCoverFrac_lctype=", pid, "_", idx, "$")
      layname <- grep(patt, names(r), value = TRUE)
      frac_r <- r[[layname]]
      area_km2 <- as.numeric(global(frac_r * cell_area_km2, "sum", na.rm = TRUE))
      hist_rows[[length(hist_rows)+1]] <- data.frame(
        Scenario = "historical",
        Year     = yr,
        PFT      = factor(paste0("PFT", pid), levels = paste0("PFT", pft_ids)),
        Area     = area_km2,
        stringsAsFactors = FALSE
      )
    }
  }
}

df_hist <- bind_rows(hist_rows)
levels(df_hist$PFT) <- pft_names

# 5) FUTURE FILES (2016–2100) FOR EACH SSP–RCP
ssp_codes <- c("126","245","585")
rcp_map <- c(
  "126" = "SSP1/RCP2.6",
  "245" = "SSP2/RCP4.5",
  "585" = "SSP5/RCP8.5"
)

start_fut <- seq(2016, 2096, by = 10)
end_fut   <- c(start_fut[-length(start_fut)] + 9, 2100)

fut_rows <- list()

for (j in seq_along(ssp_codes)) {
  ssp <- ssp_codes[j]
  scen <- rcp_map[[ssp]]
  dir_path <- file.path(
    "H:/SAFENET/Hoffmann et al 2021 Data",
    sprintf("LUC_future_EU_ssp%s_v1.1_1-9", ssp)
  )
  nc_list <- list.files(
    dir_path,
    pattern    = paste0("ssp", ssp, "_Europe_0\\.1deg_.*\\.nc$"),
    full.names = TRUE
  )
  yrs_order <- as.integer(sub(".*_0\\.1deg_([0-9]{4})_[0-9]{4}\\.nc$", "\\1", nc_list))
  nc_list   <- nc_list[order(yrs_order)]

  for (k in seq_along(nc_list)) {
    r <- rast(nc_list[k])
    cell_area_km2 <- terra::cellSize(r[[1]], unit = "km")
    sy <- start_fut[k]; ey <- end_fut[k]

    for (yr in sy:ey) {
      idx <- yr - sy + 1
      for (pid in pft_ids) {
        patt <- paste0("landCoverFrac_lctype=", pid, "_", idx, "$")
        layname <- grep(patt, names(r), value = TRUE)
        frac_r <- r[[layname]]
        area_km2 <- as.numeric(global(frac_r * cell_area_km2, "sum", na.rm = TRUE))
        fut_rows[[length(fut_rows)+1]] <- data.frame(
          Scenario = scen,
          Year     = yr,
          PFT      = factor(paste0("PFT", pid), levels = paste0("PFT", pft_ids)),
          Area     = area_km2,
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

df_fut <- bind_rows(fut_rows)
levels(df_fut$PFT) <- pft_names

# 6) COMBINE HISTORICAL + FUTURE
df_all <- bind_rows(df_hist, df_fut)

# 7) AGGREGATE INTO EIGHT CATEGORIES (DROP “Bare” LATER)
df_cat <- df_all %>%
  mutate(
    Category = case_when(
      PFT %in% c(
        "Tropical broadleaf evergreen trees",
        "Tropical deciduous trees",
        "Temperate broadleaf evergreen trees",
        "Temperate deciduous trees",
        "Evergreen coniferous trees",
        "Deciduous coniferous trees"
      ) ~ "Forest",
      PFT %in% c("Coniferous shrubs", "Deciduous shrubs") ~ "Shrubs",
      PFT %in% c("C3 grass", "C4 grass") ~ "Grass",
      PFT == "Tundra" ~ "Tundra",
      PFT == "Swamp" ~ "Swamp",
      PFT %in% c("Non-irrigated crops", "Irrigated crops") ~ "Crops",
      PFT == "Urban" ~ "Urban",
      PFT == "Bare" ~ "Bare",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Category)) %>%
  group_by(Scenario, Year, Category) %>%
  summarise(Area = sum(Area, na.rm = TRUE), .groups = "drop")

# 8) DROP “Bare” AND RESCALE REMAINING TO SUM 1 PER Year×Scenario
df_scaled <- df_cat %>%
  filter(Category != "Bare") %>%
  group_by(Scenario, Year) %>%
  mutate(FractionScaled = Area / sum(Area, na.rm = TRUE)) %>%
  ungroup() %>%
  select(Scenario, Year, Category, FractionScaled)

# 9) COMPLETE MISSING COMBINATIONS WITH ZERO
all_years <- 1950:2100
all_scenarios <- unique(df_scaled$Scenario)

df_complete <- df_scaled %>%
  complete(Scenario = all_scenarios,
           Year     = all_years,
           Category = c("Urban","Crops","Swamp","Tundra","Grass","Shrubs","Forest"),
           fill = list(FractionScaled = 0))

# 10) ORDER CATEGORIES FOR REVERSED STACKING
category_levels <- c(
  "Urban",   # top when reversed
  "Crops",
  "Swamp",
  "Tundra",
  "Grass",
  "Shrubs",
  "Forest"   # bottom when reversed
)
df_complete$Category <- factor(df_complete$Category, levels = category_levels)

# 11) SPECIFY GLOBIOM‐STYLE COLORS (minus Bare)
cat_colors2 <- c(
  "Forest" = "#000066",
  "Shrubs" = "#3366CC",
  "Grass"  = "#66CCCC",
  "Tundra" = "#99CC66",
  "Swamp"  = "#C0C0D1",
  "Crops"  = "#F2E400",
  "Urban"  = "#808080"
)

# 12) PLOT MULTI‐PANEL (HISTORICAL + SSP–RCP) IN ONE WINDOW
ggplot(df_complete, aes(x = Year, y = FractionScaled, fill = Category)) +
  geom_area(color = NA, position = position_stack(reverse = TRUE)) +
  facet_wrap(~Scenario, nrow = 1, scales = "fixed") +
  scale_fill_manual(values = cat_colors2, drop = FALSE) +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1.01), expand = c(0, 0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = NULL,
    x     = "Year",
    y     = "Fraction of European Land (%)",
    fill  = "Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major       = element_blank(),
    panel.grid.minor       = element_blank(),
    axis.text.x            = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y            = element_text(size = 8),
    strip.text             = element_text(size = 10, face = "bold"),
    legend.key.size        = unit(0.4, "cm"),
    legend.text            = element_text(size = 8),
    legend.title           = element_text(size = 9),
    legend.background      = element_rect(fill = "white", color = "grey80")
  )
# 9) FIX SCENARIO NAMES AND CREATE SEPARATE DATAFRAMES FOR HISTORICAL AND FUTURE
# First, fix the scenario names to use hyphen instead of slash
df_scaled <- df_scaled %>%
  mutate(Scenario = case_when(
    Scenario == "SSP1/RCP2.6" ~ "SSP1-RCP2.6",
    Scenario == "SSP2/RCP4.5" ~ "SSP2-RCP4.5",
    Scenario == "SSP5/RCP8.5" ~ "SSP5-RCP8.5",
    TRUE ~ Scenario
  ))

# Create separate dataframes for historical and future
df_historical <- df_scaled %>%
  filter(Scenario == "historical" & Year <= 2015)

df_future <- df_scaled %>%
  filter(Scenario != "historical" & Year >= 2015)

# 10) COMPLETE MISSING COMBINATIONS FOR EACH PERIOD SEPARATELY
# Historical: 1950-2015
hist_years <- 1950:2015
hist_scenarios <- "historical"
hist_categories <- c("Urban", "Crops", "Swamp", "Tundra", "Grass", "Shrubs", "Forest")

df_hist_complete <- df_historical %>%
  complete(Scenario = hist_scenarios,
           Year = hist_years,
           Category = hist_categories,
           fill = list(FractionScaled = 0))

# Future: 2015-2100 for each scenario
fut_years <- 2015:2100
fut_scenarios <- c("SSP1-RCP2.6", "SSP2-RCP4.5", "SSP5-RCP8.5")
fut_categories <- c("Urban", "Crops", "Swamp", "Tundra", "Grass", "Shrubs", "Forest")

df_fut_complete <- df_future %>%
  complete(Scenario = fut_scenarios,
           Year = fut_years,
           Category = fut_categories,
           fill = list(FractionScaled = 0))

# Combine both periods
df_complete <- bind_rows(df_hist_complete, df_fut_complete)

# 11) ORDER CATEGORIES FOR REVERSED STACKING
category_levels <- c(
  "Urban",   # top when reversed
  "Crops",
  "Swamp",
  "Tundra",
  "Grass",
  "Shrubs",
  "Forest"   # bottom when reversed
)
df_complete$Category <- factor(df_complete$Category, levels = category_levels)

# 12) SPECIFY GLOBIOM‐STYLE COLORS
cat_colors2 <- c(
  "Forest" = "#000066",
  "Shrubs" = "#3366CC",
  "Grass"  = "#66CCCC",
  "Tundra" = "#99CC66",
  "Swamp"  = "#C0C0D1",
  "Crops"  = "#F2E400",
  "Urban"  = "#808080"
)

# 13) CREATE CUSTOM FACET LABELS WITH YEAR RANGES
facet_labels <- c(
  "historical" = "Historical ",
  "SSP1-RCP2.6" = "SSP1-RCP2.6 ",
  "SSP2-RCP4.5" = "SSP2-RCP4.5 ",
  "SSP5-RCP8.5" = "SSP5-RCP8.5 "
)

# 14) PLOT WITH PROPER YEAR RANGES
# Create a function for custom x-axis breaks based on scenario
custom_breaks <- function(limits) {
  if (all(limits == c(1950, 2015))) {
    # Historical: 1950-2015
    seq(1950, 2015, 15)
  } else if (all(limits == c(2015, 2100))) {
    # Future: 2015-2100
    seq(2015, 2100, 15)
  } else {
    # Fallback
    pretty(limits, n = 5)
  }
}

# Plot
p <- ggplot(df_complete, aes(x = Year, y = FractionScaled, fill = Category)) +
  geom_area(color = NA, position = position_stack(reverse = TRUE)) +
  facet_wrap(~Scenario, nrow = 1, scales = "free_x",
             labeller = as_labeller(facet_labels)) +
  scale_fill_manual(values = cat_colors2, drop = FALSE) +
  scale_x_continuous(
    breaks = custom_breaks,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1.01),
    expand = c(0, 0),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "European Land Cover Change (1950-2100)",
    x = "Year",
    y = "Fraction of European Land (%)",
    fill = "Land Cover Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 10, face = "bold", hjust = 0),
    strip.background = element_rect(fill = "grey90", color = "grey70"),
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9, face = "bold"),
    legend.background = element_rect(fill = "white", color = "grey80"),
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10))
  )

print(p)



# 14) CREATE CSV OUTPUT (BEFORE PLOTTING)
# Save the complete dataset to CSV
output_csv_path <- "H:/SAFENET/deliverable/european_land_cover_fractions_1950_2100.csv"

# Prepare the data for CSV export
csv_data <- df_complete %>%
  # Convert fraction to percentage for CSV (0-100 scale)
  mutate(Percentage = FractionScaled * 100) %>%
  # Select relevant columns
  select(Scenario, Year, Category, Percentage, FractionScaled) %>%
  # Order by Scenario, then Year, then Category order
  arrange(Scenario, Year, Category)

# Write to CSV
write.csv(csv_data, output_csv_path, row.names = FALSE)

# Print confirmation
cat("CSV file saved to:", output_csv_path, "\n")
cat("Total rows:", nrow(csv_data), "\n")
cat("Scenarios:", paste(unique(csv_data$Scenario), collapse = ", "), "\n")
cat("Years:", min(csv_data$Year), "-", max(csv_data$Year), "\n")

# 15) CREATE A WIDE-FORMAT CSV (ALTERNATIVE FORMAT)
# Sometimes it's useful to have one row per Year-Scenario
wide_csv_path <- "european_land_cover_wide_format_1950_2100.csv"

wide_data <- df_complete %>%
  # Convert fraction to percentage
  mutate(Percentage = FractionScaled * 100) %>%
  # Create wide format
  pivot_wider(
    id_cols = c(Scenario, Year),
    names_from = Category,
    values_from = Percentage,
    values_fill = 0
  ) %>%
  # Order columns as per stacking order
  select(Scenario, Year, Urban, Crops, Swamp, Tundra, Grass, Shrubs, Forest) %>%
  # Order rows
  arrange(Scenario, Year)

# Write wide format CSV
write.csv(wide_data, wide_csv_path, row.names = FALSE)

cat("\nWide format CSV saved to:", wide_csv_path, "\n")
cat("Total records:", nrow(wide_data), "\n")

# 16) CREATE SUMMARY STATISTICS CSV
summary_csv_path <- "european_land_cover_summary_statistics.csv"

summary_data <- csv_data %>%
  group_by(Scenario, Category) %>%
  summarise(
    Mean_Percentage = mean(Percentage, na.rm = TRUE),
    Min_Percentage = min(Percentage, na.rm = TRUE),
    Max_Percentage = max(Percentage, na.rm = TRUE),
    Median_Percentage = median(Percentage, na.rm = TRUE),
    StdDev_Percentage = sd(Percentage, na.rm = TRUE),
    First_Year = min(Year),
    Last_Year = max(Year),
    .groups = "drop"
  ) %>%
  arrange(Scenario, Category)

write.csv(summary_data, summary_csv_path, row.names = FALSE)

cat("\nSummary statistics CSV saved to:", summary_csv_path, "\n")

# 17) PLOT THE CHART (NOW WITH CSV SAVED)
p <- ggplot(df_complete, aes(x = Year, y = FractionScaled, fill = Category)) +
  geom_area(color = NA, position = position_stack(reverse = TRUE)) +
  facet_wrap(~Scenario, nrow = 1, scales = "free_x",
             labeller = as_labeller(facet_labels)) +
  scale_fill_manual(values = cat_colors2, drop = FALSE) +
  scale_x_continuous(
    breaks = custom_breaks,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1.01),
    expand = c(0, 0),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "European Land Cover Change (1950-2100)",
    x = "Year",
    y = "Fraction of European Land",
    fill = "Land Cover Category",
    caption = paste("Data exported to:", output_csv_path)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 10, face = "bold", hjust = 0),
    strip.background = element_rect(fill = "grey90", color = "grey70"),
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9, face = "bold"),
    legend.background = element_rect(fill = "white", color = "grey80"),
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10)),
    plot.caption = element_text(size = 8, color = "grey50", hjust = 0)
  )

print(p)

# 18) SAVE THE PLOT AS PNG AND PDF
# Generate a timestamp for unique filenames
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Save as PNG
png_file <- paste0("european_land_cover_", timestamp, ".png")
ggsave(png_file, plot = p, width = 14, height = 8, dpi = 300)
cat("\nPlot saved as PNG:", png_file, "\n")

# Save as PDF
pdf_file <- paste0("european_land_cover_", timestamp, ".pdf")
ggsave(pdf_file, plot = p, width = 14, height = 8)
cat("Plot saved as PDF:", pdf_file, "\n")

# 19) CREATE A README FILE WITH DATA DESCRIPTION
readme_path <- "README_land_cover_data.txt"

readme_content <- c(
  "EUROPEAN LAND COVER DATA (1950-2100)",
  "========================================",
  "",
  "Description:",
  "This dataset contains European land cover fractions from 1950 to 2100.",
  "Historical data covers 1950-2015, while future scenarios cover 2015-2100.",
  "",
  "Files included:",
  paste("1.", output_csv_path, "- Long format data with all year-category combinations"),
  paste("2.", wide_csv_path, "- Wide format with one row per Year-Scenario"),
  paste("3.", summary_csv_path, "- Summary statistics by Scenario and Category"),
  paste("4.", png_file, "- Visualization plot (PNG)"),
  paste("5.", pdf_file, "- Visualization plot (PDF)"),
  "",
  "Data Structure:",
  "---------------",
  "- Scenario: 'historical' (1950-2015), 'SSP1-RCP2.6', 'SSP2-RCP4.5', 'SSP5-RCP8.5' (2015-2100)",
  "- Year: Year of observation (1950-2100)",
  "- Category: Land cover type (Urban, Crops, Swamp, Tundra, Grass, Shrubs, Forest)",
  "- Percentage: Land cover fraction expressed as percentage (0-100)",
  "- FractionScaled: Land cover fraction as decimal (0-1)",
  "",
  "Land Cover Categories:",
  "---------------------",
  "1. Forest   - All tree types (tropical, temperate, coniferous)",
  "2. Shrubs   - Coniferous and deciduous shrubs",
  "3. Grass    - C3 and C4 grasses",
  "4. Tundra   - Tundra vegetation",
  "5. Swamp    - Wetland/swamp areas",
  "6. Crops    - Irrigated and non-irrigated crops",
  "7. Urban    - Urban/built-up areas",
  "",
  "Notes:",
  "------",
  "- Data is scaled so that all categories sum to 100% for each Year-Scenario",
  "- Missing combinations are filled with 0%",
  "- Historical data ends at 2015",
  "- Future scenarios start at 2015 and continue to 2100",
  "- Bare land category has been excluded from analysis",
  "",
  paste("Generated on:", Sys.time()),
  "Source: Hoffmann et al. 2021 Data"
)

writeLines(readme_content, readme_path)
cat("\nREADME file created:", readme_path, "\n")

# 20) FINAL SUMMARY
cat("\n" + rep("=", 50) + "\n")
cat("DATA EXPORT COMPLETE!\n")
cat(rep("=", 50) + "\n\n")
cat("Total files created: 6\n")
cat("1. Primary data (long format):", output_csv_path, "\n")
cat("2. Wide format data:", wide_csv_path, "\n")
cat("3. Summary statistics:", summary_csv_path, "\n")
cat("4. Visualization (PNG):", png_file, "\n")
cat("5. Visualization (PDF):", pdf_file, "\n")
cat("6. Documentation:", readme_path, "\n")
cat("\n" + rep("=", 50) + "\n")

