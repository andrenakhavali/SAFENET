library(terra)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(reshape2)
library(patchwork)  # For arranging plots
library(viridis)     # For nice color palettes

# Define EU27+UK countries
eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
                  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
                  "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
                  "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
                  "Spain", "Sweden", "United Kingdom")

# Get polygons for both regions
world <- ne_countries(scale = 50, returnclass = "sf")

# 1. EU27+UK polygon (from individual countries)
eu_sf <- world[world$admin %in% eu_countries, ]
eu_union <- st_union(eu_sf)
eu_vect <- vect(eu_union)

# 2. Whole Europe polygon (from rnaturalearth Europe continent)
europe <- ne_countries(continent = "Europe", scale = 50, returnclass = "sf")
europe_union <- st_union(europe)
europe_vect <- vect(europe_union)

# Define forest PFTs
forest_pfts <- c("TempBroadEvergreen", "TempDeciduous", "EvergreenConifer")

# Load tabs_gui data
tabs_gui <- read_csv("H:/SAFENET/PFT/tabs_gui_EUCLIMIT6_LifeUpdate_21092023_scen_csv_all.csv")

# Function to process each scenario for a given region
process_scenario_region <- function(scenario_name, file_path, years, region_vect, region_name) {
  cat("Processing scenario:", scenario_name, "- Region:", region_name, "\n")

  # 1. Process SSP data -------------------------------------------------------
  if (!file.exists(file_path)) {
    cat("Warning: File not found:", file_path, "\n")
    return(NULL)
  }

  ssp <- rast(file_path)
  ssp_region <- crop(ssp, region_vect, mask = TRUE)

  # Create cell area raster (ha)
  cell_areas <- cellSize(ssp_region[[1]], unit = "ha")

  # Initialize results
  results <- data.frame(
    Year = years,
    TempBroadEvergreen = 0,
    TempDeciduous = 0,
    EvergreenConifer = 0,
    TotalForestArea = 0
  )

  # Calculate area for each forest PFT per year (in ha)
  for (i in 1:nlyr(ssp_region)) {
    current_year <- ssp_region[[i]]

    # Map PFT values to our three main types
    # TempBroadEvergreen (PFT 3)
    mask_tbe <- current_year == 3
    tbe_area <- global(mask_tbe * cell_areas, sum, na.rm = TRUE)[1, 1]
    results[i, "TempBroadEvergreen"] <- tbe_area

    # TempDeciduous (PFT 4)
    mask_td <- current_year == 4
    td_area <- global(mask_td * cell_areas, sum, na.rm = TRUE)[1, 1]
    results[i, "TempDeciduous"] <- td_area

    # EvergreenConifer (PFT 5)
    mask_ec <- current_year == 5
    ec_area <- global(mask_ec * cell_areas, sum, na.rm = TRUE)[1, 1]
    results[i, "EvergreenConifer"] <- ec_area

    # Total forest area (PFTs 1-6)
    forest_mask <- current_year >= 1 & current_year <= 6
    results[i, "TotalForestArea"] <- global(forest_mask * cell_areas, sum, na.rm = TRUE)[1, 1]
  }

  # Calculate afforestation as positive increments only
  results$Afforestation_ha <- c(0, pmax(diff(results$TotalForestArea), 0))

  # 2. Process tabs_gui data (only for EU27+UK region) -----------------------
  tabs_data_list <- list()

  if (region_name == "EU27+UK") {
    tabs_scenarios <- c(
      "primes_LIFE__REFERENCE__REFERENCE_FIX",  # Reference scenario
      "primes_LIFE__REFERENCE__GHG_CO2_200_FIX" # High-end scenario
    )

    for (tabs_scenario in tabs_scenarios) {
      if (!tabs_scenario %in% unique(tabs_gui$Scenario)) {
        cat("Warning: TabsGUI scenario", tabs_scenario, "not found.\n")
        next
      }

      # Process accumulated afforestation
      acc_affor <- tabs_gui %>%
        filter(Country %in% eu_countries,
               Scenario == tabs_scenario,
               Parameter == "area_affor_acc_ha") %>%
        select(-Country, -Scenario, -Parameter) %>%
        pivot_longer(cols = -c(), names_to = "Year", values_to = "AccAffor_ha") %>%
        group_by(Year) %>%
        summarise(AccAffor_ha = sum(AccAffor_ha, na.rm = TRUE)) %>%
        mutate(Year = as.integer(Year))

      if (nrow(acc_affor) > 0) {
        acc_affor <- acc_affor %>%
          arrange(Year) %>%
          mutate(NewForest_ha = AccAffor_ha - lag(AccAffor_ha),
                 NewForest_ha = ifelse(is.na(NewForest_ha), 0, NewForest_ha))

        # Get total forest area
        total_forest <- tabs_gui %>%
          filter(Country %in% eu_countries,
                 Scenario == tabs_scenario,
                 Parameter == "area_forest_ha") %>%
          select(-Country, -Scenario, -Parameter) %>%
          pivot_longer(cols = -c(), names_to = "Year", values_to = "TotalForest_ha") %>%
          group_by(Year) %>%
          summarise(TotalForest_ha = sum(TotalForest_ha, na.rm = TRUE)) %>%
          mutate(Year = as.integer(Year))

        # Combine data
        eu_tabs <- acc_affor %>%
          left_join(total_forest, by = "Year")

        # Store results
        tabs_data_list[[tabs_scenario]] <- list(
          years = eu_tabs$Year,
          cumulative_affor = cumsum(eu_tabs$NewForest_ha),
          cumulative_forest = eu_tabs$TotalForest_ha
        )
      }
    }
  }

  # 3. Prepare plot data -----------------------------------------------------
  # Forest PFT areas
  results_forest_long <- reshape2::melt(results[, c("Year", forest_pfts)],
                                        id.vars = "Year",
                                        variable.name = "PFT",
                                        value.name = "Area_ha")
  results_forest_long$Scenario <- scenario_name
  results_forest_long$Region <- region_name

  # Cumulative data
  cumulative_data <- data.frame(
    Year = results$Year,
    CumulativeAffor = cumsum(pmax(results$Afforestation_ha, 0)),
    CumulativeForest = pmax(results$TotalForestArea, 0),
    Scenario = scenario_name,
    Region = region_name
  )

  return(list(
    forest = results_forest_long,
    cumulative = cumulative_data,
    summary = list(
      scenario_name = scenario_name,
      region_name = region_name,
      initial_area = results$TotalForestArea[1],
      final_area = results$TotalForestArea[nrow(results)],
      net_change = results$TotalForestArea[nrow(results)] - results$TotalForestArea[1],
      total_affor = sum(pmax(results$Afforestation_ha, 0))
    )
  ))
}

# Function to create spatial difference plots
create_spatial_diff_plots <- function(scenario_path, scenario_name) {
  cat("Creating spatial difference plots for:", scenario_name, "\n")

  if (!file.exists(scenario_path)) {
    cat("Warning: File not found:", scenario_path, "\n")
    return(NULL)
  }

  # Load the scenario data
  ssp <- rast(scenario_path)

  # Get first and last year (2016 and 2100)
  first_layer <- ssp[[1]]  # 2016
  last_layer <- ssp[[nlyr(ssp)]]  # 2100

  # Crop to Europe
  first_europe <- crop(first_layer, europe_vect, mask = TRUE)
  last_europe <- crop(last_layer, europe_vect, mask = TRUE)

  # Create forest masks (PFTs 1-6 are forest types)
  forest_mask_2016 <- first_europe >= 1 & first_europe <= 6
  forest_mask_2100 <- last_europe >= 1 & last_europe <= 6

  # Calculate changes
  # 1 = Forest in 2016, not in 2100 (deforestation)
  # 2 = Forest in 2100, not in 2016 (afforestation)
  # 0 = No change (either both forest or both non-forest)
  change_map <- ifel(forest_mask_2016 & !forest_mask_2100, 1,  # Loss
                     ifel(!forest_mask_2016 & forest_mask_2100, 2,  # Gain
                          0))  # No change

  # Convert to data frame for plotting
  change_df <- as.data.frame(change_map, xy = TRUE, na.rm = TRUE)
  colnames(change_df)[3] <- "Change"

  # Get country boundaries for overlay
  countries_sf <- st_as_sf(europe)

  # Create the spatial plot
  p_spatial <- ggplot() +
    geom_raster(data = change_df,
                aes(x = x, y = y, fill = as.factor(Change))) +
    geom_sf(data = countries_sf, fill = NA, color = "gray30", linewidth = 0.3) +
    scale_fill_manual(
      name = "Forest Change (2100-2016)",
      values = c("0" = "gray90",  # No change
                 "1" = "red",      # Deforestation
                 "2" = "darkgreen"),  # Afforestation
      labels = c("No change", "Deforestation", "Afforestation")
    ) +
    coord_sf(xlim = c(-25, 45), ylim = c(35, 72)) +  # Europe extent
    labs(
      title = paste("Forest Change:", scenario_name),
      subtitle = "Difference between 2100 and 2016",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
      axis.text = element_text(size = 8),
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.background = element_rect(fill = "lightblue", color = NA),
      panel.grid = element_line(color = "gray80", linewidth = 0.2)
    ) +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1))

  return(list(
    plot = p_spatial,
    change_raster = change_map,
    stats = list(
      deforestation_area = global(change_map == 1, sum, na.rm = TRUE)[1, 1] * prod(res(change_map)),
      afforestation_area = global(change_map == 2, sum, na.rm = TRUE)[1, 1] * prod(res(change_map)),
      net_change = global(forest_mask_2100, sum, na.rm = TRUE)[1, 1] -
                   global(forest_mask_2016, sum, na.rm = TRUE)[1, 1]
    )
  ))
}

# Define scenarios
scenarios <- list(
  list(
    name = "SSP1-2.6",
    file_path = "H:/SAFENET/PFT/dominant_pft_ssp126_2016-2100.nc",
    years = 2016:2100
  ),
  list(
    name = "SSP2-4.5",
    file_path = "H:/SAFENET/PFT/dominant_pft_ssp245_2016-2100.nc",
    years = 2016:2100
  ),
  list(
    name = "SSP5-8.5",
    file_path = "H:/SAFENET/PFT/dominant_pft_ssp585_2016-2100.nc",
    years = 2016:2100
  )
)

# Process all scenarios for both regions
all_data <- list()
for (sc in scenarios) {
  # Process for Whole Europe
  result_europe <- process_scenario_region(
    scenario_name = sc$name,
    file_path = sc$file_path,
    years = sc$years,
    region_vect = europe_vect,
    region_name = "Whole Europe"
  )

  # Process for EU27+UK
  result_eu <- process_scenario_region(
    scenario_name = sc$name,
    file_path = sc$file_path,
    years = sc$years,
    region_vect = eu_vect,
    region_name = "EU27+UK"
  )

  if (!is.null(result_europe)) all_data <- c(all_data, list(result_europe))
  if (!is.null(result_eu)) all_data <- c(all_data, list(result_eu))
}

# Combine data for plotting
combined_forest <- do.call(rbind, lapply(all_data, function(x) x$forest))
combined_cumulative <- do.call(rbind, lapply(all_data, function(x) x$cumulative))

# Create spatial difference plots
spatial_plots <- list()
for (sc in scenarios) {
  spatial_result <- create_spatial_diff_plots(sc$file_path, sc$name)
  if (!is.null(spatial_result)) {
    spatial_plots[[sc$name]] <- spatial_result
  }
}

# Create plots ----------------------------------------------------------------

# Color palette for scenarios
scenario_colors <- c(
  "SSP1-2.6" = "#1b9e77",
  "SSP2-4.5" = "#d95f02",
  "SSP5-8.5" = "#7570b3"
)

# PFT colors
pft_colors <- c(
  "TempBroadEvergreen" = "red",
  "TempDeciduous" = "blue",
  "EvergreenConifer" = "darkgreen"
)

# Plot 1: PFT Areas - Whole Europe
p1_europe <- ggplot(combined_forest %>% filter(Region == "Whole Europe"),
                    aes(x = Year, y = Area_ha/1e6, color = PFT, linetype = Scenario)) +
  geom_line(linewidth = 1) +
  scale_color_manual(name = "Forest Type", values = pft_colors) +
  scale_linetype_manual(name = "Scenario",
                       values = c("SSP1-2.6" = "solid",
                                  "SSP2-4.5" = "dashed",
                                  "SSP5-8.5" = "dotdash")) +
  labs(
    title = "Forest PFT Areas - Whole Europe",
    y = "Area (Million ha)",
    x = "Year"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.box = "vertical"
  ) +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, 25)) +
  scale_x_continuous(limits = c(2016, 2100), breaks = seq(2020, 2100, 20))

# Plot 1: PFT Areas - EU27+UK
p1_eu <- ggplot(combined_forest %>% filter(Region == "EU27+UK"),
                aes(x = Year, y = Area_ha/1e6, color = PFT, linetype = Scenario)) +
  geom_line(linewidth = 1) +
  scale_color_manual(name = "Forest Type", values = pft_colors) +
  scale_linetype_manual(name = "Scenario",
                       values = c("SSP1-2.6" = "solid",
                                  "SSP2-4.5" = "dashed",
                                  "SSP5-8.5" = "dotdash")) +
  labs(
    title = "Forest PFT Areas - EU27+UK",
    y = "Area (Million ha)",
    x = "Year"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.box = "vertical"
  ) +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, 20)) +
  scale_x_continuous(limits = c(2016, 2100), breaks = seq(2020, 2100, 20))

# Plot 2: Cumulative Afforestation
p2_europe <- ggplot(combined_cumulative %>% filter(Region == "Whole Europe"),
                    aes(x = Year, y = CumulativeAffor/1e6, color = Scenario)) +
  geom_line(linewidth = 1) +
  scale_color_manual(name = "Scenario", values = scenario_colors) +
  labs(
    title = "Cumulative Afforestation - Whole Europe",
    y = "Cumulative Area (Million ha)",
    x = "Year"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_x_continuous(limits = c(2016, 2100), breaks = seq(2020, 2100, 20))

p2_eu <- ggplot(combined_cumulative %>% filter(Region == "EU27+UK"),
                aes(x = Year, y = CumulativeAffor/1e6, color = Scenario)) +
  geom_line(linewidth = 1) +
  scale_color_manual(name = "Scenario", values = scenario_colors) +
  labs(
    title = "Cumulative Afforestation - EU27+UK",
    y = "Cumulative Area (Million ha)",
    x = "Year"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_x_continuous(limits = c(2016, 2100), breaks = seq(2020, 2100, 20))

# Plot 3: Cumulative Forest Area
p3_europe <- ggplot(combined_cumulative %>% filter(Region == "Whole Europe"),
                    aes(x = Year, y = CumulativeForest/1e6, color = Scenario)) +
  geom_line(linewidth = 1) +
  scale_color_manual(name = "Scenario", values = scenario_colors) +
  labs(
    title = "Cumulative Forest Area - Whole Europe",
    y = "Cumulative Forest Area (Million ha)",
    x = "Year"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_x_continuous(limits = c(2016, 2100), breaks = seq(2020, 2100, 20))

p3_eu <- ggplot(combined_cumulative %>% filter(Region == "EU27+UK"),
                aes(x = Year, y = CumulativeForest/1e6, color = Scenario)) +
  geom_line(linewidth = 1) +
  scale_color_manual(name = "Scenario", values = scenario_colors) +
  labs(
    title = "Cumulative Forest Area - EU27+UK",
    y = "Cumulative Forest Area (Million ha)",
    x = "Year"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_x_continuous(limits = c(2016, 2100), breaks = seq(2020, 2100, 20))

# Arrange plots in a grid
# First row: PFT areas
pft_row <- grid.arrange(p1_europe, p1_eu, ncol = 2)

# Second row: Cumulative afforestation
affor_row <- grid.arrange(p2_europe, p2_eu, ncol = 2)

# Third row: Cumulative forest area
forest_row <- grid.arrange(p3_europe, p3_eu, ncol = 2)

# Display all temporal plots
grid.arrange(pft_row, affor_row, forest_row, nrow = 3)

# Display spatial difference plots separately
if (length(spatial_plots) > 0) {
  cat("\n=== SPATIAL DIFFERENCE PLOTS (2100-2016) ===\n")

  # Arrange spatial plots in a grid
  spatial_grid <- do.call(grid.arrange, c(
    lapply(spatial_plots, function(x) x$plot),
    ncol = 3
  ))

  # Print statistics
  cat("\n--- Forest Change Statistics (2100-2016) ---\n")
  for (sc_name in names(spatial_plots)) {
    stats <- spatial_plots[[sc_name]]$stats
    cat(sprintf("\nScenario: %s\n", sc_name))
    cat(sprintf("  Deforestation area: %.0f ha\n", stats$deforestation_area))
    cat(sprintf("  Afforestation area: %.0f ha\n", stats$afforestation_area))
    cat(sprintf("  Net change: %.0f ha\n", stats$net_change))
  }
}

# Summary statistics ----------------------------------------------------------
cat("\n=== REGIONAL SUMMARY STATISTICS ===\n")
for (data in all_data) {
  cat(sprintf("\n--- %s - %s ---\n",
              data$summary$region_name,
              data$summary$scenario_name))
  cat(sprintf("Initial Total Forest Area: %.1f million ha\n",
              data$summary$initial_area/1e6))
  cat(sprintf("Final Total Forest Area: %.1f million ha\n",
              data$summary$final_area/1e6))
  cat(sprintf("Net Change: %.1f million ha (%.1f%%)\n",
              data$summary$net_change/1e6,
              data$summary$net_change / data$summary$initial_area * 100))
  cat(sprintf("Total Afforestation: %.1f million ha\n",
              data$summary$total_affor/1e6))
}


# Enhanced plotting code matching your uploaded image style
library(ggplot2)
library(gridExtra)
library(patchwork)
library(scales)

# Color palette matching your uploaded image EXACTLY
scenario_colors <- c(
  "SSP1-2.6" = "#1f77b4",  # Blue from your image
  "SSP2-4.5" = "#d62728",  # Red from your image
  "SSP5-8.5" = "#2ca02c"   # Green from your image
)

# Theme to match your uploaded image
image_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5,
                                margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      axis.title = element_text(size = 13, face = "bold"),
      axis.text = element_text(size = 11, color = "black"),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      legend.position = "top",
      legend.box = "horizontal",
      legend.box.just = "center",
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 10, 10, 10)
    )
}

# Create EXACT total forest area plot for Whole Europe (matching your image)
p_whole_europe <- ggplot(combined_cumulative %>% filter(Region == "Whole Europe"),
                         aes(x = Year, y = CumulativeForest/1e6,
                             color = Scenario, shape = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, stroke = 1.2) +
  scale_color_manual(
    name = "Scenario:",
    values = scenario_colors
  ) +
  scale_shape_manual(
    name = "Scenario:",
    values = c("SSP1-2.6" = 19,  # Circle
               "SSP2-4.5" = 15,  # Square
               "SSP5-8.5" = 4)   # Cross
  ) +
  scale_linetype_manual(
    name = "Scenario:",
    values = c("SSP1-2.6" = "solid",
               "SSP2-4.5" = "dashed",
               "SSP5-8.5" = "dotdash")
  ) +
  labs(
    title = "Total Forest Area - Whole Europe",
    subtitle = "2016-2100 projection",
    y = "Forest (Million ha)",
    x = "Year"
  ) +
  image_theme() +
  # Y-axis starting from 400 as you requested
  scale_y_continuous(
    limits = c(400, 500),  # Exactly as in your image
    breaks = seq(400, 500, 20),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(
    limits = c(2016, 2100),
    breaks = seq(2020, 2100, 20)
  )

# Create EXACT total forest area plot for EU27+UK (matching your image)
p_eu27_uk <- ggplot(combined_cumulative %>% filter(Region == "EU27+UK"),
                    aes(x = Year, y = CumulativeForest/1e6,
                        color = Scenario, shape = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, stroke = 1.2) +
  scale_color_manual(
    name = "Scenario:",
    values = scenario_colors
  ) +
  scale_shape_manual(
    name = "Scenario:",
    values = c("SSP1-2.6" = 19,  # Circle
               "SSP2-4.5" = 15,  # Square
               "SSP5-8.5" = 4)   # Cross
  ) +
  scale_linetype_manual(
    name = "Scenario:",
    values = c("SSP1-2.6" = "solid",
               "SSP2-4.5" = "dashed",
               "SSP5-8.5" = "dotdash")
  ) +
  labs(
    title = "Total Forest Area - EU27+UK",
    subtitle = "2016-2100 projection",
    y = "Forest (Million ha)",
    x = "Year"
  ) +
  image_theme() +
  scale_y_continuous(
    limits = c(0, 200),  # Exactly as in your image
    breaks = seq(0, 200, 40),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(
    limits = c(2016, 2100),
    breaks = seq(2020, 2100, 20)
  )

# Create PFT area plots side-by-side
# Create PFT plot for Whole Europe
pft_whole_europe <- ggplot(combined_forest %>% filter(Region == "Whole Europe"),
                           aes(x = Year, y = Area_ha/1e6,
                               color = PFT, linetype = Scenario)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = combined_forest %>%
               filter(Region == "Whole Europe", Year %in% seq(2020, 2100, 20)),
             size = 2.5, shape = 21, fill = "white", stroke = 1) +
  scale_color_manual(
    name = "PFT:",
    values = c("TempBroadEvergreen" = "#FF7F00",  # Orange
               "TempDeciduous" = "#984EA3",       # Purple
               "EvergreenConifer" = "#A65628"),    # Brown
    labels = c("Temp. Broadleaf Evergreen", "Temperate Deciduous", "Evergreen Conifer")
  ) +
  scale_linetype_manual(
    name = "Scenario:",
    values = c("SSP1-2.6" = "solid",
               "SSP2-4.5" = "dashed",
               "SSP5-8.5" = "dotdash")
  ) +
  labs(
    title = "PFT Area - Whole Europe",
    subtitle = "2016-2100 projection",
    y = "Area (Million ha)",
    x = "Year"
  ) +
  image_theme() +
  theme(legend.box = "vertical") +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(
    limits = c(2016, 2100),
    breaks = seq(2020, 2100, 20)
  )

# Create PFT plot for EU27+UK
pft_eu27_uk <- ggplot(combined_forest %>% filter(Region == "EU27+UK"),
                      aes(x = Year, y = Area_ha/1e6,
                          color = PFT, linetype = Scenario)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = combined_forest %>%
               filter(Region == "EU27+UK", Year %in% seq(2020, 2100, 20)),
             size = 2.5, shape = 21, fill = "white", stroke = 1) +
  scale_color_manual(
    name = "PFT:",
    values = c("TempBroadEvergreen" = "#FF7F00",  # Orange
               "TempDeciduous" = "#984EA3",       # Purple
               "EvergreenConifer" = "#A65628"),    # Brown
    labels = c("Temp. Broadleaf Evergreen", "Temperate Deciduous", "Evergreen Conifer")
  ) +
  scale_linetype_manual(
    name = "Scenario:",
    values = c("SSP1-2.6" = "solid",
               "SSP2-4.5" = "dashed",
               "SSP5-8.5" = "dotdash")
  ) +
  labs(
    title = "PFT Area - EU27+UK",
    subtitle = "2016-2100 projection",
    y = "Area (Million ha)",
    x = "Year"
  ) +
  image_theme() +
  theme(legend.box = "vertical") +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(
    limits = c(2016, 2100),
    breaks = seq(2020, 2100, 20)
  )

# Create afforestation plots side-by-side
# Create afforestation plot for Whole Europe
affor_whole_europe <- ggplot(combined_cumulative %>% filter(Region == "Whole Europe"),
                             aes(x = Year, y = CumulativeAffor/1e6,
                                 color = Scenario)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = combined_cumulative %>%
               filter(Region == "Whole Europe", Year %in% seq(2020, 2100, 20)),
             size = 3, shape = 21, fill = "white", stroke = 1.2) +
  scale_color_manual(
    name = "Scenario:",
    values = scenario_colors
  ) +
  labs(
    title = "Cumulative Afforestation - Whole Europe",
    subtitle = "2016-2100 projection",
    y = "Cumulative Area (Million ha)",
    x = "Year"
  ) +
  image_theme() +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(
    limits = c(2016, 2100),
    breaks = seq(2020, 2100, 20)
  )

# Create afforestation plot for EU27+UK
affor_eu27_uk <- ggplot(combined_cumulative %>% filter(Region == "EU27+UK"),
                        aes(x = Year, y = CumulativeAffor/1e6,
                            color = Scenario)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = combined_cumulative %>%
               filter(Region == "EU27+UK", Year %in% seq(2020, 2100, 20)),
             size = 3, shape = 21, fill = "white", stroke = 1.2) +
  scale_color_manual(
    name = "Scenario:",
    values = scenario_colors
  ) +
  labs(
    title = "Cumulative Afforestation - EU27+UK",
    subtitle = "2016-2100 projection",
    y = "Cumulative Area (Million ha)",
    x = "Year"
  ) +
  image_theme() +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(
    limits = c(2016, 2100),
    breaks = seq(2020, 2100, 20)
  )

# Display all plots side-by-side as requested
cat("\n=== TOTAL FOREST AREA PLOTS (Side-by-side) ===\n")
total_forest_plots <- grid.arrange(p_whole_europe, p_eu27_uk, ncol = 2)
print(total_forest_plots)

cat("\n=== PFT AREA PLOTS (Side-by-side) ===\n")
pft_area_plots <- grid.arrange(pft_whole_europe, pft_eu27_uk, ncol = 2)
print(pft_area_plots)

cat("\n=== AFFORESTATION PLOTS (Side-by-side) ===\n")
afforestation_plots <- grid.arrange(affor_whole_europe, affor_eu27_uk, ncol = 2)
print(afforestation_plots)



# Enhanced plotting code matching your uploaded image style
library(ggplot2)
library(gridExtra)
library(patchwork)
library(scales)

# Color palette matching your uploaded image EXACTLY
scenario_colors <- c(
  "SSP1-2.6" = "#1f77b4",  # Blue from your image
  "SSP2-4.5" = "#d62728",  # Red from your image
  "SSP5-8.5" = "#2ca02c"   # Green from your image
)

# Theme to match your uploaded image
image_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5,
                                margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      axis.title = element_text(size = 13, face = "bold"),
      axis.text = element_text(size = 11, color = "black"),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      legend.position = "top",
      legend.box = "horizontal",
      legend.box.just = "center",
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 10, 10, 10)
    )
}

# Create EXACT total forest area plot for Whole Europe (matching your image)
p_whole_europe <- ggplot(combined_cumulative %>% filter(Region == "Whole Europe"),
                         aes(x = Year, y = CumulativeForest/1e6,
                             color = Scenario, shape = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, stroke = 1.2) +
  scale_color_manual(
    name = "Scenario:",
    values = scenario_colors
  ) +
  scale_shape_manual(
    name = "Scenario:",
    values = c("SSP1-2.6" = 19,  # Circle
               "SSP2-4.5" = 15,  # Square
               "SSP5-8.5" = 4)   # Cross
  ) +
  scale_linetype_manual(
    name = "Scenario:",
    values = c("SSP1-2.6" = "solid",
               "SSP2-4.5" = "dashed",
               "SSP5-8.5" = "dotdash")
  ) +
  labs(
    title = "Total Forest Area - Whole Europe",
    subtitle = "2016-2100 projection",
    y = "Forest (Million ha)",
    x = "Year"
  ) +
  image_theme() +
  # Y-axis starting from 400 as you requested
  scale_y_continuous(
    limits = c(400, 500),  # Exactly as in your image
    breaks = seq(400, 500, 20),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(
    limits = c(2016, 2100),
    breaks = seq(2020, 2100, 20)
  )

# Create EXACT total forest area plot for EU27+UK (matching your image)
p_eu27_uk <- ggplot(combined_cumulative %>% filter(Region == "EU27+UK"),
                    aes(x = Year, y = CumulativeForest/1e6,
                        color = Scenario, shape = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, stroke = 1.2) +
  scale_color_manual(
    name = "Scenario:",
    values = scenario_colors
  ) +
  scale_shape_manual(
    name = "Scenario:",
    values = c("SSP1-2.6" = 19,  # Circle
               "SSP2-4.5" = 15,  # Square
               "SSP5-8.5" = 4)   # Cross
  ) +
  scale_linetype_manual(
    name = "Scenario:",
    values = c("SSP1-2.6" = "solid",
               "SSP2-4.5" = "dashed",
               "SSP5-8.5" = "dotdash")
  ) +
  labs(
    title = "Total Forest Area - EU27+UK",
    subtitle = "2016-2100 projection",
    y = "Forest (Million ha)",
    x = "Year"
  ) +
  image_theme() +
  scale_y_continuous(
    limits = c(0, 200),  # Exactly as in your image
    breaks = seq(0, 200, 40),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(
    limits = c(2016, 2100),
    breaks = seq(2020, 2100, 20)
  )

# Create PFT area plots side-by-side
# Create PFT plot for Whole Europe
pft_whole_europe <- ggplot(combined_forest %>% filter(Region == "Whole Europe"),
                           aes(x = Year, y = Area_ha/1e6,
                               color = PFT, linetype = Scenario)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = combined_forest %>%
               filter(Region == "Whole Europe", Year %in% seq(2020, 2100, 20)),
             size = 2.5, shape = 21, fill = "white", stroke = 1) +
  scale_color_manual(
    name = "PFT:",
    values = c("TempBroadEvergreen" = "#FF7F00",  # Orange
               "TempDeciduous" = "#984EA3",       # Purple
               "EvergreenConifer" = "#A65628"),    # Brown
    labels = c("Temp. Broadleaf Evergreen", "Temperate Deciduous", "Evergreen Conifer")
  ) +
  scale_linetype_manual(
    name = "Scenario:",
    values = c("SSP1-2.6" = "solid",
               "SSP2-4.5" = "dashed",
               "SSP5-8.5" = "dotdash")
  ) +
  labs(
    title = "PFT Area - Whole Europe",
    subtitle = "2016-2100 projection",
    y = "Area (Million ha)",
    x = "Year"
  ) +
  image_theme() +
  theme(legend.box = "vertical") +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(
    limits = c(2016, 2100),
    breaks = seq(2020, 2100, 20)
  )

# Create PFT plot for EU27+UK
pft_eu27_uk <- ggplot(combined_forest %>% filter(Region == "EU27+UK"),
                      aes(x = Year, y = Area_ha/1e6,
                          color = PFT, linetype = Scenario)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = combined_forest %>%
               filter(Region == "EU27+UK", Year %in% seq(2020, 2100, 20)),
             size = 2.5, shape = 21, fill = "white", stroke = 1) +
  scale_color_manual(
    name = "PFT:",
    values = c("TempBroadEvergreen" = "#FF7F00",  # Orange
               "TempDeciduous" = "#984EA3",       # Purple
               "EvergreenConifer" = "#A65628"),    # Brown
    labels = c("Temp. Broadleaf Evergreen", "Temperate Deciduous", "Evergreen Conifer")
  ) +
  scale_linetype_manual(
    name = "Scenario:",
    values = c("SSP1-2.6" = "solid",
               "SSP2-4.5" = "dashed",
               "SSP5-8.5" = "dotdash")
  ) +
  labs(
    title = "PFT Area - EU27+UK",
    subtitle = "2016-2100 projection",
    y = "Area (Million ha)",
    x = "Year"
  ) +
  image_theme() +
  theme(legend.box = "vertical") +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(
    limits = c(2016, 2100),
    breaks = seq(2020, 2100, 20)
  )

# Create afforestation plots side-by-side
# Create afforestation plot for Whole Europe
affor_whole_europe <- ggplot(combined_cumulative %>% filter(Region == "Whole Europe"),
                             aes(x = Year, y = CumulativeAffor/1e6,
                                 color = Scenario)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = combined_cumulative %>%
               filter(Region == "Whole Europe", Year %in% seq(2020, 2100, 20)),
             size = 3, shape = 21, fill = "white", stroke = 1.2) +
  scale_color_manual(
    name = "Scenario:",
    values = scenario_colors
  ) +
  labs(
    title = "Cumulative Afforestation - Whole Europe",
    subtitle = "2016-2100 projection",
    y = "Cumulative Area (Million ha)",
    x = "Year"
  ) +
  image_theme() +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(
    limits = c(2016, 2100),
    breaks = seq(2020, 2100, 20)
  )

# Create afforestation plot for EU27+UK
affor_eu27_uk <- ggplot(combined_cumulative %>% filter(Region == "EU27+UK"),
                        aes(x = Year, y = CumulativeAffor/1e6,
                            color = Scenario)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = combined_cumulative %>%
               filter(Region == "EU27+UK", Year %in% seq(2020, 2100, 20)),
             size = 3, shape = 21, fill = "white", stroke = 1.2) +
  scale_color_manual(
    name = "Scenario:",
    values = scenario_colors
  ) +
  labs(
    title = "Cumulative Afforestation - EU27+UK",
    subtitle = "2016-2100 projection",
    y = "Cumulative Area (Million ha)",
    x = "Year"
  ) +
  image_theme() +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(
    limits = c(2016, 2100),
    breaks = seq(2020, 2100, 20)
  )

# Display all plots side-by-side as requested
cat("\n=== TOTAL FOREST AREA PLOTS (Side-by-side) ===\n")
total_forest_plots <- grid.arrange(p_whole_europe, p_eu27_uk, ncol = 2)
print(total_forest_plots)

cat("\n=== PFT AREA PLOTS (Side-by-side) ===\n")
pft_area_plots <- grid.arrange(pft_whole_europe, pft_eu27_uk, ncol = 2)
print(pft_area_plots)

cat("\n=== AFFORESTATION PLOTS (Side-by-side) ===\n")
afforestation_plots <- grid.arrange(affor_whole_europe, affor_eu27_uk, ncol = 2)
print(afforestation_plots)

# Using patchwork for cleaner side-by-side arrangement
library(patchwork)

# Create compact side-by-side layouts
total_forest_compact <- (p_whole_europe + p_eu27_uk) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

pft_area_compact <- (pft_whole_europe + pft_eu27_uk) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

afforestation_compact <- (affor_whole_europe + affor_eu27_uk) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

# Display compact layouts
cat("\n=== COMPACT TOTAL FOREST AREA PLOTS ===\n")
print(total_forest_compact)

cat("\n=== COMPACT PFT AREA PLOTS ===\n")
print(pft_area_compact)

cat("\n=== COMPACT AFFORESTATION PLOTS ===\n")
print(afforestation_compact)

# Save all plots
ggsave("total_forest_area_exact_style.png", total_forest_compact,
       width = 16, height = 7, dpi = 300, bg = "white")

ggsave("pft_area_plots_exact_style.png", pft_area_compact,
       width = 16, height = 7, dpi = 300, bg = "white")

ggsave("afforestation_plots_exact_style.png", afforestation_compact,
       width = 16, height = 7, dpi = 300, bg = "white")


# Using patchwork for cleaner side-by-side arrangement
library(patchwork)

# Create compact side-by-side layouts
total_forest_compact <- (p_whole_europe + p_eu27_uk) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

pft_area_compact <- (pft_whole_europe + pft_eu27_uk) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

afforestation_compact <- (affor_whole_europe + affor_eu27_uk) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

# Display compact layouts
cat("\n=== COMPACT TOTAL FOREST AREA PLOTS ===\n")
print(total_forest_compact)

cat("\n=== COMPACT PFT AREA PLOTS ===\n")
print(pft_area_compact)

cat("\n=== COMPACT AFFORESTATION PLOTS ===\n")
print(afforestation_compact)

# Save all plots
ggsave("total_forest_area_exact_style.png", total_forest_compact,
       width = 16, height = 7, dpi = 300, bg = "white")

ggsave("pft_area_plots_exact_style.png", pft_area_compact,
       width = 16, height = 7, dpi = 300, bg = "white")

ggsave("afforestation_plots_exact_style.png", afforestation_compact,
       width = 16, height = 7, dpi = 300, bg = "white")



# Enhanced plotting code with clean line-only visualization
library(ggplot2)
library(gridExtra)
library(patchwork)
library(scales)

# Color palette matching your uploaded image EXACTLY
scenario_colors <- c(
  "SSP1-2.6" = "#1f77b4",  # Blue from your image
  "SSP2-4.5" = "#d62728",  # Red from your image
  "SSP5-8.5" = "#2ca02c"   # Green from your image
)

# PFT color palette
pft_colors <- c(
  "TempBroadEvergreen" = "#FF7F00",  # Orange
  "TempDeciduous" = "#984EA3",       # Purple
  "EvergreenConifer" = "#A65628"     # Brown
)

# Clean, modern theme
clean_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5,
                                margin = margin(b = 8)),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      axis.title = element_text(size = 13, face = "bold"),
      axis.text = element_text(size = 11, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.5),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      legend.position = "top",
      legend.key = element_rect(fill = "white", color = NA),
      legend.key.width = unit(1.5, "cm"),
      plot.margin = margin(10, 15, 10, 10)
    )
}

# Create clean total forest area plot for Whole Europe
p_whole_europe <- ggplot(combined_cumulative %>% filter(Region == "Whole Europe"),
                         aes(x = Year, y = CumulativeForest/1e6,
                             color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 1.5, alpha = 0.9) +
  scale_color_manual(
    name = "Scenario:",
    values = scenario_colors
  ) +
  scale_linetype_manual(
    name = "Scenario:",
    values = c("SSP1-2.6" = "solid", "SSP2-4.5" = "dashed", "SSP5-8.5" = "dotdash")
  ) +
  labs(
    title = "Total Forest Area - Whole Europe",
    subtitle = "2016-2100 projection",
    y = "Forest (Million ha)",
    x = "Year"
  ) +
  clean_theme() +
  scale_y_continuous(
    limits = c(400, 500),
    breaks = seq(400, 500, 20),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(
    limits = c(2016, 2100),
    breaks = seq(2020, 2100, 20)
  )

# Create clean total forest area plot for EU27+UK
p_eu27_uk <- ggplot(combined_cumulative %>% filter(Region == "EU27+UK"),
                    aes(x = Year, y = CumulativeForest/1e6,
                        color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 1.5, alpha = 0.9) +
  scale_color_manual(
    name = "Scenario:",
    values = scenario_colors
  ) +
  scale_linetype_manual(
    name = "Scenario:",
    values = c("SSP1-2.6" = "solid", "SSP2-4.5" = "dashed", "SSP5-8.5" = "dotdash")
  ) +
  labs(
    title = "Total Forest Area - EU27+UK",
    subtitle = "2016-2100 projection",
    y = "Forest (Million ha)",
    x = "Year"
  ) +
  clean_theme() +
  scale_y_continuous(
    limits = c(0, 200),
    breaks = seq(0, 200, 40),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(
    limits = c(2016, 2100),
    breaks = seq(2020, 2100, 20)
  )

# Create clean PFT area plot for Whole Europe
pft_whole_europe <- ggplot(combined_forest %>% filter(Region == "Whole Europe"),
                           aes(x = Year, y = Area_ha/1e6,
                               color = PFT, linetype = Scenario)) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  scale_color_manual(
    name = "PFT:",
    values = pft_colors,
    labels = c("Temp. Broadleaf Evergreen", "Temperate Deciduous", "Evergreen Conifer")
  ) +
  scale_linetype_manual(
    name = "Scenario:",
    values = c("SSP1-2.6" = "solid", "SSP2-4.5" = "dashed", "SSP5-8.5" = "dotdash")
  ) +
  labs(
    title = "PFT Area - Whole Europe",
    subtitle = "2016-2100 projection",
    y = "Area (Million ha)",
    x = "Year"
  ) +
  clean_theme() +
  theme(
    legend.box = "vertical",
    legend.spacing.y = unit(0.2, "cm")
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(
    limits = c(2016, 2100),
    breaks = seq(2020, 2100, 20)
  )

# Create clean PFT area plot for EU27+UK
pft_eu27_uk <- ggplot(combined_forest %>% filter(Region == "EU27+UK"),
                      aes(x = Year, y = Area_ha/1e6,
                          color = PFT, linetype = Scenario)) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  scale_color_manual(
    name = "PFT:",
    values = pft_colors,
    labels = c("Temp. Broadleaf Evergreen", "Temperate Deciduous", "Evergreen Conifer")
  ) +
  scale_linetype_manual(
    name = "Scenario:",
    values = c("SSP1-2.6" = "solid", "SSP2-4.5" = "dashed", "SSP5-8.5" = "dotdash")
  ) +
  labs(
    title = "PFT Area - EU27+UK",
    subtitle = "2016-2100 projection",
    y = "Area (Million ha)",
    x = "Year"
  ) +
  clean_theme() +
  theme(
    legend.box = "vertical",
    legend.spacing.y = unit(0.2, "cm")
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(
    limits = c(2016, 2100),
    breaks = seq(2020, 2100, 20)
  )

# Create clean afforestation plot for Whole Europe
affor_whole_europe <- ggplot(combined_cumulative %>% filter(Region == "Whole Europe"),
                             aes(x = Year, y = CumulativeAffor/1e6,
                                 color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 1.5, alpha = 0.9) +
  scale_color_manual(
    name = "Scenario:",
    values = scenario_colors
  ) +
  scale_linetype_manual(
    name = "Scenario:",
    values = c("SSP1-2.6" = "solid", "SSP2-4.5" = "dashed", "SSP5-8.5" = "dotdash")
  ) +
  labs(
    title = "Cumulative Afforestation - Whole Europe",
    subtitle = "2016-2100 projection",
    y = "Cumulative Area (Million ha)",
    x = "Year"
  ) +
  clean_theme() +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(
    limits = c(2016, 2100),
    breaks = seq(2020, 2100, 20)
  )

# Create clean afforestation plot for EU27+UK
affor_eu27_uk <- ggplot(combined_cumulative %>% filter(Region == "EU27+UK"),
                        aes(x = Year, y = CumulativeAffor/1e6,
                            color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 1.5, alpha = 0.9) +
  scale_color_manual(
    name = "Scenario:",
    values = scenario_colors
  ) +
  scale_linetype_manual(
    name = "Scenario:",
    values = c("SSP1-2.6" = "solid", "SSP2-4.5" = "dashed", "SSP5-8.5" = "dotdash")
  ) +
  labs(
    title = "Cumulative Afforestation - EU27+UK",
    subtitle = "2016-2100 projection",
    y = "Cumulative Area (Million ha)",
    x = "Year"
  ) +
  clean_theme() +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(
    limits = c(2016, 2100),
    breaks = seq(2020, 2100, 20)
  )

# Combine plots using patchwork
library(patchwork)

# Create compact layouts
total_forest_compact <- (p_whole_europe + p_eu27_uk) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top",
        legend.box = "horizontal")

pft_area_compact <- (pft_whole_europe + pft_eu27_uk) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top")

afforestation_compact <- (affor_whole_europe + affor_eu27_uk) +
  plot_layout(guides = "collect") &
  theme(legend.position = "top",
        legend.box = "horizontal")

# Display all plots
cat("\n=== TOTAL FOREST AREA ===\n")
print(total_forest_compact)

cat("\n=== PFT AREA ===\n")
print(pft_area_compact)

cat("\n=== CUMULATIVE AFFORESTATION ===\n")
print(afforestation_compact)

# Alternative: Create a single comprehensive figure
comprehensive_plot <- (total_forest_compact / pft_area_compact / afforestation_compact) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(face = "bold", size = 14))

cat("\n=== COMPREHENSIVE FIGURE (All plots) ===\n")
print(comprehensive_plot)

# Save all plots
ggsave("total_forest_area_clean.png", total_forest_compact,
       width = 16, height = 7, dpi = 300, bg = "white")

ggsave("pft_area_plots_clean.png", pft_area_compact,
       width = 16, height = 8, dpi = 300, bg = "white")

ggsave("afforestation_plots_clean.png", afforestation_compact,
       width = 16, height = 7, dpi = 300, bg = "white")

ggsave("comprehensive_plot.png", comprehensive_plot,
       width = 16, height = 20, dpi = 300, bg = "white")



# Export plot data to CSV files
library(dplyr)

# 1. Export Total Forest Area data
total_forest_data <- combined_cumulative %>%
  select(Region, Year, Scenario, CumulativeForest) %>%
  mutate(CumulativeForest_Mha = CumulativeForest / 1e6) %>%  # Convert to million hectares
  arrange(Region, Scenario, Year)

# Save to CSV
write.csv(total_forest_data,
          "H:/SAFENET/deliverable/total_forest.csv",
          row.names = FALSE,
          na = "")


# 2. Export Cumulative Afforestation data
afforested_data <- combined_cumulative %>%
  select(Region, Year, Scenario, CumulativeAffor) %>%
  mutate(CumulativeAffor_Mha = CumulativeAffor / 1e6) %>%  # Convert to million hectares
  arrange(Region, Scenario, Year)

# Save to CSV
write.csv(afforested_data,
          "H:/SAFENET/deliverable/afforested_forest.csv",
          row.names = FALSE,
          na = "")


# 3. Export PFT Area data
pft_data <- combined_forest %>%
  select(Region, Year, Scenario, PFT, Area_ha) %>%
  mutate(Area_Mha = Area_ha / 1e6) %>%  # Convert to million hectares
  arrange(Region, PFT, Scenario, Year)

# Add a column for PFT labels (optional, for readability)
pft_labels <- c(
  "TempBroadEvergreen" = "Temperate Broadleaf Evergreen",
  "TempDeciduous" = "Temperate Deciduous",
  "EvergreenConifer" = "Evergreen Conifer"
)

pft_data <- pft_data %>%
  mutate(PFT_Label = pft_labels[PFT])

# Save to CSV
write.csv(pft_data,
          "H:/SAFENET/deliverable/pft_plot.csv",
          row.names = FALSE,
          na = "")

# 4. Optional: Export summary statistics
summary_stats <- function(data, value_col, group_cols) {
  data %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      First_Value = first(!!sym(value_col)),
      Last_Value = last(!!sym(value_col)),
      Mean = mean(!!sym(value_col), na.rm = TRUE),
      Change = last(!!sym(value_col)) - first(!!sym(value_col)),
      Percent_Change = ((last(!!sym(value_col)) - first(!!sym(value_col))) / first(!!sym(value_col))) * 100,
      .groups = "drop"
    )
}

# Summary for total forest
total_forest_summary <- total_forest_data %>%
  group_by(Region, Scenario) %>%
  summarise(
    Start_Year = min(Year),
    End_Year = max(Year),
    Start_Area_Mha = first(CumulativeForest_Mha),
    End_Area_Mha = last(CumulativeForest_Mha),
    Total_Change_Mha = last(CumulativeForest_Mha) - first(CumulativeForest_Mha),
    Percent_Change = ((last(CumulativeForest_Mha) - first(CumulativeForest_Mha)) / first(CumulativeForest_Mha)) * 100,
    .groups = "drop"
  )

write.csv(total_forest_summary,
          "H:/SAFENET/deliverable/total_forest_summary.csv",
          row.names = FALSE,
          na = "")

# Summary for afforestation
afforestation_summary <- afforested_data %>%
  group_by(Region, Scenario) %>%
  summarise(
    Start_Year = min(Year),
    End_Year = max(Year),
    Start_Area_Mha = first(CumulativeAffor_Mha),
    End_Area_Mha = last(CumulativeAffor_Mha),
    Total_Afforested_Mha = last(CumulativeAffor_Mha) - first(CumulativeAffor_Mha),
    Annual_Afforestation_Rate_Mha_yr = (last(CumulativeAffor_Mha) - first(CumulativeAffor_Mha)) / (max(Year) - min(Year)),
    .groups = "drop"
  )

write.csv(afforestation_summary,
          "H:/SAFENET/deliverable/afforestation_summary.csv",
          row.names = FALSE,
          na = "")


# Summary for PFTs
pft_summary <- pft_data %>%
  group_by(Region, Scenario, PFT, PFT_Label) %>%
  summarise(
    Start_Year = min(Year),
    End_Year = max(Year),
    Start_Area_Mha = first(Area_Mha),
    End_Area_Mha = last(Area_Mha),
    Change_Mha = last(Area_Mha) - first(Area_Mha),
    Percent_Change = ((last(Area_Mha) - first(Area_Mha)) / first(Area_Mha)) * 100,
    Mean_Area_Mha = mean(Area_Mha, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(pft_summary,
          "H:/SAFENET/deliverable/pft_summary.csv",
          row.names = FALSE,
          na = "")

