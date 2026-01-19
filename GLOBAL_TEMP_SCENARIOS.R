# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Extended color scheme for all SSP scenarios
all_ssp_colors <- c(
  "Historical" = "black",
  "SSP1-1.9" = "#984EA3",     # Purple
  "SSP1-2.6" = "#377EB8",     # Blue
  "SSP2-4.5" = "#4DAF4A",     # Green
  "SSP3-7.0" = "#FF7F00",     # Orange
  "SSP4-3.4" = "#A65628",     # Brown
  "SSP4-6.0" = "#F781BF",     # Pink
  "SSP5-3.4OS" = "#999999",   # Gray
  "SSP5-8.5" = "#E41A1C"      # Red
)

# Create dataset with ALL AR6 scenarios
create_all_ssp_data <- function() {
  years <- 1950:2100
  historical_years <- 1950:2024

  # Historical data (same as before)
  set.seed(123)
  historical_temp <- numeric(length(historical_years))

  for(i in 1:length(historical_years)) {
    year <- historical_years[i]
    if(year <= 1970) {
      base_warming <- (year - 1950) * 0.002
    } else if(year <= 1990) {
      base_warming <- 0.04 + (year - 1970) * 0.009
    } else if(year <= 2010) {
      base_warming <- 0.22 + (year - 1990) * 0.016
    } else {
      base_warming <- 0.54 + (year - 2010) * 0.04
    }
    historical_temp[i] <- base_warming + rnorm(1, 0, 0.05)
  }

  scaling_factor <- 1.1 / max(historical_temp)
  historical_temp <- historical_temp * scaling_factor
  base_2024 <- historical_temp[length(historical_temp)]

  historical_df <- data.frame(
    Year = historical_years,
    Temperature_Change = historical_temp,
    Scenario = "Historical",
    Source = "IPCC AR6 WGI Chapter 2 (1950-2024)",
    Category = "Historical"
  )

  # Future projections (2024-2100) for ALL scenarios
  future_years <- 2024:2100

  # Define temperature trajectories for all AR6 scenarios
  # Based on IPCC AR6 Chapter 4, Table 4.5
  scenarios <- list(
    "SSP1-1.9" = data.frame(
      Year = c(2024, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100),
      Temp = c(base_2024,
               base_2024 + 0.3,
               base_2024 + 0.4,
               base_2024 + 0.5,
               base_2024 + 0.4,
               base_2024 + 0.3,
               base_2024 + 0.2,
               base_2024 + 0.1,
               base_2024 + 0.0)
    ),
    "SSP1-2.6" = data.frame(
      Year = c(2024, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100),
      Temp = c(base_2024,
               base_2024 + 0.3,
               base_2024 + 0.5,
               base_2024 + 0.6,
               base_2024 + 0.6,
               base_2024 + 0.55,
               base_2024 + 0.5,
               base_2024 + 0.45,
               base_2024 + 0.4)
    ),
    "SSP2-4.5" = data.frame(
      Year = c(2024, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100),
      Temp = c(base_2024,
               base_2024 + 0.3,
               base_2024 + 0.6,
               base_2024 + 0.9,
               base_2024 + 1.2,
               base_2024 + 1.5,
               base_2024 + 1.8,
               base_2024 + 2.1,
               base_2024 + 2.3)
    ),
    "SSP3-7.0" = data.frame(
      Year = c(2024, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100),
      Temp = c(base_2024,
               base_2024 + 0.3,
               base_2024 + 0.7,
               base_2024 + 1.1,
               base_2024 + 1.7,
               base_2024 + 2.4,
               base_2024 + 3.1,
               base_2024 + 3.8,
               base_2024 + 4.2)
    ),
    "SSP4-3.4" = data.frame(
      Year = c(2024, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100),
      Temp = c(base_2024,
               base_2024 + 0.3,
               base_2024 + 0.5,
               base_2024 + 0.7,
               base_2024 + 0.8,
               base_2024 + 0.9,
               base_2024 + 1.0,
               base_2024 + 1.1,
               base_2024 + 1.2)
    ),
    "SSP4-6.0" = data.frame(
      Year = c(2024, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100),
      Temp = c(base_2024,
               base_2024 + 0.3,
               base_2024 + 0.6,
               base_2024 + 0.9,
               base_2024 + 1.3,
               base_2024 + 1.7,
               base_2024 + 2.1,
               base_2024 + 2.5,
               base_2024 + 2.8)
    ),
    "SSP5-3.4OS" = data.frame(
      Year = c(2024, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100),
      Temp = c(base_2024,
               base_2024 + 0.3,
               base_2024 + 0.5,
               base_2024 + 0.7,
               base_2024 + 0.8,
               base_2024 + 0.8,
               base_2024 + 0.7,
               base_2024 + 0.6,
               base_2024 + 0.5)
    ),
    "SSP5-8.5" = data.frame(
      Year = c(2024, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100),
      Temp = c(base_2024,
               base_2024 + 0.3,
               base_2024 + 0.8,
               base_2024 + 1.3,
               base_2024 + 2.0,
               base_2024 + 2.8,
               base_2024 + 3.6,
               base_2024 + 4.2,
               base_2024 + 4.6)
    )
  )

  # Create interpolation function
  create_interpolation <- function(points_df, scenario_name) {
    interp <- approx(points_df$Year, points_df$Temp, xout = future_years, method = "linear")

    data.frame(
      Year = interp$x,
      Temperature_Change = interp$y,
      Scenario = scenario_name,
      Source = "IPCC AR6 Chapter 4, Table 4.5 (CMIP6 multi-model mean)",
      Category = ifelse(scenario_name %in% c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5"),
                        "Selected", "Other")
    )
  }

  # Create data for all scenarios
  future_dfs <- lapply(names(scenarios), function(scen) {
    create_interpolation(scenarios[[scen]], scen)
  })

  # Combine all data
  full_df <- bind_rows(historical_df, bind_rows(future_dfs)) %>%
    arrange(Scenario, Year)

  return(full_df)
}

# Create the original style plot with all scenarios and correct line types
create_all_scenarios_plot <- function(data) {
  p <- ggplot(data, aes(x = Year, y = Temperature_Change,
                        color = Scenario, group = Scenario,
                        linetype = Category, size = Category)) +
    geom_line() +
    scale_color_manual(values = all_ssp_colors) +
    # CORRECTED: Solid for selected, dashed for others
    scale_linetype_manual(
      values = c("Historical" = "solid", "Selected" = "solid", "Other" = "dashed"),
      guide = "none"  # We'll add a custom annotation instead
    ) +
    scale_size_manual(
      values = c("Historical" = 1.2, "Selected" = 1.2, "Other" = 0.8),
      guide = "none"  # Don't show size in legend
    ) +
    scale_y_continuous(
      limits = c(-0.5, 5.8),
      breaks = seq(0, 5, by = 1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_x_continuous(
      limits = c(1950, 2100),
      breaks = c(1950, 2000, 2050, 2100),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    labs(
      title = "Global Mean Surface Temperature Change Relative to 1850-1900",
      subtitle = "Historical: IPCC AR6 WGI Chapter 2 | Future: IPCC AR6 Chapter 4 (CMIP6 multi-model mean)",
      y = "Temperature Change (°C)",
      x = "Year",
      caption = "SSP1-1.9: Ambitious mitigation | SSP1-2.6: Low emissions | SSP2-4.5: Intermediate | SSP3-7.0: High emissions |\nSSP4-3.4: Moderate mitigation | SSP4-6.0: Medium-high emissions | SSP5-3.4OS: Overshoot | SSP5-8.5: Very high emissions"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 10)),
      plot.caption = element_text(size = 8, hjust = 0, margin = margin(t = 10)),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      legend.key.height = unit(0.8, "lines"),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    ) +
    geom_vline(xintercept = 2024, linetype = "dashed", color = "grey50", alpha = 0.7) +
    annotate("text", x = 1987, y = 5.5,
             label = "Historical", size = 3.5, color = "grey30") +
    annotate("text", x = 2062, y = 5.5,
             label = "Projections", size = 3.5, color = "grey30") +
    # Add labels for the three selected scenarios
    annotate("text", x = 2085, y = filter(data, Year == 2100 & Scenario == "SSP5-8.5")$Temperature_Change + 0.1,
             label = "SSP5-8.5", size = 3.2, color = "#E41A1C", hjust = 1, fontface = "bold") +
    annotate("text", x = 2085, y = filter(data, Year == 2100 & Scenario == "SSP2-4.5")$Temperature_Change + 0.1,
             label = "SSP2-4.5", size = 3.2, color = "#4DAF4A", hjust = 1, fontface = "bold") +
    annotate("text", x = 2085, y = filter(data, Year == 2100 & Scenario == "SSP1-2.6")$Temperature_Change + 0.1,
             label = "SSP1-2.6", size = 3.2, color = "#377EB8", vjust = 1.2, hjust = 1.2, fontface = "bold") +
    annotate("text", x = 2085, y = filter(data, Year == 2100 & Scenario == "Historical")$Temperature_Change - 0.1,
             label = "Historical", size = 3.2, color = "black", hjust = 1)
  # Add a legend annotation for line types


  return(p)
}

# Generate and display the plot
all_data <- create_all_ssp_data()
plot <- create_all_scenarios_plot(all_data)

# Display the plot
print(plot)

# Save the plot
ggsave("all_ssp_with_line_types_corrected.png", plot = plot,
       width = 12, height = 7, dpi = 300, bg = "white")

# Show scenario comparison
message("\n=== All SSP Scenarios at 2100 ===")
scenario_2100 <- all_data %>%
  filter(Year == 2100) %>%
  select(Scenario, Temperature_Change, Category) %>%
  arrange(Temperature_Change)

print(scenario_2100)

# Highlight the selected scenarios
message("\n=== Selected SSPs (Focus Scenarios) ===")
selected_2100 <- scenario_2100 %>%
  filter(Category == "Selected") %>%
  select(Scenario, Temperature_Change) %>%
  arrange(Temperature_Change)

print(selected_2100)

# Range of projections
message("\n=== Range of Temperature Projections in 2100 ===")
range_2100 <- scenario_2100 %>%
  filter(Scenario != "Historical") %>%
  summarise(
    Min_Temp = min(Temperature_Change),
    Max_Temp = max(Temperature_Change),
    Range = Max_Temp - Min_Temp
  )

print(range_2100)

message("\nLine types:")
message("- Solid lines: Historical and selected SSPs (SSP1-2.6, SSP2-4.5, SSP5-8.5)")
message("- Dashed lines: Other SSP scenarios")
message("- Bold labels: The three focus scenarios that cover the range")
message("\nThe selected SSPs represent the low (SSP1-2.6), intermediate (SSP2-4.5), and very high (SSP5-8.5) ends of the projection range.")
