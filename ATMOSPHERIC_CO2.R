# Load visualization suite
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggtext")) install.packages("ggtext")

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggtext)

# 1. OFFICIAL CMIP6 INPUT DATA (Meinshausen et al., 2020)
# SSPs start diverging from historical reconstruction in 2015.
# We use anchor points to reconstruct the official curves.
ssp_years <- c(2015, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)

# SSP1-2.6: "Sustainability" (approx. 2C path)
# Peaks ~446 around 2050, then slowly declines/stabilizes.
ssp126_vals <- c(399.9, 411.3, 429.1, 440.3, 445.6, 446.1, 442.7, 436.2, 427.6, 417.8) # Note: Peaks then drops

# SSP2-4.5: "Middle of the Road" (Current trajectory-ish)
# Continues rising to ~603 ppm.
ssp245_vals <- c(399.9, 411.3, 430.7, 452.7, 477.5, 503.7, 530.7, 556.8, 581.4, 602.8)

# SSP5-8.5: "Fossil-Fueled Development" (Burn everything)
# Explosive growth to >1100 ppm.
ssp585_vals <- c(399.9, 411.3, 438.4, 478.7, 540.8, 626.6, 735.0, 861.9, 999.4, 1135.2)

# 2. HISTORICAL DATA (NOAA - Verified Jan 2026 context)
# 1980-2024 Observed; 2025 Forecast/Prelim.
hist_yrs <- c(1980, 1990, 2000, 2010, 2015, 2020, 2021, 2022, 2023, 2024, 2025)
hist_val <- c(338.7, 354.4, 369.5, 389.9, 400.8, 414.2, 416.4, 418.5, 421.1, 424.6, 426.6)

# 3. SPLINE INTERPOLATION (1980 - 2100)
all_years <- 1980:2100

make_spline <- function(x_yrs, y_vals) {
  # We bridge 1980-2015 using historical to ensure smooth start for SSPs
  bridge_x <- c(1980, 1990, 2000, 2010, x_yrs)
  bridge_y <- c(338.7, 354.4, 369.5, 389.9, y_vals)
  spline(bridge_x, bridge_y, xout = all_years)$y
}

df_cmip6 <- data.frame(
  Year = all_years,
  SSP126 = make_spline(ssp_years, ssp126_vals),
  SSP245 = make_spline(ssp_years, ssp245_vals),
  SSP585 = make_spline(ssp_years, ssp585_vals)
) %>%
  pivot_longer(-Year, names_to = "Scenario", values_to = "PPM")

df_hist_obs <- data.frame(Year = hist_yrs, PPM = hist_val)

# 4. PLOTTING THE CMIP6 STANDARD
ggplot() +

  # A. ERA CONTEXT (Shading)
  annotate("rect", xmin = 1980, xmax = 2025, ymin = -Inf, ymax = Inf,
           fill = "grey94", alpha = 0.5) +
  geom_vline(xintercept = 2025, color = "black", linetype = "solid", size = 0.5) +

  # B. CMIP6 PROJECTIONS
  geom_line(data = df_cmip6, aes(x = Year, y = PPM, color = Scenario), size = 1.1) +

  # C. HISTORICAL REALITY (Overlay)
  geom_line(data = df_hist_obs, aes(x = Year, y = PPM), color = "black", size = 1.6) +
  geom_point(data = df_hist_obs %>% filter(Year == 2025), aes(x = Year, y = PPM),
             size = 4, color = "black") +

  # D. LABELS & ANNOTATION
  annotate("text", x = 2030, y = 1000, label = "CMIP6 / SSPs\nProjections",
           hjust = 0, fontface = "bold", color = "grey40") +

  annotate("label", x = 2025, y = 460, label = "2025: 426.6 ppm\n(Tracking SSP2-4.5)",
           fill = "black", color = "white", size = 3.5, fontface = "bold") +

  # E. STYLING
  scale_color_manual(
    values = c("SSP126" = "#2c7bb6", "SSP245" = "#fdae61", "SSP585" = "#d7191c"),
    labels = c(
      "SSP1-2.6 (Sustainability)",
      "SSP2-4.5 (Continuation)",
      "SSP5-8.5 (Conventional Development)"
    )
  ) +

  scale_y_continuous(limits = c(330, 1200), breaks = seq(350, 1150, 150)) +
  scale_x_continuous(expand = c(0,0), breaks = seq(1980, 2100, 20)) +

  labs(
    title = "<span style='font-size:16pt'>**CMIP6** Projected CO<sub>2</sub> Pathways</span>",
    y = "Atmospheric CO2 Concentration (ppm)",
    x = NULL,
    color = NULL,

  ) +

  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_markdown(),
    legend.position = c(0.2, 0.85),
    legend.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )
