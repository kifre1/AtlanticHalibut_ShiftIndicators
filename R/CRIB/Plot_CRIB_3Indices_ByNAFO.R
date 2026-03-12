library(ggplot2)
library(cowplot)
library(patchwork)
library(gridExtra)
library(RColorBrewer)
library(colorspace)
library(viridis)
library(grid)
library(ggnewscale)

# ── Gradient background helpers ───────────────────────────────────────────────
# direction =  1 → viridis low→high  (purple → yellow) left to right
# direction = -1 → viridis high→low  (yellow → purple) left to right

make_gradient_df <- function(xmin, xmax, n = 200, direction = 1) {
  fills <- if (direction == 1) seq(0, 1, length.out = n) else seq(1, 0, length.out = n)
  data.frame(
    xmin = seq(xmin, xmax, length.out = n + 1)[-(n + 1)],
    xmax = seq(xmin, xmax, length.out = n + 1)[-1],
    fill = fills
  )
}

gradient_rects <- function(df) {
  geom_rect(
    data        = df,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
    inherit.aes = FALSE,
    alpha       = 0.45
  )
}

viridis_fill_scale <- scale_fill_gradientn(
  colours = viridis(256, option = "D"),
  limits  = c(0, 1),
  name    = "Value",
  guide   = "none"      # hidden here; shown via shared bottom legend
)

# ── Common theme ──────────────────────────────────────────────────────────────
base_theme <- theme_bw(base_size = 14) +
  theme(
    panel.grid.major.x = element_line(colour = "white", linewidth = 0.3),
    panel.grid.minor   = element_blank(),
    panel.border       = element_rect(colour = "black", fill = NA),
    axis.ticks         = element_line(colour = "black"),
    plot.margin        = margin(5, 5, 5, 5)
  )

#--- Get Halibut data ---
halcrib<-read.csv("Data/CRIB/crib_halibut.csv")
halcrib$ToE.year<-2015+(-log(halcrib$E.Time.of.climate.emergence)/0.033) #calculate raw ToE's from standardized

library(sf)      # For spatial data handling
library(dplyr)   # For summarizing grouped data
library(here)
# Convert halcrib data frame to a spatial object
halcrib1 <- st_as_sf(halcrib, coords = c("longitude", "latitude"), crs = "WGS84")
# Reproject both datasets to align their CRS (e.g., EPSG:4326)
halcrib1 <- st_transform(halcrib1, crs = st_crs(NAFO))
# Perform a spatial join to add the NAFO zone info to each point in halcrib
halcrib_with_zones <- st_join(halcrib1, NAFO, na.rm=TRUE)

# Check the first few rows of the resulting dataset
head(halcrib_with_zones)
names(halcrib_with_zones)
unique(halcrib_with_zones$ZONE)
# Remove rows where NAFO_ID or ZONE are NA
halcrib_with_zones_clean <- halcrib_with_zones[] %>%
  filter(!is.na(NAFO_ID), !is.na(ZONE))
unique(halcrib_with_zones_clean$ZONE)
head(halcrib_with_zones_clean)

# Add new NAFO groups
halcrib_with_zones_clean1 <- halcrib_with_zones_clean %>%
  mutate(
    NAFO_Zones = case_when(
      ZONE %in% c("4Vn", "4Vs", "4W") ~ "4VW",    # If ZONE is one of these values, assign "4VW"
      ZONE %in% c("4X") ~ "4X",
      ZONE %in% c("3N","3O","3Pn","3Ps") ~ "3NOPs",
      ZONE %in% c("3K","3L") ~ "3KL",
      ZONE %in% c("2J","2H","2G") ~ "2JHG",
      ZONE %in% c("5Y","5Ze", "5Zw", "6A") ~ "5YZ6A",
      ZONE %in% c("4R","4S","4T") ~ "4RST"
    )
  )%>%
  filter(!is.na(NAFO_Zones))  # Remove rows where NAFO_Zones is NA
head(halcrib_with_zones_clean1)


# ── Plot 1 – Thermal Safety Margin (direction = 1: purple → yellow) ──────────
grad_tsm <- make_gradient_df(xmin = 0, xmax = 1, direction = 1)
p1 <- ggplot(halcrib_with_zones_clean1,
             aes(x = S.Thermal.safety.margin, y = NAFO_Zones)) +
  gradient_rects(grad_tsm) +
  viridis_fill_scale +
  geom_boxplot(
    fill         = "grey50",
    colour       = "black",
    alpha        = 0.3,
    outlier.size = 0.8,
    width        = 0.6
  ) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  annotate("text", x = 0.97, y = 0.5, label = "a",
           hjust = 1, vjust = -0.4, size = 5, fontface = "bold") +
  labs(
    title = NULL,
    x     = "Thermal Sensitivity (TSM)",
    y     = "NAFO Zones (South to North)"
  ) +
  base_theme +
  theme(legend.position = "none")

# ── Plot 2 – Thermal Habitat Availability (direction = -1: yellow → purple) ──
grad_thv <- make_gradient_df(xmin = 0, xmax = 1, direction = -1)

p2 <- ggplot(halcrib_with_zones_clean1,
             aes(x = AC.Thermal.habitat.availability, y = NAFO_Zones)) +
  gradient_rects(grad_thv) +
  viridis_fill_scale +
  geom_boxplot(
    fill         = "grey50",
    colour       = "black",
    alpha        = 0.3,
    outlier.size = 0.8,
    width        = 0.6
  ) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  annotate("text", x = 0.97, y = 0.5, label = "b",
           hjust = 1, vjust = -0.4, size = 5, fontface = "bold") +
  labs(
    title = NULL,
    x     = "Thermal Adaptability (THV)",
    y     = NULL
  ) +
  base_theme +
  theme(
    legend.position = "none",
    axis.text.y     = element_blank(),
    axis.ticks.y    = element_blank(),
    plot.margin     = margin(5, 5, 5, 20)   # extra left margin to avoid label overlap with p1
  )

# ── Plot 3 – Time of Emergence (direction = -1: yellow → purple) ─────────────
toe_min <- floor(min(halcrib_with_zones_clean1$ToE.year,   na.rm = TRUE) / 10) * 10
toe_max <- ceiling(max(halcrib_with_zones_clean1$ToE.year, na.rm = TRUE) / 10) * 10
grad_toe <- make_gradient_df(xmin = toe_min, xmax = toe_max, direction = -1)

p3 <- ggplot(halcrib_with_zones_clean1,
             aes(x = ToE.year, y = NAFO_Zones)) +
  # Layer 1: continuous viridis gradient rectangles + its own scale
  gradient_rects(grad_toe) +
  scale_fill_gradientn(
    colours = viridis(256, option = "D"),
    limits  = c(0, 1),
    guide   = "none"    # hidden; shown by shared bottom legend
  ) +
  # Open a NEW fill scale for the SSP boxplots (ggnewscale)
  new_scale_fill() +
  geom_boxplot(
    aes(fill = ssp),
    colour       = "black",
    alpha        = 0.5,
    outlier.size = 0.8,
    width        = 0.55,
    position     = position_dodge(width = 0.75)
  ) +
  scale_fill_manual(
    values = c("SSP1-2.6" = "grey80", "SSP5-8.5" = "grey15"),
    name   = "SSP Scenario"
  ) +
  scale_x_continuous(
    limits = c(toe_min, toe_max),
    expand = c(0, 0),
    breaks = pretty(c(toe_min, toe_max), n = 5)
  ) +
  annotate("text", x = toe_max - (toe_max - toe_min) * 0.03, y = 0.5, label = "c",
           hjust = 1, vjust = -0.4, size = 5, fontface = "bold") +
  labs(
    title = NULL,
    x     = "Time of Emergence (ToE Year)",
    y     = NULL
  ) +
  base_theme +
  theme(
    axis.text.y      = element_blank(),
    axis.ticks.y     = element_blank(),
    legend.position  = c(0.02, 0.98),        # inside top-left of panel
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.7), colour = NA),
    legend.key.size   = unit(0.5, "cm"),
    legend.text       = element_text(size = 11),
    legend.title      = element_text(size = 12)
  )

# ── Shared viridis gradient legend (bottom) ───────────────────────────────────
# Blend viridis colours with white at alpha = 0.45 to match panel backgrounds
# Uses base R only: col2rgb → interpolate toward white → rgb()
blend_with_white <- function(hex_cols, alpha = 0.45) {
  m <- col2rgb(hex_cols) / 255          # 3 x n matrix, values 0–1
  r <- m * alpha + (1 - alpha)          # mix toward white (1,1,1)
  rgb(r[1, ], r[2, ], r[3, ])
}
pale_cols <- blend_with_white(viridis(256, option = "D"), alpha = 0.45)

legend_plot <- ggplot(data.frame(x = 0:1, y = 0), aes(x = x, y = y, fill = x)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = pale_cols,
    limits  = c(0, 1),
    name    = "Low \u2192 High Vulnerability",
    guide   = guide_colourbar(
      title.position = "top",
      title.hjust    = 0.5,
      barwidth       = unit(12, "cm"),
      barheight      = unit(0.4, "cm"),
      direction      = "horizontal"
    )
  ) +
  theme_void() +
  theme(legend.position = "bottom")

shared_legend <- get_legend(legend_plot)

# ── Assemble ──────────────────────────────────────────────────────────────────
combined <- (p1 | p2 | p3) +
  plot_layout(widths = c(1.15, 1, 1.3))

final_plot <- plot_grid(
  combined,
  shared_legend,
  ncol        = 1,
  rel_heights = c(1, 0.12)
)

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave(
  filename = "CRIB results/AtlHalibut_CRIB_3Indices_ByNAFO.png",
  plot     = final_plot,
  width    = 18,
  height   = 7,
  dpi      = 300,
  bg       = "white"
)

message("Done! Plot saved as nafo_panels_v2.png")
