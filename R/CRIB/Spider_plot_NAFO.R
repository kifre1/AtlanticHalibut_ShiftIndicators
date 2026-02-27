# Spider Plot of NAFO Zone Climate Vulnerability Data
# Colour scheme: warm (red/orange) = southern zones → cool (blue/purple) = northern zones
# Packages required: dplyr, fmsb

# install.packages(c("dplyr", "fmsb"))

library(dplyr)
library(fmsb)

# ── 1. Input data ─────────────────────────────────────────────────────────────
data_raw <- read.csv("CRIB results/crib_AtlHalibut_byNAFO_SSPs.csv")
unique(data_raw$NAFO_Zones)
names(data_raw)

# ── 2. Zone order: South → North ──────────────────────────────────────────────
# 5Ze and 5Y are southernmost; 2G is northernmost
south_to_north <- c("5YZ6A", "4X", "4VW", "4RST", "3NOPs", "3KL", "2JHG")

data_ordered <- data_raw %>%
  mutate(NAFO_Zones = factor(NAFO_Zones, levels = south_to_north)) %>%
  arrange(NAFO_Zones)

df1<- data_ordered[data_ordered$ssp=="SSP1-2.6",]
df2<- data_ordered[data_ordered$ssp=="SSP5-8.5",]

# ── 3. Legend labels with emergence year ─────────────────────────────────────
legend_labels <- df1 %>%
  mutate(label = paste0(NAFO_Zones, "  (", round(mean_yr_climate_emergence), ")")) %>%
  pull(label)

# ── 4. Select the 5 radar variables ──────────────────────────────────────────
spider_data <- df1 %>%
  select(
    NAFO_Zones,
    Vulnerability            = mean_vulnerability,
    `Thermal Safety Margin`  = mean_s_thermal_safety_margin,
    `Climate Velocity`   = mean_e_climate_velocity,
    `Time of Climate Emergence`  = mean_e_time_climate_emergence,
    `Thermal Habitat Variab.` = mean_ac_thermal_habitat_variability
  )

# ── 5. Normalise to [0, 1] ────────────────────────────────────────────────────
spider_norm <- spider_data %>%
  mutate(across(-NAFO_Zones, ~ (.x - min(.x)) / (max(.x) - min(.x))))

# ── 6. Build fmsb data frame ──────────────────────────────────────────────────
mat <- spider_norm %>%
  select(-NAFO_Zones) %>%
  as.data.frame()

rownames(mat) <- spider_norm$NAFO_Zones

fmsb_df <- rbind(
  Max = rep(1, ncol(mat)),
  Min = rep(0, ncol(mat)),
  mat
)

# ── 7. Gradient palette: warm (south) → cool (north) ─────────────────────────
# Red/orange → yellow → teal → blue → purple
n <- nrow(mat)
gradient_colors <- colorRampPalette(
  c("#D73027",   # deep red      (southernmost)
    "#F46D43",   # orange-red
    "#FDAE61",   # orange
    "#FEE090",   # light yellow
    "#74C476",   # mid green
    "#41B6C4",   # teal
    "#2171B5",   # medium blue
    "#4A1486")   # deep purple   (northernmost)
)(n)

# ── 8. Plot ───────────────────────────────────────────────────────────────────
png("CRIB results/spider_plot_NAFO_SSP126.png", width = 1050, height = 780, res = 120, bg = "white")

par(bg = "white", mar = c(1, 1, 4, 1))

radarchart(
  fmsb_df,
  axistype   = 0,
  seg        = 4,
  pcol       = gradient_colors,
  pfcol      = NA,
  plwd       = 1.8,
  plty       = 1,
  cglcol     = "grey82",
  cglty      = 1,
  cglwd      = 0.6,
  axislabcol = "grey50",
  vlcex      = 0.78,
  title      = "Climate Vulnerability Indicators by NAFO Zone (normalised)\nTime to Climate Emergence (year) shown in legend"
)

# ── 9. Legend ─────────────────────────────────────────────────────────────────
legend(
  x         = "topleft",
  legend    = legend_labels,
  col       = gradient_colors,
  lty       = 1,
  lwd       = 2.2,
  bty       = "n",
  cex       = 0.62,
  text.col  = "grey20",
  ncol      = 2,
  title     = expression(bold("NAFO Zone  (Emergence yr)")),
  title.col = "grey25"
)

# ── 10. Gradient colour bar (south → north annotation) ───────────────────────
# Draw a small vertical gradient bar on the right side
bar_x  <- 0.97          # normalised plot coordinates
bar_y0 <- -0.55
bar_y1 <-  0.55
bar_w  <- 0.03
n_bar  <- 100
bar_colors <- colorRampPalette(c("#D73027","#F46D43","#FDAE61","#FEE090",
                                  "#74C476","#41B6C4","#2171B5","#4A1486"))(n_bar)
bar_yseq <- seq(bar_y0, bar_y1, length.out = n_bar + 1)

for (i in seq_len(n_bar)) {
  rect(bar_x, bar_yseq[i], bar_x + bar_w, bar_yseq[i + 1],
       col = bar_colors[i], border = NA)
}
rect(bar_x, bar_y0, bar_x + bar_w, bar_y1, col = NA, border = "grey60", lwd = 0.8)
text(bar_x + bar_w + 0.02, bar_y1,  "North", cex = 0.6, adj = 0, col = "grey30")
text(bar_x + bar_w + 0.02, bar_y0,  "South", cex = 0.6, adj = 0, col = "grey30")

dev.off()
message("Plot saved to: spider_plot_NAFO_output.png")
