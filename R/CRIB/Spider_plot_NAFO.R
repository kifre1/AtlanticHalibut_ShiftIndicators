# Spider Plot of NAFO Zone Climate Vulnerability Data
# Colour scheme: warm (red/orange) = southern zones → cool (blue/purple) = northern zones
# Packages required: dplyr, fmsb

# install.packages(c("dplyr", "fmsb"))

library(dplyr)
library(fmsb)

# ── 1. Input data ─────────────────────────────────────────────────────────────
data_raw <- tibble::tribble(
  ~NAFO_zone, ~mean_vulnerability, ~mean_s_thermal_safety_margin, ~mean_e_climate_velocity,
  ~mean_yr_climate_velocity, ~mean_e_time_climate_emergence, ~mean_ac_thermal_habitat_availability,
  "2G",  0.413590531, 0.027878101, 0.022489252, 0.022489252, 2095.808081, 0.352676015,
  "2H",  0.408443861, 0.050236231, 0.013610098, 0.013610098, 2095.035962, 0.436961472,
  "2J",  0.394216311, 0.095897850, 0.027453522, 0.027453522, 2095.603331, 0.513354607,
  "3K",  0.387042494, 0.268729553, 0.057663223, 0.057663223, 2095.085906, 0.597131609,
  "3L",  0.429708219, 0.674611777, 0.053810219, 0.053810219, 2091.581142, 0.747625286,
  "3N",  0.487921677, 1.000000000, 0.046397349, 0.046397349, 2063.047641, 0.847850939,
  "3O",  0.485195329, 1.000000000, 0.027370456, 0.027370456, 2049.926471, 0.814528770,
  "3Pn", 0.466924104, 1.000000000, 0.157124456, 0.157124456, 2067.564103, 0.827342371,
  "3Ps", 0.471652876, 0.987872686, 0.061531755, 0.061531755, 2068.314794, 0.841126008,
  "4R",  0.474830685, 0.832770420, 0.048122084, 0.048122084, 2069.921163, 0.622698494,
  "4S",  0.465875492, 0.835001885, 0.075589021, 0.075589021, 2078.782991, 0.603663826,
  "4T",  0.519598411, 0.968315798, 0.063883351, 0.063883351, 2042.692688, 0.616671251,
  "4Vn", 0.494240224, 1.000000000, 0.167013231, 0.167013231, 2046.925639, 0.759867330,
  "4Vs", 0.476423457, 1.000000000, 0.077805466, 0.077805466, 2044.462898, 0.803605176,
  "4W",  0.507844278, 1.000000000, 0.040780185, 0.040780185, 2036.193756, 0.751649217,
  "4X",  0.530677610, 0.886045450, 0.092948385, 0.092948385, 2022.452652, 0.781688792,
  "5Y",  0.548811807, 0.739921122, 0.213446769, 0.213446769, 2017.777778, 0.784265292,
  "5Ze", 0.552518041, 1.000000000, 0.029923514, 0.029923514, 2015.000000, 0.736961634
)

# ── 2. Zone order: South → North ──────────────────────────────────────────────
# 5Ze and 5Y are southernmost; 2G is northernmost
south_to_north <- c("5Ze", "5Y", "4X", "4W", "4Vs", "4Vn", "4T", "4S", "4R",
                    "3Ps", "3Pn", "3O", "3N", "3L", "3K", "2J", "2H", "2G")

data_ordered <- data_raw %>%
  mutate(NAFO_zone = factor(NAFO_zone, levels = south_to_north)) %>%
  arrange(NAFO_zone)

# ── 3. Legend labels with emergence year ─────────────────────────────────────
legend_labels <- data_ordered %>%
  mutate(label = paste0(NAFO_zone, "  (", round(mean_e_time_climate_emergence), ")")) %>%
  pull(label)

# ── 4. Select the 5 radar variables ──────────────────────────────────────────
spider_data <- data_ordered %>%
  select(
    NAFO_zone,
    Vulnerability            = mean_vulnerability,
    `Thermal Safety Margin`  = mean_s_thermal_safety_margin,
    `Climate Velocity (E)`   = mean_e_climate_velocity,
    `Climate Velocity (YR)`  = mean_yr_climate_velocity,
    `Thermal Habitat Avail.` = mean_ac_thermal_habitat_availability
  )

# ── 5. Normalise to [0, 1] ────────────────────────────────────────────────────
spider_norm <- spider_data %>%
  mutate(across(-NAFO_zone, ~ (.x - min(.x)) / (max(.x) - min(.x))))

# ── 6. Build fmsb data frame ──────────────────────────────────────────────────
mat <- spider_norm %>%
  select(-NAFO_zone) %>%
  as.data.frame()

rownames(mat) <- spider_norm$NAFO_zone

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
png("spider_plot_NAFO_output.png", width = 1050, height = 780, res = 120, bg = "white")

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
