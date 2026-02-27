library(tidyverse)
library(broom)
library(dplyr)

library(ggplot2)
library(cowplot)
library(patchwork)
library(gridExtra)
library(RColorBrewer)
library(scales)

library(grid)  # For unit() function
#####END theme----
#Element 1: EAO, Data prep/ slopes over time ----
#NOTE a lot of this is derived from EAOrmarkdown which is in the NancBranchDataScript folder
#NancBranchDataScript/EAOrmarkdown.Rmd
#where spring was isolated

Area_ThresholdsforEAO<- read.csv(here::here("R/DataforFinalFigs/Area_ThresholdsforEAO.csv"))
names(Area_ThresholdsforEAO)
Area_ThresholdsforEAO$Period[Area_ThresholdsforEAO$Period == "Before Warming"] <- "1990-2005"
Area_ThresholdsforEAO$Period[Area_ThresholdsforEAO$Period == "During Warming"] <- "2006-2023"

#slopes for EAO over time
Area_ThresholdsforEAO_coefficients_df <- Area_ThresholdsforEAO %>%filter(Threshold == 90)%>%
  group_by(Region,Period) %>%
  do({
    #model <- lm(scale(log10((Area_Threshold))) ~ scale(Year), data = .)
    model <- lm(log10((Area_Threshold)) ~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()
Area_ThresholdsforEAO_coefficients_df<- Area_ThresholdsforEAO_coefficients_df%>%
  #filter(term == "scale(Year)")
  filter(term == "Year")
#####END data prep and slopes for EAO over time----
#Plot: EOA----
#IN 
EAOplot<-ggplot(Area_ThresholdsforEAO %>% filter(Threshold == 90), 
                #aes(x = Year, y =Area_Threshold/1000000, color = Region, group = Region)) +
                aes(x = Year, y =Area_Threshold/1000000)) +
  geom_line(linewidth = 1,color="darkgray") +
  scale_y_log10(
    # breaks = c(0.1, 0.5, 1, 2, 5, 10, 20, 50),
    #labels = c("0.1", "0.5", "1", "2", "5", "10", "20", "50")
  ) +
  scale_x_continuous(breaks = seq(1990, 2023, by = 5)) +
  #scale_fill_manual(values = regpal) +                    # Custom fill colors
  #scale_color_manual(values = regpal) +
  guides(color = guide_legend(title = ""))+
  geom_vline(xintercept=2005,lty=2,lwd=1.2)+
  # Faceting by region
  facet_wrap(Region ~ ., 
             scales = "free",
             labeller = label_wrap_gen(width = 15)) +
  labs(title = "",
       x = expression(""),
       y = expression(atop("Area Occupied", "(Millions km  "^2*")")))+
  # Clean theme
  theme_bw() +
  
   theme(text = element_text(family = "serif"),  
        #legend.box.background = element_blank(), # Transparent legend box
        #legend.position = c(.7,.5),
        #legend.text = element_text(size = 14,family="serif"),
        plot.margin=margin(10, 10, 10, 10),
        
        # General text and styling
        #text = element_text(family = "serif"),  
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 12),

        # Strip styling
        strip.text = element_text(size = 12),
        strip.background = element_rect(colour = "black", fill = "white"),
        
        # Grid lines
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()
   ) 
  
EAOplot

#####END Plot EOA----

#ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/EAOplot.jpeg"), plot = EAOplot, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg")

#Element 2:  Plot EAOvsAbd: Tidied Area vs Abundance Plot----
library(ggpmisc)
names(Area_ThresholdsforEAO$Period)[names(Area_ThresholdsforEAO$Period) == "Before Warming"] <- "1990-2005"
names(Area_ThresholdsforEAO$Period)[names(Area_ThresholdsforEAO$Period) == "During Warming"] <- "2006-2023"
PlotEAOAbd <- ggplot(Area_ThresholdsforEAO %>% filter(Threshold == 90),
                     aes(y = Area_Threshold/1000000,  # Remove log10() transformation
                         x = Total_Abundance/1000000)) +  # Remove log10() transformation
  # Points with better styling
  geom_point(aes(color = Period), 
             size = 1.5, 
             alpha = 0.7) +
  scale_color_manual(values = c("1990-2005" = "steelblue", "2006-2023" = "orangered"))+
  # Smoothed regression lines
  geom_smooth(method = "lm", 
              se = FALSE,
              aes(group = interaction(Region, Period), 
                  color = Period),
              alpha = .4) +
  # Add log10 scales for both axes
  scale_x_log10() +  # This replaces the log10() transformation in aes()
  scale_y_log10(
    name = NULL,
    sec.axis = sec_axis(~., name = expression(atop("Area Occupied", "(Millions km  "^2*")")))
  )+
  # Faceting by region
  facet_wrap(Region ~ ., 
             scales = "free",
             labeller = label_wrap_gen(width = 15)) +
  # Improved labels - you can keep the log notation in labels
  labs(title = NULL,
       x =  expression("Total Annual Abundance (Millions)"),
       #  y = "",#expression(atop("Area Occupied", "(Millions km  "^2*")")),
       color = "Period") +
  guides(fill = "none") +
  # Clean the
  theme_bw() +
  theme(
    # Move y-axis title to the right, remove LHS label and RHS ticks/text
    axis.title.y = element_blank(),
    axis.title.y.right = element_text(
      angle = 90, 
      vjust = 0.5,
      family = "serif",
      size = 12
    ),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    #plot.margin=margin(0, 0, 0, 0),
    
    # General text and styling
    text = element_text(family = "serif"),  
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    
    # Legend styling
    legend.box.background = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.margin     = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.key = element_rect(fill = "white", color = NA),
    
    # Strip styling
    strip.text = element_text(size = 12),
    strip.background = element_rect(colour = "black", fill = "white"),
    
    # Grid lines
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  ) 
  #scale_y_log10(
   # sec.axis = dup_axis(name = expression(atop("Area Occupied", "(Millions km  "^2*")")))
 # )

# Display the plot
print(PlotEAOAbd)

# Alternative: If you want different colors for periods
# PlotEAOAbd + scale_color_manual(values = c("Period1" = "steelblue", "Period2" = "orangered"))
#####END PlotEAOvsAbd, scaled slopes fro EAO vs Abundance----
#ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/PlotEAOAbd.jpeg"), plot = PlotEAOAbd, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg")

#Element 3: Range Edge  ----
rangeedge<-read.csv(here::here("R/DataforFinalFigs/Edge_df_NSreshp.csv"))
range.spr<-rangeedge[rangeedge$Season=="Spring",]
range.spr$Period<-NULL
range.spr$Period[range.spr$Year<2006]<-"1990-2005"
range.spr$Period[range.spr$Year>2005]<-"2006-2023"
names(range.spr);summary(range.spr)

##slope for range ----
#Leading Edge N95 dna E 95 From Canada?
range.sprE_coefficients_df <- range.spr %>% 
  group_by(Period) %>%
  do({
    model <- lm(scale((Estimate_km_E_quantile_0.95)) ~ scale(Year), data = .)
    # model <- lm(Estimate_km_E_quantile_0.5~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()
range.sprN_coefficients_df <- range.spr %>%
  group_by(Period) %>%
  do({
    model <- lm(scale((Estimate_km_N_quantile_0.95)) ~ scale(Year), data = .)
    #model <- lm(Estimate_km_N_quantile_0.5 ~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()

range.sprE_coefficients_df <- range.sprE_coefficients_df%>%
  filter(term == "scale(Year)")
# filter(term == "Year")
range.sprN_coefficients_df <- range.sprN_coefficients_df%>%
  filter(term == "scale(Year)")
#filter(term == "Year")
#Trailing 05 and assign to  USA?

range.sprE_coefficients_dfTR <- range.spr %>% 
  group_by(Period) %>%
  do({
    model <- lm(scale((Estimate_km_E_quantile_0.05)) ~ scale(Year), data = .)
    # model <- lm(Estimate_km_E_quantile_0.5~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()
range.sprN_coefficients_dfTR <- range.spr %>%
  group_by(Period) %>%
  do({
    model <- lm(scale((Estimate_km_N_quantile_0.05)) ~ scale(Year), data = .)
    #model <- lm(Estimate_km_N_quantile_0.5 ~ Year, data = .)
    data.frame(t(coef(model)))
    tidy(model, conf.int = TRUE) # Includes coefficients with 95% CI by default
  }) %>%
  ungroup()

range.sprE_coefficients_dfTR <- range.sprE_coefficients_dfTR%>%
  filter(term == "scale(Year)")
# filter(term == "Year")
range.sprN_coefficients_dfTR <- range.sprN_coefficients_dfTR%>%
  filter(term == "scale(Year)")
#filter(term == "Year")


##END slope for range----
# Estimate distance shifted.----
library(geosphere)
names(range.spr)
##CHECK THE FLOOLOOWING.NOT SUR EIF RIGHT----
Leadrange.sprAgg<-range.spr%>% 
  mutate(
    # YearGroup = cut(Year, breaks = seq(1990, 2023, by = 5), right = FALSE)
    YearGroup = cut(Year, breaks = seq(1990, 2025, by = 5), right = FALSE)
  ) %>%
  group_by(YearGroup) %>%
  summarise(AveE = mean(Estimate_km_E_quantile_0.95, na.rm = TRUE),
            AveN = mean(Estimate_km_N_quantile_0.95, na.rm = TRUE))
# Reference point (first group)
ref_point <- Leadrange.sprAgg%>%
  filter(YearGroup == unique(YearGroup)[1]) %>%
  slice(1)  # just in case there are multiple rows

# Reference point (last group)
final_point <- Leadrange.sprAgg %>%
  filter(YearGroup == unique(YearGroup)[7]) %>%
  slice(1) 

# Vector of distances from the reference point to each group
# Calculate Euclidean distance to all other points
Leadrange.sprAgg <- Leadrange.sprAgg %>%
  arrange(YearGroup) %>%  # ensure chronological order
  mutate(
    PrevE = lag(AveE),
    PrevN = lag(AveN),
    StepDist = sqrt((AveE - PrevE)^2 + (AveN - PrevN)^2),
    CumulativeDist = cumsum(replace_na(StepDist, 0))
  )


library(dplyr)
centroid_1 <- Leadrange.sprAgg %>% filter(YearGroup == unique(YearGroup)[1]) %>% slice(1)
centroid_7 <- Leadrange.sprAgg %>% filter(YearGroup == unique(YearGroup)[7]) %>% slice(1)

# Directional shifts
shift_Easting <- centroid_7$AveE - centroid_1$AveE
shift_Northing <- centroid_7$AveN - centroid_1$AveN

# Total Euclidean shift
total_shift <- sqrt(shift_Easting^2 + shift_Northing^2)
shift_summary <- tibble(
  Direction = c("Easting", "Northing", "Total"),
  Shift = c(shift_Easting, shift_Northing, total_shift)
)

print(shift_summary)

set.seed(123)  # For reproducibility
pos_jitter <- position_jitter(width = .9, height = .9)  # Jitter both horizontally and vertically
names(range.spr)


#END range edge distance----
names(range.spr)
######################
#plot: RangeEdgeV3_large_terminal, Parked and replaced with mapped version----
# Larger terminal points with "final" annotation
RangeEdgeV3_large_terminal <- ggplot(range.spr,
                                     aes(x = Estimate_km_E_quantile_0.05, 
                                         y = Estimate_km_N_quantile_0.05, 
                                         color = Year)) +
  scale_x_continuous(breaks = seq(-500, 1300, by = 100)) +
  scale_y_continuous(breaks = seq(4700, 6000, by = 100)) +
  # Path for 0.05 quantile points with color gradient (no arrow)
  geom_path(aes(group = 1), 
            lwd = 1) +
  
  # Path for 0.95 quantile points with color gradient (no arrow)
  geom_path(aes(x = Estimate_km_E_quantile_0.95, 
                y = Estimate_km_N_quantile_0.95, 
                group = 2), 
            lwd = 1) +
  
  # Regular points for 0.05 quantile
  geom_point(position = pos_jitter, 
             na.rm = TRUE, 
             alpha = .75, 
             size = 2, 
             shape = 19) +
  
  # Regular points for 0.95 quantile
  geom_point(aes(x = Estimate_km_E_quantile_0.95, 
                 y = Estimate_km_N_quantile_0.95, 
                 color = Year),
             position = pos_jitter, 
             na.rm = TRUE, 
             alpha = .75, 
             size = 2, 
             shape = 19) +
  
  # Larger terminal point for 0.05 quantile (most recent year)
  geom_point(data = range.spr[range.spr$Year == max(range.spr$Year, na.rm = TRUE), ],
             aes(x = Estimate_km_E_quantile_0.05, 
                 y = Estimate_km_N_quantile_0.05, 
                 color = Year),
             position = pos_jitter, 
             na.rm = TRUE, 
             alpha = 1, 
             size = 5, 
             shape = 19) +
  
  # Larger terminal point for 0.95 quantile (most recent year)
  geom_point(data = range.spr[range.spr$Year == max(range.spr$Year, na.rm = TRUE), ],
             aes(x = Estimate_km_E_quantile_0.95, 
                 y = Estimate_km_N_quantile_0.95, 
                 color = Year),
             position = pos_jitter, 
             na.rm = TRUE, 
             alpha = 1, 
             size = 5, 
             shape = 19) +
  
  # Annotation for 0.05 quantile terminal point
  annotate("text",
           # x = range.spr$Estimate_km_E_quantile_0.05[range.spr$Year == max(range.spr$Year, na.rm = TRUE)]+100,
           #y = range.spr$Estimate_km_N_quantile_0.05[range.spr$Year == max(range.spr$Year, na.rm = TRUE)] + 
           # (max(range.spr$Estimate_km_N_quantile_0.05, na.rm = TRUE) - 
           #   min(range.spr$Estimate_km_N_quantile_0.05, na.rm = TRUE)) * 0.03,
           x=-100,y=4750,
           label = "Trailing Edge",
           color = "black",
           size = 5,
           family = "serif") +
  
  # Annotation for 0.95 quantile terminal point  
  annotate("text",
           # x = range.spr$Estimate_km_E_quantile_0.95[range.spr$Year == max(range.spr$Year, na.rm = TRUE)],
           #y = range.spr$Estimate_km_N_quantile_0.95[range.spr$Year == max(range.spr$Year, na.rm = TRUE)] + 
           # (max(range.spr$Estimate_km_N_quantile_0.95, na.rm = TRUE) - 
           #   min(range.spr$Estimate_km_N_quantile_0.95, na.rm = TRUE)) * 0.03,
           x=500,y=5300,
           label = "Leading Edge",
           color = "black",
           size = 5,
           family = "serif") +
  
  # Blue to orange gradient for Year
  scale_color_gradient2(low = "steelblue3", 
                        mid = "yellow", 
                        high = "orangered", 
                        midpoint = 2005, 
                        name = "Year") +
  
  # Labels
  xlab("Range Edge W to E (km)") +
  ylab("Range Edge S to N (km)") +
  
  # Theme
  theme_bw() +
  theme(
    legend.position = "right",
    plot.margin = margin(5, 10, 10, 10),
    axis.text.x = element_text(angle = 90, 
                               vjust = 0, 
                               hjust = 0.5, 
                               size = 14, 
                               family = "serif"),
    axis.text.y = element_text(angle = 0, 
                               vjust = 0.5, 
                               hjust = 0, 
                               size = 14, 
                               family = "serif"),
    axis.title.x = element_text(size = 14, 
                                hjust = 0.5, 
                                vjust = -2, 
                                family = "serif"),
    axis.title.y = element_text(size = 14, 
                                hjust = 0.5, 
                                vjust = 4, 
                                angle = 90, 
                                family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 14, family = "serif"),
    strip.text = element_text(size = 14, 
                              family = "serif", 
                              angle = 0),
    strip.background = element_rect(colour = "black", 
                                    fill = "white"),
    panel.grid.minor = element_blank(), panel.grid.major =  element_blank(),
  )

RangeEdgeV3_large_terminal

######################
#Prepwork to plot Range Edge Mapped------
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(ggplot2)
library(dplyr)
library(geosphere)
library(ggpmisc)

RE_DAT<- read.csv(here::here("R/DataforFinalFigs/Edge_df_NSreshp.csv"))
names(RE_DAT)
# Get high-res coastline
coast <- ne_coastline(scale = "large", returnclass = "sf")
######################
#test on subset----
# Isolate coordinates
Est_0.5 <- RE_DAT[, c("Year", "Estimate_km_E_quantile_0.5", "Estimate_km_N_quantile_0.5")]

# Convert from km to m
Est_0.5$Estimate_km_E_quantile_0.5 <- Est_0.5$Estimate_km_E_quantile_0.5 * 1000
Est_0.5$Estimate_km_N_quantile_0.5 <- Est_0.5$Estimate_km_N_quantile_0.5 * 1000

# Make sf object in local UTM (meters)
Est_0.5_pts <- st_as_sf(Est_0.5,
                        coords = c("Estimate_km_E_quantile_0.5", "Estimate_km_N_quantile_0.5"),
                        crs = "EPSG:32621")

# Reproject to WGS84
Est_0.5_pts2 <- st_transform(Est_0.5_pts, crs = 4326)

# Plot
ggplot() +
  geom_sf(data = coast, color = "gray50") +
  geom_sf(data = Est_0.5_pts2, color = "blue", size = 1) +
  #coord_sf(xlim = c(-80, -40), ylim = c(35, 60), expand = FALSE) +
  theme_minimal() +
  labs(title = "Sample Projected Coordinates in Northwest Atlantic",
       subtitle = "Reprojected from km-based custom CRS to WGS84")
######################
#function to reproject for plotting----
#separate each set of coordinates into their own objects
Est_0.05<-RE_DAT[ , c("Year","Estimate_km_E_quantile_0.05", "Estimate_km_N_quantile_0.05" )]
Est_0.5<-RE_DAT[ , c("Year", "Estimate_km_E_quantile_0.5",  "Estimate_km_N_quantile_0.5")] 
Est_0.95<-RE_DAT[ , c("Year", "Estimate_km_E_quantile_0.95",  "Estimate_km_N_quantile_0.95")] 
Std_0.05<-RE_DAT[ , c("Year", "Std_Dev_km_E_quantile_0.05",  "Std_Dev_km_N_quantile_0.05")]
Std_0.5<-RE_DAT[ , c("Year", "Std_Dev_km_E_quantile_0.5", "Std_Dev_km_N_quantile_0.5")] 
Std_0.95<-RE_DAT[ , c("Year", "Std_Dev_km_E_quantile_0.95", "Std_Dev_km_N_quantile_0.95")] 

#function
convert_km_to_wgs84 <- function(df, east_col, north_col) {
  # Convert from km to m
  df[[east_col]] <- df[[east_col]] * 1000
  df[[north_col]] <- df[[north_col]] * 1000
  # Make sf object in local CRS
  pts_local <- st_as_sf(df, coords = c(east_col, north_col), crs = "EPSG:32621")
  # Reproject to WGS84
  pts_wgs84 <- st_transform(pts_local, crs = 4326)
  return(pts_wgs84)
}

#Trailing edge Estimates
Est_0.05_pts <- convert_km_to_wgs84(
  Est_0.05,
  east_col = "Estimate_km_E_quantile_0.05",
  north_col = "Estimate_km_N_quantile_0.05"
)
#Mean Estimates
Est_0.5_pts <- convert_km_to_wgs84(
  Est_0.5,
  east_col = "Estimate_km_E_quantile_0.5",
  north_col = "Estimate_km_N_quantile_0.5"
)
#Leadind edge Estimates
Est_0.95_pts <- convert_km_to_wgs84(
  Est_0.95,
  east_col = "Estimate_km_E_quantile_0.95",
  north_col = "Estimate_km_N_quantile_0.95"
)
#Trailing edge Standard Deviation
Std_0.05_pts <- convert_km_to_wgs84(
  Std_0.05,
  east_col = "Std_Dev_km_E_quantile_0.05",
  north_col = "Std_Dev_km_N_quantile_0.05"
)
#Mean Standard Deviation
Std_0.5_pts <- convert_km_to_wgs84(
  Std_0.5,
  east_col = "Std_Dev_km_E_quantile_0.5",
  north_col = "Std_Dev_km_N_quantile_0.5"
)
#Leading edge Standard Deviation
Std_0.95_pts <- convert_km_to_wgs84(
  Std_0.95,
  east_col = "Std_Dev_km_E_quantile_0.95",
  north_col = "Std_Dev_km_N_quantile_0.95"
)
######################
#Plot:  RangeEdge_Mapped: Plot Leading and Trailing edge on a map-------
#first run Sup2DataPlots.R up to line 39 to get the shapefiles 
Land <- ne_countries(scale = "large", returnclass = "sf")

RangeEdge_Mapped <-  ggplot() +
  #start with mapping shapefiles 
  geom_sf(data = contours, color="lightblue", linewidth = .4) +
  geom_sf(data = NAFO, color="dimgrey", fill = NA) +
  geom_sf(data = Hague, color="black", linewidth = 1) +
  geom_sf(data = EEZ, color="black", linetype = "dashed", linewidth  = 1) +
  geom_sf(data = Land , color = "black", fill = "cornsilk") +
  #coord_sf(xlim = c(-72, -47), ylim = c(41, 50), expand = FALSE) 
  
  #Add Trailing Edge
  # Path connecting points by year
  geom_path(data = Est_0.05_pts %>% 
              st_coordinates() %>% 
              as.data.frame() %>% 
              cbind(Year = Est_0.05_pts$Year) %>% 
              arrange(Year),
            aes(x = X, y = Y, color = Year),
            linewidth = 0.8) +
  # Regular points for 0.05 quantile
  geom_sf(data = Est_0.05_pts, 
          aes(color = Year),
          na.rm = TRUE, 
          alpha = 1, 
          size = 2, 
          shape = 19)+
  # Larger terminal point for 0.05 quantile (most recent year)
  geom_sf(data = Est_0.05_pts[Est_0.05_pts$Year == max(Est_0.05_pts$Year, na.rm = TRUE), ],
          aes(color = Year),
          #position = pos_jitter, 
          na.rm = TRUE, 
          alpha = 1, 
          size = 5, 
          shape = 19) +
  # Annotation for 0.05 quantile terminal point
  annotate("text",
           x=-64.5, y=42.1,
           label = "Trailing Edge",
           color = "black",
           size = 4,
           family = "serif") +
  #add Leading Edge 
  # Path connecting points by year
  geom_path(data = Est_0.95_pts %>% 
              st_coordinates() %>% 
              as.data.frame() %>% 
              cbind(Year = Est_0.95_pts$Year) %>% 
              arrange(Year),
            aes(x = X, y = Y, color = Year),
            linewidth = 0.8) +
  # Regular points for 0.95 quantile
  geom_sf(data = Est_0.95_pts, 
          aes(color = Year),
          na.rm = TRUE, 
          alpha = 1, 
          size = 2, 
          shape = 19)+
  # Larger terminal point for 0.95 quantile (most recent year)
  geom_sf(data = Est_0.95_pts[Est_0.95_pts$Year == max(Est_0.95_pts$Year, na.rm = TRUE), ],
          aes(color = Year),
          #position = pos_jitter, 
          na.rm = TRUE, 
          alpha = 1, 
          size = 5, 
          shape = 19) + 
  # Annotation for 0.95 quantile terminal point  
  annotate("text",
           x=-53, y=45,
           label = "Leading Edge",
           color = "black",
           size = 4,
           family = "serif") +
  # Blue to orange gradient for Year
  scale_color_gradient2(low ="#005A9C",       ##"#0072B2", "#003366",  #"steelblue3"
                        mid = "#FFD700",      #yellow", 
                        high =  "orangered",  #"#D55E00",
                        midpoint = 2005, 
                        name = "Year") + 
  # Labels and plot range
  xlab("Range Edge W to E") +
  ylab("Range Edge S to N") +
  coord_sf(xlim = c(-72, -47), ylim = c(41, 50), expand = FALSE) +
  # Theme
  theme_bw() +
  theme(
    legend.position = "right",
    plot.margin = margin(5, 10, 10, 10),
    axis.text.x = element_text(size = 12, 
                               family = "serif"),
    axis.text.y = element_text(size = 12, 
                               family = "serif"),
    axis.title.x = element_text(size = 12, 
                                hjust = 0.5, 
                                vjust = -2, 
                                family = "serif"),
    axis.title.y = element_text(size = 12, 
                                hjust = 0.5, 
                                vjust = 4, 
                                angle = 90, 
                                family = "serif"),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, family = "serif"),
    strip.text = element_text(size = 12, 
                              family = "serif", 
                              angle = 0),
    strip.background = element_rect(colour = "black", 
                                    fill = "white"),
    panel.grid.minor = element_blank(), panel.grid.major =  element_blank(),
  )

RangeEdge_Mapped
######################
#Combineplots
# Ensure both plots have identical margins
RangeEdge2 <- RangeEdge_Mapped + theme(plot.margin = margin(35, 0,15,0))
PlotEAOAbd <- PlotEAOAbd + theme(plot.margin = margin(0, 15,10,10))#,axis.title.y = element_blank())
EAOplot <- EAOplot + theme(plot.margin = margin(15, 5,20,5))#,axis.title.y = element_blank())


# Combo plot: Proper nested approach----
# Create top row WITHOUT labels (labels will be added at the end)
bottom_row <- plot_grid(
  EAOplot, 
  PlotEAOAbd, 
  ncol = 2, 
  rel_heights = c(1,1),rel_widths =c(.5,.5), 
  align = "h",
  axis = "b"
)

# Create bottom row WITHOUT labels
top_centered <- plot_grid(
  #NULL, 
  RangeEdge2
  #NULL,
  #ncol = 3,
  #rel_widths = c(0.1, .8, 0.1)
)

# Combine rows and add ALL labels at this final step
# Combine rows WITHOUT automatic labels
EAORangeCombo_alt <- plot_grid(
  top_centered,
  bottom_row,
  nrow = 2,
  rel_heights = c(.55,.45)
)
library(cowplot)

EAORangeCombo_final <- ggdraw(EAORangeCombo_alt) +
  draw_label("Figure 5", x = 0.01, y = 0.99, hjust = 0, vjust = 1, fontfamily="serif", size = 14) +
  draw_plot_label(label = "(a)", x = 0.01, y = 0.95, family="serif", size = 12) +   # Top left
  draw_plot_label(label = "(b)", x = 0.01, y = 0.45, family="serif", size = 12) +   # Top right  
  draw_plot_label(label = "(c)", x = 0.52, y = 0.45, family="serif", size = 12)     # Bottom (centered plot)


# Display the result
print(EAORangeCombo_final)
##END Plot Range Edge----
#ggsave(here::here("NancBranchDataScript/FancyFiguresforMS/RangeEdge.jpeg"), plot = RangeEdge, dpi = 600, width = 8, height = 6, units = "in", device = "jpeg")
#END Combo plot: Proper nested approach----

ggsave(here::here("FebFigs/Figure5_EAORangeCombo_Feb.tif"), plot = EAORangeCombo_final, dpi = 600, width = 7, height = 6, units = "in", compression = "lzw", , bg = "white") 
