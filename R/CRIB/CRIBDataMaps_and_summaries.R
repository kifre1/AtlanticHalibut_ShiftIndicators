#Plot 1: Map of Core areas 
  # make colours match CAPlot
  #re-do all folder connections
  #add new core areas
  #Increase box area 
#Plot 2: A look at the distribution of RV Survey data
  #add NF
  #Plot presence only 
library(sf)
library (ggplot2)

All_region <- st_read(here::here("", "R/Shapefiles/IndexShapefiles/Full_RegionAl14.shp"))
crs <- st_crs(All_region)

#bounding box for plotting
study_area_bbox <- st_bbox(c(xmin = -76, ymin = 35.5, xmax = -45, ymax = 60))
study_area_polygon <- st_as_sfc(study_area_bbox)
st_crs(study_area_polygon) <- crs

#Mapping shapefiles
EEZ <- st_read(here::here("", "Data/Mapping_shapefiles/EEZ.shp"))
land <- st_read(here::here("", "Data/Mapping_shapefiles/poly_NAD83.shp"))
contours <- st_read(here::here("", "Data/Mapping_shapefiles/GEBCO_DepthContours.shp"))
NAFO <- st_read(here::here("", "Data/Mapping_shapefiles/Divisions.shp"))
Hague <- st_read(here::here("", "Data/Mapping_shapefiles/HagueLine.shp"))

EEZ <- st_transform (EEZ, crs)
land <- st_transform (land, crs)
contours <- st_transform (contours, crs)
NAFO <- st_transform (NAFO, crs)
Hague <- st_transform (Hague, crs)

# Clip large shapefiles shapefile to  bounding box
sf::sf_use_s2(FALSE)   # revert to GEOS-based operations

EEZ <- st_intersection(EEZ, study_area_polygon)
land <- st_intersection(land, study_area_polygon)#maybe comment out 
contours <- st_intersection(contours, study_area_polygon)
NAFO <- st_intersection(NAFO, study_area_polygon)

# Convert to data frames
All_region_df <- st_as_sf(data.frame(geometry = All_region))

library(sf)
library(ggrepel)

# Plot CoreArea
CAMAP<-ggplot() +
  #geom_sf(data = contours, color="lightblue") +
  geom_sf(data = All_region_df,  fill = NA) +
  geom_sf(data = Hague, color="navy") +
  geom_sf(data = EEZ, color="navy", linetype = "dashed", size = 1.2) +
  geom_sf(data = NAFO, color="darkgrey", fill = NA) +
  geom_sf(data = land, fill="cornsilk") +
  #scale_fill_manual(values = region_colours)+
  #scale_fill_manual(name = " ", values = c("#E41A1C","#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628","#F781BF", "#999999")) +
  labs(title="")+
  xlim(-72.5, -48) + ylim(39.355, 50)+
  theme_bw()+
  theme(axis.text = element_text(angle = 0, vjust = 0.2, hjust=1,size=8,family="serif"))
CAMAP

### Add CRIB data ###
require("readr")
url <- "https://api-proxy.edh-cde.dfo-mpo.gc.ca/catalogue/records/264692e5-7b51-4ab9-bb1f-da65f6fc0875/attachments/EN_ClimateRiskIndex_Spatial-CanEEZ_145Spp.csv"
crib.df <- read_csv(url)
head(crib.df)
#write.csv(crib.df, "Data/CRIB/cribspeciesdata.csv") #file too large

#################################################################################################
##################################### HALIBUT ###################################################
#################################################################################################
#select only halibut data
halcrib <- crib.df[crib.df$`common name` == "Atlantic Halibut",]
head(halcrib)
str(halcrib)
#write.csv(halcrib, "Data/CRIB/crib_halibut.csv")
halcrib<-read.csv("Data/CRIB/crib_halibut.csv")
halcrib$ToE.year<-2015+(-log(halcrib$E.Time.of.climate.emergence)/0.033) #calculate raw ToE's from standardized

# Convert dataframe to an `sf` object
library(sf)
library(terra)
library(gstat)
halv<-halcrib[,c("longitude","latitude","ssp","Vulnerability")]
haltsm<-halcrib[,c("longitude","latitude","ssp","S.Thermal.safety.margin")]
haltoe<-halcrib[,c("longitude","latitude","ssp","ToE.year")]
halcv<-halcrib[,c("longitude","latitude","ssp","E.Climate.velocity")]
halthv<-halcrib[,c("longitude","latitude","ssp","AC.Thermal.habitat.availability")]
# Convert to a spatial object
spatial_points1 <- vect(haltoe[haltoe$ssp=="SSP1-2.6",], geom = c("longitude", "latitude"), crs = "WGS84")
# Create an empty raster (set resolution and extent as needed)
r <- rast(extent=spatial_points1, resolution = 0.25, crs = "WGS84") # Adjust resolution
# Rasterize the points into a grid
raster_data1 <- rasterize(spatial_points1, r, field = "ToE.year", fun = mean)
spatial_points2 <- vect(haltoe[haltoe$ssp=="SSP5-8.5",], geom = c("longitude", "latitude"), crs = "WGS84")
# Create an empty raster (set resolution and extent as needed)
r <- rast(extent=spatial_points2, resolution = 0.25, crs = "WGS84") # Adjust resolution
# Rasterize the points into a grid
raster_data2 <- rasterize(spatial_points2, r, field = "ToE.year", fun = mean)

#plot(raster_data)

#add halibut vulnerability rasters to NAFO map
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis) 
raster_df<-as.data.frame(raster_data2, xy=TRUE)
colnames(raster_df) <- c("longitude", "latitude", "ToE.year") 
VMAP <- ggplot() +
  geom_raster(data = raster_df, aes(x = longitude, y = latitude, fill = `ToE.year`)) +
  geom_sf(data = All_region_df, fill = NA) +
  geom_sf(data = Hague, color = "navy") +
  geom_sf(data = EEZ, color = "navy", linetype = "dashed", size = 1.2) +
  geom_sf(data = NAFO, color = "darkgrey", size=0.9, fill = NA) +
  geom_sf(data = land, fill = "cornsilk") +
  scale_fill_viridis_c(
    option = "D", # Keep the "D" viridis colour palette
    name = "Time of Climate Emergence",
    direction = -1 # Reverse the direction of the colour scale
  ) +
  xlim(-72.5, -45) + ylim(39, 60) +
  theme_bw() +
  theme(axis.text = element_text(angle = 0, vjust = 0.2, hjust = 1, size = 8, family = "serif")) +
  labs(title = "Time of Climate Emergence SSP1-2.6", x = "Longitude", y = "Latitude", color = "Time of Climate Emergence")
print(VMAP) 

### Calculate average + SD for each indicator in zones 4X, 4VW, and 3KL:###
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

# Group halcrib_with_zones by NAFO zone and calculate summary statistics
summary_stats <- halcrib_with_zones_clean1 %>%
  group_by(NAFO_Zones, ssp) %>%
  summarise(
    mean_vulnerability = mean(Vulnerability, na.rm = TRUE),
    sd_vulnerability = sd(Vulnerability, na.rm = TRUE),
    mean_s_thermal_safety_margin = mean(`S.Thermal.safety.margin`, na.rm = TRUE),
    sd_s_thermal_safety_margin = sd(`S.Thermal.safety.margin`, na.rm = TRUE),
    mean_e_climate_velocity = mean(`E.Climate.velocity`, na.rm = TRUE),
    sd_e_climate_velocity = sd(`E.Climate.velocity`, na.rm = TRUE),
    mean_yr_climate_emergence = mean(`ToE.year`, na.rm = TRUE),
    sd_yr_climate_emergence = sd(`ToE.year`, na.rm = TRUE),
    n_yr_climate_emergence = n(),  # Number of rows in each group
    ci_yr_climate_emergence = 1.96 * (sd(ToE.year, na.rm = TRUE) / sqrt(n())),
    mean_e_time_climate_emergence = mean(`E.Time.of.climate.emergence`, na.rm = TRUE),
    sd_e_time_climate_emergence = sd(`E.Time.of.climate.emergence`, na.rm = TRUE),
    mean_ac_thermal_habitat_variability = mean(`AC.Thermal.habitat.availability`, na.rm = TRUE),
    sd_ac_thermal_habitat_variability = sd(`AC.Thermal.habitat.availability`, na.rm = TRUE)
  )

# View the resulting summarized data
print(summary_stats)
head(summary_stats)
# Remove geometry column
non_spatial_data <- st_drop_geometry(summary_stats)
head(non_spatial_data)
df<-as.data.frame(non_spatial_data)
write.csv(df, "CRIB results/crib_AtlHalibut_byNAFO_SSPs.csv", row.names = FALSE)

# Group halcrib_with_zones by NAFO zone and calculate summary risks
risk_summary <- halcrib_with_zones_clean1 %>%
  group_by(NAFO_Zones, ssp, Overall.Risk) %>% # Group by NAFO_Zones, SSP, and Overall.Risk
  summarise(
    count = n(),                              # Count the number of rows in each group
    .groups = "drop_last"                     # Keep grouping by NAFO_Zones and ssp
  ) %>%
  mutate(
    percentage = (count / sum(count)) * 100  # Calculate percentage for each Overall.Risk
  ) %>%
  ungroup() # Ungroup to return ungrouped data

# View the summary
print(risk_summary)

# Remove geometry column
non_spatial_data <- st_drop_geometry(risk_summary)
head(non_spatial_data)
non_spatial_data<-as.data.frame(non_spatial_data)
write.csv(non_spatial_data, "CRIB results/crib_risksummary_AtlHalibut_byNAFO_SSPs.csv", row.names = FALSE)

#################################################################################################
##################################### Greenland halibut ###################################################
#################################################################################################
#select Greenland halibut data
unique(crib.df$`common name`)
halcrib <- crib.df[crib.df$`common name` == "Greenland Halibut",]
head(halcrib)
str(halcrib)
#write.csv(halcrib, "Data/CRIB/crib_greenland_halibut.csv")
halcrib<-read.csv("Data/CRIB/crib_greenland_halibut.csv")
halcrib$ToE.year<-2015+(-log(halcrib$E.Time.of.climate.emergence)/0.033) #calculate raw ToE's from standardized

# Convert dataframe to an `sf` object
library(sf)
library(terra)
library(gstat)
halv<-halcrib[,c("longitude","latitude","ssp","Vulnerability")]
haltsm<-halcrib[,c("longitude","latitude","ssp","S.Thermal.safety.margin")]
haltoe<-halcrib[,c("longitude","latitude","ssp","ToE.year")]
halcv<-halcrib[,c("longitude","latitude","ssp","E.Climate.velocity")]
halthv<-halcrib[,c("longitude","latitude","ssp","AC.Thermal.habitat.availability")]
# Convert to a spatial object
spatial_points1 <- vect(haltoe[haltoe$ssp=="SSP1-2.6",], geom = c("longitude", "latitude"), crs = "WGS84")
# Create an empty raster (set resolution and extent as needed)
r <- rast(extent=spatial_points1, resolution = 0.25, crs = "WGS84") # Adjust resolution
# Rasterize the points into a grid
raster_data1 <- rasterize(spatial_points1, r, field = "ToE.year", fun = mean)
spatial_points2 <- vect(haltoe[haltoe$ssp=="SSP5-8.5",], geom = c("longitude", "latitude"), crs = "WGS84")
# Create an empty raster (set resolution and extent as needed)
r <- rast(extent=spatial_points2, resolution = 0.25, crs = "WGS84") # Adjust resolution
# Rasterize the points into a grid
raster_data2 <- rasterize(spatial_points2, r, field = "ToE.year", fun = mean)

#plot(raster_data)

#add halibut vulnerability rasters to NAFO map
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis) 
raster_df<-as.data.frame(raster_data1, xy=TRUE)
colnames(raster_df) <- c("longitude", "latitude", "ToE.year") 
VMAP <- ggplot() +
  geom_raster(data = raster_df, aes(x = longitude, y = latitude, fill = `ToE.year`)) +
  geom_sf(data = All_region_df, fill = NA) +
  geom_sf(data = Hague, color = "navy") +
  geom_sf(data = EEZ, color = "navy", linetype = "dashed", size = 1.2) +
  geom_sf(data = NAFO, color = "darkgrey", size=0.9, fill = NA) +
  geom_sf(data = land, fill = "cornsilk") +
  scale_fill_viridis_c(
    option = "D", # Keep the "D" viridis colour palette
    name = "Time of Climate Emergence",
    direction = -1 # Reverse the direction of the colour scale
  ) +
  xlim(-72.5, -45) + ylim(39, 60) +
  theme_bw() +
  theme(axis.text = element_text(angle = 0, vjust = 0.2, hjust = 1, size = 8, family = "serif")) +
  labs(title = "Time of Climate Emergence SSP1-2.6", x = "Longitude", y = "Latitude", color = "Time of Climate Emergence")
print(VMAP) 

### Calculate average + SD for each indicator in zones 4X, 4VW, and 3KL:###
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
  )
head(halcrib_with_zones_clean1)

# Group halcrib_with_zones by NAFO zone and calculate summary statistics
summary_stats <- halcrib_with_zones_clean1 %>%
  group_by(NAFO_Zones, ssp) %>%
  summarise(
    mean_vulnerability = mean(Vulnerability, na.rm = TRUE),
    sd_vulnerability = sd(Vulnerability, na.rm = TRUE),
    mean_s_thermal_safety_margin = mean(`S.Thermal.safety.margin`, na.rm = TRUE),
    sd_s_thermal_safety_margin = sd(`S.Thermal.safety.margin`, na.rm = TRUE),
    mean_e_climate_velocity = mean(`E.Climate.velocity`, na.rm = TRUE),
    sd_e_climate_velocity = sd(`E.Climate.velocity`, na.rm = TRUE),
    mean_yr_climate_emergence = mean(`ToE.year`, na.rm = TRUE),
    sd_yr_climate_emergence = sd(`ToE.year`, na.rm = TRUE),
    n_yr_climate_emergence = n(),  # Number of rows in each group
    ci_yr_climate_emergence = 1.96 * (sd(ToE.year, na.rm = TRUE) / sqrt(n())),
    mean_e_time_climate_emergence = mean(`E.Time.of.climate.emergence`, na.rm = TRUE),
    sd_e_time_climate_emergence = sd(`E.Time.of.climate.emergence`, na.rm = TRUE),
    mean_ac_thermal_habitat_variability = mean(`AC.Thermal.habitat.availability`, na.rm = TRUE),
    sd_ac_thermal_habitat_variability = sd(`AC.Thermal.habitat.availability`, na.rm = TRUE)
  )

# View the resulting summarized data
head(summary_stats)
# Remove geometry column
non_spatial_data <- st_drop_geometry(summary_stats)
head(non_spatial_data)
df<-as.data.frame(non_spatial_data)
write.csv(df, "CRIB results/crib_GreenlandHalibut_byNAFO_SSPs.csv", row.names = FALSE)

# Group halcrib_with_zones by NAFO zone and calculate summary risks
risk_summary <- halcrib_with_zones_clean1 %>%
  group_by(NAFO_Zones, ssp, Overall.Risk) %>% # Group by NAFO_Zones, SSP, and Overall.Risk
  summarise(
    count = n(),                              # Count the number of rows in each group
    .groups = "drop_last"                     # Keep grouping by NAFO_Zones and ssp
  ) %>%
  mutate(
    percentage = (count / sum(count)) * 100  # Calculate percentage for each Overall.Risk
  ) %>%
  ungroup() # Ungroup to return ungrouped data

# View the summary
print(risk_summary)

# Remove geometry column
non_spatial_data <- st_drop_geometry(risk_summary)
head(non_spatial_data)
non_spatial_data<-as.data.frame(non_spatial_data)
write.csv(non_spatial_data, "CRIB results/crib_risksummary_GreenlandHalibut_byNAFO_SSPs.csv", row.names = FALSE)
