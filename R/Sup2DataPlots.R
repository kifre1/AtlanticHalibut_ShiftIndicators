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
  scale_fill_manual(values = region_colours)+
  #scale_fill_manual(name = " ", values = c("#E41A1C","#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628","#F781BF", "#999999")) +
  labs(title="")+
  xlim(-72.5, -48) + ylim(39.355, 50)+
  theme_bw()+
  theme(axis.text = element_text(angle = 0, vjust = 0.2, hjust=1,size=8,family="serif"))
CAMAP

### Add CRIB data for Halibut ###
require("readr")
url <- "https://api-proxy.edh-cde.dfo-mpo.gc.ca/catalogue/records/264692e5-7b51-4ab9-bb1f-da65f6fc0875/attachments/EN_ClimateRiskIndex_Spatial-CanEEZ_145Spp.csv"
crib.df <- read_csv(url)
head(crib.df)
#write.csv(crib.df, "Data/CRIB/cribspeciesdata.csv") #file too large

#select only halibut data
halcrib <- crib.df[crib.df$`common name` == "Atlantic Halibut",]
head(halcrib)
str(halcrib)
write.csv(halcrib, "Data/CRIB/crib_halibut.csv")

# Convert dataframe to an `sf` object
library(sf)
library(terra)
library(gstat)
halv<-halcrib[,c("longitude","latitude","Vulnerability")]
haltsm<-halcrib[,c("longitude","latitude","S Thermal safety margin")]
haltoe<-halcrib[,c("longitude","latitude","E Time of climate emergence")]
halcv<-halcrib[,c("longitude","latitude","E Climate velocity")]
halthv<-halcrib[,c("longitude","latitude","AC Thermal habitat availability")]
# Convert to a spatial object
spatial_points <- vect(halthv, geom = c("longitude", "latitude"), crs = "WGS84")
# Create an empty raster (set resolution and extent as needed)
r <- rast(extent=spatial_points, resolution = 0.25, crs = "WGS84") # Adjust resolution
# Rasterize the points into a grid
raster_data <- rasterize(spatial_points, r, field = "AC Thermal habitat availability", fun = mean)
#plot(raster_data)

#add halibut vulnerability rasters to NAFO map
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis) 
raster_df<-as.data.frame(raster_data, xy=TRUE)
colnames(raster_df) <- c("longitude", "latitude", "AC Thermal habitat availability") 
VMAP <- ggplot() +
  geom_raster(data = raster_df, aes(x = longitude, y = latitude, fill = `AC Thermal habitat availability`)) +
  geom_sf(data = All_region_df, fill = NA) +
  geom_sf(data = Hague, color = "navy") +
  geom_sf(data = EEZ, color = "navy", linetype = "dashed", size = 1.2) +
  geom_sf(data = NAFO, color = "darkgrey", size=0.9, fill = NA) +
  geom_sf(data = land, fill = "cornsilk") +
  scale_fill_viridis_c(option = "D", name = "Thermal Habitat Variability") +
  xlim(-72.5, -48) + ylim(39.355, 50) +
  theme_bw() +
  theme(axis.text = element_text(angle = 0, vjust = 0.2, hjust = 1, size = 8, family = "serif")) +
  labs(title = "Thermal Habitat Variability", x = "Longitude", y = "Latitude", color = "Thermal Habitat Variability")
print(VMAP) 

### Calculate average + SD for each indicator in zones 4X, 4VW, and 3KL:###
library(sf)      # For spatial data handling
library(dplyr)   # For summarizing grouped data
library(here)
# Convert halcrib data frame to a spatial object
halcrib <- st_as_sf(halcrib, coords = c("longitude", "latitude"), crs = "WGS84")
# Reproject both datasets to align their CRS (e.g., EPSG:4326)
halcrib <- st_transform(halcrib, crs = st_crs(NAFO))
# Perform a spatial join to add the NAFO zone info to each point in halcrib
halcrib_with_zones <- st_join(halcrib, NAFO, na.rm=TRUE)

# Check the first few rows of the resulting dataset
head(halcrib_with_zones)
names(halcrib_with_zones)
unique(halcrib_with_zones$ZONE)
# Remove rows where NAFO_ID or ZONE are NA
halcrib_with_zones_clean <- halcrib_with_zones %>%
  filter(!is.na(NAFO_ID), !is.na(ZONE))
#test:
ggplot() +
  +     geom_sf(data = NAFO, fill = "lightgray", color = "black") +  # Plot NAFO polygons
  +     geom_sf(data = halcrib_with_zones_clean, color = "red", size = 2) + # Plot unmatched points
  +     labs(title = "Unmatched Points")

# Group halcrib_with_zones by NAFO zone and calculate summary statistics
summary_stats <- halcrib_with_zones_clean %>%
  group_by(NAFO_zone = ZONE) %>%
  summarise(
    mean_vulnerability = mean(Vulnerability, na.rm = TRUE),
    sd_vulnerability = sd(Vulnerability, na.rm = TRUE),
    mean_s_thermal_safety_margin = mean(`S Thermal safety margin`, na.rm = TRUE),
    sd_s_thermal_safety_margin = sd(`S Thermal safety margin`, na.rm = TRUE),
    mean_e_climate_velocity = mean(`E Climate velocity`, na.rm = TRUE),
    sd_e_climate_velocity = sd(`E Climate velocity`, na.rm = TRUE),
    mean_e_time_climate_emergence = mean(`E Time of climate emergence`, na.rm = TRUE),
    sd_e_time_climate_emergence = sd(`E Time of climate emergence`, na.rm = TRUE),
    mean_ac_thermal_habitat_availability = mean(`AC Thermal habitat availability`, na.rm = TRUE),
    sd_ac_thermal_habitat_availability = sd(`AC Thermal habitat availability`, na.rm = TRUE)
  )

# View the resulting summarized data
print(summary_stats)
# Remove geometry column
non_spatial_data <- st_drop_geometry(summary_stats)
head(non_spatial_data)
write.csv(non_spatial_data, "CRIB results/crib_halibut_byNAFO.csv")

# Group halcrib_with_zones by NAFO zone and calculate summary risks
most_frequent_risks <- halcrib_with_zones_clean %>%
  group_by(ZONE) %>%  # Group by NAFO zone
  summarise(
    most_frequent_overall_risk = names(sort(table(`Overall Risk`), decreasing = TRUE)[1]),
    most_frequent_s_thermal_safety_margin_risk = names(sort(table(`S Thermal safety margin risk`), decreasing = TRUE)[1]),
    most_frequent_e_climate_velocity_risk = names(sort(table(`E Climate velocity risk`), decreasing = TRUE)[1]),
    most_frequent_e_time_of_climate_emergence_risk = names(sort(table(`E Time of climate emergence risk`), decreasing = TRUE)[1]),
    most_frequent_ac_thermal_habitat_availability_risk = names(sort(table(`AC Thermal habitat availability risk`), decreasing = TRUE)[1]),
    .groups = "drop"  # Ungroup after summarizing
  )
print(most_frequent_risks)

# Remove geometry column
non_spatial_data <- st_drop_geometry(most_frequent_risks)
head(non_spatial_data)
non_spatial_data<-as.data.frame(non_spatial_data)
write.csv(non_spatial_data, "CRIB results/crib_risk_halibut_byNAFO.csv", row.names = FALSE)
