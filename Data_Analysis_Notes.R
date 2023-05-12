# Data_Analysis.R
library(dplyr)

plot.new()
# Linear workflow and notes on Data Analysis
# https://rpubs.com/spring19cp6521/Week09_Wednesday1 - raster processing
# https://www.neonscience.org/resources/learning-hub/tutorials/dc-plot-raster-data-r


# 1. What are the extents of the gridded "world" population data raster?
library(raster)

world_pop <- raster("Data/gpw_v4_population_count_rev11_2020_30_sec.tif")
world_pop
# class      : RasterLayer 
# dimensions : 21600, 43200, 933120000  (nrow, ncol, ncell)
# resolution : 0.008333333, 0.008333333  (x, y)
# extent     : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs 
# source     : gpw_v4_population_count_rev11_2020_30_sec.tif 
# names      : gpw_v4_population_count_rev11_2020_30_sec 

# 2. Need a smaller section of that, something that's ideally just the WA with "a little extra"
# see https://stackoverflow.com/questions/50827870/read-only-a-crop-or-extent-of-a-raster-in-r

library(gdalUtilities)
library(sf)

# select out WA, OR and ID from state boundaries
states <- st_read("Data/cb_2018_us_state_20m.shp")
states <- st_transform(states, crs = 4326)
glimpse(states, width = 40)
# $ STATEFP  <chr> "24", "19", "10", "39…
# $ STATENS  <chr> "01714934", "01779785…
# $ AFFGEOID <chr> "0400000US24", "04000…
# $ GEOID    <chr> "24", "19", "10", "39…
# $ STUSPS   <chr> "MD", "IA", "DE", "OH…
# $ NAME     <chr> "Maryland", "Iowa", "…
# $ LSAD     <chr> "00", "00", "00", "00…
# $ ALAND    <dbl> 25151100280, 14466126…
# $ AWATER   <dbl> 6979966958, 108418081…
# $ geometry <MULTIPOLYGON [°]> MULTIPOL…

wa <- filter(states, STUSPS == "WA")
wa
# Simple feature collection with 1 feature and 9 fields
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: -124.7258 ymin: 45.54432 xmax: -116.916 ymax: 49.00249
# Geodetic CRS:  NAD83
# STATEFP  STATENS    AFFGEOID GEOID STUSPS       NAME LSAD        ALAND      AWATER                       geometry
# 1      53 01779804 0400000US53    53     WA Washington   00 172112588220 12559278850 MULTIPOLYGON (((-123.2371 4...                                                                                                                                                                                                      


# Get the BBOX of WA feature
wa_bbox <- st_bbox(wa)
wa_bbox
#       xmin       ymin       xmax       ymax 
# -124.72584   45.54432 -116.91599   49.00249

# We want to pad out the S,W and E directions an extra degree 
wa_bbox[1] <- (wa_bbox[1] - 1.0) # W
wa_bbox[2] <- (wa_bbox[2] - 1.0) # S
wa_bbox[3] <- (wa_bbox[3] + 1.0) # E

wa_bbox
#       xmin       ymin       xmax       ymax 
# -125.72584   44.54432 -115.91599   49.00249 

# Plot the bounding box
plot(st_as_sfc(wa_bbox))
plot(wa$geometry, add=TRUE)

gdalbuildvrt(gdalfile = "Data/gpw_v4_population_count_rev11_2020_30_sec.tif", 
             output.vrt = "Data/wa_gridded_pop.vrt", 
             te = wa_bbox)

# Let's see what this looks like
wa_pop <- raster("Data/wa_gridded_pop.vrt")

writeRaster(wa_pop, 'Data/wa_pop.tif') # for later, if we don't want the big file


setMinMax(wa_pop)
plot(st_as_sfc(wa_bbox))
plot(wa_pop, legend=TRUE, axes=FALSE, add=TRUE, legend.args = list(text = 'Population'))
plot(wa$geometry, add = TRUE)

# What does it look like if I only keep pixels > 0 and < 55
r_pops <- wa_pop
r_pops[r_pops[] == 0 ] = NA  
r_pops[r_pops[] >= 400 ] = NA 
r_pops[r_pops[] <= 400 ] = 1

#plot(r_pops, col = "red", legend=FALSE, add=TRUE)

# Question 2: Where are the towns?
library(data.table)

esri_places <- fread("Data/esri_populated_places.csv", sep = "|", header = TRUE, keepLeadingZeros = TRUE)
glimpse(esri_places)

# Sample record
esri_places %>% filter(NAME == 'Port Angeles' & STATE_ABBR == 'WA') %>% glimpse()

# Rural WA Towns, pop >= 400 and pop <= 5000
wa_rural_towns <- filter(esri_places, STATE_ABBR == "WA" & POPULATION >= 400 & POPULATION <= 5000)

# Only keep "Town" and "City", remove "Census Designated Place"
#wa_rural_towns %>% filter(!CLASS == "Census Designated Place") -> wa_rural_towns

# Add geometry back into the towns
wa_rural_towns <- st_as_sf(wa_rural_towns, coords = c("X", "Y"),  crs = 4326)
wa_rural_towns

plot(wa_rural_towns$geometry, 
     col="black", 
     pch=20,
     cex=.5,
     add=TRUE)

# Question 3: Where are the Hospitals?
hosp_3857 <- st_read("Data/hospitals.shp")

# Simple feature collection with 7596 features and 31 fields
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: -19663500 ymin: -1607536 xmax: 16221970 ymax: 11504850
# Projected CRS: 3857  <---- NEEDS REPROJECTION

hosp <- st_transform(hosp_3857, crs = 4326)

# Need to filter this data set down a bit. 
hosp %>% filter(TYPE == "GENERAL ACUTE CARE", 
                STATUS == "OPEN",
                STATE %in% c("WA", "ID", "OR"),
                !TRAUMA == "NOT AVAILABLE") -> hosp
# 1. eliminate closed hospitals
# 2. eliminate types that don't provide emergency care
# 3. Only keep ones in WA, ID or OR (don't want one right across the border)
# 4. Only keep LEVEL I-IV hospitals

# Reduce the fields
hosp %>% select(
  ID,
  NAME,
  ADDRESS,
  CITY,
  STATE,
  ZIP,
  COUNTY,
  BEDS,
  TRAUMA,
  HELIPAD) -> hosp

hosp %>% filter(!grepl("LEVEL IV", TRAUMA)) -> hosp

plot(hosp$geometry, pch=20, cex=.5, col='red', add=TRUE)

# Question 4: What are the service areas for existing hospitals?
hosp_albers <- st_transform(hosp, crs = 5070) # reproject to CONUS Albers
hosp_albers
hosp_albers_buffer <- st_buffer(hosp_albers, 48300) # 48300m ~= 30 miles
hosp_buffer <- st_transform(hosp_albers_buffer, crs=4326)
plot(hosp_buffer$geometry, border='red', col='transparent', add=TRUE)

# Question 5: Towns where a new service area won't intersect existing
unique(wa_rural_towns$CLASS)
wa_rural_towns <- filter(wa_rural_towns, !CLASS == "Census Designated Place")
wa_town_albers <- st_transform(wa_rural_towns, crs = 5070)
wa_town_albers_buffer <- st_buffer(wa_town_albers, 48300)
wa_rural_towns_buffer <- st_transform(wa_town_albers_buffer, crs = 4326)

# Create a giant shape that is the union of all the hospital areas
glimpse(hosp_buffer)
hosp_buffer$type <- "combined_service_areas"
hosp_service_area <- hosp_buffer %>%
  group_by(type) %>% 
  summarise()

# Pick the town buffers that don't overlap hosp buffers
candidate_towns <- wa_rural_towns_buffer[hosp_service_area, op = st_disjoint]

# TEST
plot.new()
plot(st_as_sfc(wa_bbox), border='transparent')
plot(wa_pop, legend=TRUE, axes=FALSE, add=TRUE, legend.args = list(text = 'Population'))
plot(wa$geometry, border='dark gray', add=TRUE)
plot(st_as_sfc(wa_bbox), border='black', add=TRUE)
plot(wa_rural_towns$geometry, pch=20, cex=.5, col='#5f6266', add=TRUE)
plot(hosp_service_area, 
     border='red', 
     col='transparent',
     add=TRUE)
plot(candidate_towns$geometry, 
     add = TRUE, 
     col='transparent',
     border = "darkblue")

# Calculate the total population (from raster) covered by each candidate town
candidate_town_pops <- st_as_sf(raster::extract(wa_pop, 
                                                candidate_towns, 
                                                fun=sum, na.rm=TRUE, sp=TRUE))
View(candidate_town_pops)

top_choices <- candidate_town_pops %>% 
  arrange(desc(wa_gridded_pop)) %>% 
  slice(1:3)

plot(top_choices$geometry,
     add=TRUE,
     col='transparent',
     border="green")

glimpse(top_choices)

st_drop_geometry(candidate_town_pops) %>% 
  select(NAME, wa_gridded_pop) %>% 
  arrange(desc(wa_gridded_pop)) %>%
  slice(1:3)
