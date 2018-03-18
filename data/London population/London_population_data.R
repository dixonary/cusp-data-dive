# method to assign population density data to given point grid
# All data taken from London Data Store

# Summary:
# LOAD POINT GRID
# CENSUS DATA "resident population"
# DAYTIME POPULATION DATA "working and tourist population"
# CREATE TRAINING DATA

library(sf)
library(tidyverse)
library(rgdal)

getwd()

#######################
# LOAD POINT GRID

# load grid
points <- read.csv("input/GLApoints_30m.csv", stringsAsFactors = FALSE)

# convert to SF
pointsSF <- st_as_sf(
  points, 
  coords = c('x', 'y'),
  crs = 27700
)

#######################
# CENSUS DATA "resident population"

# read pop density csv (LSOA data)
popData <- read.csv("input/land-area-population-density-lsoa11-msoa11.csv", stringsAsFactors = FALSE)

# load London LSOA boundaries
london_lsoa <- st_read("input/lsoaSHP/LSOA_2011_London_gen_MHW.shp", quiet = TRUE, stringsAsFactors = FALSE) %>% st_set_crs(27700)

# merge population data with LSOA boundaries
popLSOA <- merge(london_lsoa, popData, by.x = "LSOA11CD", by.y = "LSOA11.Code")

# trim
popLSOA <- popLSOA[,c(22,23)]

# write_sf(popLSOA, "map1.shp")

#assign pop density to point by centroid
popPoints <- st_intersection(pointsSF, popLSOA)

# trim
popPoints <- popPoints[,c(2,3)] # NEED TO KEEP POINT IDs

#######################
# DAYTIME POPULATION DATA "working and tourist population"

# read in pop density csv
dayPopData <- read.csv("input/daytimePopDensity.csv", stringsAsFactors = FALSE)

# load London borough boundaries
london_boros <- st_read("input/boroSHP/London_Borough_Excluding_MHW.shp", quiet = TRUE, stringsAsFactors = FALSE) %>% st_set_crs(27700)

# merge data with borough boundaries
dayPopBoro <- merge(london_boros, dayPopData, by.x = "GSS_CODE", by.y = "Code")

# trim
dayPopBoro <- dayPopLSOA[,c(9,10)]

# calculate tourist numbers
dayPopBoro$tourists <- dayPopBoro$TotalDaytimePopulation - dayPopBoro$WorkdayPopulation_excTourists

#write_sf(dayPopBoro, "day2.shp")

# assign pop density to point by centroid
dayPopPoints <- st_intersection(pointsSF, dayPopBoro)

# extract density info only
dayPopPoints <- dayPopPoints[,c(2,4,5)] # NEED TO KEEP POUNTS IDs

#######################
# CREATE TRAINING DATA

# return to data.frame
st_geometry(popPoints) <- NULL
st_geometry(dayPopPoints) <- NULL

# merge data to original points data using ID
# note that some points are NA as they fall outside either LSOA or Borough boundaries (ie rivers)
trainPop <- merge(points, popPoints, by = "id", all.x = TRUE)
trainPop <- merge(trainPop, dayPopPoints, by = "id", all.x = TRUE)

# trim
trainPop <- trainPop[,-c(2)] # KEEP ID

# export
write_csv(trainPop, "output/Lon_Pop_Data_Grid.csv")




