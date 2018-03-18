## CODE FOR KERNEL DENSITY ESTIMATION (KDE) 

#LOAD PACKAGES
pkgs <- c("data.table","dplyr","plyr","rgeos","sp","rgdal","maptools","ks","raster")
for (pkg in pkgs) {
  if(pkg %in% rownames(installed.packages()) == FALSE) {install.packages(pkg)
    lapply(pkgs, require, character.only = TRUE)}
  else {
    lapply(pkgs, require, character.only = TRUE)}
}
rm(pkg,pkgs)

#LOAD REGULAR POINT GRID (300m)
#See London_grid.R for reference
regpoints <- fread("https://github.com/dixonary/cusp-data-dive/raw/master/data/pnts_test.csv",header=T) #Load regular point grid
#LOAD ACCIDENT POINTS
accpoints <- fread("https://github.com/dixonary/cusp-data-dive/raw/master/data/casualty_lonlat_2016.csv",header=T) #Load accident points
#LOAD OSM FEATURES
osmFeatures <- fread("https://github.com/dixonary/cusp-data-dive/raw/master/data/osmFeaturesLatLong.csv",header=T,data.table = FALSE) #Load OSM features

# KDE #
# 1) ACCIDENT POINTS
h <- Hns(accpoints,deriv.order = 0.01) #Get bandwidth matrix
acckde <- raster(kde(accpoints,h=h,bgridsize = c(2000,2000),binned=TRUE)) #Run KDE and rasterize output
projection(acckde) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #Project raster output
regpoints <- SpatialPoints(cbind(regpoints$x1,regpoints$x2)) #Create Spatial object from regular points coordinates
projection(regpoints) <- "+init=epsg:27700" #Set projection for regular points (BNG)
regpoints <- spTransform(regpoints,CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) #Reproject to WGS84
accdens <- extract(acckde,regpoints) #Sample regular point density values from raster (for 300m grid)
accdens <- as.data.frame(cbind(regpoints@coords,accdens)) #Save as data.frame
colnames(accdens) <- c("x","y","dens") #Rename columns

# 2) OSM FEATURES
i <- 0 #Incremental variable i
r <- list() #Create empty list to store output rasters
colz <- seq(3,ncol(osmFeatures))
final <- as.data.frame(matrix(nrow=14994)) #Create empty dataframe to store final results
for (col in colz){ #Select OSM features and save in varz 
  varz<-unique(as.character(osmFeatures[,col]))
  varz<-varz[!is.na(varz)]
  varz<-varz[varz!="NA"]
  varz<-varz[varz!=""]
  for(k in 1:length(varz)){ #Loop over every feature to run the KDE and sample values on the 300m regular grid
    i <- i+1
    print(i)
    var<-varz[k]
    sub1<-subset(osmFeatures,osmFeatures[,col]==var)
    pointz<-sub1[,c(1,2)]
    r[[i]] <- raster(kde(pointz,h=h,bgridsize = c(2000,2000),binned=TRUE))
    projection(r[[i]]) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
    names(r[[i]]) <- var
    dens <- as.data.frame(extract(r[[i]],regpoints))
    colnames(dens) <- var
    final <- cbind(final,dens)
  }
}


final <- cbind(accdens,final[,-1]) #Merge results with accident data
colnames(final) <- c("x","y","Accidents","AmenOther", #Rename columns
                      "Transp","Susten",
                      "Edu","Fin",
                      "Art","Health",
                      "ShopHealth","ShopCloth",
                      "ShopOther","ShopFood",
                      "ShopArt","ShopBook",
                      "ShopSport","ShopGen",
                      "ShopFurn","ShopDIY",
                      "ShopElect","ShopDisc",
                      "BusStop","Crossing",
                      "Elevator","GiveWay",
                      "Roundabout","Junction",
                      "SpeedCam","Stop",
                      "StreetLamp","TrafficSignal",
                      "Turning","Platform",
                      "StopPos","Tourism")

# THE DATAFRAME "FINAL" NOW CONTAINS DENSITY VALUES FOR TRAFFIC ACCIDENTS AND OSM DATA SAMPLED ON A REGULAR POINT GRID
# THE CODE RUNS WITH ANY POINT GRID FED IN ("regpoints"). WE HAVE TWO OTHER, MORE GRANULAR GRIDS AVAILABLE:
# - A 100m GRID AT https://github.com/dixonary/cusp-data-dive/raw/master/data/GLApoints_100m.csv
# - A 30m  GRID AT https://github.com/dixonary/cusp-data-dive/raw/master/data/GLApoints_30m.csv 
# THE OSM FEATURES (COLUMNS) IN "FINAL" ARE NAMED AFTER THEIR RESPECTIVE OSM CATEGORY:
# - AmenOther = Amenity (Other): https://wiki.openstreetmap.org/wiki/Key:amenity#Others
# - Transp = Transport Amenities: https://wiki.openstreetmap.org/wiki/Key:amenity#Transportation
# - Susten = Sustenance Amenities: https://wiki.openstreetmap.org/wiki/Key:amenity#Sustenance
# - Edu = Education Amenities: https://wiki.openstreetmap.org/wiki/Key:amenity#Education
# - Fin = Finance Amenities: https://wiki.openstreetmap.org/wiki/Key:amenity#Financial
# - Art = Art Amenities: https://wiki.openstreetmap.org/wiki/Key:amenity#Entertainment.2C_Arts_.26_Culture
# - Health = Health Amenities: https://wiki.openstreetmap.org/wiki/Key:amenity#Healthcare
# - Shop... = Various shop types: https://wiki.openstreetmap.org/wiki/Key:shop
# - BusStop-StopPos = Various key:highway objects: https://wiki.openstreetmap.org/wiki/Key:highway
# - Tourism = Tourism Amenities: https://wiki.openstreetmap.org/wiki/Key:tourism

# DISCRETISATION TOOL FOR ACCIDENT DENSITIES #
# DISCRETISE ACCIDENT DENSITIES (INTO j+1 RISK CLASSES)
q <- 0.2 #Set offset (every density value below the q (20%) highest values gets set to risk class zero)
j <- 19 #Define goal number of risk classes
p <- 1-q
x <- p 
for (i in 1:j) { #Define percentile offsets 
  x <- x+((1-x)/2)
  p <- c(p,x)
}

final$disc <- 0 #Create empty column to store discretised values
i <- 1
for (j in p) { #Assign according discretised value to each observation
  print(i)
  final$disc[final$Accidents>=quantile(final$Accidents,j)] <- i
  i <- i+1
}

