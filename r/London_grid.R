## CREATE GRID FOR GLA ##

#LOAD REQUIRED PACKAGES
pkgs <- c("rgeos","raster","sp","rgdal","geojsonio")
for (pkg in pkgs) {
  if(pkg %in% rownames(installed.packages()) == FALSE) {install.packages(pkg)
    lapply(pkgs, require, character.only = TRUE)}
  else {
    lapply(pkgs, require, character.only = TRUE)}
}
rm(pkg,pkgs)

#READ GLA WKT FILE (SHAPE)
GLA <- geojson_read("https://mapit.mysociety.org/area/2247.geojson",what="sp") #Read GLA GeoJSON
BNG <-  "+init=epsg:27700" #Define BNG CRS
GLA <- spTransform(GLA,BNG) #Set CRS

#CREATE GRID

#Set seed (used for random component in point generation)
set.seed(5000)

#156900  points for 100m grid
system.time(
  GLApoints_100m <- spsample(GLA,156900,type="regular") #Define number of points
)
GLApoints_100m <- as.data.frame(cbind(seq(1:length(GLApoints_100m)),GLApoints_100m@coords[,1],GLApoints_100m@coords[,2]))
colnames(GLApoints_100m) <- c("id","x","y")

#1569000 points for 30m grid
system.time(
  GLApoints_30m <- spsample(GLA,1569000,type="regular") #Define number of points
)
GLApoints_30m <- as.data.frame(cbind(seq(1:length(GLApoints_30m)),GLApoints_30m@coords[,1],GLApoints_30m@coords[,2]))
colnames(GLApoints_30m) <- c("id","x","y")

#write.csv(GLApoints_30m,"GLApoints_30m.csv")
#write.csv(GLApoints_100m,"GLApoints_100m.csv")