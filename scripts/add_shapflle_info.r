
#adding information from a shapefile

# we assume that each line of out data frame contains location information
#X,Y which is decimal long,lat
# we assume that the shapefile contains poygons, each of which
# have coordinates consistent with those used in the dataframe
#if we need more coordinate work use rgdal and readOCR

require("maptools")
require("sp")
require("rgeos")

#cama    #This is the data frame


#add the new colums

cama$ward =rep(NA, nrow(cama))
#cama$smd_id =rep(NA, nrow(cama))

std_projection=CRS("+proj=longlat +datum=WGS84")  #any projection will work,
                                                  #this is probably used
                                          
#get the shapefiles  (this assumes thay are in the working directory)

ward=readShapeSpatial("~/data/Ward__2012.shp",proj4string=std_projection)
#smd=readShapeSpatial("~/data/Single_Member_District__2013.shp",proj4string=std_projection)

for (i in 1:nrow(cama))
{
  point=SpatialPoints(list(cama[i,]$X,cama[i,]$Y),proj4string=std_projection)
  cama$ward[i] <- over(point,ward)$WARD
  #cama$smd_id[i] <- over(point,smd)$SMD_ID
}
