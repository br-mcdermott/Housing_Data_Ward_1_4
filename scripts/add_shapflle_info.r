
#adding information from a shapefile

# we assume that each line of out data frame contains location information
#X,Y which is decimal long,lat
# we assume that the shapefile contains poygons, each of which
# have coordinates consistent with those used in the dataframe
#if we need more coordinate work use rgdal and readOCR

require("maptools")
require("sp")


short    #This is the data frame


#add the new colums

short$ward =0
short$smd_id =0

std_projection=CRS("+proj=longlat +datum=WGS84")  #any projection will work,
                                                  #this is probably used
                                          
#get the shapefiles  (this assumes thay are in the working directory)

ward=readShapeSpatial("Ward__2012.shp",proj4string=std_projection)
smd=readShapeSpatial("Single_Member_District__2013.shp",proj4string=std_projection)

for (i in 1:nrow(short))
{
  point=SpatialPoints(list(short[i,]$X,short[i,]$Y),proj4string=std_projection)
  short$ward[i] <- over(point,ward)$WARD
  short$smd_id[i] <- over(point,smd)$SMD_ID
}
