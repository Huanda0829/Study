library(ncdf4) #library to read and process netcdf data
library(chron) 
library(lattice)
library(RColorBrewer)
library(sf)
library(tmap)

getwd()gtmapetwd()
setwd("C:/Users/LENOVO/Desktop/93大作业")
era <- nc_open("era5.nc" )
#check variable names 
era$nvars
names(era$var)
names(era$dim)

lon <- ncvar_get(era, "longitude")
lat <- ncvar_get(era, "latitude")
time <- ncvar_get(era, "time")
dim(time)
tunits <- ncatt_get(era,"time","units") 

#seconds since 1970-01-01
time_hour = time/60/60
chron(time_hour/24, origin=c(1, 1, 1970) ) #this function converts time

#extract ssrd
ssrd_array <- ncvar_get(era,"ssrd") #get the Surface solar radiation downwards attribute value
dim(ssrd_array) #461 171 120



dlname <- ncatt_get(era,"ssrd","long_name")
dunits <- ncatt_get(era,"ssrd","units")

# (09/25/23 12:00:00)>>98
ssrd_array <- ncvar_get(era,"ssrd") #get attribute
ssrd_slice <- ssrd_array[,, 2] #slice by dimensions
length(na.omit(as.vector(ssrd_slice))) /length(as.vector(ssrd_slice)) 

dim(ssrd_array)
dim (ncvar_get(era, 'time') )
dim(ssrd_slice)

lonlat <- as.matrix( (expand.grid(lon, lat))) 
ssrd_vec <- as.vector(ssrd_slice)  #convert array (2d) to vector(1d)

ssrd_df <- data.frame( cbind( lonlat, ssrd_vec  ))
colnames(ssrd_df) <- c("lon", "lat", "ssrd")
View(ssrd_df)
ssrd_df_value <-  na.omit (ssrd_df)
head(ssrd_df_value, 3) 

#convert long and lat to point in simple feature format
ssrd_sf<- st_as_sf( ssrd_df_value, coords = c(  "lon", "lat")  ) 
#To make it a complete geographical object we assign the WGS84 projection, which has the EPSG code 4326
st_crs(ssrd_sf) <- 4326 
ssrd_sf <- st_transform(ssrd_sf, 4326 )

tmap_mode("view")
tm_shape(ssrd_sf)+
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "viridis")


ssrd_sf<- st_as_sf( ssrd_df_value, coords = c(  "lon", "lat")  ) 
#To make it a complete geographical object we assign the WGS84 projection, which has the EPSG code 4326
st_crs(ssrd_sf) <- 4326 
ssrd_sf <- st_transform(ssrd_sf, 4326 )

tmap_mode("view")
tm_shape(ssrd_sf)+
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "viridis")



