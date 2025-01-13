#====Part_1: NetCDF data====
#====Step 1. Load pacakges and import data====
library(ncdf4) #library to read and process netcdf data
library(chron)
library(lattice)
library(RColorBrewer)
library(sp)
library(gstat)
library(raster)
install.packages("ncdf4")
install.packages("chron")
install.packages("sp")
install.packages("gstat")
install.packages("raster")
era <- nc_open("era5.nc" )
#variable, dimension and global attributes
#u is the eastward component of the wind
#v is the northward component

era #3 variables [u10, v10, ssrd], 3 dimensions [longitude,latitude,time] 

#====Step 2. Extract and get dimensions: lon, lat, time====
#ncvar_get(data, 'attributes')
lon <- ncvar_get(era, "longitude")
lat <- ncvar_get(era, "latitude")
time <- ncvar_get(era, "time")
lon
lat
time

dim(time)
# get the unit of data
tunits <- ncatt_get(era,"time","units") #tunits <- ncatt_get(era,"longitude","units")
tunits

#?chron
#chron(time/24, origin=c(tmonth,tday,tyear))
chron(time/24, origin=c(01,01,1900))
#this function converts the hours since format to the format we are quite familiar with.

#====Step 3. Extract attribute values====
#get Variables :u10,v10, ssrd. similar to get dimension data, we are going to use the same method, ncvar_get()
ssrd_array <- ncvar_get(era,"ssrd") #get the Surface solar radiation downwards
dim(ssrd_array) #dimension is 501 * 186 *8. Think about the Figure 1. The reason why it is called array is it is composed of 8 slices 
era
dlname <- ncatt_get(era,"ssrd","long_name")
dunits <- ncatt_get(era,"ssrd","units")
fillvalue <- ncatt_get(era,"ssrd","_FillValue")

#====Step 4. Slice data====
#dim(ncvar_get())
#dim(ssrd_slice)
#Get a single time slice of the data using ssrd_array
ssrd_slice <- ssrd_array[,,2] 
#The ssrd_slice is actually a matrix. class(ssrd_slice)
# What does 2 in ssrd_array[,,2] indicate?  What if I want to slice all four time slice for "07/01/19"? 

length(na.omit(as.vector(ssrd_slice))) /length(as.vector(ssrd_slice)) #8.5% are valid

dim(ssrd_slice)
image(ssrd_slice, col=rev(brewer.pal(10,"RdBu")) )
#example: check one max point
max_rad <- max(ssrd_slice, na.rm=TRUE)
max_rad

#====Step 5. combine data and visualise====
lonlat <- as.matrix( (expand.grid(lon, lat))) #lon and lat are what we extracted in step 2.

dim(lonlat) #we now have a matrix that has 93186 rows and 2columns. 93186=501*186
dim(ssrd_slice)
ssrd_vec
ssrd_vec <- as.vector( ssrd_slice) 
length(ssrd_vec)
ssrd_df <- data.frame( cbind( lonlat,ssrd_vec  ))
colnames(ssrd_df) <- c("lon", "lat", "ssrd")
View(ssrd_df)

ssrd_df_value <- na.omit (ssrd_df)
head(ssrd_df_value, 3) 
View(ssrd_df_value)

#Creating a spatial object from a lat/lon table
library(sf)
ssrd_sf<- st_as_sf( ssrd_df_value, coords = c(  "lon", "lat")  ) #convert long and lat to point in simple feature format
#To make it a complete geographical object we assign the WGS84 projection, which has the EPSG code 4326
st_crs(ssrd_sf) <- 4326 
ssrd_sf <- st_transform(ssrd_sf, 4326 )

library(tmap)
tmap_mode("view")

tm_shape(ssrd_sf)+
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "viridis")

#====Step 6. Convert solar radiation to power ====
ncatt_get(era,"ssrd","units") #joul per metre2 

# an example of a 1m2 (A) solar panel
radiation_to_power <- function(radiation, area=1, yield_r=0.175, pr=0.6, hours=1)
{ kWh <- radiation * area * yield_r * pr * hours * (1/3600000)
return(kWh) } 
# Radiation data for solar electric (photovoltaic) systems are often represented as kilowatt-hours per square meter (kWh/m2)
# 1 joule/m2 = 1/3600/1000 kWh / m2 (one 1KWh contains 3.6×106 Joules)

rad=9e+06 #scientific notation: equals to 9*10^6, you can also write as 9*10**6

ssrd_kwh <- as.data.frame (radiation_to_power (ssrd_df_value))
ssrd_df_value <- cbind(ssrd_df_value,ssrd_kwh$ssrd)
colnames(ssrd_df_value) [4] <- 'ssrd_kwh'
ssrd_sf$ssrd_kwh = ssrd_kwh$ssrd

tm_shape(ssrd_sf)+
  tm_dots(col="ssrd_kwh", style = "quantile", size=.001, palette = "YlOrRd")

#ssrd_sf indicate kWh/per m2

#===========Spatial interpolation============
# 加载必要的库
library(terra)
library(raster)
library(sp)
library(gstat)
library(tmap)

# 确保 ssrd_df_value 是 SpatialPointsDataFrame
coordinates(ssrd_df_value) <- ~lon+lat
proj4string(ssrd_df_value) <- CRS("+proj=longlat +datum=WGS84")

# 读取印尼边界并统一 CRS
indonesia <- st_read("idn_admbnda_adm0_bps_20200401.shp")
indonesia <- st_transform(indonesia, CRS("+proj=longlat +datum=WGS84"))

# 创建栅格模板
raster_template <- rast(
  resolution = 0.05,
  xmin = 95.01079, ymin = -11.00762,
  xmax = 141.01940, ymax = 6.07693,
  crs = "+proj=longlat +datum=WGS84"
)

# 转换为 RasterLayer（与 sp 兼容）
raster_layer <- raster(raster_template)

# 转换为 SpatialGridDataFrame
raster_spatial <- as(raster_layer, "SpatialGridDataFrame")

# 确保 CRS 一致
proj4string(raster_spatial) <- CRS("+proj=longlat +datum=WGS84")

# 检查数据
print(proj4string(ssrd_df_value))
print(proj4string(raster_spatial))



# 定义 IDW 模型
gs <- gstat(formula = ssrd ~ 1, locations = ssrd_df_value, nmax = Inf, set = list(idp = 2))

# 创建插值网格（从栅格模板转换而来）
grid <- as.data.frame(rasterToPoints(raster_layer))
coordinates(grid) <- ~x+y
proj4string(grid) <- CRS("+proj=longlat +datum=WGS84")

# 使用 gstat::idw 进行插值
idw_result <- idw(formula = ssrd ~ 1, locations = ssrd_df_value, newdata = grid, idp = 2)

# 转换插值结果为栅格
idw_raster <- rasterFromXYZ(as.data.frame(idw_result)[, c("x", "y", "var1.pred")],
                            crs = CRS("+proj=longlat +datum=WGS84"))

idw_df <- as.data.frame(idw_result)
idw_df$ssrd_kwh <- radiation_to_power(idw_df$var1.pred)
idw_raster_kwh <- rasterFromXYZ(
  idw_df[, c("x", "y", "ssrd_kwh")],
  crs = CRS("+proj=longlat +datum=WGS84")
)

# 可视化插值结果
plot(idw_raster, main = "IDW Interpolated SSRD")


# 剪裁插值结果到印尼边界
idw_clipped <- mask(idw_raster, indonesia)

idw_clipped_kwh <- mask(idw_raster_kwh, indonesia)

# 全年电力产能计算

90*4*2875/700
# summer
idw_clipped_kwh_summer <- idw_clipped_kwh*90*4*2875/700

#winter
idw_clipped_kwh_winter <- idw_clipped_kwh*90*4*1800/460

#yearly
idw_clipped_kwh_yearly <-(idw_clipped_kwh_summer + idw_clipped_kwh_winter)*2

# 查看剪裁后的结果
plot(idw_clipped, main = "Clipped IDW SSRD")


# 切换到交互模式
tmap_mode("view")

# 可视化插值结果
tm_shape(idw_clipped) +
  tm_raster(
    palette = "YlOrRd",              # 配色方案
    title = "Solar Radiation (SSRD)",
    style = "quantile",              # 等分位显示
    n = 10,                           # 图例分级数
) +
  tm_layout(
    main.title = "IDW Interpolation of Solar Radiation",
    legend.title.size = 1.2,   # 图例标题大小
    legend.text.size = 0.8,    # 图例文字大小
    frame = TRUE               # 显示边框
  )


tm_shape(idw_clipped_kwh) +
  tm_raster(
    palette = "YlOrRd",              # 配色方案
    title = "Solar Power (kWh/m²)",
    style = "quantile",              # 等分位显示
    n = 10,                          # 图例分级数
    alpha = 0.8                      # 设置透明度
  ) +
  tm_layout(
    main.title = "IDW Interpolation of Solar Power",
    legend.title.size = 1.2,         # 图例标题大小
    legend.text.size = 0.8,          # 图例文字大小
    frame = TRUE                     # 显示边框
  )


# 可视化全年电力产能
tm_shape(idw_clipped_kwh_yearly) +
  tm_raster(
    palette = "YlOrRd",              # 配色方案
    title = "Yearly Solar Power (kWh/m²)",
    style = "quantile",              # 等分位显示
    n = 10,                          # 图例分级数
    alpha = 0.8                      # 设置透明度
  ) +
  tm_layout(
    main.title = "Yearly Solar Power Production",
    legend.title.size = 1.2,         # 图例标题大小
    legend.text.size = 0.8,          # 图例文字大小
    frame = TRUE                     # 显示边框
  )



# 保存生成的栅格
writeRaster(idw_clipped, "idw_raster.tif", overwrite = TRUE)

# 验证生成的栅格
plot(idw_clipped, main = "Idw Raster")


# 保存生成的栅格
writeRaster(idw_clipped_kwh, "idw_kwh.tif", overwrite = TRUE)

# 验证生成的栅格
plot(idw_clipped_kwh, main = "Idw KWh Raster")


# 保存生成的栅格
writeRaster(idw_clipped_kwh_yearly, "idw_yearly.tif", overwrite = TRUE)

# 验证生成的栅格
plot(idw_clipped_kwh_yearly, main = "Idw Raster")











