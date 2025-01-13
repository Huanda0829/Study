install.packages("sf")
install.packages("terra")  
install.packages("ggplot2")
install.packages(c("sp","gstat","tmap"))
install.packages("dplyr")
install.packages("ahpsurvey")
install.packages("ggspatial")


library(sf)
library(sp)
library(ggplot2)
library(dplyr)
library(ggspatial)
library(terra)
library(ahpsurvey)


getwd()
setwd("C:/Users/LENOVO/Desktop/93大作业")

# 读取 shapefile
roads <- st_read("IDN_roads.shp")

# 查看数据结构
print(roads)

# 简单绘图
plot(st_geometry(roads), main = "Indonesia Road Network")

# 创建一个示例栅格（如真实太阳能潜力数据时替换）
example_raster <- raster(extent(roads), res = 0.01)
values(example_raster) <- 1

# 计算距离
road_distance <- raster::distanceFromPoints(example_raster, as(roads, "Spatial"))

# 保存结果
writeRaster(road_distance, "road_distance.tif", overwrite = TRUE)

# 可视化距离
plot(road_distance, main = "Distance to Nearest Road")


# 读取水域面数据
water_areas <- st_read("IDN_water_areas_dcw.shp")

# 查看数据结构
print(water_areas)

# 读取水线数据
water_lines <- st_read("IDN_water_lines_dcw.shp")

# 查看数据结构
print(water_lines)

# 绘制水域面和水线
plot(st_geometry(water_areas), col = "blue", main = "Water Bodies and Lines in Indonesia")
plot(st_geometry(water_lines), col = "cyan", add = TRUE)

# 创建一个示例栅格（替换为实际分析数据的栅格）
example_raster <- raster(extent(water_areas), res = 0.01)
values(example_raster) <- 1

# 计算距离到水域面
distance_to_water_areas <- raster::distanceFromPoints(example_raster, as(water_areas, "Spatial"))

# 计算距离到水线
distance_to_water_lines <- raster::distanceFromPoints(example_raster, as(water_lines, "Spatial"))

# 保存结果
writeRaster(distance_to_water_areas, "distance_to_water_areas.tif", overwrite = TRUE)
writeRaster(distance_to_water_lines, "distance_to_water_lines.tif", overwrite = TRUE)

# 可视化结果
plot(distance_to_water_areas, main = "Distance to Water Areas")
plot(distance_to_water_lines, main = "Distance to Water Lines")











library(ggplot2)
library(sf)


protected_area <- st_as_sf(protected_area)
idn_subnational_admin_raster_df <- st_as_sf(idn_subnational_admin_raster_df)

ggplot() +
  geom_sf(data = idn_subnational_admin_raster_df, fill = NA) +
  geom_sf(data = protected_area, aes(fill = MARINE)) +
  labs(x = "", y = "", fill = "Protected Area") +
  scale_fill_manual(values = c("darkgreen", "#9B870C", "darkblue"),
                    labels = c("Terrestrial", "Coastal", "Marine")) +
  guides(fill = guide_legend(override.aes = list(size = 6, shape = 21))) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", which_north = TRUE) +
  transparent_theme
















