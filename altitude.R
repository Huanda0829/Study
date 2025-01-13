library(terra)  # 用于处理栅格数据
library(sf)     # 用于处理矢量数据
library(ggplot2)  # 用于可视化


getwd()
setwd("C:/Users/LENOVO/Desktop/93大作业")

# 加载海拔数据
elevation <- rast("IDN_alt.vrt")  # 替换为实际路径

# 加载印度尼西亚边界数据
boundary <- st_read("C:/Users/LENOVO/Desktop/93大作业/idn_admbnda_adm0_bps_20200401.shp")  # 替换为实际路径

# 裁剪海拔数据到印度尼西亚范围
elevation_cropped <- crop(elevation, vect(boundary))  # 裁剪栅格
elevation_cropped <- mask(elevation_cropped, vect(boundary))  # 应用边界掩膜

# 定义适合太阳能发电的海拔范围 (0 - 1500米)
suitability <- elevation_cropped >= 0 & elevation_cropped <= 1500

# 筛选海拔范围 0-1500 米
elevation_filtered <- clamp(elevation_raster, lower = 0, upper = 1500, values = TRUE)

# 确保只保留 0-1500 范围的数据
elevation_filtered[elevation_filtered < 0 | elevation_filtered > 1500] <- NA

# 可视化海拔数据
plot(elevation_cropped, main = "Elevation in Indonesia", col = terrain.colors(20))

# 将适宜性区域可视化
plot(suitability, main = "Suitability Map for Solar Power Plants", col = c("red", "green"))

# 叠加边界数据
plot(vect(boundary), add = TRUE, border = "black", lwd = 0.5)


# 将栅格数据转换为数据框
elevation_df <- as.data.frame(elevation_cropped, xy = TRUE)
suitability_df <- as.data.frame(suitability, xy = TRUE)
elevation_filtered_df = as.data.frame(elevation_filtered, xy = TRUE)

# 使用 ggplot 绘制海拔地图
ggplot() +
  geom_raster(data = elevation_df, aes(x = x, y = y, fill = IDN_alt)) +
  scale_fill_gradient(low = "white", high = "brown", name = "Elevation (m)") +
  geom_sf(data = boundary, fill = "transparent", color = "black", size = 0.5) +
  labs(title = "Elevation Map of Indonesia", x = "Longitude", y = "Latitude") +
  theme_minimal()+
  annotation_scale(location = "bl", width_hint = 0.3, style = "bar", text_cex = 0.8, unit_category = "metric")+
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering)+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14)  # hjust = 0.5 居中，size 调整字体大小
  )

ggsave("altitude.png", width = 10, height = 8, dpi = 300, bg = "white")


ggplot() +
  geom_raster(data = elevation_filtered_df, aes(x = x, y = y, fill = IDN_alt)) +
  scale_fill_gradient(low = "white", high = "brown", name = "Elevation (m)") +
  geom_sf(data = boundary, fill = "transparent", color = "black", size = 0.5) +
  labs(title = "Filtered Elevation Map of Indonesia", x = "Longitude", y = "Latitude") +
  theme_minimal()+
  annotation_scale(location = "bl", width_hint = 0.3, style = "bar", text_cex = 0.8, unit_category = "metric")+
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering)+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14)  # hjust = 0.5 居中，size 调整字体大小
  )

ggsave("flitered altitude.png", width = 10, height = 8, dpi = 300, bg = "white")





head(suitability_df)


# 使用 ggplot 绘制适宜性地图
ggplot() +
  geom_raster(data = suitability_df, aes(x = x, y = y, fill = IDN_alt)) +
  scale_fill_manual(values = c("red", "green"), labels = c("Not Suitable", "Suitable"), name = "Suitability") +
  geom_sf(data = boundary, fill = "transparent", color = "black", size = 0.5) +
  labs(title = "Suitability Map for Solar Power Plants in Indonesia", x = "Longitude", y = "Latitude") +
  theme_minimal()+
  annotation_scale(location = "bl", width_hint = 0.3, style = "bar", text_cex = 0.8, unit_category = "metric")+
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering)+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14)  # hjust = 0.5 居中，size 调整字体大小
  )

ggsave("Altitude suitability analysis.png", width = 10, height = 8, dpi = 300, bg = "white")



# 导出裁剪后的海拔栅格数据为 GeoTIFF 文件
writeRaster(elevation_cropped, 
            filename = "elevation_cropped.tif", 
            filetype = "GTiff", 
            overwrite = TRUE)

# 导出适宜性栅格数据为 GeoTIFF 文件
writeRaster(suitability, 
            filename = "suitability.tif", 
            filetype = "GTiff", 
            overwrite = TRUE)

# 导出适宜性栅格数据为 GeoTIFF 文件
writeRaster(elevation_filtered, 
            filename = "elevation_filtered.tif", 
            filetype = "GTiff", 
            overwrite = TRUE)

elevation_raster <- rast("elevation_cropped.tif")

plot(elevation_raster, main = "Cropped Elevation Data", col = terrain.colors(20))

suitability_raster <- rast("suitability.tif")

plot(suitability_raster, main = "Suitability Analysis", col = c("red", "green"))

elevation_filtered_raster <- rast("elevation_filtered.tif")

plot(elevation_filtered_raster, main = "Filtered Elevation Data", col = terrain.colors(20))









