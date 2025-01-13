install.packages("terra")  # 处理栅格数据
install.packages("ggplot2")  # 数据可视化
install.packages("sf")   # 用于处理 Shapefile
install.packages("tmap")  # 高级地图制作

library(sf)
library(terra)
library(ggplot2)
library(ggspatial)
library(tmap)

getwd()
setwd("C:/Users/LENOVO/Desktop/93大作业")



# 加载 Shapefile 数据
shapefile_path <- "idn_admbnda_adm0_bps_20200401.shp"
indonesia_boundary <- st_read(shapefile_path)

# 查看数据结构
print(indonesia_boundary)

# 绘制简单边界
plot(st_geometry(indonesia_boundary))

# 查看前几行属性数据
head(indonesia_boundary)

# 查看列名
colnames(indonesia_boundary)

# 查看投影信息
st_crs(indonesia_boundary)

ggplot(data = indonesia_boundary) +
  geom_sf() +
  labs(
    title = "Indonesia Administrative Boundaries",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

# tm_shape(indonesia_boundary) +
#   tm_polygons() +
#   tm_layout(
#     title = "Indonesia Administrative Boundaries",
#     legend.outside = TRUE
#   )


# 读取人口分布栅格数据
population <- rast("idn_ppp_2020_constrained.tif")
View(population)

# 查看栅格数据的基本信息
print(population)

# 设置降分辨率比例，例如将分辨率降低
factor <- 3  # 降低分辨率的比例

# 重新采样（aggregate 会对像素进行汇总）
population_low <- aggregate(population, fact = factor, fun = sum)

# 查看降分辨率后的数据基本信息
print(population_low)
View(population_low)

# 保存为新的栅格文件
writeRaster(population_low, "idn_population_low.tif", overwrite = TRUE)


# 裁剪人口数据到边界范围
population_low_cropped <- crop(population_low, indonesia_boundary)

population_cropped <- crop(population, indonesia_boundary)

# 简单可视化
plot(population, main = "Population Distribution in Indonesia (2020)")


# 转换为数据框以便 ggplot2 可用
population_low_df <- as.data.frame(population_low_cropped, xy = TRUE)

# 检查数据框结构
head(population_low_df)

# 检查 CRS
st_crs(indonesia_boundary)

# 绘制叠加图
ggplot(data = indonesia_boundary) + 
  geom_tile(data = population_low_df, aes(x = x, y = y, fill = idn_ppp_2020_constrained), width = 0.1, height = 0.1) +
  geom_sf(fill = "lightblue", color = "black", alpha = 0.3) +
  coord_sf() +  # 必须调用 coord_sf()
  scale_fill_viridis_c(name = "Population Density",
                       limits = c(0, 1200),  # 设置数据范围
                       breaks = seq(0, 1200, by = 300),  # 调整刻度
                       guide = guide_colorbar(
                         title.position = "top",  # 标题位置：置于比例尺上方
                         title.hjust = 0.5      # 标题水平居中
                     ) )+
  labs(
    title = "Indonesia Population Density with Boundaries",
    fill = "Population",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()+
  annotation_scale(location = "bl", width_hint = 0.3, style = "bar", text_cex = 0.8, unit_category = "metric")+
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering)+
theme(
  plot.title = element_text(hjust = 0.5, size = 14)  # hjust = 0.5 居中，size 调整字体大小
)
  
ggsave("population_distribution.png", width = 10, height = 8, dpi = 300, bg = "white")




# 标准化人口分布数据到 [0, 1] 范围
min_val <- min(population_low_cropped[], na.rm = TRUE)
max_val <- max(population_low_cropped[], na.rm = TRUE)
population_normalized <- (population_low_cropped - min_val) / (max_val - min_val)

# 保存标准化栅格数据
writeRaster(population_normalized, "idn_population_normalized.tif", overwrite = TRUE)

# 验证生成的栅格
plot(population_normalized, main = "IDW population")


# 标准化人口分布数据到 [0, 1] 范围
min_val <- min(population_cropped[], na.rm = TRUE)
max_val <- max(population_cropped[], na.rm = TRUE)
population_Normalized <- (population_cropped - min_val) / (max_val - min_val)

# 保存标准化栅格数据
writeRaster(population_Normalized, "idn_total_population_mormalized.tif", overwrite = TRUE)

# 验证生成的栅格
plot(population_Normalized, main = "IDW total population")






# # 加载必要的包
# library(sf)
# library(terra)
# library(dplyr)
# 
# # 1. 读取并预处理数据
# population_raster_downsample <- rast("idn_population_low.tif")  # 使用降分辨率的人口栅格数据
# indonesia_boundary <- st_read("idn_admbnda_adm0_bps_20200401.shp")  # 印度尼西亚边界
# 
# # 确保 CRS 一致
# population_raster_downsample <- project(population_raster_downsample, crs(indonesia_boundary))
# 
# # 2. 将人口栅格转换为点
# pop_points <- as.points(population_raster_downsample)
# pop_sf <- st_as_sf(pop_points, crs = st_crs(indonesia_boundary))  # 转为 sf 对象，保持边界 CRS
# 
# # 重命名人口列
# colnames(pop_sf)[1] <- "population"
# 
# # 3. 转换为 UTM 坐标系 (EPSG:32750)
# pop_sf_utm <- st_transform(pop_sf, crs = 32750)
# 
# # 4. 创建 1 公里缓冲区，仅对人口 > 2000 的点生成
# buffer_1km <- pop_sf_utm %>%
#   filter(population > 1000) %>%
#   st_buffer(dist = 1000)
# 
# # 检查缓冲区是否为空
# if (nrow(buffer_1km) == 0) {
#   stop("缓冲区为空，没有符合条件的人口点。")
# }
# 
# # 将缓冲区转换回地理坐标系 (WGS84)
# buffer_1km <- st_transform(buffer_1km, crs = 4326)
# 
# # 添加分类列用于标注
# buffer_1km$buffer_category <- "Risk zone (Red)"
# 
# 
# # 将栅格转换为点数据
# pop_points <- as.points(population_raster_downsample)
# 
# # 转换为 sf 对象
# pop_sf <- st_as_sf(pop_points, coords = c("x", "y"), crs = crs(population_raster_downsample))
# 
# # 检查点数据
# print(pop_sf)
# head(pop_sf)
# 
# # 保存点数据为矢量文件
# st_write(pop_sf, "population_points.shp", overwrite = TRUE)
# 
# 
# # 5. 将缓冲区与重采样点数据对比
# indo_downsampled_100_sf <- st_read("population_points.shp")  # 假设你的点数据文件名
# 
# # 初始化新列 population buffer risk zone 和 population buffer
# indo_downsampled_100_sf$population_buffer <- "Safe zone"
# indo_downsampled_100_sf$population_buffer_risk_zone <- NA
# 
# # 判断哪些点在 buffer_1km 中
# matched <- st_intersects(indo_downsampled_100_sf, buffer_1km, sparse = FALSE)
# 
# # 更新匹配结果
# indo_downsampled_100_sf$population_buffer_risk_zone[apply(matched, 1, any)] <- "Risk zone"
# indo_downsampled_100_sf$population_buffer[apply(matched, 1, any)] <- NA
# 
# # 确保未匹配的点仍然标记为 Safe zone
# indo_downsampled_100_sf$population_buffer[is.na(indo_downsampled_100_sf$population_buffer)] <- "Safe zone"
# 
# # 合并 Safe 和 Risk zone 列
# indo_downsampled_100_sf$population_buffer <- ifelse(
#   !is.na(indo_downsampled_100_sf$population_buffer_risk_zone),
#   indo_downsampled_100_sf$population_buffer_risk_zone,
#   indo_downsampled_100_sf$population_buffer
# )
# 
# # 删除临时列
# indo_downsampled_100_sf <- indo_downsampled_100_sf %>%
#   select(-population_buffer_risk_zone)
# 
# # 添加分类列
# indo_downsampled_100_sf <- indo_downsampled_100_sf %>%
#   mutate(population_buffer_weight = ifelse(population_buffer == "Safe zone", 1, 0)) %>%
#   mutate(area_category = ifelse(population_buffer_weight == 1, "Suitable area", "Unsuitable area"))
# 
# # 6. 保存输出
# st_write(buffer_1km, "population_buffer_1km.shp", overwrite = TRUE)  # 保存缓冲区
# st_write(indo_downsampled_100_sf, "indo_downsampled_100_sf_updated.shp", overwrite = TRUE)  # 保存更新后的点数据
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # 2. 设置人口密集阈值，提取高密度区域
# threshold <- 0.7  # 假设阈值为 0.7（可根据需求调整）
# # 生成高密度区域（使用低分辨率数据）
# dense_population_low <- population_low >= threshold
# 
# # 将高密度区域转换为矢量数据
# dense_population_vect <- as.polygons(dense_population_low, dissolve = TRUE)
# dense_population_vect <- st_as_sf(dense_population_vect)  # 转为 sf 对象
# 
# # 绘制人口密集区域
# plot(dense_population, col = c("blue", "pink"), main = "High Population Density Areas")
# # 3. 创建缓冲区
# buffer_distance <- 5000  # 设置缓冲区半径（单位：米）
# dense_population_buffer <- st_buffer(dense_population_vect, dist = buffer_distance)
# 
# indonesia_boundary <- st_transform(indonesia_boundary, st_crs(dense_population_buffer))
# 
# # 4. 裁剪缓冲区到印度尼西亚边界范围
# dense_population_buffer_cropped <- st_crop(dense_population_buffer, st_bbox(indonesia_boundary))
# 
# # 检查 CRS
# st_crs(dense_population_buffer)  # 缓冲区对象的 CRS
# st_crs(indonesia_boundary)      # 边界对象的 CRS
# 
# dense_population_buffer <- st_make_valid(dense_population_buffer)
# indonesia_boundary <- st_make_valid(indonesia_boundary)
# 
# st_bbox(dense_population_buffer)
# st_bbox(indonesia_boundary)
# 
# # 计算交集
# dense_population_buffer_clipped <- st_intersection(dense_population_buffer_cropped, indonesia_boundary)
# class(dense_population_buffer_clipped)
# st_geometry_type(dense_population_buffer_clipped)
# 
# # 确保 dense_population_buffer_clipped 是 sf 对象
# if (!inherits(dense_population_buffer_clipped, "sf")) {
#   stop("dense_population_buffer_clipped is not an sf object.")
# }
# 
# dense_population_buffer_clipped <- st_make_valid(dense_population_buffer_clipped)
# 
# # 获取几何列
# geom <- st_geometry(dense_population_buffer_clipped)
# 
# # 检查缓冲区结果
# plot(st_geometry(boundary), col = "lightblue", main = "Population Buffer Zone")
# plot(st_geometry(dense_population_buffer), add = TRUE, col = "red", alpha = 0.5)
# 
# # 5. 保存缓冲区为矢量文件
# st_write(dense_population_buffer, "population_buffer_zone.shp", overwrite = TRUE)
# 
# # 6. 将缓冲区叠加到地图上
# plot(boundary$geometry, col = "lightblue", main = "Population Buffer Zone with Boundary")
# plot(dense_population_buffer$geometry, col = "red", add = TRUE, alpha = 0.5)








