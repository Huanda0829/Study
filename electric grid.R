# library(sf)
# library(ggplot2)

# # 加载上传的电网数据
# electric_grid <- st_read("grid.geojson")
# 
# # 查看数据结构
# print(electric_grid)
# 
# # 可视化电网数据
# ggplot() +
#   geom_sf(data = electric_grid, color = "blue", size = 0.5) +
#   labs(title = "Electric Grid in Indonesia", x = "Longitude", y = "Latitude") +
#   theme_minimal()

library(osmdata)
library(tmap)
library(sf)
install.packages("osmdata")
#步骤 1 通过 bbox 定义查询区域
coords<-matrix(c(95.01079,141.01940,-11.00762, 6.07693),byrow = TRUE,nrow =2,ncol = 2, dimnames = list(c('x','y'),c('min','max')))
location <- opq(coords)

#步骤 2 搜索感兴趣的特征
powerplant = add_osm_feature(location, key = 'power', value = "line" )
powerplant.sf = osmdata_sf(powerplant)
tm_shape(powerplant.sf$osm_lines)+tm_lines(col="voltage")


# 转换电网数据为 terra 的矢量对象
power_lines_vect <- vect(powerplant.sf$osm_lines)

# 创建空栅格模板
grid_raster <- rast(
  extent = ext(power_lines_vect),  # 使用电网矢量数据的范围
  resolution = 0.01,              # 设置分辨率（度）
  crs = crs(power_lines_vect)     # 使用电网数据的投影
)

# 为电网矢量数据添加字段值
power_lines_vect$line_value <- 1  # 所有电网线赋值为 1

# 栅格化电网数据
power_lines_raster <- rasterize(
  power_lines_vect,      # 矢量数据
  grid_raster,           # 栅格模板
  field = "line_value",  # 栅格化字段
  fun = "sum"            # 统计值
)

# 转为布尔值栅格（0 和 1）
power_lines_raster[power_lines_raster > 0] <- 1

# 保存电网栅格为 GeoTIFF 文件
writeRaster(power_lines_raster, "power_lines_raster.tif", overwrite = TRUE)

# 验证生成的栅格
plot(power_lines_raster, main = "Power Lines Raster")







library(sf)
library(terra)

# 1. 读取 CSV 文件
grid_dist <- read.csv("grid_points_distances.csv")
head(grid_dist)

colnames(grid_dist)  # 检查列名

# 假设原列名为 `lon` 和 `lat`
colnames(grid_dist)[which(colnames(grid_dist) == "X")] <- "longitude"
colnames(grid_dist)[which(colnames(grid_dist) == "Y")] <- "latitude"

# 2. 转换为 sf 对象
grid_sf <- st_as_sf(
  grid_dist,
  coords = c("longitude", "latitude"),  # 替换为实际的列名
  crs = 4326                           # 假设 CRS 为 WGS84
)

# 3. 创建栅格模板
grid_extent <- ext(grid_sf)  # 使用点数据范围
resolution <- 0.01               # 设置分辨率
raster_template <- rast(
  extent = grid_extent,
  resolution = resolution,
  crs = crs(grid_sf)          # 使用点数据 CRS
)

# 4. 栅格化
grid_vect <- vect(grid_sf)
rasterized <- rasterize(
  grid_vect,
  raster_template,
  field = "distance_to_grid",  # 替换为实际值列名
  fun = "mean"
)

# 5. 保存栅格文件
writeRaster(rasterized, "power_lines_raster.tif", overwrite = TRUE)

# 6. 可视化
plot(rasterized, main = "Power Lines Raster")










