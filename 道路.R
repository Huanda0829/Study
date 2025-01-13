# 加载必要的包
library(sf)
library(ggplot2)
library(ggspatial)

install.packages("prettymapr")

getwd()
setwd("C:/Users/LENOVO/Desktop/93大作业")
# 设置文件路径
roads_path <- "IDN_roads.shp"
boundary_path <- "idn_admbnda_adm0_bps_20200401.shp"

# 读取 Shapefile 数据
roads <- st_read(roads_path)
indonesia_boundary <- st_read(boundary_path)
# 查看数据结构
print(roads)
print(indonesia_boundary)

st_bbox(indonesia_boundary)  # 查看边界数据范围
st_bbox(roads)               # 查看道路数据范围

st_crs(indonesia_boundary)  # 查看边界数据的 CRS
st_crs(roads)               # 查看道路数据的 CRS

# 绘制道路数据
ggplot(data = indonesia_boundary) +
  geom_sf() +
  labs(
    title = "Indonesia Administrative Boundaries",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()


ggplot() +
  geom_sf(data = indonesia_boundary, fill = "lightblue", color = "black", size = 0.5) +  # 边界数据
  geom_sf(data = roads, color = "blue", size = 0.5) +                        # 道路数据
  labs(
    title = "Roads in Indonesia with Country Boundary",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()+
  annotation_scale(location = "bl", width_hint = 0.3, style = "bar", text_cex = 0.8, unit_category = "metric")+
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering)+
  theme(
    plot.title = element_text(hjust = 0.5, size = 12)  # hjust = 0.5 居中，size 调整字体大小
  )

ggsave("road.png", width = 10, height = 8, dpi = 300, bg = "white")





library(sf)
library(terra)

# 创建一个空栅格（设置分辨率和范围）

# 检查并修复空几何体
roads <- roads[!st_is_empty(roads), ]
roads <- st_make_valid(roads)

print(roads)
print(roads_vect)
print(road_raster)
# 检查矢量数据的几何类型
st_geometry_type(roads)

# 转换为 terra 的矢量对象
roads_vect <- vect(roads)

# 创建空栅格模板
road_raster <- rast(
  extent = ext(roads_vect),  # 使用矢量数据范围
  resolution = 0.01,         # 设置分辨率（单位：度）
  crs = crs(roads_vect)      # 使用矢量数据的投影
)

# 为矢量数据添加字段值
roads_vect$road_value <- 1  # 给所有道路分配值为 1

# 栅格化道路数据
roads_raster <- rasterize(
  roads_vect,          # 矢量数据
  road_raster,         # 栅格模板
  field = "road_value",# 栅格化的字段
  fun = "sum"          # 统计值，确保标记所有道路像元
)


# 计算距离
distance_raster <- distance(roads_raster)

# 保存结果
writeRaster(distance_raster, "road_distance.tif", format = "GTiff", overwrite = TRUE)

# 可视化结果
plot(distance_raster, main = "Distance to Nearest Road")

# 设置布尔值
roads_raster[roads_raster > 0] <- 1
roads_raster[roads_raster == 0] <- NA

# 保存生成的栅格
writeRaster(roads_raster, "roads_raster.tif", overwrite = TRUE)

# 验证生成的栅格
plot(roads_raster, main = "Road Raster")





library(terra)
library(sf)

# 1. 加载栅格和道路矢量数据
road_raster <- rast("roads_raster.tif")  # 道路栅格数据
road_vector <- vect("IDN_roads.shp") # 道路矢量数据

# 确保道路矢量数据的投影与栅格一致
road_vector <- project(road_vector, crs(road_raster))

# 2. 将栅格转换为点数据
road_points <- as.points(road_raster, values = TRUE, na.rm = TRUE)

# 检查点数据
print(road_points)

# 3. 提取点的 X 和 Y 坐标
coords <- geom(road_points)[, c("x", "y")]
road_points$X <- coords[, "x"]
road_points$Y <- coords[, "y"]

# 4. 转换点数据为 sf 对象
road_points_sf <- st_as_sf(road_points, coords = c("X", "Y"), crs = crs(road_raster))

# 5. 计算每个点到道路的最短距离
road_distances <- st_distance(road_points_sf, st_as_sf(road_vector))  # 计算所有点到道路的距离矩阵

# 提取最小距离（单位通常为 CRS 的单位，比如米）
road_points_sf$distance_to_road <- apply(road_distances, 1, min)

# 检查结果
print(road_points_sf)

# 6. 保存结果为文件
write.csv(st_drop_geometry(road_points_sf), "road_points_distances.csv", row.names = FALSE)



library(sf)
library(terra)

# 1. 读取 CSV 文件
road_dist <- read.csv("road_points_distances.csv")
head(road_dist)

colnames(road_dist)  # 检查列名

# 假设原列名为 `lon` 和 `lat`
colnames(road_dist)[which(colnames(road_dist) == "X")] <- "longitude"
colnames(road_dist)[which(colnames(road_dist) == "Y")] <- "latitude"

# 2. 转换为 sf 对象
road_sf <- st_as_sf(
  road_dist,
  coords = c("longitude", "latitude"),  # 替换为实际的列名
  crs = 4326                           # 假设 CRS 为 WGS84
)

# 3. 创建栅格模板
road_extent <- ext(road_sf)  # 使用点数据范围
resolution <- 0.01               # 设置分辨率
raster_template <- rast(
  extent = road_extent,
  resolution = resolution,
  crs = crs(road_sf)          # 使用点数据 CRS
)

# 4. 栅格化
road_vect <- vect(road_sf)
rasterized <- rasterize(
  road_vect,
  raster_template,
  field = "distance_to_road",  # 替换为实际值列名
  fun = "mean"
)

# 5. 保存栅格文件
writeRaster(rasterized, "road_raster.tif", overwrite = TRUE)

# 6. 可视化
plot(rasterized, main = "Road Raster")


