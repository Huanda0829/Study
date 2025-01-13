library(sf)
library(ggplot2)
library(tmap)
library(ggspatial)
library(dplyr)   # 用于数据筛选和操作


getwd()
setwd("C:/Users/LENOVO/Desktop/93大作业")

# 设置文件路径
land_cover_path <- "C:/Users/LENOVO/Desktop/93大作业/idn_land_cover.shp"  # 替换为实际路径

# 读取土地覆盖数据
land_cover <- st_read(land_cover_path)
st_crs(land_cover)
# 检查数据结构
print(land_cover)

colnames(land_cover)  # 查看列名
head(land_cover)      # 查看前几行数据

land_types <- unique(land_cover$desc_en)  # 获取所有土地类型
land_types


# # 加载矢量数据并栅格化
# land_cover_raster <- rasterize(
#   vect(land_cover),  # 矢量数据
#   rast(extent = ext(land_cover), resolution = 0.01),  # 定义栅格分辨率（0.01 度）
#   field = "desc_en"  # 使用字段（如 desc_en）作为栅格值
# )
# 
# # 重采样到较低分辨率 (例如 1 公里)
# land_cover_resampled <- aggregate(land_cover_raster, fact = 10)  # fact = 10 表示每 10 个像元聚合为 1 个
# 
# # 检查重采样后的数据
# print(land_cover_resampled)
# 
# # 可视化重采样后的数据
# plot(land_cover_resampled, main = "Resampled Land Cover Map")


# # 简化矢量数据
# land_cover_simplified <- st_simplify(land_cover, dTolerance = 1000, preserveTopology = TRUE)  # dTolerance 控制简化程度
# land_cover_simplified
# 
# # 修复无效几何
# land_cover_fixed <- st_make_valid(land_cover)
# 
# # 再次检查几何是否有效
# validity_fixed <- st_is_valid(land_cover_fixed)
# print(sum(!validity_fixed))  # 修复后的无效几何数量


# 可视化土地覆盖类型
ggplot() +
  geom_sf(data = land_cover, aes(fill = desc_en), color = NA) +  # 使用 desc_en 作为分类列
  scale_fill_viridis_d(name = "Land Cover Types") +  # 自动分配颜色
  labs(
    title = "Land Cover Map",
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

ggsave("landcover.png", width = 10, height = 8, dpi = 300, bg = "white")



# 定义需要避免和适合开发的土地类型
avoid_types <- c(
  "Primary Dry Land Forest", "Secondary Dry Land Forest",
  "Primary Mangrove Forest", "Secondary Mangrove Forest","Swamp Shrub",
  "Primary Swamp Forest", "Secondary Swamp Forest","Estate Crop Plantation",
  "Dryland Agriculture","Shrub-Mixed Dryland Farm","Swamp", "Rice Field", 
  "Bodies of Water","Plantation Forest","Settlement Area", "Airport / Harbour", 
  "Mining Area", "Fish Pond","Transmigration Area"
)

suitable_types <- c(
   "Savannah", "Bare Land",
   "Bush / Shrub")

# 提取非几何属性数据
attribute_table <- st_drop_geometry(land_cover)

# 确保不是函数
class(attribute_table)  # 应返回 "data.frame"

# 筛选数值列
numeric_cols <- sapply(attribute_table, is.numeric)

# 转换数值列为矩阵
numeric_matrix <- data.matrix(attribute_table[, numeric_cols])

# 检查结果
print(numeric_matrix)

str(data)

ls()  # 列出当前环境中的所有变量

class(land_cover)
class(land_cover$desc_en)
unique(land_cover$desc_en)
# 去除多余空格
land_cover$desc_en <- trimws(land_cover$desc_en)

# 确保字符串匹配
avoid_types <- trimws(avoid_types)
suitable_types <- trimws(suitable_types)

# 筛选需要避免的区域
avoid_land <- land_cover %>%
  filter(desc_en %in% avoid_types)

# 筛选适合开发的区域
suitable_land <- land_cover %>%
  filter(desc_en %in% suitable_types)

unique(land_cover$desc_en)
unique(avoid_land$desc_en)

# 可视化需要避免的区域
ggplot() +
  geom_sf(data = avoid_land, aes(fill = desc_en), color = NA) +
  labs(
    title = "Land Types to Avoid for Solar Power Plants",
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

ggsave("avoid_landcover.png", width = 10, height = 8, dpi = 300, bg = "white")



# 可视化适合开发的区域
ggplot() +
  geom_sf(data = suitable_land, aes(fill = desc_en), color = NA) +
  labs(
    title = "Suitable Land Types for Solar Power Plants",
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

ggsave("suitable_landcover.png", width = 10, height = 8, dpi = 300, bg = "white")





# 添加新的分类字段
land_cover <- land_cover %>%
  mutate(
    suitability = case_when(
      desc_en %in% avoid_types ~ "Avoid",
      desc_en %in% suitable_types ~ "Suitable",
      TRUE ~ "Other"
     
    )
  )

# 提取适宜地区
suitable_land <- land_cover %>%
  filter(suitability == "Suitable")

# 可视化适合和需要避免的区域
ggplot() +
  geom_sf(data = land_cover, aes(fill = suitability), color = NA) +
  scale_fill_manual(values = c("Avoid" = "red", "Suitable" = "green"),
                    name = "Suitability") +
  labs(
    title = "Land Suitability for Solar Power Plants",
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

ggsave("all landcover.png", width = 10, height = 8, dpi = 300, bg = "white")



# 保存适合开发的区域
st_write(suitable_land, "suitable_land.geojson", driver = "GeoJSON")

# 保存需要避免的区域
st_write(avoid_land, "avoid_land.geojson", driver = "GeoJSON")

library(terra)

# 创建栅格模板
raster_template <- rast(
  extent = ext(land_cover),        # 定义范围
  resolution = 0.01,               # 分辨率（单位是数据的 CRS，例如 0.01 度）
  crs = st_crs(land_cover)$proj4string  # 坐标系
)



# 栅格化适宜地区
suitable_raster <- rasterize(
  vect(suitable_land),      # 转换为 terra 矢量对象
  raster_template,          # 栅格模板
  field = "suitability"     # 使用 suitability 字段作为栅格值
)

# 确保栅格值为数值类型（适宜地区标记为 1，其余为 NA）
suitable_raster[suitable_raster == "Suitable"] <- 1

terra::writeRaster(suitable_raster, "suitable_land.tif", filetype = "GTiff", overwrite = TRUE)

# 验证文件是否保存成功
saved_raster <- rast("suitable_land.tif")

print(suitable_raster)  # 查看栅格数据的基本信息

# 查看矢量数据的字段和唯一值
names(suitable_land)
unique(suitable_land$suitability_numeric)

# 检查几何有效性
validity <- st_is_valid(suitable_land)
table(validity)  # 如果有 FALSE，说明存在无效几何

# 检查唯一值
unique(values(suitable_raster))

# 用 0 替换 NA
suitable_raster[is.na(suitable_raster)] <- 0

# 设置分类信息
levels(suitable_raster) <- data.frame(
  value = c(0, 1),
  category = c("Not Suitable", "Suitable")
)

# 绘制分类栅格
plot(suitable_raster, main = "Suitable Land", col = c("grey", "green"))







