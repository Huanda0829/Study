# 加载所需包
install.packages("ahpsurvey")

library(sf)        # 处理矢量数据
#library(raster)    # 处理栅格数据
library(tidyverse) # 数据处理与可视化
library(ggplot2)   # 绘图
library(terra)   # 用于读写和处理栅格
library(stats)   # R自带，包含一些基础统计与矩阵运算函数
library(ahpsurvey)
remove.packages("raster")

getwd()
setwd("C:/Users/LENOVO/Desktop/93大作业")

# 1. 加载数据
solar_raster <- rast("idw_raster.tif")                      # 太阳能辐射栅格数据
population_raster <- rast("idn_population_normalized.tif")  # 人口分布
#road_raster <- rast("roads_raster.tif")                     # 道路数据
#powerlines_raster <- rast("power_lines_raster.tif")         # 电网数据
landcover_raster <- rast("suitable_land.tif")               # 土地覆盖数据
altitude_raster <- rast("elevation_filtered.tif")           # 海拔
grid_dist <- rast("power_lines_raster.tif")
road_dist <- rast("road_raster.tif")


# 让 r2 与 r1 对齐
solar_raster_aligned <- resample(
  solar_raster, 
  road_raster,               # 参考栅格
  method   = "bilinear",    # 连续型数据用 bilinear / cubic ...
  filename = "solar_raster_aligned.tif", 
  overwrite= TRUE
)

# 让 r3 与 r1 对齐
population_raster_aligned <- resample(
  population_raster, 
  road_raster, 
  method   = "bilinear",       
  filename = "population_raster_aligned.tif",
  overwrite= TRUE
)
# r4
# powerlines_raster_aligned <- resample(
#   powerlines_raster, 
#   road_raster,               # 参考栅格
#   method   = "bilinear",    # 连续型数据用 bilinear / cubic ...
#   filename = "powerlines_raster_aligned.tif", 
#   overwrite= TRUE
# )
# r5
landcover_raster_aligned <- resample(
  landcover_raster, 
  road_raster,               # 参考栅格
  method   = "near",     # 如果是分类数据(如土地覆盖)，用 nearest neighbor
  filename = "landcover_raster_aligned.tif", 
  overwrite= TRUE
)
# r6
altitude_raster_aligned <- resample(
  altitude_raster, 
  road_raster,               # 参考栅格
  method   = "bilinear",    # 连续型数据用 bilinear / cubic ...
  filename = "altitude_raster_aligned.tif", 
  overwrite= TRUE
)

grid_dist_aligned <- resample(
  grid_dist, 
  road_raster,               # 参考栅格
  method   = "bilinear",    # 连续型数据用 bilinear / cubic ...
  filename = "grid_dist_aligned.tif", 
  overwrite= TRUE
)

road_dist_aligned <- resample(
  road_dist, 
  road_raster,               # 参考栅格
  method   = "bilinear",    # 连续型数据用 bilinear / cubic ...
  filename = "road_dist_aligned.tif", 
  overwrite= TRUE
)

# 这样就都在投影、分辨率、范围上统一了

ext(grid_dist); crs(grid_dist); res(grid_dist)
ext(grid_dist_aligned); crs(grid_dist_aligned); res(grid_dist_aligned)

ext(solar_raster); crs(solar_raster); res(solar_raster)
ext(solar_raster_aligned); crs(solar_raster_aligned); res(solar_raster_aligned)

ext(population_raster); crs(population_raster); res(population_raster)
ext(population_raster_aligned); crs(population_raster_aligned); res(population_raster_aligned)

ext(road_raster); crs(road_raster); res(road_raster)

ext(powerlines_raster); crs(powerlines_raster); res(powerlines_raster)
ext(powerlines_raster_aligned); crs(powerlines_raster_aligned); res(powerlines_raster_aligned)


ext(landcover_raster); crs(landcover_raster); res(landcover_raster)
ext(landcover_raster_aligned); crs(landcover_raster_aligned); res(landcover_raster_aligned)

ext(altitude_raster); crs(altitude_raster); res(altitude_raster)
ext(altitude_raster_aligned); crs(altitude_raster_aligned); res(altitude_raster_aligned)


####标准化####


safeMinMaxNormalize <- function(r) {
  # 1. 提取栅格像元为向量 (或大矩阵)
  vals <- values(r, mat=FALSE)  # mat=FALSE 取出向量
  
  # 2. 计算最小值和最大值
  r_min <- min(vals, na.rm=TRUE)
  r_max <- max(vals, na.rm=TRUE)
  
  # 3. 进行归一化 (避免除0错误)
  if (r_max == r_min) {
    # 若全为同一个值，可直接设为0或其它
    vals_norm <- rep(0, length(vals))
  } else {
    vals_norm <- (vals - r_min) / (r_max - r_min)
  }
  
  # 4. 将标准化后的向量写回原栅格
  r_norm <- setValues(r, vals_norm)
  return(r_norm)
}




# solar_raster_aligned 是 SpatRaster
solar_norm <- safeMinMaxNormalize(solar_raster_aligned)

# altitude
alt_temp   <- safeMinMaxNormalize(altitude_raster_aligned)
alt_norm   <- 1 - alt_temp

# population
pop_temp   <- safeMinMaxNormalize(population_raster_aligned)
pop_norm   <- 1 - pop_temp

# 如果 landcover_raster_aligned 是连续数据
landcover_norm <- safeMinMaxNormalize(landcover_raster_aligned)

# 如果你有 road_dist_aligned
road_temp  <- safeMinMaxNormalize(road_dist_aligned)
road_norm  <- 1 - road_temp

# 同理， grid_dist_aligned
grid_temp  <- safeMinMaxNormalize(grid_dist_aligned)
grid_norm  <- 1 - grid_temp


# 简单可视化对比
par(mfrow=c(2,3))
plot(solar_norm, main="Solar Norm")
plot(alt_norm, main="Altitude Norm")
plot(pop_norm, main="Population Norm")
plot(landcover_norm, main="Landcover Norm")
plot(road_norm, main="Distance to Road Norm")
plot(grid_norm, main="Distance to Grid Norm")

#构建AHP矩阵
# ahp_matrix[i,j]: 指标i相比指标j的重要性 (1~9 及其倒数)
# Solar (太阳辐射，solar_norm)
# Altitude (海拔，alt_norm)
# Population (人口分布，pop_norm)
# Landcover (土地覆盖，landcover_norm)
# Road (距道路，road_norm)
# Grid (距电网，grid_norm)
ahp_matrix <- matrix(c(
  1,   5,   7,   6,   4,   5,   # Solar (太阳辐射，solar_norm) Solar 相比其他重要程度
  1/5, 1,   3,   4,   3,   4,   # Altitude (海拔，alt_norm)
  1/7, 1/3, 1,   3,   2,   3 ,  # Population (人口分布，pop_norm)
  1/6, 1/4, 1/3, 1,   2,   2,   # Landcover (土地覆盖，landcover_norm)
  1/4, 1/3, 1/2, 1/2, 1,   3,   # Road (距道路，road_norm)
  1/5, 1/4, 1/3, 1/2, 1/3, 1    # Grid (距电网，grid_norm)
), nrow=6, byrow=TRUE)

# Solar 是最重要的，和其他所有指标相比权重都较高。例如，Solar/Altitude = 5 表示太阳辐射的重要性是海拔的 5 倍。
# Road 和 Grid 次之，但不如 Solar，且二者之间权重接近。
# Population 和 Altitude 处于较低的重要性，但仍需考虑。

# 指定行列名
rownames(ahp_matrix) <- c("Solar", "Altitude", "Population", "Landcover", "Road", "Grid")
colnames(ahp_matrix) <- rownames(ahp_matrix)

ahp_matrix

# 计算特征值与特征向量
eigen_res <- eigen(ahp_matrix)

# 最大特征值（主特征值）
lambda_max <- Re(eigen_res$values[1])

# 对应的特征向量
w <- Re(eigen_res$vectors[,1])

# 特征向量归一化处理(使各维度和为1)
w_normalized <- w / sum(w)

w_normalized


n <- nrow(ahp_matrix)  # 6
RI <- 1.24             # 从标准表查得

CI <- (lambda_max - n) / (n - 1) 
CR <- CI / RI

#一致性指标
CI
#一致性比率
CR # CR = 0.09732047 判断矩阵一致性可以接受

cat("=== AHP 结果 ===\n")
cat("各因素对应的权重（归一化）:\n")
print(w_normalized)

cat("最大特征值 (lambda_max):", lambda_max, "\n")
cat("一致性指标 (CI):", CI, "\n")
cat("一致性比率 (CR):", CR, "\n")

if(CR < 0.1){
  cat("判断矩阵的一致性通过 (CR < 0.1)，可以接受。\n")
} else {
  cat("判断矩阵一致性较差 (CR >= 0.1)，建议需要调整。\n")
}

crs(solar_norm)
crs(alt_norm)
crs(pop_norm)
crs(landcover_norm)
crs(road_norm)
crs(grid_norm)

#将NA值替换为0
solar_norm[is.na(solar_norm)] <- 0
alt_norm[is.na(alt_norm)] <- 0
pop_norm[is.na(pop_norm)] <- 0
landcover_norm[is.na(landcover_norm)] <- 0
road_norm[is.na(road_norm)] <- 0
grid_norm[is.na(grid_norm)] <- 0

#使用周围值进行插值填充
solar_norm <- focal(solar_norm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE)
alt_norm <- focal(alt_norm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE)
pop_norm <- focal(pop_norm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE)
landcover_norm <- focal(landcover_norm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE)
road_norm <- focal(road_norm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE)
grid_norm <- focal(grid_norm, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE)


# 计算综合评分 (加权和)
weighted_score <- 
  solar_norm      * w_normalized[1] +
  alt_norm        * w_normalized[2] +
  pop_norm        * w_normalized[3] +
  landcover_norm  * w_normalized[4] +
  road_norm       * w_normalized[5] +
  grid_norm       * w_normalized[6]

weighted_score

# 计算指定百分位数
p_min <- quantile(values(weighted_score), probs = 0.05, na.rm = TRUE)
p_max <- quantile(values(weighted_score), probs = 0.95, na.rm = TRUE)

# 百分位归一化
weighted_score_norm <- (weighted_score - p_min) / (p_max - p_min)

# 将值限制在 0 到 1 之间
weighted_score_norm[weighted_score_norm < 0] <- 0
weighted_score_norm[weighted_score_norm > 1] <- 1

# 可视化
plot(weighted_score_norm, main = "Percentile Normalized Weighted Score")

crs(weighted_score)
ext(weighted_score)
res(weighted_score)

# 可视化
plot(weighted_score, main = "Weighted Score for Solar Site Selection", 
     col = terrain.colors(10), zlim = c(0.2, 0.8))

# 将综合得分保存为一个新栅格文件
writeRaster(weighted_score, "weighted_score.tif", overwrite = TRUE)


# 检查 NA 值比例
na_ratio <- sum(is.na(values(weighted_score_norm))) / ncell(weighted_score_norm)
print(paste("NA Ratio:", na_ratio))

hist(values(weighted_score), main = "Weighted Score Distribution", 
     xlab = "Weighted Score", breaks = 50)



# 按权重计算每一层的加权分数
weighted_solar     <- solar_norm * weights[1]
weighted_altitude  <- alt_norm * weights[2]
weighted_population <- pop_norm * weights[3]
weighted_landcover <- landcover_norm * weights[4]
weighted_road      <- road_norm * weights[5]
weighted_grid      <- grid_norm * weights[6]

# 累加每一层的加权分数
weighted_score <- weighted_solar +
  weighted_altitude +
  weighted_population +
  weighted_landcover +
  weighted_road +
  weighted_grid

# 检查结果
plot(weighted_score, main = "Stepwise Comprehensive Score")


par(mfrow = c(2, 3))  # 设置绘图布局
plot(weighted_solar, main = "Solar Contribution")
plot(weighted_altitude, main = "Altitude Contribution")
plot(weighted_population, main = "Population Contribution")
plot(weighted_landcover, main = "Landcover Contribution")
plot(weighted_road, main = "Road Contribution")
plot(weighted_grid, main = "Grid Contribution")

# 检查结果
plot(weighted_score, main = "Comprehensive Weighted Score")




# 设置阈值为综合得分的前 10%
threshold <- quantile(values(weighted_score), probs = 0.90, na.rm = TRUE)

# 筛选高分区域
high_score_raster <- weighted_score > threshold

# 可视化高分区域
plot(high_score_raster, main = "High Score Areas (Top 10%)")

# 计算连通区域
connected_regions <- patches(high_score_raster, directions = 4)  # 使用 4 邻域
plot(connected_regions, main = "Connected Regions")

# 计算每个连通区域的面积
connected_regions <- patches(high_score_raster, directions = 4)  # 使用 4 邻域

region_areas <- expanse(connected_regions, transform = TRUE, unit = "m")  # 单位为平方米

summary(region_areas)  # 查看面积分布
# 获取满足面积阈值的区域索引
selected_region_indices <- which(region_areas > 100000)  # 100000 为面积阈值

# 创建筛选后的区域栅格
selected_regions <- subst(connected_regions, selected_region_indices, NA, others = TRUE)

# 检查 high_score_raster 是否只有高分值
summary(values(high_score_raster))
plot(high_score_raster, main = "High Score Raster")

high_score_raster <- classify(weighted_score, cbind(-Inf, 0.9, NA), right = FALSE)

region_areas <- expanse(connected_regions, transform = TRUE, unit = "m")
summary(region_areas)

# 可视化选出的区域
plot(selected_regions, main = "Selected High Score Regions")

# 提取高分区域的分值
high_scores <- values(weighted_score)[values(high_score_raster) == 1]


# 输出统计信息
summary(high_scores)


# 转为矢量多边形
high_score_polygons <- as.polygons(high_score_raster)

# 保存为 Shapefile
writeVector(high_score_polygons, "high_score_areas.shp", overwrite = TRUE)


# 从高分区域中随机抽样10个点作为候选
candidate_points <- spatSample(high_score_raster, size = 10, method = "random", na.rm = TRUE)

# 可视化候选点
plot(weighted_score, main = "Weighted Score with Candidates")
points(candidate_points, col = "red", pch = 19)




# 1. 检查 high_score_raster 是否正确
threshold <- 0.8  # 根据实际需求设置
high_score_raster <- classify(weighted_score, cbind(-Inf, threshold, NA), right = FALSE)
plot(high_score_raster, main = "Thresholded High Score Raster")

# 2. 确保正确的分辨率和投影
if (is.na(crs(high_score_raster))) {
  high_score_raster <- project(high_score_raster, "EPSG:32633")  # 替换为合适的投影
}
print(res(high_score_raster))
print(crs(high_score_raster))

# 3. 计算连通区域
connected_regions <- patches(high_score_raster, directions = 4)
plot(connected_regions, main = "Connected Regions")

# 4. 计算区域面积
region_areas <- expanse(connected_regions, transform = TRUE, unit = "m")
summary(region_areas)

# 5. 筛选大于面积阈值的区域
area_threshold <- 100000  # 根据需要调整
selected_region_indices <- which(region_areas > area_threshold)

# 6. 替换非选中区域为 NA
selected_regions <- classify(connected_regions, cbind(setdiff(unique(values(connected_regions)), selected_region_indices), NA))
plot(selected_regions, main = "Selected High Score Regions")







