##_Solar power and financial analysis

#1.Solar radiation to power======
# convert joule to kmh (1/3600000)

radiation_to_power <- function(radiation, area, yield_r=0.175, pr=0.6, hours=1)
{ kWh <- radiation * area * yield_r * pr * hours * (1/3600000)
   return(kWh) } 

#suppose we want to calculate how much power a 10m2 tiles could generate in one hour.
#and we know solar radiance 15000000

rad=9000000 #j m2
area=10 
power_kWh <- radiation_to_power(rad, area) 


#EU defines poor regions as the the areas with  yearly solar radiance yields lower than 900 kwh
#Can you calculate if ssrd with value above is a poor area?


#2 Net Present Value (NPV)======
rep(10,4) #create a vector by repeating 10 4 times
# output of the function above is: 10 10 10 10
seq( 1, 11, 2) #create a sequence of data start from 1 and end at 11. 2 is the increment of the sequence.
#outout will be: 1 3 5 7 9 11
#in this projectt, we do not consider opex but only capex
calc_NPV <- function(annual_revenue, i=0.05, lifetime_yrs, CAPEX, OPEX=0){
  revenue <- rep(annual_revenue, lifetime_yrs) 
  t <- seq(1, lifetime_yrs, 1) #output: 1, 2, 3, ...25
  
  NPV <- sum( (revenue )/(1 + i)**t ) - CAPEX
  return(round(NPV, 0))
}
#Exercise: if annual revenue is 14000000, and Capital expenditure is 150000000, then please calculate Net present value. Should we invest this project?
#to use the predefined function, you need to specify annual revenue and capex
npv=calc_NPV(annual_revenue = 14000000,lifetime_yrs=25, CAPEX=150000000  )
ifelse(npv>0, "Support","obeject" )

#3 Levelized cost of electricity (LCOE)=====
#Life_span_generation_kWH is one of the required inputs to estimate the Levelized
#cost of electricity (following function)
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

#NPV of cost. if you don't consider the operational cost, you can just use CAPEX as proxy for NPV of cost
LCOE <- function(NPV_cost,Life_span_generation){
  lcoe <- NPV_cost/Life_span_generation
  return(round(lcoe,2))
}
















###########################
# 1) 函数定义
###########################
capex_solar <- function(
    install_capacity = 174302.4,   # MW
    capacity_cost = 1.16,         # M USD / MW
    grid_distance,
    grid_cost = 590               # USD / (MW·km)
) {
  # 1) 容量造价 (M USD)
  capacity_expense_musd <- capacity_cost * install_capacity
  
  # 2) 电网连接造价：先求 (USD)，再除以1e6 => (M USD)
  #   grid_cost(USD/(MW·km)) × install_capacity(MW) × grid_distance(km)
  #   => 结果(USD)，再 /1e6 => M USD
  network_expense_usd   <- grid_cost * install_capacity * grid_distance
  network_expense_musd  <- network_expense_usd / 1e6
  
  # 3) 总CapEx (M USD)
  total_capex_musd      <- capacity_expense_musd + network_expense_musd
  return(total_capex_musd)
}

calc_NPV <- function(
    annual_revenue = 34556.56,  # 每年收入, 单位: M USD
    i = 0.05,                   # 贴现率
    lifetime_yrs = 25,
    CAPEX,                      # 初始资本开支, M USD
    OPEX = 30                   # 每年运维成本, M USD (示例)
){
  # 这里 OPEX=30 => 代表每年 30 M USD 的运维
  costs_op <- rep(OPEX, lifetime_yrs)     # 各年运维
  revenue  <- rep(annual_revenue, lifetime_yrs)  
  t        <- seq(1, lifetime_yrs, 1)    # 1,2,3,...25
  
  # 注意 annual_revenue, OPEX, CAPEX 都是 "M USD" 
  # => sum(...) 的结果也是 "M USD"
  NPV <- sum((revenue - costs_op) / (1 + i)^t) - CAPEX
  return(round(NPV, 2))  # 保留2位小数
}

Life_span_generation_kWH <- function(
    yearly_generation_kWH = 3.756e11,
    discount = 0.08,
    lifetime_yrs = 25
){
  # 假设每年都相同的发电量 => rep(...)
  kwh_each_year <- rep(yearly_generation_kWH, lifetime_yrs)
  t <- seq(1, lifetime_yrs, 1)
  
  # 贴现后总发电量
  L_S_G <- sum(kwh_each_year / (1 + discount)^t)
  return(round(L_S_G, 0))  # 整数化
}

LCOE <- function(
    NPV_musd,               # 净现值, 单位 M USD
    Life_span_generation    # 贴现后总发电量, kWh
){
  # 如果你要 "USD/kWh",
  # 先把 NPV_musd (百万美元) => USD (× 1e6),
  # 再除以贴现后发电量(kWh)。
  
  npv_usd <- NPV_musd * 1e6       # 转成美元
  lcoe_usd_per_kwh <- npv_usd / Life_span_generation
  
  # 多保留几位小数, 避免小数过小变成 0.00
  return(round(lcoe_usd_per_kwh, 5))
}

###########################
# 2) 示例输入数据
###########################
install_capacity        <- 174302.4        # MW
grid_distance           <- 962.2252        # km

annual_revenue_musd     <- 34556.56        # 每年收入, M USD
yearly_generation_kWH   <- 3.756148e11     # kWh/年 (示例)

###########################
# 3) 计算 CAPEX
###########################
capex_musd <- capex_solar(
  install_capacity = install_capacity,
  grid_distance = grid_distance,
  capacity_cost = 1.16,       # M USD / MW
  grid_cost = 590             # USD/(MW·km)
)

###########################
# 4) 计算 NPV (M USD)
###########################
npv_musd <- calc_NPV(
  annual_revenue = annual_revenue_musd,
  i = 0.05,
  lifetime_yrs = 25,
  CAPEX = capex_musd,   # M USD
  OPEX = 30             # M USD/年 (示例)
)

###########################
# 5) 计算贴现后总发电量 (kWh)
###########################
life_span_gen <- Life_span_generation_kWH(
  yearly_generation_kWH = yearly_generation_kWH,
  discount = 0.08,
  lifetime_yrs = 25
)

###########################
# 6) 计算 LCOE (USD/kWh)
###########################
lcoe_value <- LCOE(
  NPV_musd = npv_musd, 
  Life_span_generation = life_span_gen
)

###########################
# 7) 输出结果
###########################
cat("---- Results ----\n")
cat("CAPEX       =", round(capex_musd, 2), "M USD\n")
cat("NPV         =", round(npv_musd,   2), "M USD\n")
cat("Life Span Generation =", format(life_span_gen, scientific=FALSE), "kWh\n")
cat("LCOE        =", lcoe_value,   "USD/kWh\n")


