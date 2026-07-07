# ===================================================
# 最终训练脚本：使用已知最优参数训练 KPLS
# ===================================================

# 1️⃣ 设置工作目录
setwd("C:\\Users\\姚竞帆\\Desktop\\小程序\\5-GA-fKPLS模型")

# 2️⃣ 加载必要包
library(GA)
library(pracma)
library(cvTools)
library(epiR)
library(pROC)

# 3️⃣ 指定已知最优核参数
z_max_Gaussian_m <- 10.9902283861836
gg <- 222  # 原代码随机种子

# 4️⃣ 读取数据（保持原代码不变）
x_mRNA_filted_xy <- read.csv('sis_result.csv')
xy_m <- x_mRNA_filted_xy[,-1]

# 5️⃣ 调用训练脚本
# 保证脚本中使用的是 z_max_Gaussian_m
# 这里不会再调用 GA，也不会计算 fitness
source('1_min_T_external_Gaussian_Modified_EXP_mixed_kernel_new_Plogistic.R')

# 6️⃣ 保存最终模型
final_model <- list(
  train_X = x_m,
  train_Y = Y,
  best_C = z_max_Gaussian_m,
  T_CV = T_CV,
  U = U,
  T_matrix = T,
  CY = CY,
  mn = mn
)

save(final_model, file='final_model.Rdata')
cat("最终模型已保存为 final_model.Rdata\n")
final_model
str(final_model)
colnames(final_model$train_X)
