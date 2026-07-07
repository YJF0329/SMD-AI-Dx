# app.R
# 1. Shiny 核心 UI & 增强
library(shiny)
library(shinydashboard)
library(shinycssloaders) # 解决之前的 withSpinner 报错
library(DT)
library(plotly)

# 2. 预测模型算法 (必须全部显式加载，否则 predict 会报错)
library(glmnet)       # Lasso / Ridge
library(randomForest) # 随机森林
library(e1071)        # SVM
library(nnet)         # 神经网络
library(MASS)         # 矩阵运算 ginv

# 3. 结果评估与统计 (扫描出来的 ROCR 和 gmodels 补在这里)
library(pROC)
library(ROCR)         # 扫描结果：评估模型性能
library(epiR)         # 灵敏度/特异度计算
library(gmodels)      # 扫描结果：统计交叉表

# 4. 数据处理与工具
library(dplyr)
library(foreign)      # 扫描结果：读取外部数据格式
library(zip)
# 加载模块
source("ui_preprocessing.R")
source("server_preprocessing.R")
source("preprocessing_functions.R")
source("local_annotation_functions.R")
source("ui_feature_selection.R")
source("server_feature_selection.R")
source("ui_prediction.R")           # 重命名后的 UI
source("server_predict_model.R")    # 重命名后的 Server
source("ui_analysis.R")           # 新增：结果分析UI
source("server_analysis.R")  
source("ui_ensemble_predict.R")      # 新增
source("server_ensemble_predict.R")  # 新增

# 在应用启动时验证本地数据库
validate_on_startup <- function() {
  cat("=== 启动检查 ===\n")
  db_status <- validate_local_database()
  
  if(db_status$available) {
    cat("✅ 本地基因数据库可用\n")
    cat("   基因数:", db_status$genes_count, "\n")
    cat("   文件大小:", db_status$file_size_mb, "MB\n")
    cat("   最后更新:", format(db_status$last_modified, "%Y-%m-%d %H:%M:%S"), "\n")
  } else {
    cat("❌ 本地基因数据库问题:\n")
    cat("   错误:", db_status$message, "\n")
    cat("   路径:", db_status$path, "\n")
    cat("提示：请运行 '基因注释本地数据/download_gene_mapping.R' 下载数据库\n")
  }
  cat("==============\n\n")
}

# 执行启动检查
validate_on_startup()

# 主UI
ui <- dashboardPage(
  dashboardHeader(
    title = "重性精神障碍人工智能辅助诊断系统",
    titleWidth = 350  # 增加顶部标题栏宽度
  ),
  
  dashboardSidebar(
    width = 350,       # 必须同步增加侧边栏宽度，否则内容会错位
    sidebarMenu(
      id = "tabs",
      menuItem("数据预处理", tabName = "preprocessing", icon = icon("database")),
      menuItem("特征选择", tabName = "feature_selection", icon = icon("filter")),
      menuItem("模型预测", tabName = "prediction", icon = icon("chart-line")),
      menuItem("集成诊断", tabName = "ensemble", icon = icon("gears")),
      menuItem("结果分析", tabName = "analysis", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # 预处理模块
      tabItem(tabName = "preprocessing",
              preprocessing_ui("preprocess")),
      
      # 特征选择模块
      tabItem(tabName = "feature_selection",
              feature_selection_ui("feature")),
      
      # 模型预测模块
      tabItem(tabName = "prediction",
              ui_prediction("predict")),  
      # 多种模型模块
      tabItem(tabName = "ensemble", 
              ui_ensemble_predict("ensemble_mod")),
      # 结果分析模块
      tabItem(tabName = "analysis",
              ui_analysis("analysis"))
    )
  )
)

# 主Server
server <- function(input, output, session) {
  # 初始化预处理模块
  preprocess_results <- preprocessing_server("preprocess")
  
  # 初始化特征选择模块
  feature_results <- feature_selection_server("feature", preprocess_results$processed_data)
  
  # 初始化模型预测模块
  prediction_results <- server_predict_model("predict", feature_results)  # 使用重命名后的 server
  # 初始化结果分析模块
  ensemble_results <- server_ensemble_predict("ensemble_mod", feature_results)
  
  analysis_results <- server_analysis("analysis", 
                                      prediction_results$prediction_results,
                                      feature_results)
  
  # 观察预处理完成状态
  observeEvent(preprocess_results$preprocessing_complete(), {
    if(preprocess_results$preprocessing_complete()) {
      showNotification("预处理完成！可以进入特征选择", 
                       type = "message")
      
      # 启用下一个标签页
      updateTabItems(session, "tabs", "feature_selection")
    }
  })
  
  # 观察特征选择完成状态
  observeEvent(feature_results$selection_complete(), {
    if(is.null(feature_results$selection_complete)) return()
    
    if(feature_results$selection_complete()) {
      showNotification("特征选择完成！可以进入模型预测", 
                       type = "message")
      
      # 启用下一个标签页
      updateTabItems(session, "tabs", "prediction")
    }
  })
  
  # 观察预测完成状态
  observeEvent(prediction_results$prediction_complete(), {
    if(is.null(prediction_results$prediction_complete)) return()
    
    if(prediction_results$prediction_complete()) {
      showNotification("模型预测完成！可以进入结果分析", 
                       type = "message")
      
      # 启用下一个标签页
      updateTabItems(session, "tabs", "analysis")
    }
  })
  # 观察分析完成状态（可选，用于后续扩展）
  observeEvent(prediction_results$prediction_complete(), {
    if(prediction_results$prediction_complete()) {
      showNotification("模型预测完成！可以进入结果分析", 
                       type = "message")
      updateTabItems(session, "tabs", "analysis")
    }
  })
}

shinyApp(ui, server)