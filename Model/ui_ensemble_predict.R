# ui_ensemble_predict.R
ui_ensemble_predict <- function(id) {
  ns <- NS(id)
  tagList(
    # 第一行：控制台
    fluidRow(
      box(title = "集成诊断控制台", width = 12, status = "primary", solidHeader = TRUE,
          actionButton(ns("run_ensemble"), "启动全算法集成推理", 
                       class = "btn-success", icon = icon("play-circle")),
          span(style = "margin-left: 15px; color: #666;", "注：阈值为 0.5 (高于 0.5 为高风险)")
      )
    ),
    
    # 第二行：结果汇总表格（独立占一行）
    fluidRow(
      box(title = "预测结果汇总 (点击行进行下方图表联动)", width = 12, status = "info",
          withSpinner(DTOutput(ns("ensemble_table")))
      )
    ),
    
    # 第三行：可视化图表与风险概览
    fluidRow(
      # 左侧：模型对比图
      column(8,
             box(title = "单样本多算法得分对比", width = 12, status = "warning",
                 plotlyOutput(ns("ensemble_plot"), height = "400px")
             )
      ),
      # 右侧：风险统计卡片
      column(4,
             box(title = "当前样本风险概览", width = 12, status = "danger",
                 htmlOutput(ns("risk_summary")),
                 hr(),
                 p(style = "font-size: 12px; color: #777;", 
                   "高风险：该算法判定分值 > 0.5", br(),
                   "低风险：该算法判定分值 <= 0.5")
             )
      )
    )
  )
}