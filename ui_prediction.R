library(shiny)
library(DT)

ui_prediction <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("基于 final_model 的新样本预测"),
    fluidRow(
      column(
        width = 12,
        box(
          title = "预测控制",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          actionButton(ns("run_prediction"), 
                       "开始预测", 
                       class = "btn-primary btn-lg",
                       icon = icon("play-circle"),
                       width = "100%"),
          br(), br(),
          h5("说明："),
          p("1. 系统自动使用上一步选择的特征进行预测"),
          p("2. 如果特征与模型不匹配，系统会自动用平均值填充缺失特征"),
          p("3. 预测结果将在下方表格中显示")
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        box(
          title = "预测结果",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          DTOutput(ns("prediction_table")) %>% 
            withSpinner(type = 4, color = "#0dc5c1"),
          br(),
          tags$div(
            class = "alert alert-info",
            icon("info-circle"),
            " 预测分数大于0为Positive，小于等于0为Negative"
          )
        )
      )
    )
  )
}