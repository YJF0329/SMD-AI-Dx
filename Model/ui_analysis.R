ui_analysis <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(4,
             # 精简后的控制栏
             box(title = "报告操作", status = "primary", solidHeader = TRUE, width = 12,
                 p("风险分层标准："),
                 tags$ul(
                   tags$li("低风险: < 30%"),
                   tags$li("中风险: 30% - 70%"),
                   tags$li("高风险: > 70%")
                 ),
                 hr(),
                 downloadButton(ns("download_results"), "导出 PDF 诊断报告", 
                                class = "btn-success", style = "width:100%")
             )
      ),
      column(8,
             valueBoxOutput(ns("total_samples"), width = 6),
             valueBoxOutput(ns("avg_positive"), width = 6)
      )
    ),
    
    # 动态结论展示区（仅单样本时显示最上方）
    fluidRow(
      column(11, uiOutput(ns("conclusion_text")))
    ),
    
    fluidRow(
      tabBox(
        id = ns("result_tabs"), width = 11,
        tabPanel("风险可视化评估", icon = icon("chart-line"),
                 uiOutput(ns("dynamic_viz_ui")) 
        ),
        tabPanel("全样本详细数据", icon = icon("table"),
                 DTOutput(ns("risk_table"))
        )
      )
    )
  )
}