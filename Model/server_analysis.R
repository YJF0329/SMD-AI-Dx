library(shiny)
library(plotly)
library(DT)
library(dplyr)

server_analysis <- function(id, prediction_results, feature_results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. 固定的风险阈值（百分制）
    LOW_THR <- 30
    HIGH_THR <- 70
    
    # 2. 核心数据转换逻辑
    current_data <- reactive({
      req(prediction_results(), prediction_results()$predicted_scores)
      pred_data <- prediction_results()
      
      # 原始 Score (线性得分)
      raw_val <- as.numeric(pred_data$predicted_scores)
      
      # --- Sigmoid 转换：将任意实数映射到 0-100 ---
      # 公式: 1 / (1 + exp(-x)) * 100
      prob_val <- (1 / (1 + exp(-raw_val))) * 100
      
      df <- data.frame(
        SampleID = if(!is.null(rownames(feature_results$final_matrix()))) 
          rownames(feature_results$final_matrix()) else "Sample_1",
        RawScore = raw_val,
        ProbScore = prob_val, 
        PredictedLabel = ifelse(prob_val > 50, "Positive", "Negative"),
        stringsAsFactors = FALSE
      )
      
      # 风险分层
      df$RiskLevel <- cut(df$ProbScore,
                          breaks = c(-Inf, LOW_THR, HIGH_THR, Inf),
                          labels = c("低风险", "中风险", "高风险"),
                          include.lowest = TRUE)
      df
    })
    
    # 3. 动态 UI 渲染（修正了仪表盘不显示的问题）
    output$dynamic_viz_ui <- renderUI({
      df <- current_data()
      if (nrow(df) == 1) {
        # 单样本：增加结论文本 + 仪表盘 + 特征图
        tagList(
          column(12, uiOutput(ns("conclusion_box"))), # 显眼的结论
          fluidRow(
            column(6, plotlyOutput(ns("risk_gauge"), height = "400px")),
            column(6, plotlyOutput(ns("feature_bar"), height = "400px"))
          )
        )
      } else {
        # 多样本：饼图 + 累计曲线
        fluidRow(
          column(6, plotlyOutput(ns("risk_pie"), height = "400px")),
          column(6, plotlyOutput(ns("cumulative_curve"), height = "400px"))
        )
      }
    })
    
    # --- 显眼的结论展示 ---
    output$conclusion_box <- renderUI({
      df <- current_data()
      p_pos <- round(df$ProbScore[1], 1)
      p_neg <- round(100 - p_pos, 1)
      
      # 颜色逻辑
      theme_color <- if(p_pos > 50) "#e74c3c" else "#27ae60"
      
      div(style = paste0("padding:20px; border-radius:10px; background:#fdfefe; border: 2px solid ", theme_color, "; margin-bottom:20px; text-align:center;"),
          h3("临床诊断核心结论", style = "margin-top:0; color:#2c3e50; font-weight:bold;"),
          fluidRow(
            column(6, h4("阳性概率 (患病风险)"), h2(paste0(p_pos, "%"), style = paste0("color:", theme_color, "; font-weight:bold;"))),
            column(6, h4("非阳性概率 (阴性健康)"), h2(paste0(p_neg, "%"), style = "color:#2980b9; font-weight:bold;"))
          ),
          h4(style = "margin-top:15px;", "综合判定结果为：", tags$b(df$RiskLevel[1]))
      )
    })
    
    # --- 仪表盘 (修正了 Range 范围) ---
    output$risk_gauge <- renderPlotly({
      df <- current_data()
      val <- df$ProbScore[1]
      
      plot_ly(
        type = "indicator", mode = "gauge+number", value = val,
        title = list(text = "患病概率评估 (%)", font = list(size = 18)),
        gauge = list(
          axis = list(range = list(0, 100), tickwidth = 1),
          bar = list(color = "#34495e"),
          steps = list(
            list(range = c(0, LOW_THR), color = "#2ecc71"),
            list(range = c(LOW_THR, HIGH_THR), color = "#f39c12"),
            list(range = c(HIGH_THR, 100), color = "#e74c3c")
          ),
          threshold = list(
            line = list(color = "black", width = 4),
            thickness = 0.75,
            value = val
          )
        )
      )
    })
    
    # --- 特征条形图 ---
    output$feature_bar <- renderPlotly({
      mat <- feature_results$final_matrix()
      sample_vals <- sort(mat[1, ], decreasing = TRUE)[1:10]
      plot_ly(x = sample_vals, y = names(sample_vals), type = 'bar', orientation = 'h',
              marker = list(color = '#3498db')) %>%
        layout(title = "Top 10 关键特征表达", yaxis = list(autorange = "reversed"))
    })
    
    # --- 多样本图表保持逻辑 ---
    output$risk_pie <- renderPlotly({
      df <- current_data()
      counts <- as.data.frame(table(df$RiskLevel))
      plot_ly(counts, labels = ~Var1, values = ~Freq, type = 'pie',
              marker = list(colors = c("#2ecc71", "#f39c12", "#e74c3c"))) %>%
        layout(title = "群体风险分布")
    })
    
    output$cumulative_curve <- renderPlotly({
      df <- current_data() %>% arrange(ProbScore) %>% mutate(Rate = (1:n())/n())
      plot_ly(df, x = ~ProbScore, y = ~Rate, type = 'scatter', mode = 'lines+markers') %>%
        layout(title = "风险累积分布曲线", xaxis = list(title = "患病概率 (%)"))
    })
    
    output$total_samples <- renderValueBox({ valueBox(nrow(current_data()), "样本总数", icon = icon("users")) })
    output$avg_positive <- renderValueBox({ 
      rate <- round(mean(current_data()$ProbScore > 50) * 100, 1)
      valueBox(paste0(rate, "%"), "群体预测阳性率", color = "red", icon = icon("chart-pie")) 
    })
    output$risk_table <- renderDT({ 
      datatable(current_data(), options = list(scrollX = TRUE)) %>% 
        formatRound(columns = c("RawScore", "ProbScore"), digits = 2)
    })
  })
}