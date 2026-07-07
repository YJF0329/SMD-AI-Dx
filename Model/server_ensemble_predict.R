# server_ensemble_predict.R
library(shiny)
library(DT)
library(plotly)

server_ensemble_predict <- function(id, feature_results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 存储集成预测结果
    ensemble_data <- reactiveVal(NULL)
    
    # 1. 执行集成预测逻辑
    observeEvent(input$run_ensemble, {
      # 确保上游特征矩阵已生成
      req(feature_results$final_matrix())
      
      tryCatch({
        # 创建独立环境加载模型，防止变量污染
        env <- new.env()
        # 请确保路径正确
        model_path <- "all_models.RData"
        
        if(!file.exists(model_path)) {
          stop("未找到模型文件: all_models.RData，请检查路径。")
        }
        
        load(model_path, envir = env)
        
        # 2. 数据准备与特征强制对齐
        # 提取原始上传/处理后的矩阵
        new_raw <- as.data.frame(feature_results$final_matrix())
        # 从逻辑回归中提取训练时需要的标准特征顺序
        req_feats <- names(env$model_logit$coefficients)[-1]
        
        # 自动补全缺失列（设为0）并重排顺序
        missing_cols <- setdiff(req_feats, colnames(new_raw))
        if(length(missing_cols) > 0) {
          for(col in missing_cols) new_raw[[col]] <- 0
        }
        
        # 最终输入矩阵（严格匹配模型要求）
        final_input <- new_raw[, req_feats, drop = FALSE]
        
        # 处理可能的行内 NA
        for(i in 1:ncol(final_input)) {
          if(any(is.na(final_input[, i]))) {
            m <- mean(final_input[, i], na.rm = TRUE)
            final_input[is.na(final_input[, i]), i] <- ifelse(is.nan(m), 0, m)
          }
        }
        
        # 矩阵格式供 glmnet (Lasso/Ridge) 使用
        x_mat <- as.matrix(final_input)
        
        # 3. 批量推理
        res <- data.frame(
          SampleID = rownames(new_raw),
          Logistic = as.numeric(predict(env$model_logit, final_input, type = "response")),
          SVM      = as.numeric(attr(predict(env$model_svm, final_input, probability = TRUE), "probabilities")[, "1"]),
          RF       = as.numeric(predict(env$model_rf, final_input, type = "prob")[, "1"]),
          Lasso    = as.numeric(predict(env$model_lasso, x_mat, s = "lambda.min", type = "response")),
          Ridge    = as.numeric(predict(env$model_ridge, x_mat, s = "lambda.min", type = "response")),
          NNet     = as.numeric(predict(env$model_nnet, final_input)),
          stringsAsFactors = FALSE
        )
        
        # 4. 汇总统计
        res$Mean_Prob <- rowMeans(res[, 2:7])
        res$Decision <- ifelse(res$Mean_Prob > 0.5, "Positive (1)", "Negative (0)")
        
        # 更新状态
        ensemble_data(res)
        showNotification("✅ 集成预测与对齐任务已完成", type = "message")
        
      }, error = function(e) {
        showNotification(paste("❌ 预测失败:", e$message), type = "error")
        cat("Ensemble Error:", e$message, "\n")
      })
    })
    
    # 5. 渲染结果表格
    output$ensemble_table <- renderDT({
      req(ensemble_data())
      datatable(
        ensemble_data(),
        selection = 'single', # 开启单行选择用于图表联动
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        class = 'cell-border stripe hover'
      ) %>%
        formatRound(columns = 2:8, digits = 4)
    })
    output$risk_summary <- renderUI({
      req(ensemble_data())
      
      # 获取选中的样本行
      s <- input$ensemble_table_rows_selected
      if (length(s) == 0) s <- 1
      row_data <- ensemble_data()[s, ]
      
      # 统计 6 个算法中，有多少个判定为高风险
      model_names <- c("Logistic", "SVM", "RF", "Lasso", "Ridge", "NNet")
      scores <- as.numeric(row_data[, model_names])
      high_risk_count <- sum(scores > 0.5)
      low_risk_count <- 6 - high_risk_count
      
      # 动态生成 HTML 内容
      tagList(
        h4(paste("样本编号:", row_data$SampleID)),
        div(style = "padding: 10px; border-radius: 5px; background-color: #f8f9fa;",
            p(tags$b("综合判定结果: "), 
              span(style = ifelse(row_data$Mean_Prob > 0.5, "color:red;", "color:green;"),
                   row_data$Decision)),
            p(tags$b("算法表决情况: ")),
            tags$ul(
              tags$li(paste("高风险算法数:", high_risk_count)),
              tags$li(paste("低风险算法数:", low_risk_count))
            )
        )
      )
    })
    
    # 6. 渲染可视化图表 (Plotly)
    output$ensemble_plot <- renderPlotly({
      req(ensemble_data())
      s <- input$ensemble_table_rows_selected
      if (length(s) == 0) s <- 1
      row_data <- ensemble_data()[s, ]
      
      model_names <- c("Logistic", "SVM", "RF", "Lasso", "Ridge", "NNet")
      probs <- as.numeric(row_data[, model_names])
      
      # 颜色定义：高风险红，低风险绿
      colors <- ifelse(probs > 0.5, 'rgba(231, 76, 60, 0.8)', 'rgba(46, 204, 113, 0.8)')
      
      plot_ly(x = model_names, y = probs, type = 'bar',
              marker = list(color = colors, line = list(color = '#2c3e50', width = 1))) %>%
        layout(yaxis = list(title = "评分", range = c(0, 1)),
               xaxis = list(title = "模型算法"),
               shapes = list(list(type = "line", x0 = -0.5, x1 = 5.5, y0 = 0.5, y1 = 0.5,
                                  line = list(color = "black", dash = "dash"))))
    })
    
    # 返回模块输出
    return(list(
      ensemble_complete = reactive({ !is.null(ensemble_data()) }),
      ensemble_results = ensemble_data
    ))
  })
}