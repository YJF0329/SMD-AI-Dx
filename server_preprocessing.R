# server_preprocessing.R
preprocessing_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 响应式值存储
    rv <- reactiveValues(
      raw_data = NULL,
      processed_data = NULL,
      step_log = c(),
      start_time = NULL,
      preprocessing_complete = FALSE,
      local_db_available = FALSE,
      local_db_info = NULL
    )
    
    # 1. 检查本地数据库可用性（应用启动时检查）
    observe({
      local_db_path <- "基因注释本地数据/local_gene_mapping.rds"
      
      if(file.exists(local_db_path)) {
        rv$local_db_available <- TRUE
        tryCatch({
          db_info <- file.info(local_db_path)
          rv$local_db_info <- list(
            size_mb = round(db_info$size / 1024 / 1024, 2),
            modified = db_info$mtime,
            genes_count = nrow(readRDS(local_db_path))
          )
        }, error = function(e) {
          rv$local_db_info <- NULL
        })
      } else {
        rv$local_db_available <- FALSE
        rv$local_db_info <- NULL
      }
    })
    
    # 2. 文件上传状态输出
    output$file_uploaded <- reactive({
      !is.null(rv$raw_data) && nrow(rv$raw_data) > 0
    })
    outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
    
    # 3. 显示本地注释状态
    output$annotation_status <- renderUI({
      if(rv$local_db_available && !is.null(rv$local_db_info)) {
        tagList(
          div(class = "alert alert-success",
              icon("check-circle"),
              strong("✅ 本地基因注释可用"),
              br(),
              paste("基因数:", rv$local_db_info$genes_count, "|",
                    "大小:", rv$local_db_info$size_mb, "MB"),
              br(),
              paste("更新:", format(rv$local_db_info$modified, "%Y-%m-%d"))
          )
        )
      } else {
        tagList(
          div(class = "alert alert-warning",
              icon("exclamation-triangle"),
              strong("⚠️ 本地基因注释不可用"),
              br(),
              "请先运行下载脚本",
              br(),
              actionLink(ns("open_download_folder"), 
                         "打开注释数据文件夹", 
                         icon = icon("folder-open"))
          )
        )
      }
    })
    
    # 4. 打开下载文件夹
    observeEvent(input$open_download_folder, {
      folder_path <- normalizePath("基因注释本地数据")
      if(dir.exists(folder_path)) {
        if(Sys.info()["sysname"] == "Windows") shell.exec(folder_path)
      } else {
        showNotification("注释数据文件夹不存在", type = "warning")
      }
    })
    
    # 5. 文件上传和解析
    observeEvent(input$raw_file, {
      req(input$raw_file)
      log_message("开始读取文件...")
      
      tryCatch({
        ext <- tools::file_ext(input$raw_file$name)
        if(ext == "csv") {
          rv$raw_data <- read.csv(input$raw_file$datapath, stringsAsFactors = FALSE)
        } else {
          rv$raw_data <- read.table(input$raw_file$datapath, header = TRUE, sep = "\t",
                                    stringsAsFactors = FALSE)
        }
        log_message(paste("成功读取文件，维度:", nrow(rv$raw_data), "行 ×", ncol(rv$raw_data), "列"))
      }, error = function(e) {
        log_message(paste("文件读取错误:", e$message))
        showNotification("文件格式错误，请检查文件格式", type = "error")
      })
    })
    
    # 6. 开始预处理按钮（只保留本地注释）
    observeEvent(input$start_preprocess, {
      req(rv$raw_data)
      
      rv$step_log <- c()
      rv$start_time <- Sys.time()
      rv$preprocessing_complete <- FALSE
      
      withProgress({
        incProgress(0.1, message = "基因注释...")
        
        tryCatch({
          # 仅使用本地注释
          if(!rv$local_db_available) {
            showNotification("请先下载本地基因注释数据库", type = "error", duration = 5)
            return()
          }
          log_message("开始本地基因注释...")
          annotated_data <- local_gene_annotation(rv$raw_data)
          rv$annotated_data <- annotated_data
          log_message("本地注释完成")
        }, error = function(e) {
          rv$annotated_data <- NULL
          log_message(paste("注释失败:", e$message))
          showNotification(paste("注释失败:", e$message), type = "error", duration = 5)
          return()
        })
        
        # 后续预处理步骤
        incProgress(0.1, message = "处理重复基因...")
        dedup_data <- remove_duplicate_genes(annotated_data)
        
        incProgress(0.1, message = "处理零表达值...")
        zero_processed <- handle_zero_expression(dedup_data)
        
        incProgress(0.1, message = "过滤低表达基因...")
        filtered_data <- filter_low_expression(zero_processed)
        
        incProgress(0.1, message = "log2转换...")
        log_transformed <- apply_log2_transform(filtered_data)
        
        incProgress(0.3, message = "0-1归一化...")
        normalized_data <- normalize_0_1(log_transformed)
        
        incProgress(0.2, message = "保存结果...")
        rv$processed_data <- normalized_data
        rv$preprocessing_complete <- TRUE
        
        log_message("预处理完成！")
      }, value = 1, message = "预处理进行中...")
    })
    
    # 7. 结果展示的值框
    output$genes_before_box <- renderValueBox({
      valueBox(
        value = ifelse(!is.null(rv$raw_data), nrow(rv$raw_data), "N/A"),
        subtitle = "原始基因数",
        icon = icon("dna"),
        color = "light-blue"
      )
    })
    
    output$genes_after_box <- renderValueBox({
      valueBox(
        value = ifelse(!is.null(rv$processed_data), 
                       nrow(rv$processed_data), "N/A"),
        subtitle = "处理后基因数",
        icon = icon("filter"),
        color = "green"
      )
    })
    
    output$samples_box <- renderValueBox({
      valueBox(
        value = "1",
        subtitle = "样本数量",
        icon = icon("user"),
        color = "blue"
      )
    })
    
    output$qc_status_box <- renderValueBox({
      valueBox(
        value = "通过",
        subtitle = "质控状态",
        icon = icon("check-circle"),
        color = "green"
      )
    })
    
    output$processing_time_box <- renderValueBox({
      if(!is.null(rv$start_time)) {
        duration <- round(as.numeric(Sys.time() - rv$start_time), 1)
        valueBox(
          value = paste(duration, "秒"),
          subtitle = "处理时间",
          icon = icon("clock"),
          color = "yellow"
        )
      }
    })
    # 在server中添加这两个输出
    
    # 8.1 步骤状态指示器
    output$step_indicator <- renderUI({
      steps <- list(
        list(icon = "upload", title = "数据上传", status = if(!is.null(rv$raw_data)) "complete" else "pending"),
        list(icon = "tag", title = "基因注释", status = if(!is.null(rv$annotated_data)) "complete" else "pending"),
        list(icon = "filter", title = "质量控制", status = if(!is.null(rv$processed_data)) "complete" else "pending"),
        list(icon = "chart-line", title = "预处理完成", status = if(rv$preprocessing_complete) "complete" else "pending")
      )
      
      tagList(
        div(class = "step-indicator",
            lapply(1:length(steps), function(i) {
              step <- steps[[i]]
              div(class = paste("step", step$status),
                  div(class = "step-icon",
                      icon(step$icon)
                  ),
                  div(class = "step-title", step$title),
                  if(i < length(steps)) div(class = "step-connector")
              )
            })
        ),
        tags$style("
      .step-indicator {
        display: flex;
        justify-content: space-between;
        margin: 20px 0;
      }
      .step {
        text-align: center;
        flex: 1;
        position: relative;
      }
      .step-icon {
        width: 40px;
        height: 40px;
        border-radius: 50%;
        background-color: #e0e0e0;
        display: flex;
        align-items: center;
        justify-content: center;
        margin: 0 auto 10px;
      }
      .step.complete .step-icon {
        background-color: #28a745;
        color: white;
      }
      .step.pending .step-icon {
        background-color: #e0e0e0;
        color: #777;
      }
      .step-connector {
        position: absolute;
        top: 20px;
        right: -50%;
        width: 100%;
        height: 2px;
        background-color: #e0e0e0;
        z-index: -1;
      }
      .step.complete .step-connector {
        background-color: #28a745;
      }
      .step-title {
        font-size: 12px;
        color: #777;
      }
      .step.complete .step-title {
        color: #28a745;
        font-weight: bold;
      }
    ")
      )
    })
    
    # 8.2 详细日志
    output$process_log <- renderPrint({
      if(length(rv$step_log) > 0) {
        cat(paste(rv$step_log, collapse = "\n"))
      } else {
        cat("等待预处理开始...\n点击'开始预处理'按钮启动流程")
      }
    })
    # 8. 数据分布图
    output$distribution_plot <- renderPlot({
      req(rv$processed_data)
      
      par(mfrow = c(1, 2))
      
      # 原始数据分布
      if(!is.null(rv$raw_data)) {
        hist(log10(rv$raw_data[,2] + 1), 
             main = "原始表达值分布",
             xlab = "log10(表达值+1)",
             col = "lightblue")
      }
      
      # 处理后数据分布
      hist(rv$processed_data[,2],
           main = "处理后表达值分布",
           xlab = "归一化表达值",
           col = "lightgreen")
    })
    
    # 9. 下载处理结果
    output$download_processed <- downloadHandler(
      filename = function() {
        paste0("processed_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(rv$processed_data, file, row.names = FALSE)
      }
    )
    
    # 10. 预处理完成状态
    output$preprocess_complete <- reactive({
      rv$preprocessing_complete
    })
    outputOptions(output, "preprocess_complete", suspendWhenHidden = FALSE)
    
    # 11. 统计结果显示
    output$data_stats <- renderPrint({
      req(rv$processed_data)
      cat("=== 预处理结果统计 ===\n")
      cat("基因数量:", nrow(rv$processed_data), "\n")
      cat("表达值范围: [", 
          round(min(rv$processed_data$expression, na.rm = TRUE), 4), 
          ", ",
          round(max(rv$processed_data$expression, na.rm = TRUE), 4), 
          "]\n")
    })
    
    # 12. 辅助函数 - 记录日志
    log_message <- function(msg) {
      timestamp <- format(Sys.time(), "%H:%M:%S")
      rv$step_log <- c(rv$step_log, paste("[", timestamp, "] ", msg))
    }
    
    # 13. 输出预处理完成状态
    list(
      processed_data = reactive(rv$processed_data),
      preprocessing_complete = reactive(rv$preprocessing_complete),
      # 添加一个标记，表示预处理完成但需要手动确认
      ready_for_next = reactive({ 
        !is.null(rv$processed_data) && rv$preprocessing_complete 
      })
    )
  })
}