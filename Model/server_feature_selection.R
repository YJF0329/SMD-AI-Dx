# server_feature_selection.R
feature_selection_server <- function(id, preprocessed_data)  {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 状态变量
    status <- reactiveValues(
      match_complete = FALSE,
      match_success = FALSE,
      match_rate = 0,
      matched_genes = NULL,
      filtered_data = NULL,
      final_matrix = NULL,
      mapping_details = NULL
    )
    
    # 1. 内置参考基因数据（基因符号）
    reference_genes <- reactive({
      c(
        "RAD52", "REX1BD", "ELAC2", "BID", "XYLT2", "NSUN2", "MED17", "PHKA2", 
        "JKAMP", "LY75", "ELMO2", "APPBP2", "POLD1", "DDX20", "KDM4A", "MAPK6", 
        "SEL1L", "PTGS2", "MLH1", "PAG1", "FDFT1", "KIF22", "TXLNA", "POMGNT1",
        "XRN2", "ANAPC5", "SLC8B1", "PPP2R3C", "PPIL2", "PPP6R2", "CSTF2", "COG4",
        "HNRNPL", "ISYNA1", "WDR91", "ZFAND5", "MAP3K8", "CDK5RAP3", "RECQL5",
        "DDX5", "DHRS7B", "PPP6R3", "PRPF19", "CARS1", "KCTD20", "DUSP22", "EXOC2",
        "HARS2", "APBB3", "PCCB", "NPRL2", "GNB4", "EIF2B4", "INO80B", "AUP1",
        "UNC50", "SF3B1", "KDM3A", "NFE2L2", "S100PBP", "P3H1", "COQ6", "TNFRSF8",
        "DDX39A", "NCKAP1L", "TBCC", "NT5C", "CLPP", "SUMF2", "ZSWIM6", "AKAP12",
        "AOC3", "DHX30", "PTPRE", "TSPAN2", "TPP2", "DHX9", "DDX56", "RSAD1",
        "SRSF1", "CDK9", "FPGS", "PRCP", "SLC5A6", "ZNF740", "PFKL", "TARS2",
        "PIP5K1A", "GOLPH3L", "GCNA", "ERLIN2", "SURF6", "SLC25A25", "QTRT2",
        "ZDHHC7", "KCTD18", "RAB39B", "ABHD3", "GPAT4", "GNE", "VPS11", "FDPS",
        "SRSF2", "RBM15", "ANKZF1", "DNASE1L3", "ELP6", "TEX264", "RICTOR",
        "PCF11", "PIP4P1", "METTL17", "METTL3", "MCM7", "CTNNB1", "USP39",
        "PTPN9", "GUSB", "GLB1", "KYAT1", "LRRC8D", "DPAGT1", "TMEM134",
        "SLFN11", "MUS81", "NADSYN1", "OXSR1", "ZNF449", "VCPIP1", "VSIG10",
        "FAM89B", "GATD1", "SLC25A20", "FUCA1", "CLK3", "CNOT10", "KIAA0825",
        "IFT140", "ZNF292", "ANAPC7", "MCMBP", "DDX42", "TXNRD1", "SPOUT1",
        "OXLD1", "TRGV9", "CRIP1", "BAZ2B.AS1", "SNHG15", "MCPH1.DT", "HAUS5",
        "ZNF674", "H2BC20P", "RNF157.AS1", "MAP3K14.AS1"
      )
    })
    
    # 2. 检查基因匹配
    observeEvent(input$check_gene_match, {
      req(preprocessed_data())
      
      showModal(
        modalDialog(
          title = "正在匹配基因...",
          "请稍候，正在匹配基因符号中...",
          footer = NULL,
          easyClose = FALSE
        )
      )
      
      tryCatch({
        # 获取预处理数据
        processed_data <- preprocessed_data()
        
        # 调试：查看数据格式
        cat("\n=== 调试：预处理数据格式 ===\n")
        cat("列名:", colnames(processed_data), "\n")
        
        expr_matrix <- NULL
        
        if(is.data.frame(processed_data)) {
          # 使用gene_name列作为基因ID，expression列作为表达值
          gene_names <- as.character(processed_data$gene_name)
          expr_values <- as.matrix(processed_data$expression)
          
          # 确保是矩阵格式
          if(ncol(expr_values) == 0) {
            expr_values <- matrix(expr_values, ncol = 1)
            colnames(expr_values) <- "expression"
          }
          
          rownames(expr_values) <- gene_names
          expr_matrix <- expr_values
          
          cat("使用gene_name作为基因标识符\n")
          cat("基因数量:", length(gene_names), "\n")
          cat("样本数:", ncol(expr_matrix), "\n")
          cat("前5个基因名:", head(gene_names, 5), "\n")
        } else {
          stop("预处理数据不是数据框格式")
        }
        
        if(is.null(expr_matrix)) {
          stop("无法提取表达矩阵")
        }
        
        processed_genes <- rownames(expr_matrix)
        ref_genes_symbols <- reference_genes()
        
        cat("\n=== 开始基因匹配 ===\n")
        cat("参考基因数量:", length(ref_genes_symbols), "\n")
        cat("表达矩阵基因数量:", length(processed_genes), "\n")
        
        # 直接进行Gene Symbol匹配（预处理数据行名已经是gene_name）
        matched_genes <- intersect(ref_genes_symbols, processed_genes)
        
        total_ref <- length(ref_genes_symbols)
        matched_count <- length(matched_genes)
        match_rate <- round(matched_count / total_ref * 100, 2)
        
        cat("直接匹配数量:", matched_count, "\n")
        cat("匹配率:", match_rate, "%\n")
        
        # 更新状态
        status$match_rate <- match_rate
        status$matched_genes <- matched_genes
        status$match_complete <- TRUE
        
        if(match_rate >= 95) {
          # 匹配成功，提取匹配的基因
          matched_idx <- which(processed_genes %in% matched_genes)
          
          if(length(matched_idx) > 0) {
            # 提取匹配基因的表达数据
            matched_expr_matrix <- expr_matrix[matched_idx, , drop = FALSE]
            
            # 转置矩阵：样本为行，基因为列
            transposed_matrix <- t(matched_expr_matrix)
            
            # 确保列名顺序与参考基因一致
            if(all(matched_genes %in% colnames(transposed_matrix))) {
              transposed_matrix <- transposed_matrix[, matched_genes, drop = FALSE]
            }
            
            # 保存最终矩阵格式
            status$final_matrix <- transposed_matrix
            status$match_success <- TRUE
            
            cat("转换后矩阵维度:", dim(transposed_matrix), "\n")
            
            showNotification(paste0("✅ 基因匹配成功！匹配率: ", match_rate, "%"), 
                             type = "message", duration = 10)
          }
        } else {
          status$match_success <- FALSE
          showNotification(paste0("❌ 基因匹配失败！匹配率: ", match_rate, "%，低于95%"), 
                           type = "error", duration = 10)
        }
        
        removeModal()
        
      }, error = function(e) {
        removeModal()
        showNotification(paste("❌ 基因匹配出错:", e$message), type = "error")
        cat("错误信息:", e$message, "\n")
      })
    })
    
    # 3. 显示匹配结果
    output$match_result_ui <- renderUI({
      if(!status$match_complete) {
        return(tags$p("请点击'检查基因匹配'按钮开始匹配"))
      }
      
      if(status$match_success) {
        tagList(
          tags$div(class = "alert alert-success",
                   icon("check-circle"), 
                   tags$strong("✅ 匹配成功！"),
                   br(),
                   paste0("匹配率: ", status$match_rate, "%"),
                   br(),
                   paste0("匹配基因数: ", length(status$matched_genes)),
                   br(),
                   paste0("样本数: ", nrow(status$final_matrix)),
                   br(),
                   br(),
                   downloadButton(ns("download_final_matrix"), "下载最终表达矩阵",style = "color: #333; background-color: #f8f9fa; border-color: #ccc; font-weight: bold;")
          )
        )
      } else {
        tagList(
          tags$div(class = "alert alert-danger",
                   icon("exclamation-triangle"), 
                   tags$strong("❌ 匹配失败！"),
                   br(),
                   paste0("匹配率: ", status$match_rate, "% (低于95%阈值)")
          )
        )
      }
    })
    

    # 5. 下载最终矩阵
    output$download_final_matrix <- downloadHandler(
      filename = function() {
        paste0("final_expression_matrix_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(status$final_matrix)
        final_df <- as.data.frame(status$final_matrix)
        final_df <- cbind(Sample = rownames(final_df), final_df)
        write.csv(final_df, file, row.names = FALSE)
      }
    )
    # 5.1 最终矩阵预览
    output$final_matrix_preview <- renderDT({
      req(status$final_matrix)
      
      # 显示前5行前5列
      display_data <- as.data.frame(status$final_matrix)
      display_data <- cbind(Sample = rownames(display_data), display_data)
      
      # 固定显示5行5列（基因）
      genes_to_show <- min(7, ncol(status$final_matrix))
      samples_to_show <- min(7, nrow(status$final_matrix))
      
      display_data <- display_data[1:samples_to_show, 1:(genes_to_show + 1)]
      
      datatable(
        display_data,
        options = list(
          pageLength = 5,
          scrollX = FALSE,  # 关闭水平滚动
          scrollY = FALSE,  # 关闭垂直滚动
          dom = 't',        # 只显示表格，不要其他控件
          ordering = FALSE  # 关闭排序
        ),
        rownames = FALSE,
        caption = "最终表达矩阵预览（样本为行，基因为列）"
      ) %>% 
        formatRound(columns = 2:(genes_to_show + 1), digits = 15)  # 格式化小数位数
    })
    
    # 6. 其他输出
    output$reference_genes_info <- renderPrint({
      genes <- reference_genes()
      cat("参考基因信息:\n")
      cat("基因数量:", length(genes), "\n")
      cat("基因格式: Gene Symbol\n")
      cat("前10个基因:\n")
      print(head(genes, 10))
    })
    
    output$match_plot <- renderPlotly({
      req(status$match_complete)
      
      total_ref <- length(reference_genes())
      matched <- length(status$matched_genes)
      unmatched <- total_ref - matched
      
      df <- data.frame(
        category = c("匹配", "未匹配"),
        count = c(matched, unmatched),
        color = c('#2E8B57', '#DC143C')
      )
      
      plot_ly(df, x = ~category, y = ~count, type = 'bar',
              text = ~paste(count, "个基因"),
              textposition = 'outside',
              textfont = list(size = 14, color = '#333'),
              marker = list(color = ~color,
                            line = list(color = 'rgba(0,0,0,0.2)', width = 1.5))) %>%
        layout(
          title = list(text = "<b>基因匹配情况</b>", font = list(size = 18)),
          xaxis = list(title = "", 
                       tickfont = list(size = 14),
                       gridcolor = 'rgba(200,200,200,0.3)'),
          yaxis = list(title = "基因数量",
                       tickfont = list(size = 12),
                       gridcolor = 'rgba(200,200,200,0.3)',
                       zeroline = FALSE),
          plot_bgcolor = 'rgba(248,249,250,0.8)',
          paper_bgcolor = 'rgba(248,249,250,0.8)',
          margin = list(l = 50, r = 50, t = 60, b = 50),
          showlegend = FALSE
        ) %>%
        add_annotations(
          x = 0.5,
          y = max(df$count) * 1.15,
          text = paste0("总计: ", total_ref, " 个基因"),
          showarrow = FALSE,
          font = list(size = 14, color = '#666'),
          xref = "paper",
          yref = "y"
        )
    })
    
    observeEvent(input$view_reference_genes, {
      genes <- reference_genes()
      df <- data.frame(Index = 1:length(genes), Gene_Symbol = genes)
      
      showModal(
        modalDialog(
          title = "参考基因列表（Gene Symbol）",
          size = "l",
          DTOutput(ns("reference_genes_modal")),
          easyClose = TRUE,
          footer = modalButton("关闭")
        )
      )
      
      output$reference_genes_modal <- renderDT({
        datatable(df, options = list(pageLength = 15, scrollY = "500px"))
      })
    })
    
    observeEvent(input$use_matched_genes, {
      if(!status$match_success) {
        showNotification("基因匹配未成功，无法继续", type = "error")
        return()
      }
      showNotification("已选择匹配的基因，可以进入下一步", type = "message")
    })
    
    output$match_complete <- reactive({ status$match_complete })
    outputOptions(output, "match_complete", suspendWhenHidden = FALSE)
    output$match_success <- reactive({ status$match_success })
    outputOptions(output, "match_success", suspendWhenHidden = FALSE)
    # 返回结果
    return(list(
      final_matrix = reactive({ status$final_matrix }),
      filtered_data = reactive({ status$filtered_data }),
      match_success = reactive({ status$match_success }),
      selection_complete = reactive({ status$match_success })
    ))
  })
}