# ui_preprocessing.R
preprocessing_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # 文件上传区域
    box(
      title = "数据上传", 
      status = "primary", 
      solidHeader = TRUE,
      width = 12,
      
      fluidRow(
        column(6,
               fileInput(ns("raw_file"), "上传原始基因表达文件",
                         accept = c(".csv", ".txt", ".tsv"),
                         buttonLabel = "浏览...",
                         placeholder = "选择文件")
        ),
        column(6,
               # 本地注释状态显示
               uiOutput(ns("annotation_status")),
               
               helpText("文件要求："),
               tags$ul(
                 tags$li("CSV或制表符分隔的文本文件"),
                 tags$li("第一列：基因ID（Ensembl ID）"),
                 tags$li("第二列：表达值（原始计数）"),
                 tags$li("示例：", tags$code("ENSG00000121410, 15.2"))
               ),
               downloadButton(ns("download_template"), "下载模板")
        )
      ),
      
      # 预览原始数据
      conditionalPanel(
        condition = paste0("output['", ns("file_uploaded"), "']"),
        br(),
        actionButton(ns("start_preprocess"), "开始预处理", 
                     icon = icon("play"), class = "btn-success")
      )
    ),
    
    # 预处理步骤显示
    box(
      title = "预处理进度", 
      status = "info", 
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      
      # 步骤状态指示器
      uiOutput(ns("step_indicator")),
      
      # 详细日志
      verbatimTextOutput(ns("process_log"))
    ),
    
    # 预处理结果展示
    box(
      title = "预处理结果", 
      status = "success", 
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE, collapsed = TRUE,
      
      conditionalPanel(
        condition = paste0("output['", ns("preprocess_complete"), "']"),
        
        fluidRow(
          column(4,
                 valueBoxOutput(ns("genes_before_box")),
                 valueBoxOutput(ns("genes_after_box"))
          ),
          column(4,
                 valueBoxOutput(ns("samples_box")),
                 valueBoxOutput(ns("qc_status_box"))
          ),
          column(4,
                 valueBoxOutput(ns("processing_time_box")) 
          )

        ),
        
        # 数据统计
        verbatimTextOutput(ns("data_stats")),
        br(),
        
        # 数据分布图
        plotOutput(ns("distribution_plot")),
        
        # 下载处理结果
        downloadButton(ns("download_processed"), "下载预处理数据")
      )
    )
  )
}
