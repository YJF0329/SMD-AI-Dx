# ui_feature_selection.R
feature_selection_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "预测基因选择", status = "primary", solidHeader = TRUE, width = 12,
        collapsible = TRUE,
        
        # 基因匹配部分
        fluidRow(
          column(6,
                 h4("基因匹配检查"),
                 actionButton(ns("check_gene_match"), "检查基因匹配",
                              icon = icon("search"), 
                              class = "btn-info btn-lg",
                              width = "100%"),
                 br(), br(),
                 
                 # 匹配结果显示
                 uiOutput(ns("match_result_ui"))
          ),
          
          column(6,
                 h4("参考基因信息"),
                 actionButton(ns("view_reference_genes"), "查看参考基因列表",
                              icon = icon("list"),
                              class = "btn-default",
                              width = "100%"),
                 br(), br(),
                 
                 verbatimTextOutput(ns("reference_genes_info"))
          )
        ),
        
        # 匹配详情（成功后显示）
        conditionalPanel(
          condition = paste0("output['", ns("match_complete"), "']"),
          hr(),
          
          tabsetPanel(
            tabPanel("匹配统计",
                     plotlyOutput(ns("match_plot"), height = "300px")
            ),
            tabPanel("匹配的基因",
                     DTOutput(ns("final_matrix_preview"))
            )
          ),
          
          br(),
          # 完成按钮
          actionButton(ns("use_matched_genes"), "使用匹配基因进入下一步",
                       icon = icon("arrow-right"),
                       class = "btn-success btn-lg",
                       width = "100%")
        )
      )
    )
  )
}