# local_annotation_functions.R
# 使用本地数据库的基因注释函数

# 1. 加载本地基因数据库
load_local_gene_db <- function() {
  db_path <- "基因注释本地数据/local_gene_mapping.rds"
  
  if(!file.exists(db_path)) {
    stop("本地基因数据库不存在。路径: ", db_path)
  }
  
  # 使用全局缓存提高性能
  if(!exists(".local_gene_db_cache", envir = .GlobalEnv)) {
    cat("加载本地基因数据库...\n")
    .GlobalEnv$.local_gene_db_cache <- readRDS(db_path)
    cat("数据库加载完成，基因数:", 
        nrow(.GlobalEnv$.local_gene_db_cache), "\n")
  }
  
  return(.GlobalEnv$.local_gene_db_cache)
}

# 2. 本地基因注释主函数
local_gene_annotation <- function(raw_data) {
  cat("=== 本地基因注释开始 ===\n")
  
  # 验证输入数据
  if(is.null(raw_data) || nrow(raw_data) == 0) {
    stop("输入数据为空")
  }
  
  # 标准化列名
  if(ncol(raw_data) >= 2) {
    colnames(raw_data)[1:2] <- c("gene_id", "expression")
  } else {
    stop("数据需要至少两列")
  }
  
  # 统计输入
  input_genes <- nrow(raw_data)
  cat("输入基因数:", input_genes, "\n")
  
  # 去除Ensembl ID版本号
  raw_data$ensembl_id_clean <- gsub("\\..*", "", raw_data$gene_id)
  
  # 加载本地数据库
  gene_db <- load_local_gene_db()
  
  # 合并注释
  result <- merge(gene_db[, c("ensembl_gene_id", "external_gene_name")],
                  raw_data,
                  by.x = "ensembl_gene_id",
                  by.y = "ensembl_id_clean",
                  all.x = FALSE)
  
  # 重命名和整理列
  colnames(result) <- c("gene_id", "gene_name", "original_gene_id", "expression")
  result <- result[, c("gene_name", "gene_id", "expression")]
  
  # 统计输出
  output_genes <- nrow(result)
  cat("成功注释基因数:", output_genes, "\n")
  cat("注释成功率:", round(output_genes/input_genes*100, 1), "%\n")
  
  # 记录未注释的基因（用于调试）
  if(output_genes < input_genes) {
    annotated_ids <- result$gene_id
    original_ids <- raw_data$ensembl_id_clean
    unannotated <- setdiff(original_ids, annotated_ids)
    
    if(length(unannotated) > 0 && length(unannotated) <= 10) {
      cat("部分未注释基因:", paste(unannotated, collapse = ", "), "\n")
    }
  }
  
  cat("=== 本地基因注释完成 ===\n\n")
  return(result)
}

# 3. 验证数据库函数
validate_local_database <- function() {
  db_path <- "基因注释本地数据/local_gene_mapping.rds"
  
  if(!file.exists(db_path)) {
    return(list(
      available = FALSE,
      message = "数据库文件不存在",
      path = db_path
    ))
  }
  
  tryCatch({
    gene_db <- readRDS(db_path)
    
    # 检查必需列
    required_cols <- c("ensembl_gene_id", "external_gene_name")
    missing_cols <- setdiff(required_cols, colnames(gene_db))
    
    if(length(missing_cols) > 0) {
      return(list(
        available = FALSE,
        message = paste("数据库缺少必需列:", paste(missing_cols, collapse = ", ")),
        path = db_path,
        genes_count = nrow(gene_db)
      ))
    }
    
    return(list(
      available = TRUE,
      message = "数据库可用",
      path = db_path,
      genes_count = nrow(gene_db),
      file_size_mb = round(file.size(db_path) / 1024 / 1024, 2),
      last_modified = file.info(db_path)$mtime
    ))
    
  }, error = function(e) {
    return(list(
      available = FALSE,
      message = paste("数据库读取错误:", e$message),
      path = db_path
    ))
  })
}

# 4. 测试函数
test_local_annotation_quick <- function() {
  # 创建测试数据
  test_data <- data.frame(
    gene_id = c("ENSG00000141510.16", "ENSG00000146648.11", "ENSG00000130203"),
    expression = c(15.2, 8.7, 21.3)
  )
  
  cat("测试本地基因注释...\n")
  result <- local_gene_annotation(test_data)
  cat("测试完成，结果维度:", dim(result), "\n")
  return(result)
}

