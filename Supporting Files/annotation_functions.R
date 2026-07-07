# annotation_functions.R
# 只保留本地注释函数

# 1. 本地注释函数
local_gene_annotation <- function(raw_data) {
  cat("使用本地基因注释库...\n")
  
  # 检查本地数据库是否存在
  if(!file.exists("local_gene_mapping.rds")) {
    stop("本地基因数据库不存在，请先下载")
  }
  
  # 加载本地映射
  gene_mapping <- readRDS("local_gene_mapping.rds")
  cat("本地数据库基因数:", nrow(gene_mapping), "\n")
  
  # 确保数据格式正确
  if(!all(c("gene_id", "expression") %in% colnames(raw_data))) {
    if(ncol(raw_data) >= 2) {
      colnames(raw_data)[1:2] <- c("gene_id", "expression")
    } else {
      stop("输入数据必须包含基因ID和表达值两列")
    }
  }
  
  # 去除版本号
  raw_data$gene_id_clean <- gsub("\\..*", "", raw_data$gene_id)
  
  # 合并注释
  merged <- merge(gene_mapping[, c("ensembl_gene_id", "external_gene_name")], 
                  raw_data, 
                  by.x = "ensembl_gene_id", 
                  by.y = "gene_id_clean",
                  all.x = FALSE)
  
  # 重新命名和排列列
  colnames(merged)[1:2] <- c("gene_id", "gene_name")
  merged <- merged[, c("gene_name", "gene_id", "expression")]
  
  cat("本地注释成功！匹配", nrow(merged), "个基因\n")
  return(merged)
}

# 2. 统一接口函数（仅保留本地注释）
gene_annotation <- function(raw_data) {
  return(local_gene_annotation(raw_data))
}
