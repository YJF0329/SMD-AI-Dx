# download_gene_mapping.R
# 下载完整的Ensembl基因映射表

download_local_gene_mapping <- function(output_dir = "基因注释本地数据") {
  cat("正在下载本地基因注释库...\n")
  
  # 创建目录（如果不存在）
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 连接到Ensembl
  library(biomaRt)
  
  # 尝试不同的Ensembl服务器
  ensembl <- tryCatch({
    # 尝试使用稳定的archive版本
    useMart("ensembl", 
            dataset = "hsapiens_gene_ensembl",
            host = "https://dec2021.archive.ensembl.org/")
  }, error = function(e) {
    # 如果失败，使用最新版本
    useMart("ensembl", dataset = "hsapiens_gene_ensembl")
  })
  
  cat("正在获取基因信息...\n")
  
  # 分批获取，避免超时
  attributes_list <- c(
    "ensembl_gene_id",           # Ensembl ID（无版本号）
    "ensembl_gene_id_version",   # Ensembl ID（带版本号）
    "external_gene_name",        # 基因符号
    "gene_biotype"               # 基因类型
  )
  
  # 获取所有基因的映射信息
  all_genes <- getBM(
    attributes = attributes_list,
    mart = ensembl
  )
  
  # 定义文件路径
  rds_path <- file.path(output_dir, "local_gene_mapping.rds")
  csv_path <- file.path(output_dir, "local_gene_mapping.csv")
  
  # 保存到本地
  saveRDS(all_genes, rds_path)
  write.csv(all_genes, csv_path, row.names = FALSE)
  
  # 返回统计信息
  result <- list(
    success = TRUE,
    gene_count = nrow(all_genes),
    file_size_mb = round(file.size(rds_path)/1024/1024, 2),
    file_path = rds_path,
    timestamp = Sys.time()
  )
  
  cat("✅ 本地基因注释库下载完成！\n")
  cat("   基因总数:", result$gene_count, "\n")
  cat("   文件大小:", result$file_size_mb, "MB\n")
  cat("   保存路径:", result$file_path, "\n")
  
  return(result)
}

# 注释掉直接运行的代码，让Shiny控制调用
#gene_mapping <- download_local_gene_mapping()
