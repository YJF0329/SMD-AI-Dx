# preprocessing_functions.R
# 完整修复的预处理函数

# 1. 基因注释函数（修复版）
gene_annotation <- function(raw_data) {
  # 检查输入数据
  if(is.null(raw_data) || nrow(raw_data) == 0) {
    stop("输入数据为空")
  }
  
  # 确保列名正确
  required_cols <- c("gene_id", "expression")
  if(!all(required_cols %in% colnames(raw_data))) {
    if(ncol(raw_data) >= 2) {
      colnames(raw_data)[1:2] <- c("gene_id", "expression")
    } else {
      stop("输入数据必须包含基因ID和表达值两列")
    }
  }
  
  # 去除Ensembl ID版本号
  raw_data$gene_id_clean <- gsub("\\..*", "", raw_data$gene_id)
  
  # 连接到Ensembl
  if(!require(biomaRt)) {
    BiocManager::install("biomaRt")
    library(biomaRt)
  }
  
  ensembl <- tryCatch({
    useMart("ensembl", dataset = "hsapiens_gene_ensembl",
            host = "https://dec2021.archive.ensembl.org/")
  }, error = function(e) {
    useMart("ensembl", dataset = "hsapiens_gene_ensembl")
  })
  
  # 获取基因名映射
  gene_ids_to_query <- unique(raw_data$gene_id_clean)
  
  # 分批查询
  batch_size <- 5000
  n_batches <- ceiling(length(gene_ids_to_query) / batch_size)
  
  all_annotations <- data.frame()
  
  for(batch in 1:n_batches) {
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, length(gene_ids_to_query))
    batch_ids <- gene_ids_to_query[start_idx:end_idx]
    
    annotations_batch <- getBM(
      attributes = c("ensembl_gene_id", "external_gene_name"),
      filters = "ensembl_gene_id",
      values = batch_ids,
      mart = ensembl
    )
    
    all_annotations <- rbind(all_annotations, annotations_batch)
  }
  
  colnames(all_annotations) <- c("ensembl_gene_id", "gene_name")
  
  # 合并数据
  merged <- merge(all_annotations, 
                  raw_data, 
                  by.x = "ensembl_gene_id", 
                  by.y = "gene_id_clean",
                  all.x = FALSE)
  
  # 重新排列列
  merged <- merged[, c("gene_name", "ensembl_gene_id", "expression")]
  colnames(merged) <- c("gene_name", "gene_id", "expression")
  
  return(merged)
}

# 2. 去重函数（修复版）
remove_duplicate_genes <- function(annotated_data) {
  # 检查输入
  if(is.null(annotated_data) || nrow(annotated_data) == 0) {
    stop("输入数据为空")
  }
  
  if(!"gene_name" %in% colnames(annotated_data)) {
    stop("数据缺少gene_name列")
  }
  
  # 检查是否有重复的gene_name
  duplicates <- duplicated(annotated_data$gene_name)
  
  if(sum(duplicates) == 0) {
    return(annotated_data)
  }
  
  # 使用aggregate进行去重
  dedup_data <- aggregate(
    expression ~ gene_name,
    data = annotated_data,
    FUN = mean,
    na.rm = TRUE
  )
  
  # 添加gene_id
  if("gene_id" %in% colnames(annotated_data)) {
    gene_id_map <- aggregate(
      gene_id ~ gene_name,
      data = annotated_data,
      FUN = function(x) x[1]
    )
    dedup_data <- merge(dedup_data, gene_id_map, by = "gene_name")
  }
  
  return(dedup_data)
}

# 3. 零值处理函数
handle_zero_expression <- function(data) {
  # 确保有expression列
  if(!"expression" %in% colnames(data)) {
    numeric_cols <- sapply(data, is.numeric)
    if(any(numeric_cols)) {
      expr_col <- names(which(numeric_cols))[1]
      colnames(data)[colnames(data) == expr_col] <- "expression"
    } else {
      stop("找不到数值型的表达值列")
    }
  }
  
  # 找到最小非零值
  non_zero_values <- data$expression[data$expression > 0]
  
  if(length(non_zero_values) == 0) {
    min_nonzero <- 0.1
  } else {
    min_nonzero <- min(non_zero_values, na.rm = TRUE)
  }
  
  # 统计零值数量
  zero_count <- sum(data$expression == 0, na.rm = TRUE)
  
  if(zero_count > 0) {
    set.seed(123)
    data$expression[data$expression == 0] <- 
      runif(zero_count, 0, min_nonzero/10)
  }
  
  return(data)
}

# 4. 过滤低表达基因
filter_low_expression <- function(data) {
  if(!"expression" %in% colnames(data)) {
    stop("数据缺少expression列")
  }
  
  median_expr <- median(data$expression, na.rm = TRUE)
  threshold <- median_expr * 0.1
  
  keep <- data$expression > threshold
  filtered_data <- data[keep, ]
  
  return(filtered_data)
}

# 5. log2转换
apply_log2_transform <- function(data) {
  # 确保expression是数值型
  if(!is.numeric(data$expression)) {
    data$expression <- as.numeric(data$expression)
  }
  
  # 检查是否有负值
  if(any(data$expression < 0, na.rm = TRUE)) {
    data$expression[data$expression < 0] <- abs(data$expression[data$expression < 0])
  }
  
  # 执行log2(x+1)转换
  data$expression <- log2(data$expression + 1)
  
  return(data)
}

# 6. 0-1归一化
normalize_0_1 <- function(data) {
  if(!"expression" %in% colnames(data)) {
    stop("数据缺少expression列")
  }
  
  expr_values <- data$expression
  
  # 计算最小值和最大值
  min_val <- min(expr_values, na.rm = TRUE)
  max_val <- max(expr_values, na.rm = TRUE)
  
  if(max_val > min_val) {
    data$expression <- (expr_values - min_val) / (max_val - min_val)
  } else {
    data$expression <- 0
  }
  
  return(data)
}