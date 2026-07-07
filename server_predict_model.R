# server_predict_model.R
library(shiny)
library(DT)
library(plotly)
library(MASS)      # 用于 ginv

server_predict_model <- function(id, feature_results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 内置 final_model 路径和加载
    final_model <- reactiveVal(NULL)
    
    # 使用 isolate 和局部变量避免覆盖
    observe({
      tryCatch({
        # 加载到局部变量
        loaded_model <- get(load("final_model.Rdata"))
        
        # 检查必要组件
        required_components <- c("train_X", "train_Y", "CY", "mn", "U", 
                                 "T_matrix", "best_C", "T_CV")
        if(all(required_components %in% names(loaded_model))) {
          final_model(loaded_model)
          cat("✅ final_model 加载成功\n")
          cat(sprintf("模型特征数: %d\n", ncol(loaded_model$train_X)))
          cat(sprintf("特征名称: %s\n", paste(colnames(loaded_model$train_X)[1:5], collapse=", ")))
        } else {
          stop("加载的模型缺少必要组件")
        }
      }, error = function(e) {
        showNotification(paste("❌ 模型加载失败:", e$message), 
                         type = "error")
        cat("❌ 模型加载错误:", e$message, "\n")
      })
    })
    
    # 特征对齐函数：用平均值填充缺失特征
    align_features_with_model <- function(new_X, model_train_X) {
      # 确保都是矩阵
      new_X <- as.matrix(new_X)
      model_X <- as.matrix(model_train_X)
      
      # 获取特征名称
      new_features <- colnames(new_X)
      model_features <- colnames(model_X)
      
      # 计算交集和缺失
      common_features <- intersect(new_features, model_features)
      missing_features <- setdiff(model_features, new_features)
      extra_features <- setdiff(new_features, model_features)
      
      # 输出匹配信息
      cat("\n=== 特征对齐信息 ===\n")
      cat(sprintf("模型需要特征数: %d\n", length(model_features)))
      cat(sprintf("输入数据特征数: %d\n", length(new_features)))
      cat(sprintf("共同特征数: %d (匹配度: %.1f%%)\n", 
                  length(common_features), 
                  length(common_features)/length(model_features)*100))
      
      if(length(missing_features) > 0) {
        cat(sprintf("缺失特征数: %d\n", length(missing_features)))
        cat("缺失特征（前10个）:", paste(head(missing_features, 10), collapse=", "), "\n")
      }
      
      if(length(extra_features) > 0) {
        cat(sprintf("多余特征数: %d（将被忽略）\n", length(extra_features)))
      }
      cat("====================\n")
      
      # 创建对齐后的矩阵（按模型特征的顺序）
      aligned_matrix <- matrix(0, 
                               nrow = nrow(new_X),
                               ncol = length(model_features))
      colnames(aligned_matrix) <- model_features
      
      # 设置行名
      if(!is.null(rownames(new_X))) {
        rownames(aligned_matrix) <- rownames(new_X)
      } else {
        rownames(aligned_matrix) <- paste0("Sample_", 1:nrow(new_X))
      }
      
      # 1. 填充共同特征
      if(length(common_features) > 0) {
        aligned_matrix[, common_features] <- new_X[, common_features, drop = FALSE]
      }
      
      # 2. 用训练集的平均值填充缺失特征
      if(length(missing_features) > 0) {
        # 计算训练集每个特征的平均值
        model_feature_means <- colMeans(model_X)
        
        for(feature in missing_features) {
          aligned_matrix[, feature] <- model_feature_means[feature]
        }
        
        # 显示填充信息
        cat(sprintf("已用训练集平均值填充 %d 个缺失特征\n", length(missing_features)))
      }
      
      return(list(
        aligned_matrix = aligned_matrix,
        match_info = list(
          n_total = length(model_features),
          n_matched = length(common_features),
          n_missing = length(missing_features),
          match_percentage = length(common_features)/length(model_features)*100,
          common_features = common_features,
          missing_features = missing_features,
          extra_features = extra_features
        )
      ))
    }
    
    # 内置 predict_with_final_model() 函数
    predict_with_final_model <- function(final_model, aligned_X, threshold = 0){
      X_train <- final_model$train_X
      CY <- final_model$CY
      mn <- final_model$mn
      U <- final_model$U
      T_matrix <- final_model$T_matrix
      best_C <- final_model$best_C
      T_CV <- final_model$T_CV
      
      # 核函数
      Kernel_Test_G <- function(xt, x, C){
        xkt <- rbind(x, xt)
        Kt <- matrix(, nrow(xt), nrow(x))
        for(i in (nrow(x)+1):(nrow(x)+nrow(xt))){
          for(j in 1:nrow(x)){
            Kt[i-nrow(x), j] <- exp(-0.5 * sum((xkt[i,] - xkt[j,])^2) / C^2)
          }
        }
        return(Kt)
      }
      
      im <- diag(rep(1, nrow(X_train)))
      one <- matrix(1, nrow = nrow(X_train), ncol = 1)
      K_train_centered <- im - one %*% t(one) / nrow(X_train)
      
      Kt <- Kernel_Test_G(aligned_X, X_train, best_C)
      onet <- matrix(1, nrow = nrow(aligned_X), ncol = 1)
      imt <- diag(rep(1, nrow(X_train)))
      Kt_centered <- (Kt - onet %*% t(one) %*% K_train_centered / nrow(X_train)) %*% 
        (imt - one %*% t(one) / nrow(X_train))
      
      # 使用 MASS::ginv 替代 pinv
      external_yth <- Kt_centered %*% U[,1:T_CV] %*% 
        ginv(t(T_matrix[,1:T_CV]) %*% K_train_centered %*% U[,1:T_CV]) %*% 
        t(T_matrix[,1:T_CV]) %*% CY
      external_yth <- external_yth + mn
      predicted_labels <- ifelse(external_yth > threshold, 1, -1)
      
      # 只返回预测结果
      list(
        predicted_scores = as.vector(external_yth),
        predicted_labels = predicted_labels
      )
    }
    
    # 当点击运行预测按钮
    pred_result <- reactiveVal(NULL)
    match_info <- reactiveVal(NULL)  # 存储匹配信息
    
    observeEvent(input$run_prediction, {
      req(feature_results$final_matrix())
      req(final_model())
      
      tryCatch({
        new_X <- as.matrix(feature_results$final_matrix())
        
        # 1. 特征对齐和填充
        alignment_result <- align_features_with_model(new_X, final_model()$train_X)
        aligned_X <- alignment_result$aligned_matrix
        match_info(alignment_result$match_info)
        
        # 显示匹配信息
        showNotification(
          sprintf("特征匹配完成: %d/%d (%.1f%%)，缺失特征已用平均值填充",
                  match_info()$n_matched,
                  match_info()$n_total,
                  match_info()$match_percentage),
          type = "message",
          duration = 5
        )
        
        # 2. 进行预测
        res <- predict_with_final_model(final_model(), aligned_X)
        pred_result(res)
        
        showNotification("✅ 模型预测完成", type = "message")
        
      }, error = function(e) {
        showNotification(paste("❌ 预测失败:", e$message), 
                         type = "error")
        cat("预测错误:", e$message, "\n")
      })
    })
    
    # 输出预测结果表格
    output$prediction_table <- renderDT({
      req(pred_result())
      
      # 确保有预测结果
      if(is.null(pred_result()) || 
         is.null(pred_result()$predicted_scores) ||
         length(pred_result()$predicted_scores) == 0) {
        return(datatable(data.frame(Message = "暂无预测结果")))
      }
      
      # 创建数据框
      scores <- pred_result()$predicted_scores
      labels <- pred_result()$predicted_labels
      
      # 获取样本名
      if(!is.null(rownames(feature_results$final_matrix()))) {
        sample_names <- rownames(feature_results$final_matrix())
      } else {
        sample_names <- paste0("Sample_", 1:length(scores))
      }
      
      # 确保长度一致
      n_samples <- length(scores)
      if(length(sample_names) != n_samples) {
        sample_names <- paste0("Sample_", 1:n_samples)
      }
      
      df <- data.frame(
        Sample = sample_names,
        Score = round(scores, 4),
        Label = ifelse(labels == 1, "Positive", "Negative"),
        stringsAsFactors = FALSE
      )
      
      datatable(df, 
                options = list(
                  pageLength = 10, 
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                ),
                rownames = FALSE,
                class = 'cell-border stripe hover')
    })
    
    # 返回模块状态
    return(list(
      prediction_complete = reactive({ !is.null(pred_result()) }),
      prediction_results = pred_result
    ))
  })
}