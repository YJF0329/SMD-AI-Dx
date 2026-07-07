# ===============================================
# 使用 final_model 对新数据进行预测的函数
# ===============================================

library(pracma)
library(pROC)
library(epiR)

predict_with_final_model <- function(final_model, new_X, threshold = 0){
  
  # ----------------------
  # 从 final_model 提取参数
  # ----------------------
  X_train <- final_model$train_X
  CY <- final_model$CY
  mn <- final_model$mn
  U <- final_model$U
  T_matrix <- final_model$T_matrix
  best_C <- final_model$best_C
  T_CV <- final_model$T_CV
  
  # ----------------------
  # 核函数定义
  # ----------------------
  Kernel_Test_G <- function(xt, x, C){
    xkt <- rbind(x, xt)
    Kt <- matrix(, nrow(xt), nrow(x))
    for(i in (nrow(x)+1):(nrow(x)+nrow(xt))){
      for(j in 1:nrow(x)){
        Kt[i-nrow(x), j] <- exp(-0.5 * t((xkt[i,] - xkt[j,])) %*% (xkt[i,] - xkt[j,]) / C^2)
      }
    }
    return(Kt)
  }
  
  # ----------------------
  # 生成中心化核矩阵
  # ----------------------
  im <- diag(rep(1, nrow(X_train)))
  one <- matrix(1, nrow = nrow(X_train), ncol = 1)
  K_train_centered <- im - one %*% t(one) / nrow(X_train)
  
  Kt <- Kernel_Test_G(new_X, X_train, best_C)
  onet <- matrix(1, nrow = nrow(new_X), ncol = 1)
  imt <- diag(rep(1, nrow(X_train)))
  
  Kt_centered <- (Kt - onet %*% t(one) %*% K_train_centered / nrow(X_train)) %*% (imt - one %*% t(one) / nrow(X_train))
  
  # ----------------------
  # 外部预测
  # ----------------------
  external_yth <- Kt_centered %*% U[,1:T_CV] %*% pinv(t(T_matrix[,1:T_CV]) %*% K_train_centered %*% U[,1:T_CV]) %*% t(T_matrix[,1:T_CV]) %*% CY
  external_yth <- external_yth + mn
  
  # ----------------------
  # 二分类阈值
  # ----------------------
  predicted_labels <- ifelse(external_yth > threshold, 1, -1)
  
  # ----------------------
  # 计算诊断指标
  # ----------------------
  table_obs <- table(factor(final_model$train_Y[1:length(predicted_labels)], levels = c(-1,1)), factor(predicted_labels, levels = c(-1,1)))
  fold_3 <- if(length(table_obs) < 4){
    matrix(c(table_obs, rep(0, 4 - length(table_obs))), 2, 2)
  } else table_obs
  rownames(fold_3) <- c('Gold_negative','Gold_positive')
  colnames(fold_3) <- c('Test_negative','Test_positive')
  four_fold_diag <- matrix(c(fold_3[4], fold_3[3], fold_3[2], fold_3[1]),2,byrow=TRUE)
  
  rval <- epi.tests(four_fold_diag, conf.level = 0.95)
  diag_test <- summary(rval)$est
  TN <- fold_3[1]
  FN <- fold_3[2]
  FP <- fold_3[3]
  TP <- fold_3[4]
  F1_score <- (2*TP)/(2*TP + FP + FN)
  G_mean <- function(x){ (prod(x))^(1/length(x)) }
  se_sp <- diag_test[c(3,4)]
  Gmeans <- G_mean(se_sp)
  
  # AUC
  roc_obj <- roc(final_model$train_Y[1:length(predicted_labels)], as.vector(external_yth))
  AUC <- roc_obj$auc
  
  # 返回结果
  result <- list(
    predicted_scores = external_yth,
    predicted_labels = predicted_labels,
    AUC = AUC,
    sensitivity = diag_test[3],
    specificity = diag_test[4],
    F1_score = F1_score,
    MCC = (TN*TP - FN*FP)/sqrt((TN+FP)*(TN+FN)*(TP+FP)*(TP+FN)),
    Gmeans = Gmeans
  )
  
  return(result)
}
# new_X = 新测试数据矩阵，列顺序和训练集一致
pred_result <- predict_with_final_model(final_model, new_X)

# 查看预测结果和性能
pred_result$predicted_labels   # 二分类结果
pred_result$predicted_scores   # 连续预测分数
pred_result$AUC                # ROC AUC
pred_result$sensitivity
pred_result$specificity
pred_result$F1_score
pred_result$MCC
pred_result$Gmeans
