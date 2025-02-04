##############################################################################
# Stratified k-Fold Cross-Validation for Cetacean eDNA Species Distribution Models (SDMs)
# USING CARET'S createFolds() FOR STRATIFICATION
##############################################################################

# **Overview
# This code contains R scripts for implementing stratified k-fold cross-validation for Generalized Additive Models (GAMs) 
# used in cetacean species distribution modeling (SDM) based on environmental DNA (eDNA) metabarcoding data. 
# The models predict the probability of eDNA occurrence for three cetacean species along the Washington State coast:
# - Pacific White-Sided Dolphin (Lagenorhynchus obliquidens)
# - Humpback Whale (Megaptera novaeangliae)
# - Risso’s Dolphin (Grampus griseus)

# Load required packages
library(mgcv)      # for GAM modeling
library(caret)     # for createFolds, confusionMatrix
library(ROCR)      # for ROC curve and AUC
library(dplyr)     # optional for summarizing results

# 1) Convert your presence variable into a factor
Laob.unique_083_F$Presence <- factor(
  Laob.unique_083_F$Presence,
  levels = c(0,1)
)

# 2) Define number of folds
k_folds <- 5 

# 3) Create stratified folds
set.seed(123)  # reproducibility
folds <- createFolds(Laob.unique_083_F$Presence, k = k_folds, list = TRUE, returnTrain = FALSE)

# Display how many rows each fold got
cat("Fold sizes:\n")
sapply(folds, length)

# 4) Storage for results
cv_results <- data.frame(
  Fold         = integer(),
  Accuracy     = numeric(),
  Kappa        = numeric(),
  Sensitivity  = numeric(),
  Specificity  = numeric(),
  F1           = numeric(),
  AUC          = numeric(),
  stringsAsFactors = FALSE
)

# 5) Cross-validation loop
for(i in seq_len(k_folds)) {
  # Indices for test fold
  test_idx <- folds[[i]]
  
  # Training indices are everything else
  train_idx <- setdiff(seq_len(nrow(Laob.unique_083_F)), test_idx)
  
  train_data <- Laob.unique_083_F[train_idx, ]
  test_data  <- Laob.unique_083_F[test_idx, ]
  
  #--- Fit GAM for Pacific White-Sided Dolphin
  model_cv <- gam(
    Presence ~ s(dist_shore, bs = "ts") + s(lon, bs = "ts"),
    data    = train_data,
    family  = binomial,
    method  = "REML"
  )
  
  #--- Predict probabilities on test set
  pred_prob <- predict(model_cv, newdata = test_data, type = "response")
  pred_prob <- as.numeric(pred_prob)
  
  #--- AUC (ROCR)
  # Convert factor presence to numeric for ROCR
  obs_numeric <- as.numeric(as.character(test_data$Presence))
  pred_obj <- prediction(pred_prob, obs_numeric)
  auc_perf <- performance(pred_obj, "auc")
  auc_val  <- auc_perf@y.values[[1]]
  
  #--- Threshold-based classification
  threshold <- 0.3  
  pred_class <- ifelse(pred_prob > threshold, 1, 0)
  
  # Convert to factor
  pred_factor <- factor(pred_class, levels = c(0,1))
  obs_factor  <- test_data$Presence  # factor(0,1)
  
  # Confusion matrix
  cm <- confusionMatrix(data = pred_factor, reference = obs_factor, positive = "1")
  
  # Extract metrics
  accuracy_val    <- cm$overall["Accuracy"]
  kappa_val       <- cm$overall["Kappa"]
  sensitivity_val <- cm$byClass["Sensitivity"]
  specificity_val <- cm$byClass["Specificity"]
  
  # F1-score
  precision_val <- cm$byClass["Pos Pred Value"]
  recall_val    <- cm$byClass["Sensitivity"]
  f1_val <- if(!is.na(precision_val) && !is.na(recall_val) && (precision_val + recall_val) > 0){
    2 * (precision_val * recall_val) / (precision_val + recall_val)
  } else {
    NA
  }
  
  # Store results
  cv_results <- rbind(
    cv_results,
    data.frame(
      Fold         = i,
      Accuracy     = as.numeric(accuracy_val),
      Kappa        = as.numeric(kappa_val),
      Sensitivity  = as.numeric(sensitivity_val),
      Specificity  = as.numeric(specificity_val),
      F1           = as.numeric(f1_val),
      AUC          = as.numeric(auc_val)
    )
  )
}

# 6) Summarize across folds
summary_cv <- cv_results %>%
  summarize(
    Mean_Accuracy    = mean(Accuracy, na.rm = TRUE),
    SD_Accuracy      = sd(Accuracy, na.rm = TRUE),
    Mean_Kappa       = mean(Kappa, na.rm = TRUE),
    SD_Kappa         = sd(Kappa, na.rm = TRUE),
    Mean_Sensitivity = mean(Sensitivity, na.rm = TRUE),
    SD_Sensitivity   = sd(Sensitivity, na.rm = TRUE),
    Mean_Specificity = mean(Specificity, na.rm = TRUE),
    SD_Specificity   = sd(Specificity, na.rm = TRUE),
    Mean_F1          = mean(F1, na.rm = TRUE),
    SD_F1            = sd(F1, na.rm = TRUE),
    Mean_AUC         = mean(AUC, na.rm = TRUE),
    SD_AUC           = sd(AUC, na.rm = TRUE)
  )

cat("Pacific White-Sided Dolphin (Stratified k-Fold CV) Results Per Fold:\n")
print(cv_results)

cat("\nPacific White-Sided Dolphin (Stratified k-Fold CV) Summary:\n")
print(summary_cv)


##############################################################################
# STRATIFIED k-FOLD CV FOR HUMPBACK WHALE MODEL (THRESHOLD = 0.3)
##############################################################################

# Load packages
library(mgcv)
library(caret)
library(ROCR)
library(dplyr)

# Ensure 'Presence' is a factor c("0","1") in Meno.unique_083_F
Meno.unique_083_F$Presence <- factor(Meno.unique_083_F$Presence, levels = c(0,1))

# Set seed for reproducibility
set.seed(123)

# Choose k for the stratified folds
k_folds <- 5

# Create stratified folds based on 'Presence'
folds <- createFolds(Meno.unique_083_F$Presence, k = k_folds, list = TRUE, returnTrain = FALSE)

# Store fold-level metrics
cv_results <- data.frame(
  Fold         = integer(),
  Accuracy     = numeric(),
  Kappa        = numeric(),
  Sensitivity  = numeric(),
  Specificity  = numeric(),
  F1           = numeric(),
  AUC          = numeric(),
  stringsAsFactors = FALSE
)

# Stratified k-fold loop
for(i in seq_len(k_folds)) {
  cat("\n=====================================================\n")
  cat("HUMPBACK WHALE - STRATIFIED FOLD:", i, "\n")
  cat("=====================================================\n")
  
  # Split into train/test
  test_idx <- folds[[i]]
  train_idx <- setdiff(seq_len(nrow(Meno.unique_083_F)), test_idx)
  
  train_data <- Meno.unique_083_F[train_idx, ]
  test_data  <- Meno.unique_083_F[test_idx, ]
  
  cat("Train data size:", nrow(train_data), "\n")
  cat("Test data size:",  nrow(test_data), "\n")
  cat("Train class distribution:\n")
  print(table(train_data$Presence))
  cat("Test class distribution:\n")
  print(table(test_data$Presence))
  
  # Fit the GAM model (bathy, SWT)
  model_cv <- gam(
    Presence ~ s(bathy, bs = "ts") + s(SWT, bs = "ts"),
    data = train_data,
    family = binomial,
    method = "REML"
  )
  
  # Predict probabilities
  pred_prob <- predict(model_cv, newdata = test_data, type = "response")
  
  # Convert pred_prob to plain numeric if needed to avoid dimension issues
  pred_prob <- as.numeric(pred_prob)
  
  # Calculate AUC with ROCR
  # Convert test_data$Presence to numeric 0/1 for ROCR
  obs_numeric <- as.numeric(as.character(test_data$Presence))
  pred_obj <- prediction(pred_prob, obs_numeric)
  perf_auc <- performance(pred_obj, "auc")
  auc_value <- perf_auc@y.values[[1]]
  
  # Classification threshold = 0.3
  threshold <- 0.3
  pred_class <- ifelse(pred_prob > threshold, 1, 0)
  
  # Convert to factor with levels c(0,1)
  pred_factor <- factor(pred_class, levels = c(0,1))
  obs_factor  <- test_data$Presence  # already factor(c(0,1))
  
  # confusionMatrix from caret
  cm <- confusionMatrix(data = pred_factor, reference = obs_factor, positive = "1")
  
  # Print confusion matrix
  cat("Confusion Matrix (threshold = 0.3):\n")
  print(cm)
  
  # Extract metrics
  acc_val    <- cm$overall["Accuracy"]
  kappa_val  <- cm$overall["Kappa"]
  sens_val   <- cm$byClass["Sensitivity"]   # TPR
  spec_val   <- cm$byClass["Specificity"]   # TNR
  precision_val <- cm$byClass["Pos Pred Value"]
  recall_val    <- sens_val
  
  # F1-score
  f1_val <- if(!is.na(precision_val) && !is.na(recall_val) && (precision_val + recall_val) > 0){
    2 * (precision_val * recall_val) / (precision_val + recall_val)
  } else {
    NA
  }
  
  # Store results
  cv_results <- rbind(
    cv_results,
    data.frame(
      Fold         = i,
      Accuracy     = as.numeric(acc_val),
      Kappa        = as.numeric(kappa_val),
      Sensitivity  = as.numeric(sens_val),
      Specificity  = as.numeric(spec_val),
      F1           = as.numeric(f1_val),
      AUC          = as.numeric(auc_value)
    )
  )
}

# Summarize across folds
summary_cv <- cv_results %>%
  summarize(
    Mean_Accuracy    = mean(Accuracy, na.rm = TRUE),
    SD_Accuracy      = sd(Accuracy, na.rm = TRUE),
    Mean_Kappa       = mean(Kappa, na.rm = TRUE),
    SD_Kappa         = sd(Kappa, na.rm = TRUE),
    Mean_Sensitivity = mean(Sensitivity, na.rm = TRUE),
    SD_Sensitivity   = sd(Sensitivity, na.rm = TRUE),
    Mean_Specificity = mean(Specificity, na.rm = TRUE),
    SD_Specificity   = sd(Specificity, na.rm = TRUE),
    Mean_F1          = mean(F1, na.rm = TRUE),
    SD_F1            = sd(F1, na.rm = TRUE),
    Mean_AUC         = mean(AUC, na.rm = TRUE),
    SD_AUC           = sd(AUC, na.rm = TRUE)
  )

cat("\nHumpback Whale (Threshold = 0.3) Stratified k-Fold Results:\n")
cat("Results per fold:\n")
print(cv_results)

cat("\nSummary of metrics across folds:\n")
print(summary_cv)

##############################################################################
# STRATIFIED K-FOLD CROSS-VALIDATION FOR RISSO'S DOLPHIN
# Using threshold = 0.3 (example) to classify presence/absence
##############################################################################

library(mgcv)
library(caret)
library(ROCR)
library(dplyr)

# 1) Ensure Presence is factor("0","1")
Grgr.unique_083_F$Presence <- factor(Grgr.unique_083_F$Presence, levels = c(0,1))

# 2) Set up stratified folds
set.seed(123)
k_folds <- 5
folds <- createFolds(Grgr.unique_083_F$Presence, k = k_folds, list = TRUE, returnTrain = FALSE)

# 3) Store results for each fold
cv_results <- data.frame(
  Fold         = integer(),
  Accuracy     = numeric(),
  Kappa        = numeric(),
  Sensitivity  = numeric(),
  Specificity  = numeric(),
  F1           = numeric(),
  AUC          = numeric(),
  stringsAsFactors = FALSE
)

# 4) Cross-validation loop
threshold <- 0.3  # example threshold

for (i in seq_len(k_folds)) {
  cat("\n=====================================================\n")
  cat("RISSO'S DOLPHIN - STRATIFIED FOLD:", i, "\n")
  cat("=====================================================\n")
  
  # Indices for test/training
  test_idx <- folds[[i]]
  train_idx <- setdiff(seq_len(nrow(Grgr.unique_083_F)), test_idx)
  
  train_data <- Grgr.unique_083_F[train_idx, ]
  test_data  <- Grgr.unique_083_F[test_idx, ]
  
  cat("Train data size:", nrow(train_data), "\n")
  cat("Test data size:",  nrow(test_data), "\n")
  
  cat("Train class distribution:\n")
  print(table(train_data$Presence))
  cat("Test class distribution:\n")
  print(table(test_data$Presence))
  
  # 5) Fit the GAM model: Presence ~ s(lon) + s(slope)
  model_cv <- gam(
    Presence ~ s(lon, bs = "ts") + s(slope, bs = "ts"),
    data    = train_data,
    family  = binomial,
    method  = "REML"
  )
  
  # 6) Predict probabilities
  pred_prob <- predict(model_cv, newdata = test_data, type = "response")
  pred_prob <- as.numeric(pred_prob)  # ensure it's plain numeric
  
  # 7) Compute AUC with ROCR
  obs_numeric <- as.numeric(as.character(test_data$Presence))  # 0/1 as numeric
  pred_obj <- prediction(pred_prob, obs_numeric)
  perf_auc <- performance(pred_obj, "auc")
  auc_value <- perf_auc@y.values[[1]]
  
  # 8) Classification at threshold
  pred_class <- ifelse(pred_prob > threshold, 1, 0)
  pred_factor <- factor(pred_class, levels = c(0,1))
  obs_factor  <- test_data$Presence  # factor(c(0,1))
  
  # 9) confusionMatrix
  cm <- confusionMatrix(pred_factor, obs_factor, positive = "1")
  cat("Confusion Matrix (Threshold =", threshold, "):\n")
  print(cm)
  
  # Extract metrics
  acc_val    <- cm$overall["Accuracy"]
  kappa_val  <- cm$overall["Kappa"]
  sens_val   <- cm$byClass["Sensitivity"]
  spec_val   <- cm$byClass["Specificity"]
  precision_val <- cm$byClass["Pos Pred Value"]
  
  # F1 Score
  f1_val <- if (!is.na(precision_val) && !is.na(sens_val) && (precision_val + sens_val) > 0) {
    2 * (precision_val * sens_val) / (precision_val + sens_val)
  } else {
    NA
  }
  
  # 10) Store the fold results
  cv_results <- rbind(
    cv_results,
    data.frame(
      Fold         = i,
      Accuracy     = as.numeric(acc_val),
      Kappa        = as.numeric(kappa_val),
      Sensitivity  = as.numeric(sens_val),
      Specificity  = as.numeric(spec_val),
      F1           = as.numeric(f1_val),
      AUC          = as.numeric(auc_value)
    )
  )
}

# 11) Summarize metrics across folds
summary_cv <- cv_results %>%
  summarize(
    Mean_Accuracy    = mean(Accuracy, na.rm = TRUE),
    SD_Accuracy      = sd(Accuracy, na.rm = TRUE),
    Mean_Kappa       = mean(Kappa, na.rm = TRUE),
    SD_Kappa         = sd(Kappa, na.rm = TRUE),
    Mean_Sensitivity = mean(Sensitivity, na.rm = TRUE),
    SD_Sensitivity   = sd(Sensitivity, na.rm = TRUE),
    Mean_Specificity = mean(Specificity, na.rm = TRUE),
    SD_Specificity   = sd(Specificity, na.rm = TRUE),
    Mean_F1          = mean(F1, na.rm = TRUE),
    SD_F1            = sd(F1, na.rm = TRUE),
    Mean_AUC         = mean(AUC, na.rm = TRUE),
    SD_AUC           = sd(AUC, na.rm = TRUE)
  )

# Print final results
cat("\nRisso's Dolphin (Threshold =", threshold, ") Stratified k-Fold Results:\n")
cat("Results per fold:\n")
print(cv_results)

cat("\nSummary of metrics across folds:\n")
print(summary_cv)

###############
# CREATE SUMMARY TABLE
###############

library(dplyr)

# Example placeholders:
pwd_summary <- data.frame(
  Mean_Accuracy = 0.84, SD_Accuracy = 0.19,
  Mean_Kappa = 0.61, SD_Kappa = 0.39,
  Mean_Sensitivity = 0.75, SD_Sensitivity = 0.27,
  Mean_Specificity = 0.87, SD_Specificity = 0.19,
  Mean_F1 = 0.72, SD_F1 = 0.27,
  Mean_AUC = 0.92, SD_AUC = 0.17
)

hump_summary <- data.frame(
  Mean_Accuracy = 0.80, SD_Accuracy = 0.17,
  Mean_Kappa = 0.45, SD_Kappa = 0.45,
  Mean_Sensitivity = 0.70, SD_Sensitivity = 0.45,
  Mean_Specificity = 0.85, SD_Specificity = 0.18,
  Mean_F1 = 0.68, SD_F1 = 0.25,
  Mean_AUC = 0.87, SD_AUC = 0.22
)

risso_summary <- data.frame(
  Mean_Accuracy = 0.74, SD_Accuracy = 0.15,
  Mean_Kappa = 0.06, SD_Kappa = 0.32,
  Mean_Sensitivity = 0.30, SD_Sensitivity = 0.45,
  Mean_Specificity = 0.80, SD_Specificity = 0.16,
  Mean_F1 = 0.50, SD_F1 = 0.24,
  Mean_AUC = 0.62, SD_AUC = 0.16
)

# Combine them into one table
combined_results <- rbind(
  cbind(Species = "Pacific W-S Dolphin", pwd_summary),
  cbind(Species = "Humpback Whale", hump_summary),
  cbind(Species = "Risso's Dolphin", risso_summary)
)

combined_results

