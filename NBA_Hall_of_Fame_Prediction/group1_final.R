# 110020022許鈞崴 start

# read data  
nba_data <- read.csv("NBA_players_clean_final_ver2.csv")

#EDA draw 10*features  
library(pROC)
library(GGally)
nba_data_no_player <- nba_data[, !colnames(nba_data) %in% "Player"]
variables <- colnames(nba_data_no_player)
hof_index <- which(variables == "HOF")
num_vars_per_plot <- 10
num_plots <- ceiling((length(variables) - 1) / num_vars_per_plot)
for (i in 1:num_plots) {
  start_index <- (i - 1) * num_vars_per_plot + 1
  end_index <- min(i * num_vars_per_plot, length(variables))
  if (start_index != 1) {
    current_vars <- c("HOF", variables[start_index:end_index])
  } else {
    current_vars <- variables[start_index:end_index]
  }
  data_subset <- nba_data_no_player[, current_vars, drop = FALSE]
  print(ggpairs(data_subset))
}

#EDA draw  
library(GGally)
par(mfrow = c(2, 2))
nba_data_no_player <- nba_data[, !colnames(nba_data) %in% "Player"]
nba_data_no_player[] <- lapply(nba_data_no_player, function(x) {
  if (is.character(x)) {
    return(factor(x))  
  } else {
    return(x)  
  }
})
vars <- colnames(nba_data_no_player)
for (var in vars) {
  if (var != "HOF") {
    boxplot(nba_data_no_player[[var]] ~ nba_data_no_player$HOF,
            main = paste("Boxplot of", var),
            xlab = "HOF",
            ylab = var,
            col = c("lightblue", "lightgreen"))
  }
}

#predict  
set.seed(42)
library(randomForest)
train_index <- sample(1:nrow(nba_data), 0.8 * nrow(nba_data))
train_data <- nba_data[train_index, ]
test_data <- nba_data[-train_index, ]
rf_model <- randomForest(HOF ~ . - Player - Pos, data = train_data, importance = TRUE, ntree = 500)
print(rf_model)
predictions_prob <- predict(rf_model, test_data, type = "response")
predicted_class <- ifelse(predictions_prob > 0.5, 1, 0)
print(predicted_class)
accuracy <- sum(predicted_class == test_data$HOF) / nrow(test_data)
print(paste("Accuracy:", accuracy))

# summary feature importance  
importance_values <- importance(rf_model)
importance_df <- data.frame(
  Feature = rownames(importance_values),
  Importance = importance_values[, 1]  # MeanDecreaseGini
)
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
library(ggplot2)
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Feature Importance in Random Forest", x = "Features", y = "Importance")

# confusion matrix  
confusion_matrix <- table(Predicted = predicted_class, Actual = test_data$HOF)
print("Confusion Matrix:")
print(confusion_matrix)
true_positive <- confusion_matrix[2, 2]  # 預測為 1，實際為 1
false_positive <- confusion_matrix[2, 1]  # 預測為 1，實際為 0
true_negative <- confusion_matrix[1, 1]  # 預測為 0，實際為 0
false_negative <- confusion_matrix[1, 2]  # 預測為 0，實際為 1
# Accuracy
accuracy <- (true_positive + true_negative) / sum(confusion_matrix)
# Precision
precision <- true_positive / (true_positive + false_positive)
# Recall
recall <- true_positive / (true_positive + false_negative)
# F1-score
f1_score <- 2 * (precision * recall) / (precision + recall)

# print  
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1-score:", f1_score))

# ROC curve  
library(pROC)
par(mfrow = c(1, 1))
predicted_class <- as.numeric(predicted_class) - 1
roc_curve_random_forest <- roc(test_data$HOF, predicted_class)
plot(roc_curve_random_forest, main = "ROC Curve of Random Forest", col = "blue", lwd = 2, print.auc = TRUE, auc.polygon = TRUE, legacy.axes = TRUE, smooth = TRUE, print.thres = TRUE, print.thres.cex = 0.8)
print(paste("AUC:", auc(roc_curve_random_forest)))

# predict future HOF  
# 用To = 2022表示當時還在打球的選手
nba_data_N2022 <- nba_data[nba_data$To != 2022, ]
rf_model_2022 <- randomForest(HOF ~ . - Player - Pos, data = nba_data_N2022, importance = TRUE, ntree = 500)
nba_data_2022 <- nba_data[nba_data$To == 2022, ]
predictions_prob <- predict(rf_model_2022, nba_data_2022, type = "response")
predicted_class <- ifelse(predictions_prob > 0.5, 1, 0)
selected_indices <- which(predicted_class == 1)
selected_people <- nba_data_2022[selected_indices, ]
print(selected_people)

# 110020022許鈞崴 end

# 110020026蔡易庭 start
library(corrplot)
library(e1071)
library(MLmetrics)
library(class)
library(pROC)

# read data 蔡易庭
data <- read.csv('NBA_players_clean_final_ver2.csv', fileEncoding="latin1")
data

# EDA 蔡易庭
summary(data)
table(data$HOF) # Check the number of players in HOF
par(mfrow = c(2, 2)) # Four histograms 
hist(data$Years, main = "Years in The League") 
table(data$Pos, data$HOF) # Number of HOF players of each positions
hist(data$Height, main = "Height")
hist(data$Wt, main = "Weight")
par(mfrow = c(1, 1))
par(mfrow = c(2, 2)) # Four histograms
hist(data$G, main = "Games Played")
hist(data$PTS, main = "Points Per Game")
hist(data$TRB, main = "Total Rebound Percentage")
hist(data$AST, main = "Assists Per Game")
par(mfrow = c(1, 1))

par(mfrow = c(2, 3)) # Five density plots
plot(density(data$FG.), main = "Field Goals Percentages")
plot(density(data$FG3.), main = "3 Points Field Goal Percentages")
plot(density(data$FT.), main = "Free Throws Percentages")
plot(density(data$eFG.), main = "Effective Field Goal Percentage")
plot(density(data$PER), main = "Player Efficiency Rating")
par(mfrow = c(1, 1))

par(mfrow = c(2, 3)) # Six histograms
hist(data$WS, main = "Win Share")
hist(data$All.Star[data$All.Star!=0], main = "All Stars Without 0 Participations")
hist(data$All.NBA[data$All.NBA!=0], main = "All NBAs Without 0 Participations")
hist(data$All.ABA[data$All.ABA!=0], main = "All ABA Without 0 Participations")
hist(data$All.Rookie, main = "All Rookie Team")
hist(data$All.Defensive[data$All.Defensive!=0], main = "All Defensive Team Without 0 Participations")
par(mfrow = c(1, 1))

par(mfrow = c(2, 3)) # Six histograms and checking the numbers of the awards
hist(data$BLK.Champ[data$BLK.Champ!=0], main = "BLK Champ Without 0 Participations")
sum(data$BLK.Champ)
hist(data$STL.Champ[data$STL.Champ!=0], main = "STL Champ Without 0 Participations")
sum(data$STL.Champ)
hist(data$TRB.Champ[data$TRB.Champ!=0], main = "TRB Champ Without 0 Participations")
sum(data$TRB.Champ)
hist(data$AST.Champ[data$AST.Champ!=0], main = "AST Champ Without 0 Participations")
sum(data$AST.Champ)
hist(data$Scoring.Champ[data$Scoring.Champ!=0], main = "Scoring Champ Without 0 Participations")
sum(data$Scoring.Champ)
hist(data$Most.Improved, main = "Most Improved Players")
sum(data$Most.Improved)
par(mfrow = c(1, 1))

par(mfrow = c(3, 3)) # Eight histograms
hist(data$Sixth.Man[data$Sixth.Man!=0], main = "Sixth Man Without 0 Participations")
sum(data$Sixth.Man)
hist(data$DPOY[data$DPOY!=0], main = "Defensive Player of The Year Without 0 Participations")
sum(data$DPOY)
hist(data$ROY, main = "Rookie of The Year")
sum(data$ROY)
hist(data$AS.MVP[data$AS.MVP!=0], main = "All Star Game MVP Without 0 Participations")
hist(data$CF.MVP, main = "Conference Finals MVP") # Start from 2022
hist(data$Finals.MVP[data$Finals.MVP!=0], main = "Finals MVP Without 0 Participations")
hist(data$MVP[data$MVP!=0], main = "MVP Without 0 Participations")
hist(data$Championships[data$Championships!=0], main = "Championship Won Without 0 Participations")
par(mfrow = c(1, 1))

table(data$NBA.75.Team, data$HOF) # Number of HOF in the 75 team
table(data$ABA.All.Time.Team, data$HOF) # Number of HOF in ABA All Time Team

# splitting data 蔡易庭
set.seed(42)
n <- nrow(data)
p <- 0.8
index <- sample(1: n, size = p * n)
data_rest <- data[index, ]
data_test <- data[-index, ] # Split into 0.2 testing data and 0.8 for training
n_ <- nrow(data_rest)
index_ <- sample(1: n_, size = p * n_)
data_val <- data_rest[-index_, ] # Used for validation
data_train <- data_rest[index_, ] # Used to train the model for validation
data_train

# NaiveBayes (Assumption: Conditional independence) 蔡易庭
naive <- naiveBayes(HOF ~ ., data = data_rest)
nb_pred <- predict(naive, data_test) # Use naivebayes to predict

confusion_nb <- table(nb_pred, data_test$HOF)
mean(nb_pred == data_test$HOF) # Output accuracy

precision_nb <- confusion_nb[4] / (confusion_nb[2] + confusion_nb[4])
recall_nb <- confusion_nb[4] / (confusion_nb[3] + confusion_nb[4])
f1_nb <- 2 * (precision_nb * recall_nb) / (precision_nb + recall_nb)
confusion_nb # Output the confusion matrix
precision_nb # Output precision
recall_nb # Output recall
f1_nb # Output F1-score
nb_pred <- as.numeric(nb_pred) - 1
roc_curve_nb <- roc(data_test$HOF, nb_pred)
plot(roc_curve_nb, main = "ROC Curve of NaiveBayes", col = "blue", lwd = 2, print.auc = TRUE, auc.polygon = TRUE, legacy.axes = TRUE, smooth = TRUE, print.thres = TRUE, print.thres.cex = 0.8)
# Plot the ROC curve
print(paste("AUC:", auc(roc_curve_nb))) # Output AUC

# KNN (Assumption: 特徵的距離是相同尺度且可比較的、分布的密度要一致、噪音較少、independence)
train_x <- as.matrix((data_train[-6])[-1])
test_x <- as.matrix((data_val[-6])[-1])
train_y <- as.factor(t(data_train[2]))
train_y <- as.factor(train_y)
best_acc <- 0
best_k <- 0
for(i in 1:50){ # Use the validation data to test the best number for K
  knn_pred <- knn(train_x, test_x, train_y, k = i)
  if(mean(knn_pred == data_val$HOF) > best_acc){
    best_acc <- mean(knn_pred == data_val$HOF)
    best_k <- i
  }
}
knn_pred_best <- knn(train_x, test_x, train_y, k = best_k)
confusion_knn <- table(knn_pred_best, data_val$HOF)
mean(knn_pred_best == data_val$HOF) # Output accuracy of validation

precision_knn <- confusion_knn[4] / (confusion_knn[2] + confusion_knn[4])
recall_knn <- confusion_knn[4] / (confusion_knn[3] + confusion_knn[4])
f1_knn <- 2 * (precision_knn * recall_knn) / (precision_knn + recall_knn)
confusion_knn # Output the confusion matrix of validation
precision_knn # Output precision of validation
recall_knn # Output recall of validation
f1_knn # Output F1-score of validation
knn_pred_best <- as.numeric(knn_pred_best) - 1
roc_curve_knn <- roc(data_val$HOF, knn_pred_best)
plot(roc_curve_knn, main = "ROC Curve of KNN", col = "blue", lwd = 2, print.auc = TRUE, auc.polygon = TRUE, legacy.axes = TRUE, smooth = TRUE, print.thres = TRUE, print.thres.cex = 0.8)
# Plot the ROC curve of validation

# KNN all train data
train_x <- as.matrix((data_rest[-6])[-1])
test_x <- as.matrix((data_test[-6])[-1])
train_y <- as.factor(t(data_rest[2]))
train_y <- as.factor(train_y)

knn_pred_best <- knn(train_x, test_x, train_y, k = best_k) # Predicting with the best K and all of the training data
confusion_knn <- table(knn_pred_best, data_test$HOF)
mean(knn_pred_best == data_test$HOF) # Output accuracy

precision_knn <- confusion_knn[4] / (confusion_knn[2] + confusion_knn[4])
recall_knn <- confusion_knn[4] / (confusion_knn[3] + confusion_knn[4])
f1_knn <- 2 * (precision_knn * recall_knn) / (precision_knn + recall_knn)
confusion_knn # Output the confusion matrix
precision_knn # Output precision
recall_knn # Output recall
f1_knn # Output F1-score
knn_pred_best <- as.numeric(knn_pred_best) - 1
roc_curve_knn <- roc(data_test$HOF, knn_pred_best)
plot(roc_curve_knn, main = "ROC Curve of KNN", col = "blue", lwd = 2, print.auc = TRUE, auc.polygon = TRUE, legacy.axes = TRUE, smooth = TRUE, print.thres = TRUE, print.thres.cex = 0.8)
# Plot the ROC curve

# 110020026蔡易庭 end



# 110020017 楊哲宇 start

# 安裝並載入必要的套件
if (!require("xgboost")) install.packages("xgboost", dependencies = TRUE)
if (!require("caret")) install.packages("caret", dependencies = TRUE)
if (!require("data.table")) install.packages("data.table", dependencies = TRUE)
if (!require("pROC")) install.packages("pROC", dependencies = TRUE)
library(xgboost)
library(caret)
library(data.table)
library(pROC)

# 讀取本地 CSV 資料
file_path <- "NBA_players_clean_final_ver2.csv"  # 替換為你的 .csv 檔案路徑
data <- fread(file_path)
# 確認資料結構
str(data)

# 將資料中的 "HOF" 設為標籤，其餘特徵排除 "Player" 和 "Pos"
# 將資料切割為訓練集與測試集
set.seed(42)
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
train_label <- as.numeric(train_data$HOF)
test_label <- as.numeric(test_data$HOF)
train_data <- train_data[, setdiff(names(train_data), c("HOF", "Player", "Pos")), with = FALSE]
test_data <- test_data[, setdiff(names(test_data), c("HOF", "Player", "Pos")), with = FALSE]

negative_count <- sum(train_label == 0)  # 負訓練樣本數
negative_count
positive_count <- sum(train_label == 1)  # 正訓練樣本數
positive_count
T_negative_count <- sum(test_label == 0)  # 負測試樣本數
T_negative_count
T_positive_count <- sum(test_label == 1)  # 正測試樣本數
T_positive_count

# 轉換資料為 xgboost 的 DMatrix 格式
dtrain <- xgb.DMatrix(data = as.matrix(train_data), label = train_label)
dtest <- xgb.DMatrix(data = as.matrix(test_data), label = test_label)

# 設定 xgboost 的參數
XGB_params <- list(
  booster = "gbtree",
  objective = "binary:logistic",  # 假設是二元分類
  eval_metric = "logloss",
  eta = 0.1,  # 學習率
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# 進行 Boosting 訓練
watchlist <- list(train = dtrain, test = dtest)
XGB_model <- xgb.train(
  XGB_params = XGB_params,
  data = dtrain,
  nrounds = 200,
  watchlist = watchlist,
  early_stopping_rounds = 20,  # 若測試集表現不再提升，提早停止
  verbose = 1
)

# 使用測試集進行預測
predictions <- predict(XGB_model, dtest)
predicted_labels <- ifelse(predictions > 0.5, 1, 0)  # 將機率轉換為類別

# 評估結果
confusionMatrix(as.factor(predicted_labels), as.factor(test_label))
precision <- posPredValue(as.factor(predicted_labels), as.factor(test_label), positive="1")
recall <- sensitivity(as.factor(predicted_labels), as.factor(test_label), positive="1")
F1 <- (2 * precision * recall) / (precision + recall)
precision
recall
F1

# ROC curve
roc_curve_boosting <- roc(test_label, predicted_labels)
plot(roc_curve_boosting, main = "ROC Curve of XGBoost", col = "blue", lwd = 2, print.auc = TRUE, auc.polygon = TRUE, legacy.axes = TRUE, smooth = TRUE, print.thres = TRUE, print.thres.cex = 0.8)

# 顯示變量重要性
# 排除不相關的變量（"HOF", "Player", "Pos"）
irrelevant_features <- c("HOF", "Player", "Pos")
feature_names <- setdiff(colnames(train_data), irrelevant_features)
# 計算特徵重要性
importance_matrix <- xgb.importance(feature_names = feature_names, model = XGB_model)
# 顯示特徵重要性
print(importance_matrix)
# 可視化特徵重要性
xgb.plot.importance(importance_matrix)

# 110020017 楊哲宇 end


# 110020014 王靖宇 start
library(dplyr)
library(caret)
library(pROC)
nba_data <- read.csv("NBA_players_clean_final_ver2.csv")
# Calculate correlation matrix for numeric columns, handling missing values
correlations <- cor(nba_data %>% select(where(is.numeric)), use = "complete.obs")
# Sort correlations with Hall of Fame (HOF) in descending order
cor_sorted <- sort(correlations["HOF", ], decreasing = TRUE) 
cor_sorted

# Select top 5 correlated predictors
top_predictors <- names(cor_sorted)[2:6]  
print(top_predictors)
# Data with HOF and top correlated variables
important_columns <- c("HOF", top_predictors)
model_data <- nba_data[, important_columns]

# Ensure HOF is a binary factor
model_data$HOF <- as.factor(model_data$HOF)

set.seed(42)
# Create training set by randomly sampling 80% of the data
train_index <- sample(1 : nrow(nba_data), 0.8 * nrow(nba_data))
train_data <- model_data[train_index, ]
# The rest 20% data becomes test data
test_data <- model_data[-train_index, ]

# Logistic regression
formula <- as.formula(paste("HOF ~", paste(top_predictors, collapse = " + ")))
logistic_model <- glm(formula, data = train_data, family = binomial())
test_data$predicted_prob <- predict(logistic_model, newdata = test_data, type = "response")

# ROC curve
roc_curve <- roc(test_data$HOF, test_data$predicted_prob)
plot(roc_curve, main = "ROC Curve of Logistic Regression", col = "blue", lwd = 2, print.auc = TRUE, auc.polygon = TRUE, legacy.axes = TRUE, smooth = TRUE, print.thres = TRUE, print.thres.ces = 0.8)
# The predicted probabilities > 0.5 are classified as 1, and 0 instead 
pred_class <- ifelse(test_data$predicted_prob > 0.5, 1, 0)
# Confusion matrix
confusion_matrix <- table(Prediction = pred_class, Reference = test_data$HOF)
print(confusion_matrix)

# Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy, 4)))
# Precision
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
cat("Precision:", round(precision, 4))
# Recall
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
cat("Recall:", round(recall, 4))
# Precision
f1_score <- (2 * precision * recall) / (precision + recall)
cat("F1 score:", round(f1_score, 4))

# 110020014 王靖宇 end

# 110020036張家榕 start
library(dplyr)
library(e1071)
library(caret)
library(kernlab)
library(ggplot2)
library("pROC")
library("ROCR")
library(iml)
library(DALEX)

## 110020036
raw_data <- read.csv("NBA_players_clean_final_ver2.csv")
data_filtered <- raw_data[, !(names(raw_data) %in% c("Player","From","To"))]
covariates <- names(raw_data)[!(names(raw_data) %in% c("Player", "HOF"))]

# Loop over each covariate and create the corresponding plot
for (covariate in covariates) {
  # Check if the covariate is a factor (categorical) or numeric
  if (is.factor(raw_data[[covariate]]) || is.character(raw_data[[covariate]])) {
    # Plot for categorical covariates
    p <- ggplot(raw_data, aes_string(x = covariate, fill = "factor(HOF)")) + 
      geom_bar(position = "dodge") +
      labs(
        title = paste(covariate, "vs HOF"), 
        x = covariate, 
        y = "Count"
      ) +
      scale_fill_manual(values = c("lightblue", "lightcoral"), labels = c("Not HOF", "HOF"))
  } else {
    # Plot for numeric covariates
    p <- ggplot(raw_data, aes_string(x = "factor(HOF)", y = covariate, fill = "factor(HOF)")) + 
      geom_boxplot() +
      labs(
        title = paste(covariate, "vs HOF"), 
        x = "HOF", 
        y = covariate
      ) +
      scale_fill_manual(values = c("lightblue", "lightcoral"), labels = c("Not HOF", "HOF"))
  }
  
  # Print the plot
  print(p)
}

# Plotting the two features with HOF
ggplot(raw_data, aes(x = PER, y = Pos, color = HOF)) +
  geom_point(size = 3) +
  labs(title = " NBA players: Relation of Player Efficency Rating, Position and HOF") +
  theme_minimal()

# Ploting the pie chart
table(raw_data$HOF)
# Simulated data
hof_data <- data.frame(
  HOF = c("0", "1"),
  Count = c(4874, 149)
)

# Calculate percentages
hof_data$Percentage <- round(hof_data$Count / sum(hof_data$Count) * 100, 1)

# Create a pie chart with both percentage and actual count
ggplot(hof_data, aes(x = "", y = Count, fill = HOF)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +  # Transform to pie chart
  labs(title = "HOF Status, in = 1, not in = 0") +
  theme_void() +  # Remove unnecessary background
  scale_fill_manual(values = c("lightgreen", "coral")) +  # Custom colors
  geom_text(aes(label = paste0(Count, " (", Percentage, "%)")), 
            position = position_stack(vjust = 0.5), size = 5)  # Display count and percentage

#It seem the player that get in HOF at each position has to play at least 10 years generally.

# Normalized the data for SVM
normalized_data <- as.data.frame(scale(raw_data[, !(names(raw_data) %in% c("Player", "HOF","Pos"))]))
normalized_data$Pos <- as.factor(raw_data$Pos)
normalized_data$HOF <- as.factor(raw_data$HOF)

set.seed(42)
train_indices = sample(1:nrow(normalized_data), 0.8 * nrow(normalized_data))
train_data <- normalized_data[train_indices,]
test_data <- normalized_data[-train_indices,]
test_data

# Using linear kernel with cost 1 to train a SVM model
svm_model_linear <-  svm(HOF ~ ., data = train_data, kernel = "linear", cost = 1)
summary(svm_model_linear)
predictions <- predict(svm_model_linear, test_data)
conf_matrix <- confusionMatrix(predictions, test_data$HOF)
conf_matrix

# The accuracy is about 0.99 with is almost 1.

# train control with 5-fold CV
train_control <- trainControl(method = "cv", number = 5)
set.seed(42)
# Train SVM with a linear kernel
# Using 5-fold CV to find the best tuned model with cost 0.001, 0.01, 0.1, 1, 10, 100 and 1000
svm_model_linear_tuned <- train(
  HOF ~ ., 
  data = train_data, 
  method = "svmLinear", 
  trControl = train_control,
  tuneGrid = expand.grid(C = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)),  # Grid search for C
  Metric = "Accuracy"
)
svm_model_linear_tuned$bestTune
svm_model_linear_tuned$results

# As we can see that for this training set, cost = 0.1 is the best setting.

predictions <- predict(svm_model_linear_tuned, test_data)
conf_matrix <- table(predictions, test_data$HOF)
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy: ", round(accuracy, 4)))
TP <- conf_matrix[2, 2]  # True Positives
FP <- conf_matrix[1, 2]  # False Positives
FN <- conf_matrix[2, 1]  # False Negatives
TN <- conf_matrix[1, 1]  # True Negatives

# Calculate metrics
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
F1 <- 2 * (precision * recall) / (precision + recall)

# Create a data frame
metrics <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Value = round(c(accuracy, precision, recall, F1), 6)
)

# Print the metrics table
print(metrics)

set.seed(42)
svm_model <-  svm(HOF ~ ., data = train_data, kernel = "linear", cost = 0.1,decision.values = TRUE)
fitted_opt_train = attributes(predict(svm_model, test_data, decision.values = TRUE))$decision.values
roc_curve <- roc(test_data$HOF, fitted_opt_train)
plot(
  roc_curve,
  main = "ROC Curve of SVM with linear kernel(cost = 0.1)",
  col = "blue",
  lwd = 2,
  print.auc = TRUE,
  auc.polygon = TRUE,
  legacy.axes = TRUE,
  smooth = TRUE,
  print.thres = TRUE,
  print.thres.cex = 0.8  # Ensure this value is specified
)


# Different sigma values (sigma = 1/(2*gamma^2))
tune_grid <- expand.grid(
  .C = c(0.1, 1, 10, 100),   # Different cost values
  .sigma = c(0.01, 0.1, 1, 2)  # Different gamma values
  
)
set.seed(42)
# Train the model using SVM with RBF kernel and 5-fold CV
svm_rbf_model <- train(
  HOF ~ .,               # Formula
  data = normalized_data,                # Dataset
  method = "svmRadial",       # Use RBF kernel
  tuneGrid = tune_grid,       # Hyperparameter grid
  trControl = train_control,  # Cross-validation
  metric = "Accuracy",
  probability = TRUE
)

# Print the best model details
print(svm_rbf_model)

# Plot the results to see the performance across different hyperparameters
plot(svm_rbf_model)

# sigma = 0.01 and cost = 10

predictions <- predict(svm_rbf_model, test_data)
conf_matrix <- table(predictions, test_data$HOF)
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy: ", round(accuracy, 6)))
TP <- conf_matrix[2, 2]  # True Positives
FP <- conf_matrix[1, 2]  # False Positives
FN <- conf_matrix[2, 1]  # False Negatives
TN <- conf_matrix[1, 1]  # True Negatives

# Calculate metrics
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
F1 <- 2 * (precision * recall) / (precision + recall)

# Create a data frame
metrics <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Value = round(c(accuracy, precision, recall, F1), 6)
)

# Print the metrics table
print(metrics)

# the accuracy of the model is 0.994. Which is better than the accuracy of line

set.seed(55)
opt_svm_rbf_model <- svm(HOF~., data = train_data, kernel = "radial", sigma = 0.01, cost = 10,decision.values = TRUE)
fitted_opt_train = attributes(predict(opt_svm_rbf_model, test_data, decision.values = TRUE))$decision.values
roc_curve <- roc(test_data$HOF, fitted_opt_train)
plot(
  roc_curve,
  main = "ROC Curve of SVM with RBF kernel(sigmal = 0.01, cost = 10)",
  col = "blue",
  lwd = 2,
  print.auc = TRUE,
  auc.polygon = TRUE,
  legacy.axes = TRUE,
  smooth = TRUE,
  print.thres = TRUE,
  print.thres.cex = 0.8  # Ensure this value is specified
)

predictor <- Predictor$new(
  model = opt_svm_rbf_model, 
  data = train_data[, -which(names(train_data) == "HOF")],  # Exclude the response variable
  y = train_data$HOF
)

# Compute feature importance
feature_importance <- FeatureImp$new(predictor, loss = "ce")  # Use cross-entropy for classification

# Plot the results
plot(feature_importance)

## 110020036 end

#110020036 張家榕 end