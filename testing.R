# 有處理共線性
# 將建模預測100的資料合併
data=training_data[training_data$LOSS_RATE==Ypred,]
data2=testing_data[testing_data$LOSS_RATE==Ypred,]
data=rbind(data,data2)
# 再將資料拿去重新建模
xdata = model.matrix(~.-1,data[names(data)!='LOSS_RATE'])
#計算應變數幾個元素
m = nlevels(data$LOSS_RATE)
Y = as.integer(data$LOSS_RATE)-1

# xgboost 參數設定 (xgboost parameters setup)
param = list("objective" = "multi:softprob",
             "eval_metric" = "mlogloss",
             "num_class" = m
)
# 進行建模
result = xgboost(param=param, data=xdata, label=Y, nrounds=500)

# 計算預測值 (get prediction)
# xdata = sparse.model.matrix(PAYOUT_RATE ~ .-1, data = data)
Ypred = predict(result,xdata)
Ypred = t(matrix(Ypred,m,length(Ypred)/m))
# colnames(Ypred) = levels(data$PAYOUT_RATE)
Ypred = levels(data$LOSS_RATE)[max.col(Ypred)]

# 混淆矩陣 (confusion matrix)
t0 = table(data$LOSS_RATE,Ypred,dnn = c("實際", "預測"))
t0

# 預測正確率 (accuracy)
accuracy <- sum(diag(t0)) / sum(t0)
accuracy

#各別準確率
l<-c(1:3)
diag(t0)[l]/rowSums(t0)[l]

###########################相較原本訓練資料########################
xdata = model.matrix(~.-1,training_data[names(training_data)!='LOSS_RATE'])
Ypred = predict(result,xdata)
Ypred = t(matrix(Ypred,m,length(Ypred)/m))
# colnames(Ypred) = levels(data$PAYOUT_RATE)
Ypred = levels(training_data$LOSS_RATE)[max.col(Ypred)]
# 混淆矩陣 (confusion matrix)
t0 = table(training_data$LOSS_RATE,Ypred,dnn = c("實際", "預測"))
t0
# 預測正確率 (accuracy)
accuracy <- sum(diag(t0)) / sum(t0)
accuracy
#各別準確率
l<-c(1:3)
diag(t0)[l]/rowSums(t0)[l]

###########################相較原本測試資料########################
xdata = model.matrix(~.-1,testing_data[names(testing_data)!='LOSS_RATE'])
Ypred = predict(result,xdata)
Ypred = t(matrix(Ypred,m,length(Ypred)/m))
# colnames(Ypred) = levels(data$PAYOUT_RATE)
Ypred = levels(testing_data$LOSS_RATE)[max.col(Ypred)]
# 混淆矩陣 (confusion matrix)
t0 = table(testing_data$LOSS_RATE,Ypred,dnn = c("實際", "預測"))
t0
# 預測正確率 (accuracy)
accuracy <- sum(diag(t0)) / sum(t0)
accuracy
#各別準確率
l<-c(1:3)
diag(t0)[l]/rowSums(t0)[l]

###########################隨機抓資料 全隨機########################
table(testing_data$LOSS_RATE)

set.seed(10)

data_idx<-sample(seq_len(nrow(ABT_CONTRACT)),10000,replace = F)
testing_data<-ABT_CONTRACT[data_idx,] # 測試資料

###########################隨機抓資料 比例抽樣########################
# 抽出 L
data_idx<-sample(seq_len(nrow(ABT_CONTRACT_L)),replace = F)
testing_data<-ABT_CONTRACT_L[data_idx[1:4311],] # 測試資料
# 抽出 M
data_idx<-sample(seq_len(nrow(ABT_CONTRACT_M)),replace = F)
testing_data<-rbind(testing_data,ABT_CONTRACT_M[data_idx[1:2161],]) # 測試資料
# 抽出 H
data_idx<-sample(seq_len(nrow(ABT_CONTRACT_H)),replace = F)
testing_data<-rbind(testing_data,ABT_CONTRACT_H[data_idx[1:3528],]) # 測試資料




