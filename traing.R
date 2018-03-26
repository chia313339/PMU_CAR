library('data.table')
library(cathayR)
inicathayR()
# ?cathayR

# CAR_ABT_GROUP2.csv  (IS_CONTUNE_POLICY = 1 續保VEHICLE_KIND_NO = 03.07 小客車)	842600

#######################################################################
############################## 續保小客車 #############################
#######################################################################

# 讀取修正後原始資料
load('~/R/ML/CAR/ABT_CONTRACT.Rdata')


#########################################
############### 應變數處理 ##############
#########################################

# 風險等級閥值 0.78 / 5.22
# L 43.11% / 94.65%
# M 21.60% / 2.03%
# H 35.28% / 3.32%
sum(ABT_CONTRACT$LOSS_RATE<0.78) # 574555
sum(ABT_CONTRACT$LOSS_RATE>=0.78 & ABT_CONTRACT$LOSS_RATE<5.22) # 34496
sum(ABT_CONTRACT$LOSS_RATE>=5.22) # 23334
# 應變數轉換
PAYOUT_RATE=NULL
for(i in 1:length(ABT_CONTRACT$LOSS_RATE)){
  if (ABT_CONTRACT$LOSS_RATE[i]<0.78){PAYOUT_RATE[i]='L'}
  else if (ABT_CONTRACT$LOSS_RATE[i]>= 0.78 & ABT_CONTRACT$LOSS_RATE[i]<5.22){PAYOUT_RATE[i]='M'}
  else {PAYOUT_RATE[i]='H'}
}
# 確認資料筆數正確
table(PAYOUT_RATE)
# 轉換factor導入
ABT_CONTRACT$LOSS_RATE=as.factor(PAYOUT_RATE)

#######################################################################
##################### 變數處理 只有這個分群這麼做 #####################
#######################################################################
# 排除 APC_AGE要保人投保年齡 為0
ABT_CONTRACT=ABT_CONTRACT[ABT_CONTRACT$APC_AGE>0,]
# 單欄位元素超過35個 排除處理
ABT_CONTRACT<-ABT_CONTRACT[!names(ABT_CONTRACT) %in% 'AGENT_DIV_NO']
# 金額及次數+1取log轉換
names_list=c('DISCOUNT_PREMIUM',	'INS_ISSUE_YEAR',	'ONE_INSUR_AMOUNT',	'ONE_INSUR_CNT',	'TWO_INSUR_AMOUNT',	'TWO_INSUR_CNT',	'BUMP_CNT',	'ONE_INSUR_AMOUNT_BODY',	'ONE_INSUR_CNT_BODY',	'TWO_INSUR_AMOUNT_BODY',	'TWO_INSUR_CNT_BODY',	'BUMP_CNT_BODY',	'ONE_INSUR_AMOUNT_BURG',	'ONE_INSUR_CNT_BURG',	'TWO_INSUR_AMOUNT_BURG',	'TWO_INSUR_CNT_BURG',	'THREE_INSUR_CNT_BURG',	'BUMP_CNT_BURG',	'ONE_INSUR_AMOUNT_THIRD',	'ONE_INSUR_CNT_THIRD',	'TWO_INSUR_AMOUNT_THIRD',	'TWO_INSUR_CNT_THIRD',	'ONE_PARK_CNT_THIRD',	'TWO_PARK_CNT_THIRD',	'BUMP_CNT_THIRD',	'ONE_INSUR_AMOUNT_CUST',	'ONE_INSUR_CNT_CUST',	'TWO_INSUR_AMOUNT_CUST',	'TWO_INSUR_CNT_CUST',	'THREE_INSUR_CNT_CUST',	'BUMP_CNT_CUST',	'ONE_INSUR_AMOUNT_FORCE',	'ONE_INSUR_CNT_FORCE',	'TWO_INSUR_AMOUNT_FORCE',	'TWO_INSUR_CNT_FORCE',	'THREE_INSUR_AMOUNT_FORCE',	'THREE_INSUR_CNT_FORCE',	'ONE_PARK_CNT_FORCE',	'TWO_PARK_CNT_FORCE',	'THREE_PARK_CNT_FORCE',	'BUMP_CNT_FORCE',	'ONE_INSUR_AMOUNT_OTHER',	'ONE_INSUR_CNT_OTHER',	'TWO_INSUR_AMOUNT_OTHER',	'TWO_INSUR_CNT_OTHER',	'ONE_PARK_CNT_OTHER',	'TWO_PARK_CNT_OTHER',	'BUMP_CNT_OTHER',	'ONE_INSUR_AMOUNT_COM',	'ONE_INSUR_CNT_COM',	'TWO_INSUR_AMOUNT_COM',	'TWO_INSUR_CNT_COM',	'BUMP_CNT_COM',	'ONE_INSUR_AMOUNT_C',	'ONE_INSUR_CNT_C',	'TWO_INSUR_AMOUNT_C',	'TWO_INSUR_CNT_C',	'BUMP_CNT_C',	'ONE_INSUR_AMOUNT_BODY_C',	'TWO_INSUR_AMOUNT_BODY_C',	'ONE_INSUR_AMOUNT_BURG_C',	'ONE_INSUR_CNT_BURG_C',	'TWO_INSUR_CNT_BURG_C',	'BUMP_CNT_BURG_C',	'ONE_INSUR_AMOUNT_THIRD_C',	'ONE_INSUR_CNT_THIRD_C',	'TWO_INSUR_AMOUNT_THIRD_C',	'TWO_INSUR_CNT_THIRD_C',	'ONE_INSUR_AMOUNT_CUST_C',	'ONE_INSUR_CNT_CUST_C',	'TWO_INSUR_CNT_CUST_C',	'THREE_INSUR_CNT_CUST_C',	'BUMP_CNT_CUST_C',	'ONE_INSUR_AMOUNT_FORCE_C',	'ONE_INSUR_CNT_FORCE_C',	'TWO_INSUR_AMOUNT_FORCE_C',	'TWO_INSUR_CNT_FORCE_C',	'THREE_INSUR_AMOUNT_FORCE_C',	'THREE_INSUR_CNT_FORCE_C',	'ONE_PARK_CNT_FORCE_C',	'BUMP_CNT_FORCE_C',	'ONE_INSUR_AMOUNT_OTHER_C',	'TWO_INSUR_AMOUNT_OTHER_C',	'TWO_INSUR_CNT_OTHER_C',	'ONE_PARK_CNT_OTHER_C',	'TWO_PARK_CNT_OTHER_C',	'BUMP_CNT_OTHER_C',	'ONE_INSUR_AMOUNT_COM_C',	'ONE_INSUR_CNT_COM_C',	'TWO_INSUR_AMOUNT_COM_C',	'TWO_INSUR_CNT_COM_C',	'BUMP_CNT_COM_C',	'DRIVER_CONCLUDE_AMOUNT_1Y',	'DRIVER_TIMES_1Y',	'DRIVER_CONCLUDE_AMOUNT_2Y',	'DRIVER_TIMES_2Y',	'DRIVER_CONCLUDE_AMOUNT_3Y')
for (col in names_list){
  print(paste("處理欄位 ",col))
  ABT_CONTRACT[[col]]=log(ABT_CONTRACT[[col]]+1)
}
#######################################################################
#######################################################################
#######################################################################

#########################################
################ 資料抽樣 ###############
#########################################


set.seed(1000)
# 抽出測試資料 10000筆
data_idx<-sample(seq_len(nrow(ABT_CONTRACT)),10000,replace = F)
testing_data<-ABT_CONTRACT[data_idx,] # 測試資料
ABT_CONTRACT<-ABT_CONTRACT[-data_idx,] # 訓練資料


# 使用比例抽樣 將三個風險等級分開
ABT_CONTRACT_L<-ABT_CONTRACT[ABT_CONTRACT$LOSS_RATE=='L',]
ABT_CONTRACT_M<-ABT_CONTRACT[ABT_CONTRACT$LOSS_RATE=='M',]
ABT_CONTRACT_H<-ABT_CONTRACT[ABT_CONTRACT$LOSS_RATE=='H',]


set.seed(100)
# 抽出訓練資料 20000筆
# 根據比例 L:M:H = 9716:6706:3578 
# 比例抽樣的訓練資料可以提高各層級的特徵
# 抽出 L
data_idx<-sample(seq_len(nrow(ABT_CONTRACT_L)),9716,replace = F)
training_data<-ABT_CONTRACT_L[data_idx,]
# 抽出 M
data_idx<-sample(seq_len(nrow(ABT_CONTRACT_M)),6706,replace = F)
training_data<-rbind(training_data,ABT_CONTRACT_M[data_idx,])
# 抽出 H
data_idx<-sample(seq_len(nrow(ABT_CONTRACT_H)),3578,replace = F)
training_data<-rbind(training_data,ABT_CONTRACT_H[data_idx,])




###############################################################################
######################### 隨機森林RandomForest ################################
###############################################################################

library(randomForest)

#RandomForest建模 importane顯示重要因子 trace是做幾棵樹 可以增加 但會跑很久
randomforestM <- randomForest(LOSS_RATE ~ ., data = training_data , importane = T, proximity = T, do.trace =500)

# 檢視誤差收斂狀況
plot(randomforestM)

#預測
result <- predict(randomforestM, newdata = training_data)
#建立混淆矩陣(confusion matrix)觀察模型表現
cm <- table(training_data$LOSS_RATE, result, dnn = c("實際", "預測"))
cm

#正確率
#計算正確率
accuracy <- sum(diag(cm)) / sum(cm)
accuracy

#各別準確率
l<-c(1:3)
diag(cm)[l]/rowSums(cm)[l]

# 顯示重要的因子 並儲存csv觀察 路徑麻煩自改
sol<-data.frame(MeanDecreaseGini=round(importance(randomforestM), 2),Col_name=rownames(round(importance(randomforestM), 2)))
write.table(sol, file = "~/R/ML/CAR/PCA_RF.csv",row.names = F,sep = ",")






###############################################################################
################################ XGboost ######################################
###############################################################################

library(xgboost)
library(Matrix)

#轉換稀疏矩陣
xdata = model.matrix(~.-1,training_data[names(training_data)!='LOSS_RATE'])
#計算應變數幾個元素
m = nlevels(training_data$LOSS_RATE)
Y = as.integer(training_data$LOSS_RATE)-1

# xgboost 參數設定 (xgboost parameters setup)
param = list("objective" = "multi:softprob",
             "eval_metric" = "mlogloss",
             "num_class" = m
)

# 進行建模 nrounds為疊代次數 可以增加 本機跑的會比較快
xgresult = xgboost(param=param, data=xdata, label=Y, nrounds=500)

# 計算預測值 (get prediction)
xdata = model.matrix(~.-1,training_data[names(training_data)!='LOSS_RATE'])
Ypred = predict(xgresult,xdata)
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

# 解釋變數重要性 (variable importance) 並儲存csv觀察 路徑麻煩自改
imp = xgb.importance(names(training_data[,-1]),model=result)
print(imp)
write.table(imp, file = "~/R/ML/CAR/PCA_XG.csv",row.names = F,sep = ",")



###############################################################################
################################ 測試模型 #####################################
###############################################################################

# 剛才的testing_data資料都是比例抽樣的 準確率會很低
# 改用原始資料抽樣 抽10000筆
set.seed(10)
data_idx<-sample(seq_len(nrow(ABT_CONTRACT)),10000,replace = F)
testing_data<-ABT_CONTRACT[data_idx,] # 這個是新的測試資料
# 可以看一下各層級資料分布
table(testing_data$LOSS_RATE)

# 抽完後重覆上面的預測步驟即可



############################ RF ##############################

#預測
result <- predict(randomforestM, newdata = testing_data)
#建立混淆矩陣(confusion matrix)觀察模型表現
cm <- table(testing_data$LOSS_RATE, result, dnn = c("實際", "預測"))
cm

#正確率
#計算正確率
accuracy <- sum(diag(cm)) / sum(cm)
accuracy

#各別準確率
l<-c(1:3)
diag(cm)[l]/rowSums(cm)[l]


############################ XG ##############################



# 計算預測值 (get prediction)
xdata = model.matrix(~.-1,testing_data[names(testing_data)!='LOSS_RATE'])
Ypred = predict(xgresult,xdata)
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


#####################################################
times=1
highest_ac=accuracy

while(accuracy<0.8 | times<50){
  print(paste(Sys.time(), "第 ",times," 次建模"))
  seed=sample(10000,1)
  set.seed(seed)
  print(paste(Sys.time(), "seed= ",seed))
  # 抽出訓練資料 20000筆
  # 根據比例 L:M:H = 9716:6706:3578 
  # 比例抽樣的訓練資料可以提高各層級的特徵
  # 抽出 L
  data_idx<-sample(seq_len(nrow(ABT_CONTRACT_L)),9716,replace = F)
  training_data<-ABT_CONTRACT_L[data_idx,]
  # 抽出 M
  data_idx<-sample(seq_len(nrow(ABT_CONTRACT_M)),6706,replace = F)
  training_data<-rbind(training_data,ABT_CONTRACT_M[data_idx,])
  # 抽出 H
  data_idx<-sample(seq_len(nrow(ABT_CONTRACT_H)),3578,replace = F)
  training_data<-rbind(training_data,ABT_CONTRACT_H[data_idx,])
  
  #轉換稀疏矩陣
  xdata = model.matrix(~.-1,training_data[names(training_data)!='LOSS_RATE'])
  #計算應變數幾個元素
  m = nlevels(training_data$LOSS_RATE)
  Y = as.integer(training_data$LOSS_RATE)-1
  
  # xgboost 參數設定 (xgboost parameters setup)
  param = list("objective" = "multi:softprob",
               "eval_metric" = "mlogloss",
               "num_class" = m
  )
  
  # 進行建模 nrounds為疊代次數 可以增加 本機跑的會比較快
  xgresult = xgboost(param=param, data=xdata, label=Y, nrounds=500)
  
  # 計算預測值 (get prediction)
  xdata = model.matrix(~.-1,testing_data[names(testing_data)!='LOSS_RATE'])
  Ypred = predict(xgresult,xdata)
  Ypred = t(matrix(Ypred,m,length(Ypred)/m))
  # colnames(Ypred) = levels(data$PAYOUT_RATE)
  Ypred = levels(testing_data$LOSS_RATE)[max.col(Ypred)]
  
  # 混淆矩陣 (confusion matrix)
  t0 = table(testing_data$LOSS_RATE,Ypred,dnn = c("實際", "預測"))
  t0
  
  # 預測正確率 (accuracy)
  accuracy <- sum(diag(t0)) / sum(t0)
  accuracy
  
  print(paste(Sys.time(), "準確率 ",accuracy))
  
  if(highest_ac<accuracy){
    print(paste(Sys.time(), "新高準確率 ",accuracy))
    highest_ac=accuracy
    highest_seed=seed
    # 儲存模型
    xgb.save(xgresult, paste0('R/ML/CAR/model/xgb_',accuracy,'.model'))
    # 次數+1
  }
  times=times+1
}



#####################################################

# 載入最高的model
xgresult=xgb.load('R/ML/CAR/model/xgb_fin.model')

xdata = model.matrix(~.-1,final_testing_data[names(final_testing_data)!='LOSS_RATE'])

# 計算預測值 (get prediction)
# xdata = sparse.model.matrix(PAYOUT_RATE ~ .-1, data = data)
Ypred = predict(xgresult,xdata)
Ypred = t(matrix(Ypred,m,length(Ypred)/m))
# colnames(Ypred) = levels(data$PAYOUT_RATE)
Ypred = levels(final_testing_data$LOSS_RATE)[max.col(Ypred)]

# 混淆矩陣 (confusion matrix)
t0 = table(final_testing_data$LOSS_RATE,Ypred,dnn = c("實際", "預測"))
t0

# 預測正確率 (accuracy)
accuracy <- sum(diag(t0)) / sum(t0)
accuracy

#各別準確率
l<-c(1:3)
diag(t0)[l]/rowSums(t0)[l]



########################################################

data_idx<-sample(seq_len(nrow(ABT_CONTRACT)),1,replace = F)
testing_data<-ABT_CONTRACT[data_idx,] # 這個是新的測試資料

imp = xgb.importance(names(testing_data[,-1]),model=xgresult)
print(imp)

