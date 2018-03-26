data=ABT_CONTRACT


autoStatTable<-function(data){
  data = data.frame(data)
  result = data.frame()
  print(paste(Sys.time(), "進行變數主成分處理"))
  col_list = names(data)
  for( col in col_list){
    print(paste(Sys.time(), "處理欄位",col))
    if(class(data[[col]]) == 'numeric'){
      # 提取統計量
      stat=as.vector(summary(data[[col]]))
      result = rbind(result,data.frame(COLNAME=col, MIN=stat[1], QU1=stat[2], MID=stat[3], MEAN= stat[4], QU3=stat[5], MAX=stat[6]))
    }
    else if(class(data[[col]]) == 'integer'){
      stat=as.vector(summary(data[[col]]))
      result = rbind(result,data.frame(MIN=stat[1], QU1=stat[2], MID=stat[3], MEAN= stat[4], QU3=stat[5], MAX=stat[6]))
    }
    else {
      next
    }
  }
  print(paste(Sys.time(), "處理完成"))
  return(result)
}


autoDataGrade<-function(data,stattable){
  print(paste(Sys.time(), "進行變數分級處理"))
  stattable = as.data.frame(stattable)
  data_tmp = data[names(data) %in% as.character(stattable$COLNAME)] # 能進行比較的欄位
  col_list = names(data_tmp)
  for(col in col_list){
    print(paste(Sys.time(), "處理欄位",col))
    stat = stattable[stattable$COLNAME==col,]
    if(data_tmp[[col]] <= stat$QU1){
      stattable[stattable$COLNAME==col,'GD'] = "低於其他75%的案件"
    } else if(between(data_tmp[[col]],stat$QU1,stat$MID)){
      stattable[stattable$COLNAME==col,'GD'] = "低於其他50%的案件"
    } else if(between(data_tmp[[col]],stat$MID,stat$QU3)){
      stattable[stattable$COLNAME==col,'GD'] = "高於其他50%的案件"
    } else if(between(data_tmp[[col]],stat$QU3,stat$MAX)){
      stattable[stattable$COLNAME==col,'GD'] = "高於其他75%的案件"
    } else if(data_tmp[[col]] > stat$MAX){
      stattable[stattable$COLNAME==col,'GD'] = "高於其他99%的案件"
    }
  }
  print(paste(Sys.time(), "處理完成"))
  return(stattable)
}

data=testing_data
stattable=a
col='DISCOUNT_PREMIUM'

data_tmp = data[names(data) %in% as.character(stattable$COLNAME)]


STdf=autoStatTable(data)
DGdf=autoDataGrade(data,STdf)

# 載入最高的model
xgresult=xgb.load('R/ML/CAR/model/xgb_fin.model')
m=3
# 計算預測值 (get prediction)
xdata = model.matrix(~.-1,testing_data[names(testing_data)!='LOSS_RATE'])
Ypred = predict(xgresult,xdata)
Ypred = t(matrix(Ypred,m,length(Ypred)/m))
# colnames(Ypred) = levels(data$PAYOUT_RATE)
# Ypred = levels(testing_data$LOSS_RATE)[max.col(Ypred)]
Ypred = c('H','L','M')[max.col(Ypred)]

# 混淆矩陣 (confusion matrix)
t0 = table(testing_data$LOSS_RATE,Ypred,dnn = c("實際", "預測"))
t0

# 預測正確率 (accuracy)
accuracy <- sum(diag(t0)) / sum(t0)
accuracy

#各別準確率
l<-c(1:3)
diag(t0)[l]/rowSums(t0)[l]

imp = xgb.importance(names(testing_data[,-1]),model=xgresult)
print(imp)
write.table(imp, file = "~/R/ML/CAR/pca_xg.csv",row.names = F,sep = ",")

class(testing_data[[imp$Feature]])







