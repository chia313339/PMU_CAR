library('data.table')
library(rpart)
library(rpart.plot)
library(cathayR)
inicathayR()
# ?cathayR


#######################################################################
##############################  人的分群 ##############################
#######################################################################

# 讀取 csv
ABT_CUST <- fread('~/R/ML/CAR/ABT_CAR_CUST.csv', header = TRUE)
# 觀察欄位數111個欄位 與ABT相符
dim(ABT_CUST)


#########################################
################ 資料抽樣 ###############
#########################################

set.seed(1000)
n=20000
sample_idx<-sample(seq_len(nrow(ABT_CUST)),n,replace = F)
sample_data<-ABT_CUST[sample_idx]


#########################################
############### 資料前處理 ##############
#########################################

# 欄位名稱統一轉大寫 此步驟很重要 避免找不到欄位
colnames(sample_data)<-toupper(names(sample_data))
# 車資料有中文顯示問題 用轉換 encoding 的方式解決 -> UTF8
sample_data$RISK_FIELD_SOURCE <- iconv(sample_data$RISK_FIELD_SOURCE, 'big5', 'utf8')


#########################################
################ 資料探勘 ###############
#########################################

# 因為已經作過一次 只需載入探勘結果即可
# DM_ABT_CUST <- autoDataMining(sample_data)
# 載入探勘結果
DM_ABT_CUST<-read.csv('~/R/ML/CAR/data_mining_cust.csv', header = TRUE)
# 對所有欄位進行智能型態轉換
sample_data<-autoTypeConv(sample_data,DM_ABT_CUST$COLNAME,DM_ABT_CUST$TYPE)

# 再次確保沒有欄位全為空 並刪除他
col_list<-as.character(findNullCol(sample_data))
remove<-names(sample_data) %in% col_list
#################### 不明原因無法使用 ###########################
data<-sample_data[remove]
#################################################################
# 改作法
del_list<-ifelse(remove==1, index(remove), NA)[!is.na(as.character(ifelse(remove==1, index(remove), NA)))]
del_list
data<-sample_data[,-c(18,51,52,53,54,55,56,57,58,103,104)]
# 保留處理好的資料
data<-sample_data


#########################################
################ 補缺失值 ###############
#########################################

# 開始進行缺失值處理 若有需人工補值的部份請先行處理
# 把剩下的一般欄位 使用智能缺失函數補值 此函數也會將字串及日期欄位做刪除處理
data<-autoTransMissValue(data)
# 補完值後將整個欄位factor都是同一元素的欄位剔除 這樣的欄位分析沒有意義
# 同樣使用智能踢除函數處理
data<-removeRepFac(data)
# VIEW一下資料有無問題
head(data,5)


#########################################
################ 資料分群 ###############
#########################################

# 用智能分群函數顯示分群結果
cluster <- autoHierClust(data,"gower", "ward.D2",2,'Y')
# 將分群結果與資料合併
base_addClust1 <- cbind(data, cluster)
attach(base_addClust1)
# 找出並繪出分群節點
input.dat <- base_addClust1
output.tree <- rpart(cluster ~ .,data = input.dat,method='class',control=rpart.control(cp=0.01))
prp(output.tree,faclen=0,fallen.leaves=TRUE, extra=2, varlen=0, main="ward.D2")



#######################################################################
##############################  車的分群 ##############################
#######################################################################

# 讀取資料
ABT_CAR <- fread('~/R/ML/CAR/CAR_ABT_CAR.csv', header = TRUE)

# 觀察欄位數64個欄位 與ABT相符
dim(ABT_CAR)
#篩選不為機車的資料 因為已在datastage篩選過 所以MOTOR_FLG皆為N
table(ABT_CAR$MOTOR_FLG)
# 觀察保發中心的汽車分類 車體險含有的類別
table(ABT_CAR$VEHICLE_KIND_NO)

### 因為等比例抓取樣本資料，會導致小樣本的汽車分類特徵不明顯，無法有效分群
### 為了解決此問題，將提高小樣本的數量，剩餘汽車分類在依等比例篩選

#########################################
### 特徵放大篩選法 增加小樣本資料筆數 ###
#########################################

set.seed(1000)
# 小樣本的資料全抓
sample_data = ABT_CAR[ABT_CAR$VEHICLE_KIND_NO %in% c('04','06','07','11','15'),]
table(sample_data$VEHICLE_KIND_NO)
# 20000筆樣本剪去已抓的 剩下的案件在隨機篩選(會自動根據母體比例)
n=20000-nrow(sample_data)
tmp_data =  ABT_CAR[ABT_CAR$VEHICLE_KIND_NO %in% c('03','22'),]
tmp_data_idx<-sample(seq_len(nrow(tmp_data)),n,replace = F)
sample_data2<-tmp_data[tmp_data_idx]
# 兩個樣本合併成20000筆資料
sample_data=rbind(sample_data,sample_data2)
# 出來的sample_data有20000筆 且資料分部平均 有利於特徵放大
table(sample_data$VEHICLE_KIND_NO)


#########################################
############### 資料前處理 ##############
#########################################

# 欄位名稱統一轉大寫 此步驟很重要 避免找不到欄位
colnames(sample_data)<-toupper(names(sample_data))
# 車資料有中文顯示問題 用轉換 encoding 的方式解決 -> UTF8
sample_data$KIND_NO <- iconv(sample_data$KIND_NO, 'big5', 'utf8')
sample_data$CAR_NO <- iconv(sample_data$CAR_NO, 'big5', 'utf8')
sample_data$ENGINE_NO <- iconv(sample_data$ENGINE_NO, 'big5', 'utf8')
# 人工修正NEW_CAR_DATE日期空字串
sample_data$NEW_CAR_DATE<-ifelse(sample_data$NEW_CAR_DATE=='',NA,sample_data$NEW_CAR_DATE)


#########################################
################ 資料探勘 ###############
#########################################

# 因為已經作過一次 只需載入探勘結果即可
# DM_ABT_CAR <- autoDataMining(sample_data)
# 載入探勘結果
DM_ABT_CAR<-read.csv('~/R/ML/CAR/data_mining_car.csv', header = TRUE)
# 對所有欄位進行智能型態轉換
sample_data<-autoTypeConv(sample_data,DM_ABT_CAR$COLNAME,DM_ABT_CAR$TYPE)

# 再次確保沒有欄位全為空
findNullCol(sample_data)
# 保留處理好的資料
data<-sample_data


#########################################
################ 補缺失值 ###############
#########################################

# 開始進行缺失值處理 若有需人工補值的部份請先行處理
# 把剩下的一般欄位 使用智能缺失函數補值 此函數也會將字串及日期欄位做刪除處理
data<-autoTransMissValue(data)
# 補完值後將整個欄位factor都是同一元素的欄位剔除 這樣的欄位分析沒有意義
# 同樣使用智能踢除函數處理
data<-removeRepFac(data)
# VIEW一下資料有無問題
head(data,5)


#########################################
################ 資料分群 ###############
#########################################

# 用智能分群函數顯示分群結果
cluster <- autoHierClust(data,"gower", "ward.D2",2,'Y')
# 將分群結果與資料合併
base_addClust1 <- cbind(data, cluster)
attach(base_addClust1)
# 找出並繪出分群節點
input.dat <- base_addClust1
output.tree <- rpart(cluster ~ .,data = input.dat,method='class',control=rpart.control(cp=0.01))
prp(output.tree,faclen=0,fallen.leaves=TRUE, extra=2, varlen=0, main="ward.D2")


