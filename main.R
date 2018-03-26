library('data.table')
library(cathayR)
inicathayR()
# ?cathayR

# CAR_ABT_GROUP2.csv	(IS_CONTUNE_POLICY = 1 ��OVEHICLE_KIND_NO = 03.07 �p�Ȩ�)	842600


#######################################################################
############################## ��O�p�Ȩ� #############################
#######################################################################



#########################################
############### ��ƫe�B�z ##############
#########################################

# Ū�����
ABT_CONTRACT <- fread('~/R/ML/CAR/CAR_ABT_GROUP2.csv', header = TRUE,na.strings="")
# �[������292����� �PABT�۲�
dim(ABT_CONTRACT)
# ²�汴��
str(ABT_CONTRACT)
names(ABT_CONTRACT)

# ���W�ٲΤ@��j�g ���B�J�ܭ��n �קK�䤣�����
colnames(ABT_CONTRACT)<-toupper(names(ABT_CONTRACT))
# �����2016�~����� 632385��
ABT_CONTRACT=ABT_CONTRACT[which(START_DATETIME<'2017-01-01'),]


#########################################
################ ��Ʊ��� ###############
#########################################

# �]���w�g�@�L�@�� �u�ݸ��J���ɵ��G�Y�i
# DM_ABT_CONTRACT <- autoDataMining(ABT_CONTRACT)
# Ū���ץ��ᱴ�ɵ��G
DM_ABT_CONTRACT<-read.csv('~/R/ML/CAR/data_mining_contract.csv', header = TRUE)
# ����κA�ഫ
ABT_CONTRACT <- autoTypeConv(ABT_CONTRACT,DM_ABT_CONTRACT$COLNAME,DM_ABT_CONTRACT$TYPE)
# �A���T�O�S���������� �çR���L
nacol_list<-as.character(findNullCol(ABT_CONTRACT))
nacol_list # ������ݧR��"OCCU_CATCODE_INS" "OCCU_CATCODE_APC"
remove<-names(ABT_CONTRACT) %in% nacol_list
ABT_CONTRACT=data.frame(ABT_CONTRACT) # �����ഫ�~�ઽ���R��
ABT_CONTRACT<-ABT_CONTRACT[!remove]

#########################################
################ �ɯʥ��� ###############
#########################################

# �}�l�i��ʥ��ȳB�z �Y���ݤH�u�ɭȪ������Х���B�z
# ��ѤU���@����� �ϥδ���ʥ���Ƹɭ� ����Ƥ]�|�N�r��Τ����찵�R���B�z
ABT_CONTRACT<-autoTransMissValue(ABT_CONTRACT)
# �ɧ��ȫ�N������factor���O�P�@���������簣 �o�˪������R�S���N�q
# �P�˨ϥδ���𰣨�ƳB�z
ABT_CONTRACT<-removeRepFac(ABT_CONTRACT)
# VIEW�@�U��Ʀ��L���D
head(ABT_CONTRACT,5)
# ���Ƴ�261��

# ��X���צ@�u�����
CollineList<-findCollineList(ABT_CONTRACT,0.9)
# �������צ@�u�����
remove<-names(ABT_CONTRACT) %in% as.character(CollineList$column)
ABT_CONTRACT<-ABT_CONTRACT[!remove]
# ���Ƴ�145��

#########################################
# ��J��k �R��3�~�H�W�O��
CollineList<-c('FIVE_INSUR_AMOUNT','FIVE_INSUR_CNT','FIVE_INSUR_AMOUNT_BODY','FIVE_INSUR_CNT_BODY','FIVE_INSUR_AMOUNT_BURG','FIVE_INSUR_CNT_BURG','FIVE_INSUR_AMOUNT_THIRD','FIVE_INSUR_CNT_THIRD','FIVE_INSUR_AMOUNT_CUST','FIVE_INSUR_CNT_CUST','FIVE_INSUR_AMOUNT_FORCE','FIVE_INSUR_CNT_FORCE','FIVE_INSUR_AMOUNT_OTHER','FIVE_INSUR_CNT_OTHER','FIVE_INSUR_AMOUNT_COM','FIVE_INSUR_CNT_COM','FIVE_INSUR_AMOUNT_C','FIVE_INSUR_CNT_C','FIVE_INSUR_AMOUNT_BODY_C','FIVE_INSUR_CNT_BODY_C','FIVE_INSUR_AMOUNT_BURG_C','FIVE_INSUR_CNT_BURG_C','FIVE_INSUR_AMOUNT_THIRD_C','FIVE_INSUR_CNT_THIRD_C','FIVE_INSUR_AMOUNT_CUST_C','FIVE_INSUR_CNT_CUST_C','FIVE_INSUR_AMOUNT_FORCE_C','FIVE_INSUR_CNT_FORCE_C','FIVE_INSUR_AMOUNT_OTHER_C','FIVE_INSUR_CNT_OTHER_C','FIVE_INSUR_AMOUNT_COM_C','FIVE_INSUR_CNT_COM_C','FOUR_INSUR_AMOUNT','FOUR_INSUR_CNT','FOUR_INSUR_AMOUNT_BODY','FOUR_INSUR_CNT_BODY','FOUR_INSUR_AMOUNT_BURG','FOUR_INSUR_CNT_BURG','FOUR_INSUR_AMOUNT_THIRD','FOUR_INSUR_CNT_THIRD','FOUR_INSUR_AMOUNT_CUST','FOUR_INSUR_CNT_CUST','FOUR_INSUR_AMOUNT_FORCE','FOUR_INSUR_CNT_FORCE','FOUR_INSUR_AMOUNT_OTHER','FOUR_INSUR_CNT_OTHER','FOUR_INSUR_AMOUNT_COM','FOUR_INSUR_CNT_COM','FOUR_INSUR_AMOUNT_C','FOUR_INSUR_CNT_C','FOUR_INSUR_AMOUNT_BODY_C','FOUR_INSUR_CNT_BODY_C','FOUR_INSUR_AMOUNT_BURG_C','FOUR_INSUR_CNT_BURG_C','FOUR_INSUR_AMOUNT_THIRD_C','FOUR_INSUR_CNT_THIRD_C','FOUR_INSUR_AMOUNT_CUST_C','FOUR_INSUR_CNT_CUST_C','FOUR_INSUR_AMOUNT_FORCE_C','FOUR_INSUR_CNT_FORCE_C','FOUR_INSUR_AMOUNT_OTHER_C','FOUR_INSUR_CNT_OTHER_C','FOUR_INSUR_AMOUNT_COM_C','FOUR_INSUR_CNT_COM_C')
# ����3�~�H�W�O��
remove<-names(ABT_CONTRACT) %in% CollineList
ABT_CONTRACT<-ABT_CONTRACT[!remove]
# ���Ƴ�197��

#########################################
############### ���ܼƳB�z ##############
#########################################

# ���I���Ż֭� 0.78 / 5.22
# L 43.11% / 94.65%
# M 21.60% / 2.03%
# H 35.28% / 3.32%
sum(ABT_CONTRACT$LOSS_RATE<0.78) # 574555
sum(ABT_CONTRACT$LOSS_RATE>=0.78 & ABT_CONTRACT$LOSS_RATE<5.22) # 34496
sum(ABT_CONTRACT$LOSS_RATE>=5.22) # 23334
# ���ܼ��ഫ
PAYOUT_RATE=NULL
for(i in 1:length(ABT_CONTRACT$LOSS_RATE)){
  if (ABT_CONTRACT$LOSS_RATE[i]<0.78){PAYOUT_RATE[i]='L'}
  else if (ABT_CONTRACT$LOSS_RATE[i]>= 0.78 & ABT_CONTRACT$LOSS_RATE[i]<5.22){PAYOUT_RATE[i]='M'}
  else {PAYOUT_RATE[i]='H'}
}
# �T�{��Ƶ��ƥ��T
table(PAYOUT_RATE)
# �ഫfactor�ɤJ
ABT_CONTRACT$LOSS_RATE=as.factor(PAYOUT_RATE)

#########################################
################ ��Ʃ�� ###############
#########################################

# �x�s�ץ����l���
# save.image('~/R/ML/CAR/ABT_CONTRACT.Rdata')
# Ū���ץ����l���
# load('~/R/ML/CAR/ABT_CONTRACT.Rdata')

######################## �ܼƳB�z ###############################
# �ư� APC_AGE�n�O�H��O�~�� ��0
ABT_CONTRACT=ABT_CONTRACT[ABT_CONTRACT$APC_AGE>0,]
# ����줸���W�L35�� �ư��B�z
ABT_CONTRACT<-ABT_CONTRACT[!names(ABT_CONTRACT) %in% 'AGENT_DIV_NO']
# ���B�Φ���+1��log�ഫ
names_list=c('DISCOUNT_PREMIUM',	'INS_ISSUE_YEAR',	'ONE_INSUR_AMOUNT',	'ONE_INSUR_CNT',	'TWO_INSUR_AMOUNT',	'TWO_INSUR_CNT',	'BUMP_CNT',	'ONE_INSUR_AMOUNT_BODY',	'ONE_INSUR_CNT_BODY',	'TWO_INSUR_AMOUNT_BODY',	'TWO_INSUR_CNT_BODY',	'BUMP_CNT_BODY',	'ONE_INSUR_AMOUNT_BURG',	'ONE_INSUR_CNT_BURG',	'TWO_INSUR_AMOUNT_BURG',	'TWO_INSUR_CNT_BURG',	'THREE_INSUR_CNT_BURG',	'BUMP_CNT_BURG',	'ONE_INSUR_AMOUNT_THIRD',	'ONE_INSUR_CNT_THIRD',	'TWO_INSUR_AMOUNT_THIRD',	'TWO_INSUR_CNT_THIRD',	'ONE_PARK_CNT_THIRD',	'TWO_PARK_CNT_THIRD',	'BUMP_CNT_THIRD',	'ONE_INSUR_AMOUNT_CUST',	'ONE_INSUR_CNT_CUST',	'TWO_INSUR_AMOUNT_CUST',	'TWO_INSUR_CNT_CUST',	'THREE_INSUR_CNT_CUST',	'BUMP_CNT_CUST',	'ONE_INSUR_AMOUNT_FORCE',	'ONE_INSUR_CNT_FORCE',	'TWO_INSUR_AMOUNT_FORCE',	'TWO_INSUR_CNT_FORCE',	'THREE_INSUR_AMOUNT_FORCE',	'THREE_INSUR_CNT_FORCE',	'ONE_PARK_CNT_FORCE',	'TWO_PARK_CNT_FORCE',	'THREE_PARK_CNT_FORCE',	'BUMP_CNT_FORCE',	'ONE_INSUR_AMOUNT_OTHER',	'ONE_INSUR_CNT_OTHER',	'TWO_INSUR_AMOUNT_OTHER',	'TWO_INSUR_CNT_OTHER',	'ONE_PARK_CNT_OTHER',	'TWO_PARK_CNT_OTHER',	'BUMP_CNT_OTHER',	'ONE_INSUR_AMOUNT_COM',	'ONE_INSUR_CNT_COM',	'TWO_INSUR_AMOUNT_COM',	'TWO_INSUR_CNT_COM',	'BUMP_CNT_COM',	'ONE_INSUR_AMOUNT_C',	'ONE_INSUR_CNT_C',	'TWO_INSUR_AMOUNT_C',	'TWO_INSUR_CNT_C',	'BUMP_CNT_C',	'ONE_INSUR_AMOUNT_BODY_C',	'TWO_INSUR_AMOUNT_BODY_C',	'ONE_INSUR_AMOUNT_BURG_C',	'ONE_INSUR_CNT_BURG_C',	'TWO_INSUR_CNT_BURG_C',	'BUMP_CNT_BURG_C',	'ONE_INSUR_AMOUNT_THIRD_C',	'ONE_INSUR_CNT_THIRD_C',	'TWO_INSUR_AMOUNT_THIRD_C',	'TWO_INSUR_CNT_THIRD_C',	'ONE_INSUR_AMOUNT_CUST_C',	'ONE_INSUR_CNT_CUST_C',	'TWO_INSUR_CNT_CUST_C',	'THREE_INSUR_CNT_CUST_C',	'BUMP_CNT_CUST_C',	'ONE_INSUR_AMOUNT_FORCE_C',	'ONE_INSUR_CNT_FORCE_C',	'TWO_INSUR_AMOUNT_FORCE_C',	'TWO_INSUR_CNT_FORCE_C',	'THREE_INSUR_AMOUNT_FORCE_C',	'THREE_INSUR_CNT_FORCE_C',	'ONE_PARK_CNT_FORCE_C',	'BUMP_CNT_FORCE_C',	'ONE_INSUR_AMOUNT_OTHER_C',	'TWO_INSUR_AMOUNT_OTHER_C',	'TWO_INSUR_CNT_OTHER_C',	'ONE_PARK_CNT_OTHER_C',	'TWO_PARK_CNT_OTHER_C',	'BUMP_CNT_OTHER_C',	'ONE_INSUR_AMOUNT_COM_C',	'ONE_INSUR_CNT_COM_C',	'TWO_INSUR_AMOUNT_COM_C',	'TWO_INSUR_CNT_COM_C',	'BUMP_CNT_COM_C',	'DRIVER_CONCLUDE_AMOUNT_1Y',	'DRIVER_TIMES_1Y',	'DRIVER_CONCLUDE_AMOUNT_2Y',	'DRIVER_TIMES_2Y',	'DRIVER_CONCLUDE_AMOUNT_3Y')
for (col in names_list){
  print(paste("�B�z��� ",col))
  ABT_CONTRACT[[col]]=log(ABT_CONTRACT[[col]]+1)
}
#######################################################################


# �ϥΤ�ҩ�� �N�T�ӭ��I���Ť��}
ABT_CONTRACT_L<-ABT_CONTRACT[ABT_CONTRACT$LOSS_RATE=='L',]
ABT_CONTRACT_M<-ABT_CONTRACT[ABT_CONTRACT$LOSS_RATE=='M',]
ABT_CONTRACT_H<-ABT_CONTRACT[ABT_CONTRACT$LOSS_RATE=='H',]

set.seed(1000)
# ��X���ո�� 10000��
# �ھڤ�� L:M:H = 4311:2161:3528 
# �ھڤ�� L:M:H = 5000:3000:2000 
# ��X L
data_idx<-sample(seq_len(nrow(ABT_CONTRACT_L)),replace = F)
testing_data<-ABT_CONTRACT_L[data_idx[1:4311],] # ���ո��
ABT_CONTRACT_L<-ABT_CONTRACT_L[data_idx[4312:nrow(ABT_CONTRACT_L)],] # �V�m���
# ��X M
data_idx<-sample(seq_len(nrow(ABT_CONTRACT_M)),replace = F)
testing_data<-rbind(testing_data,ABT_CONTRACT_M[data_idx[1:2161],]) # ���ո��
ABT_CONTRACT_M<-ABT_CONTRACT_M[data_idx[2162:nrow(ABT_CONTRACT_M)],] # �V�m���
# ��X H
data_idx<-sample(seq_len(nrow(ABT_CONTRACT_H)),replace = F)
testing_data<-rbind(testing_data,ABT_CONTRACT_H[data_idx[1:3528],]) # ���ո��
ABT_CONTRACT_H<-ABT_CONTRACT_H[data_idx[3529:nrow(ABT_CONTRACT_H)],] # �V�m���

# �x�s���ո�� �ݭn�ɤ~�ϥ�
# save.image('~/R/ML/CAR/testing_data.Rdata')

set.seed(100)
# ��X�V�m��� 20000��
# �ھڤ�� L:M:H = 9716:6706:3578 
# ��X L
data_idx<-sample(seq_len(nrow(ABT_CONTRACT_L)),9716,replace = F)
training_data<-ABT_CONTRACT_L[data_idx,]
# ��X M
data_idx<-sample(seq_len(nrow(ABT_CONTRACT_M)),6706,replace = F)
training_data<-rbind(training_data,ABT_CONTRACT_M[data_idx,])
# ��X H
data_idx<-sample(seq_len(nrow(ABT_CONTRACT_H)),3578,replace = F)
training_data<-rbind(training_data,ABT_CONTRACT_H[data_idx,])




###############################################################################
######################### �H���˪LRandomForest ################################
###############################################################################

library(randomForest)

#RandomForest�ؼ� importane��ܭ��n�]�l
randomforestM <- randomForest(LOSS_RATE ~ ., data = training_data , importane = T, proximity = T, do.trace =500)

################## �ɥR�Ϥ@ ######################
#�ѩ�]�l����j��53������ �i��ɭP�t��k����
#��X�j��53�Ӥ������]�l���
col.list<-names(training_data)
sol<-NULL
#��X�]�l�ƥ�53�ӥH�W
for(col_name in col.list){
  if(length(names(table(data[[col_name]])))>53 && class(data[[col_name]])=='factor') sol<-rbind(sol,col_name)
}
sol
##################################################

# ��ܭ��n���]�l ���x�spca_test.csv�[��
sol<-data.frame(MeanDecreaseGini=round(importance(randomforestM), 2),Col_name=rownames(round(importance(randomforestM), 2)))
write.table(sol, file = "~/R/ML/CAR/pca_RF.csv",row.names = F,sep = ",")
# �˵��~�t���Ī��p
plot(randomforestM)

#�w��
result <- predict(randomforestM, newdata = testing_data)
#�إ߲V�c�x�}(confusion matrix)�[��ҫ����{
cm <- table(testing_data$LOSS_RATE, result, dnn = c("���", "�w��"))
cm

#���T�v
#�p�⥿�T�v
accuracy <- sum(diag(cm)) / sum(cm)
accuracy

#�U�O�ǽT�v
l<-c(1:4)
diag(cm)[l]/rowSums(cm)[l]



saveRDS(randomforestM, paste0('R/ML/CAR/model/RF_fin.model'))
# RFM<-readRDS('R/ML/CAR/model/RF_fin.model')

save(randomforestM,file = "R/ML/CAR/model/RF_fin.RData")




###############################################################################
################################ XGboost ######################################
###############################################################################

library(xgboost)
library(Matrix)

#�ഫ�}���x�}
# xdata = sparse.model.matrix(LOSS_RATE ~ .-1, data = training_data)
xdata = model.matrix(~.-1,training_data[names(training_data)!='LOSS_RATE'])
#�p�����ܼƴX�Ӥ���
m = nlevels(training_data$LOSS_RATE)
Y = as.integer(training_data$LOSS_RATE)-1

# xgboost �ѼƳ]�w (xgboost parameters setup)
param = list("objective" = "multi:softprob",
             "eval_metric" = "mlogloss",
             "num_class" = m
)

# �i��ؼ�
result = xgboost(param=param, data=xdata, label=Y, nrounds=500)

# �p��w���� (get prediction)
# xdata = sparse.model.matrix(PAYOUT_RATE ~ .-1, data = data)
Ypred = predict(result,xdata)
Ypred = t(matrix(Ypred,m,length(Ypred)/m))
# colnames(Ypred) = levels(data$PAYOUT_RATE)
Ypred = levels(training_data$LOSS_RATE)[max.col(Ypred)]

# �V�c�x�} (confusion matrix)
t0 = table(training_data$LOSS_RATE,Ypred,dnn = c("���", "�w��"))
t0

# �w�����T�v (accuracy)
accuracy <- sum(diag(t0)) / sum(t0)
accuracy

#�U�O�ǽT�v
l<-c(1:3)
diag(t0)[l]/rowSums(t0)[l]

# �����ܼƭ��n�� (variable importance)
imp = xgb.importance(names(training_data[,-1]),model=result)
print(imp)
write.table(imp, file = "~/R/ML/CAR/pca_xg.csv",row.names = F,sep = ",")


#===================================================================================

data_idx<-sample(seq_len(nrow(ABT_CONTRACT_L)),replace = F)
testing_data<-ABT_CONTRACT_L[data_idx[1:4311],] # ���ո��

xdata = model.matrix(~.-1,testing_data[names(testing_data)!='LOSS_RATE'])

# �p��w���� (get prediction)
# xdata = sparse.model.matrix(PAYOUT_RATE ~ .-1, data = data)
Ypred = predict(result,xdata)
Ypred = t(matrix(Ypred,m,length(Ypred)/m))
# colnames(Ypred) = levels(data$PAYOUT_RATE)
Ypred = levels(testing_data$LOSS_RATE)[max.col(Ypred)]

# �V�c�x�} (confusion matrix)
t0 = table(testing_data$LOSS_RATE,Ypred,dnn = c("���", "�w��"))
t0

# �w�����T�v (accuracy)
accuracy <- sum(diag(t0)) / sum(t0)
accuracy

#�U�O�ǽT�v
l<-c(1:3)
diag(t0)[l]/rowSums(t0)[l]




data_idx<-sample(seq_len(nrow(ABT_CONTRACT)),replace = F)
testing_data<-ABT_CONTRACT[data_idx[1:10000],] # ���ո��

xgb.save(result, paste0('R/ML/CAR/model/xgb_fin.model'))
# XBGM=xgb.load('R/ML/CAR/model/xgb_fin.model')


