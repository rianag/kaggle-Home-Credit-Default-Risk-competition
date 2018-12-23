
#loading required libraries
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(ROCR)
library(ade4)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(stringr)
library(MASS)
library(car)
library(lift)
library(stringr)
library(plyr)
library(AUC)

## define working directory using the command setwd to the directory where your input files are located
##** Clear environment variables **
rm(list=ls())
setwd("~/old computer/welcome/kaggle/New folder")
###################reading and intitial cleaning of bureau dataset
bureau<-read.csv("bureau.CSV",stringsAsFactors=FALSE)
bureau_balance<-read.csv("bureau_balance.csv",stringsAsFactors=FALSE)
#cleaning bureau_balance
#setting status=X or C to 0
bureau_balance$STATUS[which(bureau_balance$STATUS=="C")]<-0
bureau_balance$STATUS[which(bureau_balance$STATUS=="X")]<-0
bureau_balance$STATUS<-as.numeric(bureau_balance$STATUS)
bureau_balance<-bureau_balance%>%group_by(SK_ID_BUREAU)%>% slice(which.max(STATUS))
colnames(bureau_balance)[colnames(bureau_balance)=="MONTHS_BALANCE"]<-"bureau_MONTHS_BALANCE"
#joining bureau balance and bureau dataset
bureau<-left_join(bureau,bureau_balance, by="SK_ID_BUREAU")
#cleaning bureau
bureau$STATUS[which(is.na(bureau$STATUS))]<-0


length(which(duplicated(bureau$SK_ID_BUREAU)))#<-0
colSums(is.na(bureau))
bureau<-bureau[,-c(3,4,5,6,7,8,15,16)]
bureau<-bureau%>%group_by(SK_ID_CURR)%>%summarise_all(funs(mean), na.rm = TRUE)
rm(bureau_balance)
#####initial cleaning of train dataset
train<-read.csv("application_train.csv",stringsAsFactors=FALSE,na.strings=c("","NA"))

cb<-train[,117:122]

colSums(is.na(cb))
cb$AMT_REQ_CREDIT_BUREAU_DAY[which(is.na(cb$AMT_REQ_CREDIT_BUREAU_DAY))]<-0
cb$AMT_REQ_CREDIT_BUREAU_WEEK[which(is.na(cb$AMT_REQ_CREDIT_BUREAU_WEEK))]<-0
cb$AMT_REQ_CREDIT_BUREAU_MON[which(is.na(cb$AMT_REQ_CREDIT_BUREAU_MON))]<-0
cb$AMT_REQ_CREDIT_BUREAU_QRT[which(is.na(cb$AMT_REQ_CREDIT_BUREAU_QRT))]<-0
cb$AMT_REQ_CREDIT_BUREAU_YEAR[which(is.na(cb$AMT_REQ_CREDIT_BUREAU_YEAR))]<-0
cb$AMT_REQ_CREDIT_BUREAU_HOUR[which(is.na(cb$AMT_REQ_CREDIT_BUREAU_HOUR))]<-0

train[,117:122]<-cb

#########################################################################################
#merging the data file
train<-left_join(train,bureau, by="SK_ID_CURR")

colnames(train)[colnames(train)=="AMT_ANNUITY.y"]<-"AMT_ANNUITY.cb"
colnames(train)[colnames(train)=="AMT_ANNUITY.x"]<-"AMT_ANNUITY.train"
rm(cb, bureau)
###################reading and intitial cleaning of previous_application#########################
previous_application<-read.csv("previous_application.csv",stringsAsFactors=FALSE)
POS_CASH_balance<-read.csv("POS_CASH_balance.csv",stringsAsFactors=FALSE)
colSums(is.na(POS_CASH_balance))
POS_CASH_balance<-POS_CASH_balance%>%group_by(SK_ID_PREV)%>% slice(which.max(SK_DPD))
previous_application<-left_join(previous_application,POS_CASH_balance, by="SK_ID_PREV")
rm(POS_CASH_balance)

################################################################################################
#cleaning installments_payments
installments_payments<-read.csv("installments_payments.csv",stringsAsFactors=FALSE)
installments_payments$DAYS_LATE<-installments_payments$DAYS_ENTRY_PAYMENT-installments_payments$DAYS_INSTALMENT
installments_payments$AMT_PENDING<- installments_payments$AMT_INSTALMENT-installments_payments$AMT_PAYMENT
installments_payments<-installments_payments%>%group_by(SK_ID_PREV)%>% slice(which.max(AMT_PENDING))
previous_application<-left_join(previous_application,installments_payments, by="SK_ID_PREV")
rm(installments_payments)
################################################################################################
#cleaning credit_card_balance
credit_card_balance<-read.csv("credit_card_balance.CSV",stringsAsFactors=FALSE)
credit_card_balance$credit_PENDING<- credit_card_balance$AMT_INST_MIN_REGULARITY-credit_card_balance$AMT_PAYMENT_TOTAL_CURRENT
credit_card_balance<-credit_card_balance%>%group_by(SK_ID_PREV)%>% slice(which.max(credit_PENDING))
previous_application<-left_join(previous_application,credit_card_balance, by="SK_ID_PREV")
rm(credit_card_balance)


################################################################################################


length(which(duplicated(previous_application$SK_ID_PREV)))
colSums(is.na(previous_application))
previous_application<-previous_application%>%group_by(SK_ID_CURR.x)%>%slice(which.max(AMT_PENDING))
train<-left_join(train,previous_application, by=c("SK_ID_CURR"= "SK_ID_CURR.x"))
write.csv(train,"train.csv")
###############################################################################################

rm(list=ls())

final_train<-read.csv("train.csv",stringsAsFactors = F)
final_test<-read.csv("final.csv",stringsAsFactors = F)
final_test$TARGET<-100
final_test<-final_test[names(final_train)]
final<-rbind(final_test,final_train)
rm(final_train,final_test)
final$SK_ID_CURR.y.y <-NULL
final$SK_ID_BUREAU<-NULL
final$SK_ID_CURR.x.x<-NULL
final$X<-NULL
final$SK_ID_CURR.y<-NULL
final$SK_ID_PREV<-NULL
final$SK_ID_CURR<-NULL

#remove columns with same value in all rows other than NAs
final<-final[,sapply(final,  function(x) length(unique(x[!(is.na(x))]))>1)]


final_numeric<-final[,sapply(final,is.numeric)]
final_discrete<-final_numeric[,sapply(final_numeric,  function(x) length(unique(x[!(is.na(x))]))<=5)]
final_discrete$DEF_60_CNT_SOCIAL_CIRCLE<-final$DEF_60_CNT_SOCIAL_CIRCLE
final_discrete$DEF_30_CNT_SOCIAL_CIRCLE<-final$DEF_30_CNT_SOCIAL_CIRCLE
final_numeric<-final_numeric[,!colnames(final_numeric)%in%colnames(final_discrete)]
final_character<-final[,sapply(final,is.character)]
final_discrete<-final_discrete[,-1]
############################################################################################
#OUTLIER TREATMENT

a<- vector(mode="numeric", length=0)
outlier<-function(x)
{
 
  plo<-boxplot.stats(x)
  a<-which(x %in% plo$out )
  if(length(a)!=0)
  {
        for(i in 1:length(a))
      {
            if (x[a[i]]>plo$stats[5])
            {
              x[ a[i]]<-plo$stats[5]}
            else{ x[ a[i]]<-plo$stats[1]}  
            
          }
          
              i<-i+1
      }
  return(x)
  a<- vector(mode="numeric", length=0) 
  
}
######################################################################

for(i in 1:ncol(final_numeric))
  
{  
  final_numeric[,i]<-outlier(final_numeric[,i])
  
  i<-i+1
  
}

#######################################################################################################

####################################################################################################
#scaling numeric dataset
final_numeric_scaled<-scale(final_numeric)
final_numeric_scaled<-data.frame(final_numeric_scaled)
final_numeric_scaled<-final_numeric_scaled[,sapply(final_numeric_scaled,function(x) sum(length(which(is.na(x))))!=307511)]
#####################################################################################
#cor<-data.frame(cor(final_numeric_scaled,use="pairwise.complete.obs"))
#diag(cor)<-0
#for( i in 1:nrow(cor) )
#  {
 # for(j in 1:ncol(cor) )
  #  {
 #   ifelse(cor[i,j]>0.8|cor[i,j]<=-0.8,cor<-cor[-j,-j],cor<-cor)
 # }
#}
#final_numeric_scaled<-final_numeric_scaled[,which(colnames(final_numeric_scaled)%in%colnames(cor))]
##############################################################################################
#missing values imputation
which(colnames(final_numeric_scaled)=="AMT_REQ_CREDIT_BUREAU_DAY")
for(i in 1:62)
{
  final_numeric_scaled[which(is.na(final_numeric_scaled[i])),i]<-median(final_numeric_scaled[,i],na.rm = TRUE)
}
for(i in 63:ncol(final_numeric_scaled))
{
  final_numeric_scaled[which(is.na(final_numeric_scaled[i])),i]<-0
}
######################################################################################################
#####################converting character variables to factor
colSums(is.na(final_character))
char<-final_character


colSums(is.na(char))

char[is.na(char)]<-"not available"
char<-data.frame(sapply(char, function(x) factor(x)))

char<- data.frame(sapply(char, 
                              function(x) data.frame(model.matrix(~x,data =char))[,-1]))

final_discrete[is.na(final_discrete)]<-"not available"
final_discrete<-data.frame(sapply(final_discrete, function(x) factor(x)))

final_discrete<- data.frame(sapply(final_discrete, 
                         function(x) data.frame(model.matrix(~x,data =final_discrete))[,-1]))

loan<- cbind(char,final_numeric_scaled,final_discrete,final$TARGET)
colnames(loan)[colSums(is.na(loan)) > 0]
write.csv(loan,"loan.csv")
###############################################################################################

rm(list=ls())
loan<-read.csv("loan.csv")
loan$X<-NULL
#colnames(loan)[colSums(is.na(loan)) > 0]
#str(loan$final.TARGET)
final<-loan
loan<-final[c(48745:356255),]
test_final<-final[c(1:48744),]
test_final$final.TARGET<-NULL
rm(final)
set.seed(100)

indices = sample.split(loan$final.TARGET, SplitRatio = 0.7)

train = loan[indices,]

test = loan[!(indices),]

rm(loan)
# Logistic Regression: 

###############################################################################################
model_3 = glm(formula =final.TARGET ~ ., data = train[,c(51:100,461)], family = "binomial")

# Stepwise selection
model_4<- stepAIC(model_3, direction="both")
rm(model_3)
###############################################################################################
model_5 = glm(formula =final.TARGET ~ ., data = train[,c(101:150,461)], family = "binomial")

# Stepwise selection
model_6<- stepAIC(model_5, direction="both")
rm(model_5)
###############################################################################################
model_7 = glm(formula =final.TARGET ~ ., data = train[,c(151:200,461)], family = "binomial")

# Stepwise selection
model_8<- stepAIC(model_7, direction="both")
rm(model_7)
###############################################################################################
model_9 = glm(formula =final.TARGET ~ ., data = train[,c(201:250,461)], family = "binomial")

# Stepwise selection
model_10<- stepAIC(model_9, direction="both")
rm(model_9)
###############################################################################################
model_11 = glm(formula =final.TARGET ~ ., data = train[,c(251:325,461)], family = "binomial")

# Stepwise selection
model_12<- stepAIC(model_11, direction="both")
rm(model_11)
###########################################################################################
model_13 = glm(formula =final.TARGET ~ ., data = train[,c(326:400,461)], family = "binomial")

# Stepwise selection
model_14<- stepAIC(model_13, direction="both")
rm(model_13)
###############################################################################
model_15 = glm(formula =final.TARGET ~ ., data = train[,c(401:460,461)], family = "binomial")

# Stepwise selection
model_16<- stepAIC(model_15, direction="both")
rm(model_15)


#######################################################################
#############################################################################
###############################################################################
loan<-read.csv("loan.csv")
loan$X<-NULL
set.seed(50)

indices = sample.split(loan$final.TARGET, SplitRatio = 0.7)

train = loan[indices,]

test = loan[!(indices),]
model_19<-glm(formula = final.TARGET ~ CODE_GENDER.xM + FLAG_OWN_CAR + 
                 NAME_INCOME_TYPE.xPensioner + 
                NAME_EDUCATION_TYPE.xSecondary...secondary.special + NAME_FAMILY_STATUS.xMarried + 
                NAME_HOUSING_TYPE.xOffice.apartment + OCCUPATION_TYPE.xCooking.staff + 
                OCCUPATION_TYPE.xDrivers + OCCUPATION_TYPE.xLaborers + OCCUPATION_TYPE.xLow.skill.Laborers + 
                OCCUPATION_TYPE.xRealty.agents + OCCUPATION_TYPE.xSales.staff + 
                OCCUPATION_TYPE.xSecurity.staff + ORGANIZATION_TYPE.xHotel + 
                ORGANIZATION_TYPE.xMedicine + ORGANIZATION_TYPE.xPolice  + 
                 ORGANIZATION_TYPE.xTransport..type.2 + 
                ORGANIZATION_TYPE.xTransport..type.3 + HOUSETYPE_MODE.xspecific.housing + 
                WALLSMATERIAL_MODE.xnot.available + WALLSMATERIAL_MODE.xOthers + 
                WALLSMATERIAL_MODE.xStone..brick  + 
                WEEKDAY_APPR_PROCESS_START.y.xMONDAY + 
                NAME_CASH_LOAN_PURPOSE.xRepairs + NAME_CASH_LOAN_PURPOSE.xXNA + 
                NAME_CLIENT_TYPE.xRefreshed + NAME_CLIENT_TYPE.xRepeater + 
                NAME_SELLER_INDUSTRY.xConsumer.electronics+ 
                PRODUCT_COMBINATION.xCash.Street..low +  
                PRODUCT_COMBINATION.xCash.X.Sell..low + PRODUCT_COMBINATION.xPOS.mobile.with.interest + 
                NAME_CONTRACT_STATUS.xnot.available + AMT_INCOME_TOTAL + 
                AMT_CREDIT.x + AMT_ANNUITY.train + DAYS_EMPLOYED + DAYS_ID_PUBLISH + 
                OWN_CAR_AGE + EXT_SOURCE_1 + EXT_SOURCE_2 + EXT_SOURCE_3 + 
                COMMONAREA_AVG  + FLAG_DOCUMENT_3 + AMT_CREDIT_MAX_OVERDUE + 
                AMT_CREDIT_SUM_DEBT + bureau_MONTHS_BALANCE + AMT_CREDIT.y + 
                AMT_DOWN_PAYMENT + HOUR_APPR_PROCESS_START.y + CNT_PAYMENT + 
                NUM_INSTALMENT_NUMBER + DAYS_ENTRY_PAYMENT + AMT_PENDING + 
                 AMT_RECIVABLE, 
              family = "binomial", data = train)
summary(model_19)
#AIC: 107383

final_model<-model_19
saveRDS(final_model,"log_reg1")
names<-names(model.frame(model_19))
loan<-loan[,which(colnames(loan)%in%names)]
###############################################################################################
### Model Evaluation

### Test Data ####

#predicted probabilities of default for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-461])

summary(test_pred)

pred<-prediction(test_pred,test$final.TARGET)
eva<-performance(pred,"sens","spec")
evaA<-performance(pred,'acc')

plot(evaA)
sensitivity <- eva@y.values[[1]]
cutoff <- eva@alpha.values[[1]]
specificity<- eva@x.values[[1]]
accuracy<-evaA@y.values[[1]]
plot(cutoff,sensitivity,col="red")
lines(cutoff,specificity,col="green")
lines(cutoff,accuracy,col="blue")

#lets use 0.161 as the cutoff as its the point where accuracy, sensitivity  and specificity curves  meets


perf<-performance(pred,'tpr','fpr')
ks<-max(perf@y.values[[1]]-perf@x.values[[1]])
# 0.42
#gain and lift chart
gain<-performance(pred,'tpr','rpp')
deciles<-performance(pred,'rpp')
#ks chart
k_stat_matrix<-data.frame(10*(deciles@y.values[[1]]),10*(perf@y.values[[1]]-perf@x.values[[1]]))
colnames(k_stat_matrix)[1]<-"deciles"
colnames(k_stat_matrix)[2]<-"k_statistic"
k_stat_matrix$k_statistic<-round(k_stat_matrix$k_statistic,2)
k_stat_matrix[which(k_stat_matrix$k_statistic==4.2),]
plot(k_stat_matrix)
#kstatistic lies withinin first 4 deciles 

auc(roc(test_pred,factor(test$final.TARGET)))
#####################################################################################
###########################################################################################
#XG boost model

library(xgboost)

rm(list=ls())
loan<-read.csv("loan.csv")
loan$X<-NULL
#colnames(loan)[colSums(is.na(loan)) > 0]
#str(loan$final.TARGET)
final<-loan
loan<-final[c(48745:356255),]
test_final<-final[c(1:48744),]
test_final$final.TARGET<-NULL
rm(final,test_final)
set.seed(100)

indices = sample.split(loan$final.TARGET, SplitRatio = 0.7)

train = as.matrix(sapply(loan[indices,],as.numeric))

test = loan[!(indices),]

rm(loan,indices)

#train<-as.matrix(sapply(train,as.numeric))
test<-as.matrix(sapply(test,as.numeric))



params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eval_metric ="auc",
  eta=0.3,
  gamma=6,
  max_depth=2,
  min_child_weight=2,
  subsample=1,
  colsample_bytree= 0.933,
  scale_pos_weight = 307511/24825
)

#xgboost cross validation
bst.cv = xgb.cv(
  params=params,
 data = train[,-461],
label =train[,461],
 nfold = 2,
 nrounds=500,
prediction=T)

#Find where the minimum logloss occurred
min.loss.idx = which.max(bst.cv$evaluation_log[, test_auc_mean]) 
cat ("Minimum logloss occurred in round : ", min.loss.idx, "\n")
# Minimum logloss
print(bst.cv$evaluation_log[min.loss.idx,])
watchlist <- list(train, test)
####################################################################################
#making model with min.loss.idx
xgb.model <- xgboost(params=params, data=train[,-461],
                     label=train[,461],
                     nrounds = 253, watchlist)
saveRDS(xgb.model,"xgb.model100")
xgb.predictions <- predict(xgb.model, test[,-461],type = "response")
test_pred<-data.frame(xgb.predictions)

auc(roc(xgb.predictions,factor(test[,461])))


###################################################################################
######################################################################
 #svm model
library(kernlab)
library(gridExtra)
library(parallel)
library(scales)

rm(list=ls())
loan<-read.csv("loan.csv")
loan$X<-NULL
#colnames(loan)[colSums(is.na(loan)) > 0]
#str(loan$final.TARGET)
final<-loan
loan<-final[c(48745:356255),]
test_final<-final[c(1:48744),]
test_final$final.TARGET<-NULL
rm(final)
set.seed(30)

indices = sample.split(loan$final.TARGET, SplitRatio = 0.05)

train = loan[indices,]

test = loan[!(indices),]

test$final.TARGET<-factor(test$final.TARGET)

train$final.TARGET<-factor(train$final.TARGET)
rm(loan,train)

#Constructing Model
Model_linear<-readRDS("Linear_svm_10.rds")

#Using Linear Kernel
#Model_linear <- ksvm(final.TARGET~ ., data = train, scaled = FALSE, kernel = "vanilladot",class.weights=c("0"=1,"1"=12),C = 0.01)
Eval_linear<- predict(Model_linear, test[,-461])
auc(roc(Eval_linear,factor(test[,461])))
svm_pred<-predict(Model_linear, test_final)
#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$final.TARGET)


saveRDS(Model_linear,"Linear_svm_10.rds")
polynomial_svm<-readRDS("Linear_svm.rds")
Eval_linear<- predict(Model_linear, test[,-55])
confusionMatrix(Eval_linear,test$final.TARGET)

auc(roc(Eval_linear,factor(test[,55])))
##################################################################################################
#linear CROSS VALIDATION
#used 2401 observations for training and 2 fold cross validation  due to lack of computational resources to perform 5 fold cross validation on 5000 observations.
loan<-read.csv("loan.csv")
loan$X<-NULL
set.seed(30)

indices = sample.split(loan$final.TARGET, SplitRatio = 0.05)

train = loan[indices,]

test = loan[!(indices),]

test$final.TARGET<-factor(test$final.TARGET)

train$final.TARGET<-factor(train$final.TARGET)
Control <- trainControl(method="cv", number=2)
# Number - Number of folds 
# Method - cross validation

metric <- "Accuracy"



# making a grid of C values. 
grid <- expand.grid(C=c(0.01))

# Performing 5-fold cross validation
fit.svm2<- train(final.TARGET~., data=train, method="svmLinear", metric= metric,
                 tuneGrid=grid, trControl=Control,class.weights=c("0"=1,"1"=307511/24825))

#The final values used for the model were degree = 5, scale = 100 and C = 0.01.


evaluate_rbf_test<- predict(fit.svm2, test[c(1:400),-461])
confusionMatrix(evaluate_rbf_test, test$final.TARGET)
auc(roc(evaluate_rbf_test,factor(test[c(1:400),461])))

saveRDS(fit.svm2,"Linear_svm_final.rds")
#########################################################################################
########################################################################################
#rbf CROSS VALIDATION
#used 2401 observations for training and 2 fold cross validation  due to lack of computational resources to perform 5 fold cross validation on 5000 observations.
Control <- trainControl(method="cv", number=2)
# Number - Number of folds 
# Method - cross validation

metric <- "Accuracy"



# making a grid of C values. 
grid <- expand.grid(.sigma=c(0.06), .C=c(0.08))

# Performing 5-fold cross validation
fit.svm2<- train(final.TARGET~., data=train, method="svmRadial", metric= metric,
                 tuneGrid=grid, trControl=Control,class.weights=c("0"=1,"1"=12))

#The final values used for the model were degree = 5, scale = 100 and C = 0.01.

print(fit.svm2)
plot(fit.svm2)
evaluate_rbf_test<- predict(fit.svm2, test[,-55])
confusionMatrix(evaluate_rbf_test, test$final.TARGET)
auc(roc(evaluate_rbf_test,factor(test[,55])))

#sigma  C     Accuracy   Kappa     
#06   0.06  0.6788265  0.13127275
#0.06   0.07  0.7359334  0.15130134
#0.06   0.08  0.7651375  0.15480059
#0.07   0.06  0.6487123  0.11909327
#0.07   0.07  0.7323561  0.14225672
#0.07   0.08  0.7762595  0.15642984
#0.08   0.06  0.5635102  0.09179963
#0.08   0.07  0.7054279  0.13282340