



library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
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
setwd("C:/Users/WEL COME/Downloads/kaggle/New folder")
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

final<-read.csv("train.csv",stringsAsFactors=FALSE,na.strings=c("","NA"))
colSums(is.na(final))
final$SK_ID_CURR.y.y <-NULL
final$SK_ID_BUREAU<-NULL
final$SK_ID_CURR.x.x<-NULL
final$X<-NULL
final$SK_ID_CURR.y<-NULL
final$SK_ID_PREV<-NULL
############################################################################################
#OUTLIER TREATMENT

a<- vector(mode="numeric", length=0)
outlier<-function(x)
{
  if((is.numeric(x)))
  {
  plo<-boxplot.stats(x)
  a<-which(x %in% plo$out )
  if(length(a)!=0|length(is.na(a)==0))
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
}
######################################################################

for(i in 1:ncol(final))
  
{  
  final[,i]<-outlier(final[,i])
  
  i<-i+1
  
}
final<- data.frame(sapply((final, function(x) outlier(x))))

##############################################################################################
#DIVIDING THE FINAL DATASET TO CHARACTER AND NUMERICAL

x<-select_if(final[,123:ncol(final)], is.numeric)
names<-c("REGION_RATING_CLIENT" ,"NUM_INSTALMENT_VERSION" ,"REGION_RATING_CLIENT_W_CITY")
x<-x[,!colnames(x)%in%names]


x[is.na(x)]<-0
y<-final[,!colnames(final)%in%colnames(x)]
train<-cbind(x,y)
train<-train[,sapply(train, function(x) length(unique(x[!(is.na(x))]))>1)]

rm(final,x,y)
num<-select_if(train[,], is.numeric)
num<-num[,!colnames(num)%in%names]
num_scaled<-num[,sapply(num, function(x) length(unique(x[!(is.na(x))]))>1)]


char<-train[,!colnames(train)%in%colnames(num)]


which(colnames(num)=="TARGET")
num<-num[,-63]
which(colnames(num)=="SK_ID_CURR")
num<-num[,-62]
str(train$SELLERPLACE_AREA)
which(colnames(num)=="SELLERPLACE_AREA")
num<-num[,-21]

SELLERPLACE_AREA<-data.frame(train$SELLERPLACE_AREA)

target<-data.frame(train$TARGET)
#calculating percentage default
length(which(train$TARGET==1))/307511*100#<-8%
#check unique customer id
length(unique(train$SK_ID_CURR))#<-307511





y<-num[,colSums(is.na(num))!=0]
colSums(is.na(y))
for(i in 1:ncol(y))
{
  y[which(is.na(y[i])),i]<-median(y[,i],na.rm = TRUE)
}
num<-num[,!colnames(num)%in%colnames(y)]
num<-cbind(num,y)
y<-cbind(train$TARGET,y)
rm(y,i)
#####################converting discrete variables to factor


char[is.na(char)]<-"not available"
char<-data.frame(sapply(char, function(x) factor(x)))
y<-char[,colSums(is.na(char))!=0]



char<- data.frame(sapply(char, 
                              function(x) data.frame(model.matrix(~x,data =char))[,-1]))



num<-data.frame(scale(num, center = TRUE, scale = TRUE))
num_scaled<-num[,sapply(num, function(x) length(unique(x[!(is.na(x))]))>1)]

which(sapply(num, function(x) length(which(x == ""))>0)==T) # Returned 0 -> No blanks
 
num_scaled<-num[,sapply(num, function(x) length(unique(x[!(is.na(x))]))>1)]

loan<- cbind(num, char, target, SELLERPLACE_AREA)
write.csv(loan,"loan.csv")
###############################################################################################
rm(list=ls())
loan<-read.csv("loan.csv",stringsAsFactors=FALSE,na.strings=c("","NA"))
y<-num_scaled[,colSums(is.na(num_scaled))!=0]
y<-char[,colSums(is.na(char))!=0]
length(which(is.na(num$AMT_CREDIT_MAX_OVERDUE)))

plo<-boxplot.stats(train$AMT_CREDIT_MAX_OVERDUE)

