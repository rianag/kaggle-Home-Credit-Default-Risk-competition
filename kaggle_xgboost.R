
loan<-read.csv("loan.csv")
loan$X<-NULL
#colnames(loan)[colSums(is.na(loan)) > 0]
#str(loan$final.TARGET)
final<-loan
loan<-final[c(48745:356255),]
test_final<-final[c(1:48744),]
test_final$final.TARGET<-NULL
rm (final,loan)
test<-as.matrix(sapply(test_final,as.numeric))
xgb.model<-readRDS("xgb.model10")
xgb.predictions10 <- predict(xgb.model, test,type = "response")
write.csv(xgb.predictions10,"xgb.predictions_10")


xgb.model<-readRDS("xgb.model100")
xgb.predictions100 <- predict(xgb.model, test,type = "response")
write.csv(xgb.predictions100,"xgb.predictions_100")


xgb.model<-readRDS("xgb.model50")
xgb.predictions50 <- predict(xgb.model, test,type = "response")
write.csv(xgb.predictions50,"xgb.predictions_50")


test_submission<-read.csv("sample_submission.csv")
test_submission<-cbind(test_submission,xgb.predictions10,xgb.predictions100,xgb.predictions50)
test_submission$TARGET<-(test_submission$xgb.predictions10+test_submission$xgb.predictions100+test_submission$xgb.predictions50)/3
test_submission$xgb.predictions10<-NULL
test_submission$xgb.predictions100<-NULL
test_submission$xgb.predictions50<-NULL
write.csv(test_submission,"xgb_submission.csv")

######################################################################################3
test_submission<-read.csv("xgb_submission.csv")
svm<-read.csv("svm_predictions.csv")
svm$X<-NULL
test_submission<-cbind(test_submission,log_predictions)
test_submission$TARGET<-(test_submission$TARGET+test_submission$log_predictions)/2

test_submission$log_predictions<-NULL
write.csv(test_submission,"test_submission.csv")
