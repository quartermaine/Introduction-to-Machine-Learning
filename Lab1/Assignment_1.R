##Assignment 1


spam<-readxl::read_excel("spambase.xlsx")
#spam$Spam<-as.factor(spam$Spam)
#levels(spam$Spam)<-c("not spam","spam")
###1
n=dim(spam)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=spam[id,]
test=spam[-id,]

model1<-glm(Spam~.,data=train,family = binomial(link = "logit"))
pred_train<-predict(model1,train,type="response")
pred_test<-predict(model1,test,type="response")
broom::glance(model1) # use to see diagnostics of the model
fit.pred_train<-ifelse(pred_train>0.5,1,0)
fit.pred_test<-ifelse(pred_test>0.5,1,0)
#confusion matrix for train data
table(fit.pred_train,train$Spam,dnn = c("Predictions","True train values"))

train_accuracy<-(803+344)/(803+142+344+81)
train_precision<-(803)/(803+81)
cat("=====================================\nAccuracy on train data is: ", train_accuracy,"\nPrecision on train data is:",train_precision)

#confusionmatrix for test data
table(fit.pred_test,test$Spam,dnn = c("Prediction","True test values"))
test_accuracy<-(791+336)/(791+97+146+336)
test_precision<-791/(791+97)
cat("=====================================\nAccuracy on test data is: ", test_accuracy,"\nPrecision on test data is:",test_precision)

#model accuracy on train data
mean(train$Spam==fit.pred_train)
#model accuracy on test data
mean(test$Spam==fit.pred_test)

#ROC
ROC<-pROC::roc(test$Spam,fit.pred_test)#using the package pROC
plot(ROC,color="blue")
pROC::auc(ROC)

#We can also use package POCR
# auc<-performance(prediction(fit.pred_test,test$Spam),"auc")
# unlist(slot(auc, "y.values"))

#calculate misclassification errors wtih mis_error

mis_error<-function(X,X1){
  n<-length(X)
  return(1-sum(diag(table(X,X1)))/n) #misclassification error function
}

miserror_train<-mis_error(fit.pred_train,train$Spam)
miserror_test<-mis_error(fit.pred_test,test$Spam)
cat("=====================================\nMissclasification error in train: ", miserror_train,"\nMissclasification error in test:",miserror_test)


###2

fit.pred_train1<-ifelse(pred_train>0.9,1,0)
fit.pred_test1<-ifelse(pred_test>0.9,1,0)
#confusion matrix for train data
table(fit.pred_train1,train$Spam,dnn = c("Predictions","True train values"))
#confusionmatrix for test data
table(fit.pred_test1,test$Spam,dnn = c("Predictions","True train values"))

miserror_train1<-mis_error(fit.pred_train1,train$Spam)
miserror_test1<-mis_error(fit.pred_test1,test$Spam)
cat("=====================================\nMissclasification error in train: ", miserror_train1,"\nMissclasification error in test:",miserror_test1)


#model accuracy on train data
mean(train$Spam==fit.pred_train1)
#model accuracy on test data
mean(test$Spam==fit.pred_test1)


###3
library(kknn)
s<-spam
s$Spam<-as.factor(s$Spam)
n=dim(s)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
strain=s[id,]
stest=s[-id,]


#first train the model with train,train and then make predictions
(knn<-kknn(Spam ~ ., strain,strain, k = 30))
table(knn$fit,strain$Spam,dnn=c("Predicted class","Real class"))
e1<-mis_error(knn$fit,strain$Spam)


(knn1<-kknn(Spam ~ .,strain,stest, k = 30))
table(knn1$fit,stest$Spam,dnn=c("Predicted class","Real class"))
e2<-mis_error(knn1$fit,stest$Spam)


cat("=====================================\nMissclasification error in train: ", e1,"\nMissclasification error in test:",e2)


plot(knn)


###4
(knn3<-kknn(Spam ~ .,strain,strain, k = 1))
table(knn3$fit,strain$Spam,dnn=c("Predicted class","Real class"))
e3<-mis_error(knn3$fit,strain$Spam)

(knn4<-kknn(Spam ~ .,strain,stest, k = 1))
table(knn4$fit,stest$Spam,dnn=c("Predicted class","Real class"))
e4<-mis_error(knn4$fit,stest$Spam)


cat("=====================================\nMissclasification error in train: ", e3,"\nMissclasification error in test:",e4)





