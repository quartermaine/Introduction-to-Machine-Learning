##Assignmet 2 - Analysis of credic scoring 

scoring<-readxl::read_excel("creditscoring.xls")
scoring<-as.data.frame(scoring)
###1
n=dim(scoring)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=scoring[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
valid=scoring[id2,]
id3=setdiff(id1,id2)
test=scoring[id3,]

###2
library(tree)

n=dim(train)[1]
train$good_bad<-as.factor(train$good_bad)
fit_dev<-tree(good_bad~., data=train,split="deviance")
fit_gini<-tree(good_bad~.,data=train,split="gini")

plot(fit_dev)
text(fit_dev,pretty = 0)

plot(fit_gini)
text(fit_gini,pretty=0)

mis_error<-function(X,X1){
  n<-length(X)
  return(1-sum(diag(table(X,X1)))/n) #misclassification error function
}

###################### deviance#################################
pred_dev_train<-predict(fit_dev, newdata=train, type="class")
table(train$good_bad,pred_dev_train)

mis.error1<-mis_error(pred_dev_train,train$good_bad)
mis.error1

pred_dev_test<-predict(fit_dev,newdata = test,type="class")
table(test$good_bad,pred_dev_test)

mis.error2<-mis_error(pred_dev_test,test$good_bad)
mis.error2

#################### gini#######################################

pred_gini_train<-predict(fit_gini, newdata=train, type="class")
table(train$good_bad,pred_gini_train)

mis.error3<-mis_error(pred_gini_train,train$good_bad)
mis.error3

pred_gini_test<-predict(fit_gini,newdata = test,type="class")
table(test$good_bad,pred_gini_test)

mis.error4<-mis_error(test$good_bad,pred_gini_test)
mis.error2

df<-cbind(c(mis.error1,mis.error2),c(mis.error3,mis.error4))
df<-as.data.frame(df)
colnames(df)<-c("deviance","gini")
df$data<-c("train","test")
df

################ plot #################################################
library(ggplot2)
ggplot(df,aes(x=data,y=deviance,group=1))+geom_point(aes(color="deviance"))+
  geom_point(aes(y=gini,color="gini"))+
  scale_color_manual(labels = c("deviance", "gini"), values = c("red", "blue"))

#deviance provides better explaination

###3

valid$good_bad<-as.factor(valid$good_bad)

fit=tree(good_bad~., data=train)
trainScore=rep(0,12)
testScore=rep(0,12)
for(i in 2:12) {
  prunedTree=prune.tree(fit,best=i)
  pred=predict(prunedTree, newdata=valid,type="tree")
  trainScore[i]=deviance(prunedTree)
  testScore[i]=deviance(pred)
}

plot(2:12, trainScore[2:12], type="b", col="red",ylim=c(200,800))
points(2:12, testScore[2:12], type="b", col="blue")                #optimal tree depth is 4

test$good_bad<-as.factor(test$good_bad)

finalTree<-prune.tree(fit, best=4)
pred_best<-predict(finalTree, newdata=test,type="class")            
table(test$good_bad,pred_best)

summary(finalTree) #to see the variables selcted               # savings,duration,history
plot(finalTree)
text(finalTree,pretty = 0)      #we can see if the savings more than 2.5 predicting good else we go to branch duration,if duration  
                                #more than 43.5 then bad else move to history where if <1.5 good else bad 

mis.error_b<-mis_error(test$good_bad,pred_best)
mis.error_b     #misclassification for test data with the best tree with leaves

#4

library(MASS)
library(e1071)

naive=naiveBayes(good_bad~., data=train)


pred_naive_train<-predict(naive,newdata=train)
table(train$good_bad,pred_naive_train,dnn = c("actual train naive","predictions train naive"))

pred_naive_test<-predict(naive, newdata=test)
table(test$good_bad,pred_naive_test,dnn=c("actual test naive","predictions test naive"))

enaive_train<-mis_error(train$good_bad,pred_naive_train)
enaive_test<-mis_error(test$good_bad,pred_naive_test)

enaive_train #misclassification error for train naive
enaive_test #misclassification error for test naive

#misclassification rates for naive bayes are more than the ones from tree model 3 


#5

# ROC <- function(Y, Yhat, p){
#   m = length(p)
#   TPR = numeric(m)
#   FPR = numeric(m)
#   for (i in 1:m){
#     t = table(Yhat > p[i], Y)
#     print(t)
#     TPR[i] = t[2,2]/sum(t[,2])
#     FPR[i] = t[2,1]/sum(t[,1])
#     }
#   return (list(TPR = TPR, FPR=FPR))
# }




l<-seq(0.05,0.95,0.05)
p_naive<-as.data.frame(predict(naive,test,type="raw"))$good
mat<-vector()
for (i in l){
  pred_naive<-ifelse(p_naive>i,"good","bad")
  t<-table(test$good_bad,pred_naive)
  tpr<-t[1]/(t[1]+t[3])
  fpr<-t[2]/(t[2]+t[4])
  mat<-rbind(mat,c(tpr,fpr))
  
}

dmat<-as.data.frame(mat)
colnames(dmat)<-c("tpr","fpr")
plot(y=dmat$tpr,x=dmat$fpr,type="l",col="red")    # ROC plot for naive Bayes  



p_tree<-as.data.frame(predict(finalTree,test,type="vector"))$good
vec<-vector()
for (i in l){
  pred_tree<-ifelse(p_tree>i,"good","bad")
  t1<-table(test$good_bad,pred_tree)
  tpr1<-t1[1]/(t1[1]+t1[3])
  fpr1<-t1[2]/(t1[2]+t1[4])
  vec<-rbind(vec,c(tpr1,fpr1))
  
}

dmat1<-as.data.frame(vec)
colnames(dmat1)<-c("tpr","fpr")
plot(y=dmat1$tpr,x=dmat1$fpr,type="l",col="blue")

#6

############# using rpart ############################################# 
library(rpart)
fit_mod <- rpart(good_bad ~ ., data=train, method="class", parms=list(loss=matrix(c(0,10,1,0), byrow=TRUE, nrow=2)))

preds_fit_train<-predict(fit_mod,train,type="class")
preds_fit_test<-predict(fit_mod,test,type="class")

table(train$good_bad,preds_fit_train,dnn=c("actual train naive mat","predictions train"))
table(test$good_bad,preds_fit_test,dnn=c("actual test naive mat","predictions test"))

#############
pntr<-predict(naive,newdata=train,type="raw")
pntes<-predict(naive,newdata=test,type="raw")

my_fun<-function(X,Y){
  X<-as.data.frame(X)
  X[,1]<-X[,1]*10
  X$naive<-ifelse(X[,1]>X[,2],"bad","good")
  table(Y,X$naive,dnn = c("actual","predictions"))
}

my_fun(pntr,train$good_bad)  #confusion matrix for train
my_fun(pntes,test$good_bad)  #confusion matrix for test
