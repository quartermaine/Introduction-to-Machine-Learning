#cos_distance function ,given 2  

cos_distance<-function(X,Y){
  X<-as.matrix(X)
  Y<-as.matrix(Y)
  X_hat<- t(apply(X,1,function(x){x/sqrt(sum(x*x))})) #sweep(trainS,1,sqrt(rowSums(trainS*trainS)),"/")
  Y_hat<- t(apply(Y,1,function(x){x/sqrt(sum(x*x))}))
  cosine<-X_hat%*%t(Y_hat)
  D<-as.matrix(1-cosine)
  D
}



knearest<-function(data, k, newdata,cl){
  cl<-as.data.frame(cl)
  cosdisM<-cos_distance(data,newdata)
  cosdisM<-as.data.frame(cosdisM)
  cosdisM$labels<-cl
  
  n<-ncol(cosdisM)-1
  bestM<-matrix(0,nrow=k)
 
  for (i in 1:n){
    ord_cosdisM<-cosdisM[order(cosdisM[,i]),]
    best_ks<-as.vector(ord_cosdisM[2:(k+1),"labels"])
    bestM<-cbind(bestM,best_ks)
    
  }
  bestM<-as.data.frame(bestM)
  bestM[,1]<-NULL
  
  #colnames(bestM)<-rownames(newdata)
  #bestM
  #pred_cl<-apply(bestM,2,function(x){names(which(table(x)==max(table(x)))[1])})
  pred_cl<-apply(bestM,2,function(x){ifelse(mean(x)>0.5,1,0)})
  pred_cl
  
}



# set.seed(12345)
# iris<-iris[sample(nrow(iris)),]
# iris_data<-iris[,-5]
# iris_labels<-iris[,5]
# 
# n=dim(iris_data)[1]
# 
# id=sample(1:n, floor(n*0.6))
# trainS=iris_data[id,]
# testS=iris_data[-id,]
# 
# trainE<-iris_labels[id]
# testE<-iris_labels[-id]
# 
# DD<-cos_distance(trainS,trainS)
# KNN<-knearest(trainS, 30, trainS,trainE)
# error<-1-mean(as.vector(as.vector(trainE)==as.vector(KNN)))
# mis_error(KNN,trainE)
# 
# 
# KNN1<-knearest(trainS,30,testS,trainE)
# error1<-1-mean(as.vector(as.vector(testE)==as.vector(KNN1)))
# mis_error(KNN1,testE)

#################################################################################


spam<-readxl::read_excel("spambase.xlsx")
#spam$Spam<-as.factor(spam$Spam)
#levels(spam$Spam)<-c("not spam","spam")
spam<-as.data.frame(spam)
spam<-spam[sample(nrow(spam)),]
###split data
n=dim(spam)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=spam[id,]
test=spam[-id,]

train_X<-train[,!names(train)%in%c("Spam")]
train_labels<-train[,"Spam"]

test_X<-test[,!names(test)%in%c("Spam")]
test_labels<-test[,"Spam"]


#DD1<-cos_distance(train_X,train_X)

mis_error<-function(X,X1){
  n<-length(X)
  return(1-sum(diag(table(X,X1)))/n) #misclassification error function
}

##############################################################
K<-knearest(train_X, 30, train_X,train_labels)
mis_error(as.vector(K),as.matrix(train_labels))  #0.2627737



K1<-knearest(train_X, 30, test_X,train_labels)
mis_error(as.vector(K1),as.matrix(test_labels))  #0.3094891

##############################################################


# v131<-c(7.4,2.8,6.1,1.9)
# v16<-c(5.7,4.4,1.5,0.4)
# 1-(t(v131)%*%v16/(sqrt(sum(v131^2))*sqrt(sum(v16^2)))) #0.1401729    #distance for 2 vectors 
# 
# 1-lsa::cosine(v131,v16) 

