feature_selection<-function(X,Y,N){
 
  n<-ncol(X)
  idx<-1:2^n-1
  t<-vector()
  mat<-sapply(idx, function(id){
    t<-cbind(t,as.integer(intToBits(id)))
    t})
  m<-mat[1:n,2:ncol(mat)]
  ######################################
  set.seed(12345)
  #X<-X[sample(nrow(X)),]
  #Y<-Y[sample(length(Y))]
  id<-sample(nrow(X))
  X<-X[id,]
  Y<-Y[id]
  #Create N equally size folds
  folds <- cut(seq(1,nrow(X)),breaks=N,labels=FALSE)
  d<-matrix(0,nrow=N,ncol=dim(m)[2])
  n_features<-rep(0,ncol(d))
  for (i in 1:ncol(m)){
    x<-X[which(m[,i]==1)]
    n_features[i]<-ncol(x)
    for(j in 1:N){
      
      testIndexes <- which(folds==j,arr.ind=TRUE)
      testX <- as.matrix(x[testIndexes, ])
      trainX <- as.matrix(x[-testIndexes, ])
      testy<-Y[testIndexes]
      trainy<-Y[-testIndexes]
      trainX<-cbind(1,trainX)
      testX<-cbind(1,testX)
  
      w<-round(as.vector(solve(t(trainX)%*%trainX)%*%t(trainX)%*%trainy),3)
      y_pred<-round(as.matrix(testX%*%w),3)
      sse<-sum((testy-y_pred)^2)
      d[j,i]<-sse
    }
  }
 d<-d
 s<-apply(d, MARGIN = 2, function(x) mean(x, na.rm=TRUE))
  bindex<-which(s==min(s))
  best_comb<-X[which(m[,bindex]==1)]
  print(list("best combination"=colnames(best_comb),"best cv score"=s[bindex]))
  plot(x=seq(s),y=s,type="p",xlab="number of features",ylab="CV score",col=ifelse(s==s[bindex],"red","black"))
}


Y<-swiss[,"Fertility"]
X<-swiss[!names(swiss)%in%c("Fertility")]
D<-feature_selection(X,Y,5)






