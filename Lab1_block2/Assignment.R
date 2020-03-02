###Ensemble Methods
#spam=1 , no spam=0


library(mboost)
library(randomForest)
sp <- read.csv2("spambase.csv")
sp$Spam <- as.factor(sp$Spam)
n=dim(sp)[1]
set.seed(12345)
id=sample(1:n, floor(n*2/3))
train_spam=sp[id,]
test_spam=sp[-id,]


mis_error<-function(X,X1){
  n<-length(X)
  return(1-sum(diag(table(X,X1)))/n) #misclassification error function
}

#adaboost
steps<-seq(10,100,10)
er_tr_ada<-double(10)
er_tes_ada<-double(10)
for (i in steps){
  ada<-blackboost(Spam~.,data=train_spam,family = AdaExp(),control=boost_control(m=i))
  preds_tr_ada<-predict(ada,train_spam,type="class")
  preds_tes_ada<-predict(ada,test_spam,type="class")
  er_tr_ada[i/10]<-mis_error(train_spam$Spam,preds_tr_ada)
  er_tes_ada[i/10]<-mis_error(test_spam$Spam,preds_tes_ada)
  ada_mat<-cbind(er_tr_ada,er_tes_ada)
}
ada_df<-as.data.frame(ada_mat)
plot(steps,ada_df[,1],type="l",col="red")
lines(steps,ada_df[,2],type="l",col="blue")



##random forest
set.seed(12345)
er_tr_rnd<-double(10)
er_tes_rnd<-double(10)
for (i in steps){
  rndforest<-randomForest(Spam~.,data=train_spam,ntree=i)
  preds_tr_rnd<-predict(rndforest,train_spam)
  preds_tes_rnd<-predict(rndforest,test_spam)
  er_tr_rnd[i/10]<-mis_error(train_spam$Spam,preds_tr_rnd)
  er_tes_rnd[i/10]<-mis_error(test_spam$Spam,preds_tes_rnd)
  rand_mat<-cbind(er_tr_rnd,er_tes_rnd)
}

rnd_df<-as.data.frame(rand_mat)
plot(steps,rnd_df[,1],type="l",col="red",ylim = c(0,0.07))
lines(steps,rnd_df[,2],type="l",col="blue")


###Mixture models

set.seed(1234567890)
max_it <- 100 # max number of EM iterations
min_change <- 0.1 # min change in log likelihood between two consecutive EM iterations
N=1000 # number of training points
D=10 # number of dimensions
x <- matrix(nrow=N, ncol=D) # training data
true_pi <- vector(length = 3) # true mixing coefficients
true_mu <- matrix(nrow=3, ncol=D) # true conditional distributions
true_pi=c(1/3, 1/3, 1/3)
true_mu[1,]=c(0.5,0.6,0.4,0.7,0.3,0.8,0.2,0.9,0.1,1)
true_mu[2,]=c(0.5,0.4,0.6,0.3,0.7,0.2,0.8,0.1,0.9,0)
true_mu[3,]=c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)
plot(true_mu[1,], type="o", col="blue", ylim=c(0,1))
points(true_mu[2,], type="o", col="red")
points(true_mu[3,], type="o", col="green")
# Producing the training data
for(n in 1:N) {
  k <- sample(1:3,1,prob=true_pi)
  for(d in 1:D) {
    x[n,d] <- rbinom(1,1,true_mu[k,d])
  }
}
K=4 # number of guessed components
z <- matrix(nrow=N, ncol=K) # fractional component assignments
pi <- vector(length = K) # mixing coefficients
mu <- matrix(nrow=K, ncol=D) # conditional distributions
llik <- vector(length = max_it) # log likelihood of the EM iterations
# Random initialization of the paramters
pi <- runif(K,0.49,0.51)
pi <- pi / sum(pi)
for(k in 1:K) {
  mu[k,] <- runif(D,0.49,0.51)
}
pi
mu
for(it in 1:max_it) {
  plot(mu[1,], type="o", col="blue", ylim=c(0,1))
  points(mu[2,], type="o", col="red")
  points(mu[3,], type="o", col="green")
  points(mu[4,], type="o", col="purple")
  points(mu[4,], type="o", col="black")
  #Sys.sleep(0.5)
  # E-step: Computation of the fractional component assignments
  # Your code here
  m=matrix(nrow=1000,ncol=K)
  for (i in 1:1000){
    for (j in 1:K){
      m[i,j]<-prod((mu[j,]^x[i,])*(1-mu[j,])^(1-x[i,]))
      
      #unlist(lapply(mu[j,],function(y){ifelse(x[i,which(y %in% mu[j,])]==1,1-y,y)}))
    }
  }
  m<-t(t(m)*pi)
  z<-m/rowSums(m)
  
  #Log likelihood computation.
  # Your code here
  #  l<-0
  #  for (n in 1:1000){
  #    for (k in 1:3){
  #       for (D in 1:10){
  #        l<-m[n,k]*(log(pi[k])+x[n,D]*log(mu[k,D])+(1-x[n,D])*(log(1-mu[k,D])))
  #        l=+l
  # #       l<-m[n,k]*(log(pi[k])+sum( x[n,]*log(mu[k,])+ ( (1-x[n,])*log(1-mu[k,])) ))
  # #      l=+l
  #     }
  #    }
  #  }
   
  l <- 0
  for (n in 1:1000) {
    for (k in 1:K) {
      sum <- 0
      for (D in 1:10) {
        sum =sum+ (x[n, D] * log(mu[k, D]) + (1 - x[n, D]) * log(1 - mu[k, D]))
      }
      
      l = l+ (z[n,k] * (log(pi[k]) + sum))
    }
  }
  
  llik[it]<-l
  cat("iteration: ", it, "log likelihood: ", llik[it], "\n")
  #flush.console()
  # Stop if the lok likelihood has not changed significantly
  # Your code here
  if(abs(llik[it]-(llik[it-1]))<=min_change&&(it>1)){
    
    break
  }
  #M-step: ML parameter estimation from the data and fractional component assignments
  # Your code here
  pi<-colSums(z)/N
  mu<-t(z)%*%x
  mu<-mu/colSums(z)
}
pi
mu
plot(llik[1:it], type="o")

