LDA <- function(x, y) {
  #indexes of 2 classes
  group1_index <- which( y == "Male" )
  group2_index <- which( y == "Female" )
  #priors probs of classes
  p1 <- length(group1_index) / length(y)
  p2 <- length(group2_index) / length(y)
  prior_probs<-cbind(p1,p2)
  #means of the classes
  mean_class1 <- colMeans(x[group1_index, ])
  mean_class2 <- colMeans(x[group2_index, ])
  group_centers<-cbind(mean_class1,mean_class2)
  #subtract the mean of the group from each group
  x1 = as.matrix(sweep(x[group1_index, ], 2, mean_class1, "-"))
  x2 = as.matrix(sweep(x[group2_index, ], 2, mean_class2, "-"))
  #covariance matices 
  C1 = cov(x1)     
  C2 = cov(x2)      
  #within-class matrix
  S_w = (C1 + C2 )
  #means of global means
  mean_global<-colMeans(x)
  #between-class matrix 
  S1<-(mean_class1-mean_global)%*%t(mean_class1-mean_global)
  S2<-(mean_class2-mean_global)%*%t(mean_class2-mean_global)
  S_b<-length(group1_index)*S1+length(group2_index)*S2
  #eigen vectors ,slope,intercept of decision equation
  ev <- eigen(solve(S_w) %*% S_b)$vectors
  means<-rbind(mean_class2,mean_class1) #rbind means of 2 classes
  rownames(means)<-c("Female","Male")
  gmean<-prior_probs%*%means
  const <- drop(gmean%*%ev)
  lda_slope <- - ev[1,1] / ev[2,1]    #slope of decision line
  lda_intercept <- (const/ev[2,1])[1]  #intercept of decision line
  
  # vec1<-as.matrix(x)%*%solve(t(S_w))%*%(mean_class2-mean_class1)
  # vec2<-0.5*(t(mean_class2+mean_class1)%*%solve(S_w)%*%(mean_class2-mean_class1))-log(length(group1_index)/length(group2_index))
  # preds<-apply(vec1,1,function(x){ ifelse(x>vec2,"Female","Male")})
  
  #discriminat function of male
  a0_m<- -0.5*(mean_class1%*%solve(S_w)%*%mean_class1)
  a1_m<- solve(S_w)%*%mean_class1 
  a0_f<--0.5*(mean_class2%*%solve(S_w)%*%mean_class2)
  a1_f<- solve(S_w)%*%mean_class2 

  
  a0<-log(p1/p2)-0.5*((mean_class1+mean_class2)%*%solve(S_w)%*%(mean_class2-mean_class1))
  a1<-t((mean_class2-mean_class1))%*%solve(S_w) #%*%t(as.matrix(x))
  
  
  
  #make predictions if w*x+C>0-->Male else Female
  w<-as.matrix(t(solve(S_w)%*%(mean_class1-mean_class2)))%*%t(as.matrix(x))
  c<-log(p1/p2)-0.5*(mean_class1%*%solve(S_w)%*%mean_class1)+0.5*(mean_class2%*%solve(S_w)%*%mean_class2)
  preds2<-ifelse(apply(w,2,function(x){x+c})>0,"Male","Female")
  
  
  
  return(list("predictions"=preds2,"slope"=lda_slope,"intercept"=lda_intercept,"Group_means"=means,
              "discriminant_function_Male"=c("inter"=a0_m,"coeffs"=a1_m),
              "discriminant_function_Male"=c("inter"=a0_f,"coeffs"=a1_f),
              "equation of decision bountary"=c("inter"=a0,"coeffs"=a1)))
  
}


crab<-read.csv("australian-crabs.csv",sep=",",header = T)

x<-crab[,5:6]
y<-crab[,2]


L<-LDA(x,y)

plot(x[,1],x[,2],col=as.factor(L$predictions))
abline(L$intercept,L$slope,col="green",lty=2, lwd=3)


mean(L$predictions==y)





