##Assignment 3 - Uncertainty estimation

###1
library(ggplot2)

state<-read.csv("State.csv",header = T,sep=";")

for (i in 2:6){
  state[,i]<-as.numeric(gsub(",",".",state[,i]))
}


ord_state<-state[order(state[,"MET"]),]

pl<-ggplot(ord_state,aes(y=EX,x=MET))+geom_point()
pl


###2
library(tree)

modtree<-tree(EX~MET,data=ord_state,minsize=8)
set.seed(12345)
cv.res=cv.tree(modtree)
plot(cv.res$size, cv.res$dev, type="b",col="red")       #
plot(log(cv.res$k), cv.res$dev,type="b", col="red")     # optimal number of leaves is 3??

best_leaf <- cv.res$size[which(cv.res$dev==min(cv.res$dev))] 
best_leaf                                              #confirmed

btree<-prune.tree(modtree, best = best_leaf)
btree

pred_btree<-predict(btree,ord_state)
res_tree<-pred_btree-ord_state$EX

plot(ord_state$EX,col="blue")   #plot of the original data
points(pred_btree,col="red")    #ading point of predictions

hist(abs(res_tree))             #The following histogram of residuals suggests 
                                #that the residuals are not normally distributed. 
                                #the distribution of the residuals is quite skewed maybe following chi-square 



plot(ord_state$MET,ord_state$EX, pch=19, col="black")
partition.tree(btree, label="tree", add=TRUE)


###3
library(boot)
set.seed(12345)
# computing bootstrap samples
f=function(data, ind){
  data1=data[ind,]# extract bootstrap sample
  mod<-tree(EX~MET,data=data1,minsize=8)
  modd<-prune.tree(mod,best=3)  #fit tree model
  #predict values 
  pEX=predict(modd,newdata=ord_state)
  return(pEX)
}

np_boot<-boot(ord_state, f, R=1000) #make bootstrap


e=envelope(np_boot) #compute confidence bands

plot(ord_state$MET, ord_state$EX, pch=21, bg="orange")
points(ord_state$MET,pred_btree,type="l") #plot fitted line
#plot cofidence bands
points(ord_state$MET,e$point[2,], type="l", col="blue")
points(ord_state$MET,e$point[1,], type="l", col="blue")


###4


rng=function(data, mle) {
  data1=data.frame(EX=data$EX, MET=data$MET)
  n=length(data$EX)
  #generate new EX
  m<-predict(mle,data=data1)
  s<- sqrt(var(predict(mle, newdata=data1),ord_state$EX))     #
  data1$EX<-rnorm(n,m,s)
  return(data1)
}

f1=function(data1){
  mod<-tree(EX~MET,data=data1,minsize=8)
  modd<-prune.tree(mod,best=3)                 #fit tree
    #predict values from the original data
  predictedP<-predict(modd,ord_state)
  return(predictedP)
  
}

p_boot=boot(ord_state, statistic=f1, R=10000,
         mle=btree,ran.gen=rng, sim="parametric")


e1=envelope(p_boot) #compute confidence bands

plot(ord_state$MET, ord_state$EX, pch=21, bg="orange")
points(ord_state$MET,pred_btree,type="l") #plot fitted line
#plot cofidence bands
points(ord_state$MET,e1$point[2,], type="l", col="blue")
points(ord_state$MET,e1$point[1,], type="l", col="blue")

