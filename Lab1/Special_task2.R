eucl_dist<-function(X){
  n<-nrow(X)
  dist_mat_e<-matrix(0,nrow = n,ncol=n)
  for (i in 1:nrow(X)){
    for(j in 1:nrow(X)){
      dist_mat_e[i,j]<-sqrt(sum((X[i,]-X[j,])^2))
    }
  }
  dist_mat_e
}

knn_density<-function(data,k,cls){
  distM<-eucl_dist(data)#as.matrix(dist(cars)) 
  distM<-as.data.frame(distM)
  distM$cls<-cls
  N<-ncol(distM)-1
  bestM<-list()
  K<-for (i in 1:N ){
    ord_disM<-distM[order(distM[,i]),]
    best_ks<-ord_disM[(k+1),i]
    bestM[[i]]<-best_ks
    c<-unlist(bestM)
  }
  bestM<-as.data.frame(bestM)
  k/(nrow(data)*c)
}


dens<-knn_density(cars,6,cars[,1])
hist(cars[,1],freq = F)
lines(seq(.5,25,0.5),dens)




#############################################################################################################################

fw<-cbind(cars[,1],dens)
fw<-as.data.frame(fw)
colnames(fw)<-c("speed","counts")

library(dplyr)
fw_grouped<-fw%>%group_by(speed) %>% summarise_all(funs(sum))
V<-list()
for (i in 1:nrow(fw_grouped)){
  V[[i]]<-rep(as.numeric(fw_grouped[i,1]),as.numeric(fw_grouped[i,2]))
  df<-unlist(V)
  
}
############--Hist&Density of knn kernel density estimator--################## 
hist(dens,freq = F)
lines(density(dens))

############--Hist&Density of cars kernel density estimator--################## 
hist(cars[,1],freq = F)
lines(density(cars[,1]))



fw_grouped$densities<-fw_grouped$counts/nrow(cars)
ggplot(fw_grouped,aes(speed,dens))+geom_density(aes(y = ..density..))+geom_bar(stat="identity",aes(y=densities),width=1.5)



# #barplot(fw_grouped$speed,fw_grouped$V1)
# plot(fw,type="l")
# plot(density(cars[,1]))

df <- approxfun(dens)
plot(dens,type="l")
xnew <- c(0.45,1.84,2.3)
points(xnew,df(xnew),col=2)
