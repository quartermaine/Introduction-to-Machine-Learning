set.seed(12345)
ind<-sample(1:nrow(crab),floor(nrow(crab))*0.5)
ctrain<-crab[ind,]
ctest<-crab[-ind,]

group_blue <- which( ctrain$species == "Blue" )
group_orange <- which( ctrain$species == "Orange")

ctrain_blue<-ctrain[group_blue,]
ctrain_orange<-ctrain[group_orange,]

pr_blue_male<-nrow(ctrain_blue[ctrain_blue$sex=="Male",])/nrow(ctrain_blue)
pr_blue_female<-nrow(ctrain_blue[ctrain_blue$sex=="Female",])/nrow(ctrain_blue)
pr_orange_male<-nrow(ctrain_orange[ctrain_orange$sex=="Male",])/nrow(ctrain_orange)
pr_orange_female<-nrow(ctrain_orange[ctrain_orange$sex=="Female",])/nrow(ctrain_orange)


func<-function(y){
  mean<-c()
  sd<-c()
  for (i in 4:ncol(y)){
    mean[i-3]<-mean(y[,i])
    sd[i-3]<-sd(y[,i])
  }
  list("means_blue"=mean,"sd_blue"=sd)
}
  
mb<-func(ctrain_blue)
mo<-func(ctrain_orange)

