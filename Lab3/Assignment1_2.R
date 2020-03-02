##Assignment 1

set.seed(1234567890)
library(geosphere)

stations <- read.csv("stations.csv",stringsAsFactors = F,fileEncoding = "latin1")
temps <- read.csv("temps50k.csv",stringsAsFactors = F)
st <- merge(stations,temps,by="station_number")
h_distance <- 20000        # These three values are up to the students
h_date <-20
h_time <-5
a <- 58.4274 #latitude # The point to predict (up to the students)
b <- 14.826  #logitude
date1 <- "2013-08-23" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00","08:00:00" ,"10:00:00","12:00:00","14:00:00", 
           "16:00:00","18:00:00","20:00:00","22:00:00","24:00:00")


temp <- vector(length=length(times))
# Students’ code here

timesf<-as.POSIXct(times,format="%H:%M:%S")
pred_date<-as.POSIXct( date1 , format = "%Y-%m-%d")

st$date<-as.POSIXct(st$date,format="%Y-%m-%d")
st$time<-as.POSIXct(st$time,format="%H:%M:%S")


st_filtered<-st[st$date< pred_date ,]



d1<-distm(st_filtered[,c("longitude","latitude")], c(b,a),fun=distHaversine)
d2<-as.numeric(difftime(pred_date ,st_filtered$date,units = "days"))


func <-function( time ,h){
  dist <- as.numeric(difftime(st_filtered$time,time , units="hours"))
  dist [dist >12] <- 24 - dist [dist >12]
  exp(-(dist/h)^2)
}


k1<-exp(-(d1/h_distance)^2)

k2<-exp(-(d2/h_date)^2)



for(i in 1:length(timesf)){
  k3<-func(timesf[i],h_time)
  K<-as.vector(k1)*as.vector(k2)*as.vector(k3)
  temp[i]<-sum(K*st_filtered$air_temperature)/sum(K)
  
}

plot(timesf,temp, type="o")




##################################Summation#############################################################


##Assignment 1

set.seed(1234567890)
library(geosphere)

stations <- read.csv("stations.csv",stringsAsFactors = F,fileEncoding = "latin1")
temps <- read.csv("temps50k.csv",stringsAsFactors = F)
st <- merge(stations,temps,by="station_number")
h_distance <- 200000        # These three values are up to the students
h_date <-2000
h_time <-5
a <- 58.4274 #latitude # The point to predict (up to the students)
b <- 14.826  #logitude
date1 <- "2013-08-25" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00","08:00:00" ,"10:00:00","12:00:00","14:00:00", 
           "16:00:00","18:00:00","20:00:00","22:00:00","24:00:00")


temp <- vector(length=length(times))
# Students’ code here

timesf<-as.POSIXct(times,format="%H:%M:%S")
pred_date<-as.POSIXct( date1 , format = "%Y-%m-%d")

st$date<-as.POSIXct(st$date,format="%Y-%m-%d")
st$time<-as.POSIXct(st$time,format="%H:%M:%S")



st_filtered<-st[st$date< pred_date & st$time,]



d1<-distm(st_filtered[,c("longitude","latitude")], c(b,a),fun=distHaversine)
d2<-as.numeric(difftime(pred_date ,st_filtered$date,units = "days"))


func <-function( time ,h){
  dist <- as.numeric(difftime(st_filtered$time,time , units="hours"))
  exp(-(dist/h)^2)
}


k1<-exp(-(d1/h_distance)^2)

k2<-exp(-(d2/h_date)^2)



for(i in 1:length(timesf)){
  k3<-func(timesf[i],h_time)
  K<-as.vector(k1)+as.vector(k2)+as.vector(k3)
  temp[i]<-sum(K*st_filtered$air_temperature)/sum(K)
  
}

plot(timesf,temp, type="o")


##Assignment 2


library(kernlab)

#library(e1071)
data("spam")

###Model selection 
n=dim(spam)[1]
set.seed(1234567890)
id=sample(1:n, floor(n*0.5))
train_spam=spam[id,]
id1=setdiff(1:n, id)
set.seed(1234567890)
id2=sample(id1, floor(n*0.3))
valid_spam=spam[id2,]
id3=setdiff(id1,id2)
test_spam=spam[id3,]


svm1<-ksvm(type~.,data=train_spam,
           kernel = "rbfdot", kpar =list(sigma = 0.05),C = 0.5)

svm2<-ksvm(type~.,data=train_spam,
           kernel = "rbfdot", kpar =list(sigma = 0.05),C = 1)

svm3<-ksvm(type~.,data=train_spam,
           kernel = "rbfdot", kpar =list(sigma = 0.05),C = 5)


index_class_column<-which(names(spam)=="type")
svm1_preds_valid<-predict(svm1,valid_spam[,-index_class_column],
                          type="response")

svm2_preds_valid<-predict(svm2,valid_spam[,-which(names(spam)=="type")],
                          type="response")

svm3_preds_valid<-predict(svm3,valid_spam[,-index_class_column],
                          type="response")

#table(valid_spam$type,svm1_preds_valid)

error1<-mean(valid_spam$type!=svm1_preds_valid)
error2<-mean(valid_spam$type!=svm2_preds_valid)
error3<-mean(valid_spam$type!=svm3_preds_valid)

errors_valid<-c(error1,error2,error3)

barplot(errors_valid,names.arg=c("svm-C=0.5", "svm-C=1", "svm-C=5"),col=c("green","red","blue"),
        main = "Validation errors")
text((errors_valid/1.1),labels=paste0(round(errors_valid*100,digits=3),"%"))

#we choose svm3 because it has the lowest validation error 


###generalization error
dt<-rbind(train_spam,valid_spam)
svm_best<-ksvm(type~.,data=dt,
               kernel = "rbfdot", kpar =list(sigma = 0.05),C = 5)

svm3_preds_test<-predict(svm_best,test_spam[,-58],type="response")

error6<-mean(test_spam$type!=svm3_preds_test)
cat("The generalization error for the best svm model is: ",error6)

###report the svm 
svm_final<-ksvm(type~.,data=spam,
                kernel = "rbfdot", kpar =list(sigma = 0.05),C = 5)
svm_final
###role of C
#the role of the C is 


#kernlab::plot(svm2,data=spam[,-ncol(spam)]) i cannot plot the svm's models?!





