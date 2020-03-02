##Assignment 1

set.seed(1234567890)
options(scipen=999)
library(geosphere)

stations <- read.csv("stations.csv",stringsAsFactors = F,fileEncoding = "latin1")
temps <- read.csv("temps50k.csv",stringsAsFactors = F)
st <- merge(stations,temps,by="station_number")
h_distance <- 20000       # These three values are up to the students
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



kernel_dist_length<-function(a,b,h_distance){
  dist<-distm(st_filtered[,c("longitude","latitude")], c(b,a),fun=distHaversine)
  exp(-(dist/h_distance )^2)
}

kernel_dist_days<-function(date,h_date){
  dist<-as.numeric(difftime(date,st_filtered$date,units = "days"))
  exp(-(dist/h_date)^2)
}



kernel_dist_time <-function( time ,h_time){
  dist <- as.numeric(difftime(st_filtered$time,time , units="hours"))
  dist [dist >12] <- 24 - dist [dist >12]
  exp(-(dist/h_time )^2)
}


kernel_sum <-function(time){
  K<- as.vector(kernel_dist_length(a,b,h_distance)) * as.vector(kernel_dist_time(time,h_time)) *
    as.vector(kernel_dist_days(pred_date,h_date))
  
  sum(K*st_filtered$air_temperature)/sum(K)
  
}


for (i in 1:length(timesf)){
  temp[i]<-kernel_sum(timesf[i])
}

plot(timesf,temp, type="o")
