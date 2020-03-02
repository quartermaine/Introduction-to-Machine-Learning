##Assignment 1

set.seed(1234567890)
options(scipen=999)
library(geosphere)

stations <- read.csv("stations.csv",stringsAsFactors = F,fileEncoding = "latin1")
temps <- read.csv("temps50k.csv",stringsAsFactors = F)
st <- merge(stations,temps,by="station_number")
h_distance <- 20000        # These three values are up to the students
h_date <-200
h_time <-5
a <- 58.4274 #latitude # The point to predict (up to the students)
b <- 14.826  #logitude
date1 <- "2013-05-25" # The date to predict (up to the students)
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
#d2<-as.matrix()

mtimes<-matrix(0,nrow=nrow(st_filtered), ncol = length(timesf))

for(i in 1:length(timesf)){
  diff_hours<-difftime(timesf[i] , st_filtered$time, units = c("hours"))
  #diff_hours[diff_hours >12] <- 24 - diff_hours [diff_hours >12]
  mtimes[,i]<- as.vector(as.numeric(diff_hours))
}


k1<-sapply(d1,function(x){exp(-(x/h_distance)^2)})

k2<-sapply(d2,function(x){exp(-(x/h_date)^2)})

k3<-apply(mtimes,2,function(x){exp(-(x/h_time)^2)})

kernel<-function(a,b,c,type){
  a<-as.vector(a)
  b<-as.vector(b)
  c<-as.matrix(c)
  if (type=="sum"){
    temps<-as.vector(st_filtered$air_temperature)
    K1<-sum(a*temps)+sum(b*temps)+colSums(as.matrix(c)*temps)
    K2<-sum(a)+sum(b)+colSums(c)
    res<-K1/K2
    return(res)
    
  }
  else if(type=="mult"){
    temps<-as.vector(st_filtered$air_temperature)
    K1<-sum(a*temps)*sum(b*temps)*colSums(c*temps)
    K2<-sum(a)*sum(b)*colSums(c)
    res<-K1/K2
    return(res)
  }
  
}


plot(timesf,kernel(k1,k1,k3,"mult"),type="o")



plot(timesf,temp, type="o")








  
   
     