##Assignment 1

###1
data<-readxl::read_xlsx("ïnfluenza.xlsx")

plot(data$Time,data$Mortality,type="l",col="red",ylim=c(0,2800))
lines(data$Time,data$Influenza,type="l",col="blue")


###2
library(mgcv)
set.seed(12345)
gam.fit=gam(Mortality~s(Week,k=length(unique(data$Week)))+Year, data=data,method="GCV.Cp")


coef(gam.fit)

###3

preds<-predict(gam.fit,data)
plot(data$Time,data$Mortality,type="l",col="green")
lines(data$Time,preds,col="red")


summary(gam.fit)

#we can see from the p-values that the intercept,Year are not significant and the Week is significant 
#we can see from the plot that there is definetely a trend in mortality since we have one peak  every year and one trough.

vis.gam(gam.fit, n.grid = 50, theta = 35, phi = 32, zlab = "",
        ticktype = "detailed", color = "topo", main = "t2(D, W)")


plot(gam.fit)  #plot spline component in the question 

require(visreg)
visreg(gam.fit, "Week", gg=TRUE)



###4

gam.fit_low=gam(Mortality~s(Week,k=length(unique(data$Week)),sp=0.1)+
                  Year, data=data,method="GCV.Cp")
gam.fit_high=gam.fit=gam(Mortality~s(Week,k=length(unique(data$Week)),sp=1000)+
                           Year, data=data,method="GCV.Cp")


preds_low<-predict(gam.fit_low,data)

plot(data$Time,data$Mortality,type="l",col="green")
lines(data$Time,preds_low,col="red")



preds_high<-predict(gam.fit_high,data)

plot(data$Time,data$Mortality,type="l",col="green")
lines(data$Time,preds_high,col="red")



###5
plot(data$Time,gam.fit$residuals,type="l")
lines(data$Time,data$Influenza,col="orange")

cor(data$Influenza,gam.fit$residuals)

###6

gam.fit1=gam(Mortality~s(Week,k=length(unique(data$Week)))+
               s(Year,k=length(unique(data$Year)))+s(Influenza,k=length(unique(data$Influenza))), data=data)

summary(gam.fit)

plot(data$Time,data$Mortality,type="l")
#lines(data$Time,gam.fit1$fitted.values,col="red")  
lines(data$Time,predict(gam.fit1,data),col="red")


##Assignment 2

###1
df<-read.csv2("data.csv",sep=";")
df$Conference<-as.factor(df$Conference)
set.seed(12345)
ind<-sample(nrow(df),floor(0.7*nrow(df)))
dftrain<-df[ind,]
dftest<-df[-ind,]


library(pamr)
x<-t(dftrain[,-which(names(dftrain) == "Conference")])
y<-as.factor(dftrain$Conference)

list_df=list(x=x,y=y,
             geneid=as.character(1:nrow(x)), genenames=rownames(x))

par.model=pamr.train(list_df)
cvmodel=pamr.cv(par.model,list_df)
thres=cvmodel$threshold[which(cvmodel$error==min(cvmodel$error))]
#best_threshold=min(thres)  #we use min because we have 2 best thresholds

#pamr.plotcv(cvmodel)
pamr.plotcen(par.model, list_df, threshold=best_threshold)


a=pamr.listgenes(par.model,list_df,threshold=best_threshold,genenames=T)
nrow(a) #number of parameters selected
a[1:10,] #10 most contributing features ;{papers,important,submission,due,published,position,call.conference,dates,candidates}

dftest_features<-t(dftest[,-which(names(dftest) == "Conference")])
pred_pamr<-pamr.predict(par.model,dftest_features, threshold=best_threshold,type="class")

cat("===============================================\n",
    "The misclssification error for test data is : ",mean(pred_pamr!=dftest$Conference),
    "and the accuracy for test data is : ",mean(pred_pamr==dftest$Conference))


###2a
library(glmnet)
library(parallel)
#require(doMC)
#registerDoMC(cores=4)

X_data=dftrain[,-which(names(dftrain) == "Conference")]
y_data=as.factor(dftrain$Conference)



##########################################################################################
elastic.fit<-cv.glmnet(as.matrix(X_data),y_data,family="binomial",alpha=0.5,parallel=T)



fitted_test<-as.matrix(dftest[,-which(names(dftest)=="Conference")])
elastic_preds<-predict(elastic.fit,fitted_test,type="class",s="lambda.min")

cat("===============================================\n",
    "The misclssification error for test data is : ",mean(elastic_preds!=dftest$Conference),
    "and the accuracy for test data is : ",mean(elastic_preds==dftest$Conference))



coef(elastic.fit)

###2b
library(kernlab)

svm<-ksvm(as.matrix(X_data),y_data,kernel="vanilladot") #getting a warring

svm_preds<-predict(svm,fitted_test,type="response")
cat("===============================================\n",
    "The misclssification error for test data is : ",mean(svm_preds!=dftest$Conference),
    "and the accuracy for test data is : ",mean(svm_preds==dftest$Conference))


length(coef(svm)[[1]])

###3
library(parallel)
cl <- makeCluster(4)

t<-sapply(df[,-which(names(df)=="Conference")], function(x) t.test(x[df$Conference==1],x[df$Conference==0])[["p.value"]])

#parApply(cl=cl,df[,-which(names(df)=="Conference")],MARGIN=2, FUN=function(x) t.test(x[df$Conference==1],x[df$Conference==0])[["p.value"]])
#t[which(p.adjust(t,method = "hochberg")<0.05)]



benj<-function(p_values,alpha=0.05){
  p_values<-sort(p_values)
  indexes<-c(1:length(p_values))
  L<-p_values-((alpha*indexes)/length(p_values))
  best_p<-max(L[which(L<0)])
  cutoff<-p_values[L==best_p]
  rejected_values<-p_values[p_values<=cutoff]
  list(cutoff,rejected_values)
  
               
}


benj(t)

#the rejected are benj[[2]] no38






t[p.adjust(t, method = 'hochberg', n = length(t))<0.05]


q<-BH(t,alpha=0.05)
ifelse(q > 0.05, "norej", "rej")



