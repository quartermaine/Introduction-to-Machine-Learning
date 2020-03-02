library(ggplot2)
tecator<-readxl::read_excel("tecator.xlsx")

##1

p<-ggplot(tecator,aes(x=Moisture,y=Protein))+geom_point(color='darkblue')+ggtitle("ScatterPlot Moisture vs Protein")+theme_bw()+
  geom_smooth(method = 'nls', formula = y ~ x^b, start = list(b=3),se=FALSE) 
p


##2 
find the code in the report (use LATEX)

##3

n=dim(tecator)[1]
set.seed(123)
id=sample(1:n, floor(n*0.5))
traindata=tecator[id,]
validata=tecator[-id,]

d<- matrix(0, ncol = 2, nrow = 6)
colnames(d)<-c("mse_train","mse_test")
rownames(d)<-c("poly1","poly2","poly3","poly4","poly5","poly6")

for (i in 1:6){
  m<-lm(Moisture~poly(Protein,i),data=traindata)
  preds_valid<-predict(m,validata)
  preds_train<-predict(m,traindata)
  
  mse <- function(true,predicted) {
    return (mean((true-predicted)^2))
    }
  
  mse_train<-mse(traindata$Moisture,preds_train)
  mse_test<-mse(validata$Moisture,preds_valid)
  d[i,1]<-mse_train
  d[i,2]<-mse_test
}

dd<-as.data.frame(d)
dd$model<-rownames(dd)
###plot with ggplot
p1<-ggplot(dd,aes(x=model,y=mse_train,group=1))+geom_point(color="blue")+geom_line(colour="blue")
p1+geom_point(data=dd, aes(x=model,y=mse_test,color="red"))+geom_line(data=dd,aes(x=model,y=mse_test,color="red"))

###plot with plotly
library(plotly)

pp <- plot_ly(dd, x = ~model, y = ~mse_train, name = 'mse_train', type = 'scatter',mode="lines+markers")%>%
  add_trace(x=~model,y = ~mse_test, name =" mse_test", mode = 'markers+lines') 
pp



#====================Use the entire dataset for the rese======================================= 
###4
library(MASS)
tecator_fat<- tecator[!names(tecator)%in%c("Sample","Protein","Moisture")]
mod<-lm(Fat~.,data= tecator_fat)
step<-stepAIC(mod,direction="both",trace=F)
summary(step)
#the coefficients selected are
length(step$coefficients) # 64 wirh the intercept
mean((step$fit-tecator_fat$Fat)^2) #mean square error

###5##Ridge
library(glmnet)

covariates<-scale(tecator_fat[,-101])
responses<-scale(tecator_fat[,101])
mod1<-glmnet(as.matrix(covariates),responses,family="gaussian",alpha=0)
min_lambda=min(mod1$lambda)
plot(mod1,xvar="lambda",label=TRUE)

###6##LASSO
mod2<-glmnet(as.matrix(covariates),responses,family="gaussian",alpha=1)
plot(mod2,xvar="lambda",label=TRUE)

###7##CV-LASSO###

mod3<-cv.glmnet(as.matrix(covariates),responses,alpha=1,family="gaussian",lambda=seq(0,2,0.0001))
plot(mod3)
mod3$lambda.min
mod3$lambda.1se
co<-coef(mod3,s="lambda.1se")
length(co@x) #co@p
min(mod3$cvm)

#mse for LASSO
mean((tecator_fat$Fat-predict(mod3,tecator_fat[-101],s="lambda.1se"))^2)


