require(neuralnet)

set.seed(1234567890)

##one layer


points<-runif(50,0,10)
dat<-data.frame(points,sin(points))
train_dat<-dat[1:25,]
val_dat<-dat[26:50,]

restrain <- double(10)
resval <- double(10)
weights<-runif(41, -1, 1)


for (i in 1:10){
  nn<-neuralnet(sin.points.~points,data=train_dat,
                hidden=c(10),startweights = weights,
                threshold = i/1000,lifesign="none")
  
  res1<-compute(nn,train_dat[,1])$net.result
  restrain[i]<-mean((train_dat[,2]-res1)^2)
  
  res2<-compute(nn,val_dat[,1])$net.result
  resval[i]<-mean((val_dat[,2]-res2)^2)
  
}

plot(restrain, type = "o")
plot(resval, type = "o")
restrain
resval


##2 layers

set.seed(1234567890)
points<-runif(50,0,10)
dat<-data.frame(points,sin(points))
train_dat<-dat[1:25,]
val_dat<-dat[26:50,]

restrain2 <- double(10)
resval2 <- double(10)
# Random initializaiton of the weights in the interval [-1, 1]
winit1 <- runif(22, -1, 1) 
for(i in 1:10) {
  nn <- neuralnet(formula = sin.points. ~ points, data = train_dat, 
                  hidden = c(3,3), startweights = winit1,
                  threshold = i/1000, lifesign = "none")
  
  # nn$result.matrix
  # Compute predictions for the trainig set and their mean squared error
  res3 <- compute(nn, train_dat[,1])$net.result 
  restrain2[i] <- mean((train_dat$sin.points.-res3)^2)
  # The same for the validation set
  res4 <- compute(nn, val_dat[,1])$net.result 
  resval2[i] <-mean ((val_dat$sin.points.-res4)^2)
}

plot(restrain2, type = "o")
plot(resval2, type = "o")
restrain2
resval2



##best architecture one layer with threshold=4/1000

p <- runif(50, 0, 10)
d <- data.frame(p, Sin=sin(p))

weights_init <- runif(31, -1, 1)
nn <- neuralnet(formula = sin.points. ~ points, data = dat, 
                hidden = 10, startweights = weights_init,
                threshold = 4/1000, lifesign = "full")

y_nn<-compute(nn,d[,1])$net.result

sum((d[,2] - y_nn)^2)/2 #squared error
#mean((d[,2]-y_nn)^2) #mean squared error




