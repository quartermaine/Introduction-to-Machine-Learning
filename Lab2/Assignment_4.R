#Assignment 4 - Principal components

#1
nir<-read.csv2("NIRspectra.csv",sep=";")

pca<-prcomp(nir[,1:126],scale=T)   # we scale because features have diffrent variance


pca.var<-pca$sdev^2     # calculate variance of each PCA
pca.var.per<-round(pca.var/sum(pca.var)*100,2)     # calculate percentage of variance of PCAs 




#############  plot of percentages of PCAs variance
library(ggplot2)

D_pca<-as.data.frame(pca.var.per[1:7])
D_pca$PCAs<-c("PCA1","PCA2","PCA3","PCA4","PCA5","PCA6","PCA7")

ggplot(D_pca,aes(y=D_pca[,1],x=D_pca[,2],group=1,fill=D_pca[,2]))+
  geom_bar(stat = "identity")+labs(title="Percentages of PCAs",y="percentage",x="PCAs")

################ plot of PCAs 
pca_df= as.data.frame(pca$x)    #data frame of pca 2 points 

ggplot(data = pca_df, aes(x = PC1, y = PC2)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(colour = "blue") +
  ggtitle("PCA plot")

  #geom_line(aes(y= pca$rotation[,1]))

#biplot(pca, scale = 0) #plot of pcas with labels with biplot

biplot(pca, col = c("gray", "black"),scale=0)

#2

U=pca$rotation
plot(U[,1], main="Traceplot, PC1")   #trace plot PCA1
plot(U[,2],main="Traceplot, PC2")    #trace plots PCA2



#As we can see 

#3

#
library(fastICA)

set.seed(12345)
a <- fastICA(nir[,c(1:126)], 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "R", row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = T)


w<-a$K%*%a$W


plot(w[,1],main="PCA2")
plot(w[,2],main="PCA1")

#

