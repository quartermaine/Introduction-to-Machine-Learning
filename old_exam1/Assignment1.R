videos<-read.csv("video.csv",sep=",",header=T)


n=dim(videos)[1]
set.seed(12345)
id<-sample(1:n,floor(n*0.5))
train<-videos[id,]
test<-videos[-id,]


#numeric<-names(dplyr::select_if(train,is.numeric))
#train_num<-train[,numeric]
#names(dplyr::select_if(train,is.numeric))
train_num<-train[, sapply(train, class) %in% c("integer","numeric") ]
train_pca<-train_num[,!names(train_num)%in%c("utime")]

pca_unscaled<-prcomp(train_pca)
pca.var<-pca_unscaled$sdev^2     
# calculate percentage of variance of PCAs
pca.var.per<-round(pca.var/sum(pca.var)*100,3)

print("The percentage of variance for feature space ")
pca.var.per

library(RColorBrewer)
par(mfrow=c(1,2))

barplot(pca.var.per[1:14],names.arg=c("PCA1","PCA2","PCA3","PCA4","PCA5","PCA6","PCA7","PCA8","PCA9","PCA10","PCA11","PCA12","PCA13","PCA14"),
        col=brewer.pal(8, "Set2"))

plot(cumsum(pca.var.per), xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#only one component needed to eplain 95% variation on the data


pca_scaled<-prcomp(train_pca,scale=T)
pca.var1<-pca_scaled$sdev^2     
# calculate percentage of variance of PCAs
pca.var.per1<-round(pca.var1/sum(pca.var1)*100,3)

print("The percentage of variance for feature space ")
pca.var.per1

library(RColorBrewer)
par(mfrow=c(1,2))

barplot(pca.var.per1[1:14],names.arg=c("PCA1","PCA2","PCA3","PCA4","PCA5","PCA6","PCA7","PCA8","PCA9","PCA10","PCA11","PCA12","PCA13","PCA14"),
        col=brewer.pal(8, "Set2"))

plot(cumsum(pca.var.per1), xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

cumsum(pca.var.per1) #9
#here we need 9 components to achive 95% explaination in variation of data 



##2
data_num<-videos[1:100, sapply(videos, class) %in% c("integer","numeric") ]
data=t(apply(as.matrix(data_num), 1, combn, 3, prod))
df<-as.data.frame(scale(data))

library(pamr)
x <- t(df)
y <- as.factor(videos$codec[1:100])

mydata<-list(x=x,y=y,geneid=as.character(1:nrow(x)), genenames=rownames(x))
cen_fit<-pamr.train(mydata,threshold=seq(0,4, 0.1))
set.seed(12345)
cen_cv<-pamr.cv(cen_fit,mydata)
pamr.plotcv(cen_cv)


##3
thes_max<-cen_cv$threshold[which(cen_cv$loglik==max(cen_cv$loglik))]


##4
videos2=videos

videos2$class=ifelse(videos2$codec=="mpeg4","mpeg4","other")

plot(videos2$frames,videos2$duration,col=as.factor(videos2$class))


##5
library(MASS)

lda.fit<-lda(as.factor(class)~frames+duration,data=videos2)
preds_lda<-predict(lda.fit,videos2,type="class")
plot(videos2$frames,videos2$duration, col=preds_lda$class)

mean(videos2$class!=preds_lda$class)


##6
library(tree)
tree_fit<-tree(as.factor(class)~frames+duration,data=videos2)
set.seed(12345)
cv.res=cv.tree(tree_fit)

plot(cv.res$size, cv.res$dev, type="b",
     col="red")

cv.res$size[which.min(cv.res$dev)]
tree_best<-prune.tree(tree_fit,best=11)

preds_tree<-predict(tree_best,videos2,type="class")
mean(videos2$class!=preds_tree)
mean(videos2$class!=predict(tree_fit,type="class"))




