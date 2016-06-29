
install.packages("psych")
library(psych)

df <- read.csv("/Users/Bhuti/Desktop/CDC/CDC30K.csv")

rownames(df)=df[,1]
df[,1]<-NULL
df[,11]<-NULL

df[,1:10]=lapply(df[,1:10],factor)
df_dum=model.matrix(~., df)
df_dum=data.frame(df_dum[,-1])

suicide <- data.frame(df_dum[,47])
dfsc <- scale(df_dum[,-47])

# K-means clustering

set.seed(12345)
km.out=kmeans(dfsc,5,nstart=20)
km.out1=kmeans(dfsc,6,nstart=20)
km.out2=kmeans(dfsc,8,nstart=20)

dist(km.out$centers)
clusplot(dfsc,km.out$cluster,color = TRUE,shade = TRUE,labels = 2,lines = 0)
clusplot(dfsc,km.out1$cluster,color = TRUE,shade = TRUE,labels = 2,lines = 0)
clusplot(dfsc,km.out2$cluster,color = TRUE,shade = TRUE,labels = 2,lines = 0)

plotcluster(dfsc,km.out$cluster)

cluster <- km.out1$cluster
dfclus<-data.frame(df$MannerOfDeath,cluster)
aggregate(.~cluster,data=dfclus,mode)
