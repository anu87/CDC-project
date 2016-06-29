file.rename(CDC30K,df)

CDC30K$Sex=factor(CDC30K$Sex,levels = c("F", "M"), labels = c(0,1))
CDC30K$MaritalStatus=factor(CDC30K$MaritalStatus,levels = c("S","M","W","D","U"), labels = c(1,2,3,4,5))

CDC30K[,1:11]=lapply(CDC30K[,1:11],dummy)

df_0 <- head(CDC30K[CDC30K$MannerOfDeath==0,],7000)
df_1 <- head(CDC30K[CDC30K$MannerOfDeath==1,],6591)
df<-rbind(df_1,df_0)
df=df[,-11]

set.seed(12345)
intrain<-sample(nrow(df),0.7*nrow(df))
train<-df[intrain,]
test<-df[-intrain,]

#CART Model
library("tree")
tree.suicide=tree(MannerOfDeath~.-MannerOfDeath, train)
plot(tree.suicide,col=brewer.pal(3,"Set3"),lwd=3)
text(tree.suicide,pretty=0,cex=1,col=brewer.pal(2,"Set1"))
title("CART for Suicide", font=1)
summary(tree.suicide)
tree.pred=predict(tree.suicide,test,type = "class")
confusionMatrix(tree.pred,test$MannerOfDeath)


set.seed(1)
bag.suicide=randomForest(MannerOfDeath~.,data=train,mtry = 3,importance = TRUE)
bag.suicide

y.hat=predict(bag.suicide,newdata=test)
table(y.hat,test$MannerOfDeath)

confusionMatrix(y.hat,test$MannerOfDeath)

rf.table<-data.frame(FPR=fpr, TPR=tpr)
FPR <- sum(y.hat==1 & test$MannerOfDeath == 0)/sum(test$MannerOfDeath == 0)
knn.table$TPR[i] <- sum(pred.prop.knn > cutoff[i] & dfvalidation$paid == 1)/sum(dfvalidation$paid == 1)

# Naive Bayes

library(e1071)
model <- naiveBayes(MannerOfDeath~., data=train)
model
prediction <- predict(model, newdata = test[,-8])
confusionMatrix(test$MannerOfDeath,prediction,dnn=list('actual','predicted'))
model$apriori

predicted.probability <- predict(model, newdata = test[,-8], type="raw")
predicted.probability








