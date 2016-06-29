
DeathRecords <- read.csv("~/Desktop/DeathRecords/DeathRecords.csv")
df <- subset(DeathRecords, MannerOfDeath !="7")
df2 <- subset(df, MannerOfDeath !=0)
df3 <- subset(df, MannerOfDeath !=5)

rm(DeathRecords)
rm(df)
rm(df2)

load("/Users/Bhuti/Desktop/CDC2/CDC30K.RData")

CDC30K$Sex=factor(CDC30K$Sex,levels = c("F", "M"), labels = c(0,1))
CDC30K$MaritalStatus=factor(CDC30K$MaritalStatus,levels = c("S","M","W","D","U"), labels = c(1,2,3,4,5))

summary(df3)
df[,1:12]=as.numeric(df[,1:12])

install.packages("caret")
set.seed(12345)
train <- sample(nrow(CDC30K), 0.7*nrow(CDC30K))
dftrain <- CDC30K[train,]
dftest <- CDC30K[-train,]

install.packages("neuralnet")
library("neuralnet")

df3[,1:4]=lapply(df3[,1:4],factor)
df3[,5:12]=lapply(df3[,5:12],factor)

install.packages("dummy")
attach(dftrain)

y_train <- dftrain[,37]
dftrain=dftrain[,-37]

df_dum <- model.matrix(~.,CDC30K)

df_dum=df_dum[,-1]

set.seed(12345)
train <- sample(nrow(df_dum), 0.7*nrow(df_dum))
dftrain <- df_dum[train,]
dftest <- df_dum[-train,]

a <- paste(colnames(dftrain[,-38]),collapse = '+')

nnet <- neuralnet(y_train ~ ResidentStatus2+ResidentStatus3+ResidentStatus4+
                    Education2003Revision1+Education2003Revision2+Education2003Revision3+
                    Education2003Revision4+Education2003Revision5+Education2003Revision6+
                    Education2003Revision7+Education2003Revision8+Education2003Revision9+
                    MonthOfDeath+Sex1+AgeRecode122+AgeRecode123+AgeRecode124+AgeRecode125+
                    AgeRecode126+AgeRecode127+AgeRecode128+AgeRecode129+AgeRecode1210+AgeRecode1211+
                    AgeRecode1212+MaritalStatus2+MaritalStatus3+MaritalStatus4+MaritalStatus5+
                    DayOfWeekOfDeath2+DayOfWeekOfDeath3+DayOfWeekOfDeath4+DayOfWeekOfDeath5+
                    DayOfWeekOfDeath6+DayOfWeekOfDeath7+DayOfWeekOfDeath9+
                    PlaceOfInjury2+PlaceOfInjury3+PlaceOfInjury4+PlaceOfInjury5+PlaceOfInjury6+
                    PlaceOfInjury7+PlaceOfInjury8+PlaceOfInjury9+PlaceOfInjury99+RaceRecode52+
                    RaceRecode53+RaceRecode54+HispanicOriginRaceRecode2+HispanicOriginRaceRecode3+
                    HispanicOriginRaceRecode4+HispanicOriginRaceRecode5+HispanicOriginRaceRecode6+
                    HispanicOriginRaceRecode7+HispanicOriginRaceRecode8+HispanicOriginRaceRecode9, 
                  dftrain, hidden = 3)



dfnet <- nnet(MannerOfDeath~.,dftrain,size = 3)
plot.nn(nnet)
  