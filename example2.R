#Bank Marketing data
setwd("D:/OneDrive - CBS - Copenhagen Business School/Clustering Project/Results/grasp_data1/paper")
library("data.table");library("curl");library("fastDummies");library("plyr");library("dplyr");library("magrittr")
library("caret");library("plotly");library("MLmetrics"); library("pROC")
mydata <- read.csv("bank-additional.csv",sep=";",stringsAsFactors = T)

##### 1. Pre-treatment of categorical variables #####

mydata$default = ifelse(mydata$default=="yes",1,0)
mydata$housing = ifelse(mydata$housing=="yes",1,0)
mydata$loan = ifelse(mydata$loan=="yes",1,0)
mydata$contact = ifelse(mydata$contact=="cellular",1,0)


######2. Separating categorical and binary-continuous variables####

fact = Filter(is.factor, mydata[,1:(ncol(mydata)-1)])
num = Filter(is.numeric, mydata[,1:(ncol(mydata))])
mydata = data.frame(fact,num,mydata$y)
colnames(mydata)[21] = c("y")

for(i in 1:6){
  mydata[,ncol(mydata)+1] = table(mydata[,i])[mydata[,i]]
}
categ_thr <- floor(0.002*nrow(mydata))

newdata = mydata
for(i in 22:ncol(mydata)){
  newdata <- newdata[ which(newdata[,i]>categ_thr), ]
}

mydata = newdata[,1:21]

rm(newdata)


colnames(mydata) = c(paste0("X",letters[1:6]),paste0("Z",letters[1:14]),"Y")
levels(mydata$Y) = c(0,1)
for(i in 1:6){
  mydata[,i] = factor(mydata[,i])
}

## Divide in training and testing
set.seed(123)
smp_size <- floor(0.7 * nrow(mydata))
train_ind <- sample(seq_len(nrow(mydata)), size = smp_size)

train <- mydata[train_ind, ]
test <- mydata[-train_ind, ]
j = ncol(fact)

install.packages("clustcat")
library(clustcat)
order = ordered_categ(train,j,mydata)
feasible_clusterings(train,j,mydata)

data_final = clustered_model(train,j,mydata)

model_orig = glm(Y~.,train,family="binomial")
p = predict(model_orig,newdata=test,type="response")
acc_orig = Accuracy(ifelse(p>0.5,1,0),test$Y)

train <- data_final[train_ind, ]
test <- data_final[-train_ind, ]
model_final = glm(Y~.,train,family="binomial")
p = predict(model_final,newdata=test,type="response")
acc_final = Accuracy(ifelse(p>0.5,1,0),test$Y)
