# set working directory

rm(list=ls())

setwd("E:/Kaggle/Santander")



library(xgboost)
library(ggplot2)

# Reading the data
df_train <- read.csv("train.csv", stringsAsFactors = F)
df_test <- read.csv("test.csv", stringsAsFactors = F)


# Merging training and test data
df_test$TARGET <- NA
df_all <- rbind(df_train, df_test)



# checking all the column types
columnList<-sapply(df_all, function(x)(class(x)))

columnList[columnList=="numeric"]
length(columnList[columnList=="numeric"])

columnList[columnList=="integer"]
length(columnList[columnList=="integer"])

columnList[columnList=="character"]
length(columnList[columnList=="character"])


# Check for columnwise values
columnList<-sapply(df_all, function(x)(length(unique(x))))
columnList
columnList<-columnList[columnList>1]
columnList
qplot(names(columnList),columnList,geom="point",ylim=c(0,10))


# Current columnlist
length(columnList)

sort(columnList,decreasing=TRUE)[1:10]
qplot(df_train$var38,geom="density")

# Understand the distribution of each and every variable
qplot(columnList,geom="Histogram",xlim=c(0,200))
col_standard<-columnList[columnList>200]

col_standard
length(col_standard)

# Standardizing columns greater than 200 
for (col in names(col_standard))
{
  df_all[,col]<-as.numeric(scale(df_all[,col],center=TRUE,scale=TRUE))
}


# check for 

