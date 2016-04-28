rm(list=ls())

setwd("E:/Kaggle/Santander")
library(xgboost)
library(ggplot2)


# Reading the data
df_train <- read.csv("train.csv", stringsAsFactors = F)
df_test <- read.csv("test.csv", stringsAsFactors = F)


# Merging training and test data

target<-df_train$TARGET
df_train$TARGET <- NULL
df_all <- rbind(df_train, df_test)



# Removing constant columns
# Check for columnwise values
columnList<-sapply(df_all, function(x)(length(unique(x))))
#columnList
columnList<-columnList[columnList>1]
#columnList


columnIndexes<-which(colnames(df_all) %in% names(columnList))
df_all<-df_all[,columnIndexes]


duplicateColumns<-c()
 
# Removing duplicate columns
for (i in 2:(ncol(df_all)-1))
{
  for (j in (i+1):(ncol(df_all)))
  {
    if(identical(df_all[,i],df_all[,j]) == TRUE)
    {
      duplicateColumns<-c(duplicateColumns,i)
    }
    
  }
}

duplicateColumns<-unique(duplicateColumns)


# duplicate columns
df_all<-df_all[,-duplicateColumns]

num_train_rows<-nrow(df_train)








#### Feature Engineering block

# Feature1 : Adding all the zero values
countzeros <- function(x) {
  return( length(which(x == 0)) )
}

# Feature1 : Adding all the zero values

countnegatives <- function(x) {
  return( length(which(x < 0)) )
}



df_all$countzeros<-apply(df_all,1,countzeros)

df_all$countnegs<-apply(df_all,1,countnegatives)



# Adding two way interactions
# get all the highly correlated variables to target,do the interaction between them







#Building the model
set.seed(1234)

param <- list("objective" = "binary:logistic",
               "booster" = "gbtree",
              "eval_metric" = "auc",
              "eta"=0.01,
              "max.depth"=5,
              "nthread" = 3,
              "colsample_bytree" = 0.7, 
              "subsample" = 0.8)


dtrain<-xgb.DMatrix(data=data.matrix(df_all[c(1:num_train_rows),-c(1)]),label=target)
watchlist<-list(train=dtrain)

set.seed(1)
# Prediction on test data
xgb<-xgb.train(   params              = param, 
                  data                = dtrain, 
                  nrounds             = 950, #1500, 
                  verbose             = TRUE,  #1
                  metrics={'auc'},
                  watchlist = watchlist,
                  print.every.n = 30
)

xgbimp<-xgb.importance(colnames(df_all)[-1],model=xgb)


y_pred <- predict(xgb, data.matrix(df_all[c(1:num_train_rows),-c(1)]))

res <- data.frame(ID = test$ID,TARGET = y_pred)

write.csv(res,"submission_7.csv", row.names = FALSE)



seeds<-c(1,2,3,4,5)
avg_cv<-c()
for (i in seeds)
  
{
  set.seed(i)
  param <- list("objective" = "binary:logistic",
                "booster" = "gbtree",
                "eval_metric" = "auc",
                "eta"=0.01,
                "max.depth"=5,
                "nthread" = 3,
                "colsample_bytree" = 0.65, 
                "subsample" = 0.69)
  
  k<-xgb.cv(params=param,nrounds=850,data=dtrain,nfold=5,
         metrics={'auc'},
         print.every.n = 50,
         verbose=TRUE,
         showsd=FALSE,
         maximize=FALSE)
  
  avg_cv<-append(avg_cv,unlist(k[nrow(k),"test.auc.mean",with=FALSE]))
}
paste("AVG CV: ",mean(avg_cv),"SD CV : ",sd(avg_cv))

