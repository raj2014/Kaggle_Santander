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



# Removing constant columns
# Check for columnwise values
columnList<-sapply(df_all, function(x)(length(unique(x))))
columnList
columnList<-columnList[columnList>1]
columnList


columnIndexes<-which(colnames(df_all) %in% names(columnList))
df_all<-df_all[,columnIndexes]


duplicateColumns<-c()
 
# Removing duplicate columns
for (i in 1:(ncol(df_all)-1))
{
  for (j in (i+1):ncol(df_all))
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


## check for highly correlated columns

correlatedColumns<-c()

for (i in 1:(ncol(df_all)-2))
{
  for (j in (i+1):(ncol(df_all)-1))
  {
    if(cor(df_all[,i],df_all[,j])>0.98)
    {
      
      correlatedColumns<-c(correlatedColumns,i)
      
    }
    
  }
}

correlatedColumns<-unique(correlatedColumns)
length(correlatedColumns)
df_all<-df_all[,-correlatedColumns]



#### Feature Engineering block

# Feature1 : Adding all the zero values
countzeros <- function(x) {
  return( sum(x == 0) )
}

gzeros <- function(x) {
  return( sum(x < 0) )
}


derived_features<-data.frame(matrix(0, nrow = nrow(df_all)))
derived_features$zeroCounts <- apply(df_all[,-c(1,ncol(df_all))], 1, FUN=countzeros)
derived_features$gzeroCounts<- apply(df_all[,-c(1,ncol(df_all))], 1, FUN=gzeros)


# Not a right one for centering all the columns
# #scale and center for all the columns
# for (i in 2:(ncol(df_all)-1))
# {
#   df_all[,i]<- as.numeric(scale(df_all[,i],center=TRUE,scale=TRUE))
# }

# Splitting the data for model
train <- cbind(df_all[1:nrow(df_train),-c(ncol(df_all))],derived_features[1:nrow(df_train),-c(1)],TARGET=df_all[1:nrow(df_train),c(ncol(df_all))])
test  <- cbind(df_all[-(1:nrow(df_train)),-c(ncol(df_all))],derived_features[-(1:nrow(df_train)),-c(1)],TARGET=df_all[-(1:nrow(df_train)),c(ncol(df_all))])



# Adding two way interactions
# get all the highly correlated variables to target,do the interaction between them








# # Removing highly correlated variables
# #Removing highly correlated variables
# # On Hold
# cor_v<-abs(cor(df_all))
# diag(cor_v)<-0
# cor_v[upper.tri(cor_v)] <- 0
# cor_f <- as.data.frame(which(cor_v > 0.85, arr.ind = T))
# all_dat <- all_dat[,-unique(cor_f$row)]






#Building the model
set.seed(1)

param <- list("objective" = "binary:logistic",
               "booster" = "gbtree",
              "eval_metric" = "auc",
              "eta"=0.01,
              "max.depth"=5,
              "nthread" = 3,
              "colsample_bytree" = 0.7, 
              "subsample" = 0.65)

y <- as.numeric(train$TARGET)



dtrain<-xgb.DMatrix(data=data.matrix(train[,-c(1,ncol(train))]),label=train$TARGET)
watchlist<-list(train=dtrain)


# Prediction on test data
xgb<-xgb.train(   params              = param, 
                  data                = dtrain, 
                  label=y,
                  nrounds             = 1000, #1500, 
                  verbose             = TRUE,  #1
                  metrics={'auc'},
                  watchlist = watchlist,
                  print.every.n = 30
)

y_pred <- predict(xgb, data.matrix(test[,-c(1,ncol(test))]))

res <- data.frame(ID = test$ID,TARGET = y_pred)

write.csv(res,"submission_4.csv", row.names = FALSE)



y<-train$TARGET
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
                "colsample_bytree" = 0.7, 
                "subsample" = 0.65)
  
  k<-xgb.cv(params=param,nrounds=900,data=as.matrix(train[,-c(1,ncol(train))]),nfold=5,
         label=y,
         metrics={'auc'},
         print.every.n = 50,
         verbose=TRUE,
         showsd=FALSE,
         maximize=FALSE)
  
  avg_cv<-append(avg_cv,unlist(k[nrow(k),"test.auc.mean",with=FALSE]))
}
paste("AVG CV: ",mean(avg_cv),"SD CV : ",sd(avg_cv))

