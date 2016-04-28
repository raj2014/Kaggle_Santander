rm(list=ls())

setwd("E:/Kaggle/Santander")
library(xgboost)
library(ggplot2)


# Reading the data
df_train <- read.csv("train.csv", stringsAsFactors = F)
df_test <- read.csv("test.csv", stringsAsFactors = F)



# magic bounds




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

df_train<-df_all[1:num_train_rows,]
df_test<-df_all[(num_train_rows+1):nrow(df_all),]



# magic bounds
m_var15 =as.vector(df_test[,'var15'])
m_saldo_medio_var5_hace2 = as.vector(df_test[,'saldo_medio_var5_hace2'])
m_saldo_var33 = as.vector(df_test['saldo_var33'])
m_var38 = as.vector(df_test[,'var38'])
m_NV=as.vector(df_test[,'num_var33']+df_test[,'saldo_medio_var33_ult3']+df_test[,'saldo_medio_var44_hace2']+df_test[,'saldo_medio_var44_hace3']+
  df_test[,'saldo_medio_var33_ult1']+df_test[,'saldo_medio_var44_ult1'])
m_V21 = as.vector(df_test[,'var21'])


print('Setting min-max lims on test data')
for(f in colnames(df_train)[-1]){
  lim <- min(df_train[,f])
  df_test[df_test[,f]<lim,f] <- lim
  
  lim <- max(df_train[,f])
  df_test[df_test[,f]>lim,f] <- lim
 
}




# binding it back to a single dataframe
df_all<-rbind(df_train,df_test)


#Building the model
set.seed(1234)

param <- list("objective" = "binary:logistic",
               "booster" = "gbtree",
              "eval_metric" = "auc",
              "eta"=0.01,
              "max.depth"=5,
              "nthread" = 3,
              "colsample_bytree" = 0.7, 
              "subsample" = 0.6815)


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


preds <- predict(xgb, data.matrix(df_all[-c(1:num_train_rows),-c(1)]))


preds[which(m_var15 < 23)] = 0
preds[which(m_saldo_medio_var5_hace2 > 160000)]=0
preds[which(m_saldo_var33 > 0)]=0
preds[which(m_var38 > 3988596)]=0
preds[which(m_NV>0)]=0
preds[which(m_V21>7500)]=0

res <- data.frame(ID = test$ID,TARGET = preds)

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

