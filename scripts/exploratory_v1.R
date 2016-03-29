columnList<-sapply(df_all, function(x)(length(unique(x))))

columnList

prop.table(table(df_all$TARGET))

table(df_all$imp_trasp_var33_out_ult1)


table(df_all$imp_trasp_var33_out_ult1,df_all$TARGET)


imp_op_var41_ult1 

qplot(var38,facets=TARGET~.,data=train,geom="density")


# could not boost accuracy

# two way interaction between highly correlated variables and target

cor_target<-c()

for (i in 2:(ncol(train)-1))
{
  
  cor_target<-c(cor_target,abs(cor(train[,i],train$TARGET)))
  
}

names(cor_target)<-colnames(train)[2:(ncol(train)-1)]


cor_target<-sort(cor_target,decreasing=TRUE) [1:10]

cor_target


twoway_train<-matrix(0,nrow=nrow(train))
twoway_test<-matrix(0,nrow=nrow(test))
m<-combn(c(1:10),2)

for (i in 1:ncol(m))
{
  pair<- m[,i]
  pair<-names(cor_target)[pair]
  k<-unlist(train[,pair[1]])-unlist(train[,pair[2]])
  print (length(k))
  twoway_train<-cbind(twoway_train,k)
  k<-unlist(test[,pair[1]])-unlist(test[,pair[2]])
  twoway_test<-cbind(twoway_test,k)
  
  ## difference
}
colnames(twoway_train)<-paste("k_",c(1:ncol(twoway_train)),sep="")
colnames(twoway_test)<-paste("k_",c(1:ncol(twoway_test)),sep="")


train <- cbind(train[,-c(ncol(train))],twoway_train[,-1],TARGET=train[,ncol(train)])
test  <- cbind(test[,-c(ncol(test))],twoway_test[,-1],TARGET=test[,ncol(test)])


# Getting interactions

columnList<-sapply(train, function(x)(length(unique(x))))
columnList
