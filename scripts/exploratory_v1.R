<<<<<<< HEAD
columnList<-sapply(df_all, function(x)(length(unique(x))))

columnList

prop.table(table(df_all$TARGET))

table(df_all$imp_trasp_var33_out_ult1)


table(df_all$imp_trasp_var33_out_ult1,df_all$TARGET)


imp_op_var41_ult1 

qplot(var38,facets=TARGET~.,data=train,geom="density")


# Correlated columns
#Correlation check
correlatedColumns<-c()

for (i in 1:(ncol(df_all)-2))
{
  for (j in (i+1):(ncol(df_all)-1))
  {
    if(cor(df_all[c(1:num_train_rows),i],df_all[c(1:num_train_rows),j])>0.97)
    {
      
      correlatedColumns<-c(correlatedColumns,i)
      
    }
    
  }
}

correlatedColumns<-unique(correlatedColumns)
length(correlatedColumns)
df_all<-df_all[,-correlatedColumns]




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


# creating a summary dataframe
for (i in 2:(ncol(df_all)-1))
{
  if (i==2)
  {
    summary_df_train<-data.frame(colIndex=i,name=colnames(df_all)[i],type_class=class(df_all[,i]),maximum_value=max(df_all[,i]),
                            minimum=min(df_all[,i]),uniqueValues=length(unique(df_all[,i])),
                            quantiles_1=quantile(df_all[,i], c(.20))[1],
                            quantiles_2=quantile(df_all[,i], c(.40))[1],
                            quantiles_3=quantile(df_all[,i], c(.60))[1],
                            quantiles_4=quantile(df_all[,i], c(.80))[1]
                           )
  }
  else
  {
    summary_df_train<-rbind(summary_df_train,data.frame(colIndex=i,name=colnames(df_all)[i],type_class=class(df_all[,i]),maximum_value=max(df_all[,i]),
                                                        minimum=min(df_all[,i]),uniqueValues=length(unique(df_all[,i])),
                                                        quantiles_1=quantile(df_all[,i], c(.20))[1],
                                                        quantiles_2=quantile(df_all[,i], c(.40))[1],
                                                        quantiles_3=quantile(df_all[,i], c(.60))[1],
                                                        quantiles_4=quantile(df_all[,i], c(.80))[1]
                                                        ))
  }
}

# print all the column names with binary values
summary_df_train[summary_df_train$uniqueValues==2,"name"]

# Take sum according to the designated categories
binaryColumns_ind<-summary_df_train[summary_df_train$uniqueValues==2,"colIndex"][1:21]
binaryColumns_numvar<-summary_df_train[summary_df_train$uniqueValues==2,"colIndex"][22:26]
binaryColumns_deltaimp<-summary_df_train[summary_df_train$uniqueValues==2,"colIndex"][27:28]
binaryColumns_indothers<-summary_df_train[summary_df_train$uniqueValues==2,"colIndex"][29:41]



# Aggregations for numerical columns

numeric_columns<-summary_df_train[summary_df_train$type_class=="numeric","colIndex"]


# PCA and tsne for dimensionality reduction on numerical columns

wal.pca <- prcomp(df_all[,c(numeric_columns)],
                  center = TRUE,
                  scale. = TRUE) 

#Visualize the dimensionality
plot(wal.pca, type = "l")
summary(wal.pca)

dim(wal.pca$x)

derived_features<-cbind(derived_features,wal.pca$x[,c(1:29)])


# understanding distribution of values

qplot(var3,facets=TARGET~.,data=test,geom="density")

=======
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


>>>>>>> origin/master
