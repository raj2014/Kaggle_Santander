<<<<<<< HEAD
# Feature Engineering Snippets
gzeros <- function(x) {
  return( sum(x < 0) )
}
derived_features$gzeroCounts<- apply(df_all[,-c(1,ncol(df_all))], 1, FUN=gzeros)



# Not a right one for centering all the columns
# #scale and center for all the columns
# for (i in 2:(ncol(df_all)-1))
# {
#   df_all[,i]<- as.numeric(scale(df_all[,i],center=TRUE,scale=TRUE))
# }


##############################################################################################################

## Correlation Logic
## check for highly correlated columns
# Flaw :test data should not be used to check correlation

correlatedColumns<-c()

for (i in 1:(ncol(df_all)-2))
{
  for (j in (i+1):(ncol(df_all)-1))
  {
    if(abs(cor(df_all[,i],df_all[,j]))>0.95)
    {
      
      correlatedColumns<-c(correlatedColumns,i)
      
    }
    
  }
}

correlatedColumns<-unique(correlatedColumns)
length(correlatedColumns)
df_all<-df_all[,-correlatedColumns]
###############################################################################################
=======
# Feature Engineering Snippets
gzeros <- function(x) {
  return( sum(x < 0) )
}
derived_features$gzeroCounts<- apply(df_all[,-c(1,ncol(df_all))], 1, FUN=gzeros)



# Not a right one for centering all the columns
# #scale and center for all the columns
# for (i in 2:(ncol(df_all)-1))
# {
#   df_all[,i]<- as.numeric(scale(df_all[,i],center=TRUE,scale=TRUE))
# }


##############################################################################################################

## Correlation Logic
## check for highly correlated columns
# Flaw :test data should not be used to check correlation

correlatedColumns<-c()

for (i in 1:(ncol(df_all)-2))
{
  for (j in (i+1):(ncol(df_all)-1))
  {
    if(abs(cor(df_all[,i],df_all[,j]))>0.95)
    {
      
      correlatedColumns<-c(correlatedColumns,i)
      
    }
    
  }
}

correlatedColumns<-unique(correlatedColumns)
length(correlatedColumns)
df_all<-df_all[,-correlatedColumns]
###############################################################################################
>>>>>>> origin/master
