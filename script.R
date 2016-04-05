# First load in the imputed train and test datasets, assign names to features and split into conceptually different subarrays
train <- read.csv("impTrain.csv",header=F,sep=',')
returns <- c()
for(i in 2:180){ returns <- c( returns,paste('Ret_',i,sep='') ) }
dailyRets <- c( "Ret_MinusTwo", "Ret_MinusOne", "Ret_PlusOne", "Ret_PlusTwo", "Weight_Intraday", "Weight_Daily" )
features = c()
for(i in 1:25){ features <- c( features,paste('Feature_',i,sep='') ) }
colnames(train)[1]       <- 'Id'
colnames(train)[2:26]    <- features
colnames(train)[27:205]  <- returns
colnames(train)[206:211] <- dailyRets
train_rets   <- train[,27:205]
train_feats  <- train[,2:26]
train_extras <- train[,206:211]

test <- read.csv("impTest.csv",header=F,sep=',')
colnames(test)[1]       <- 'Id'
colnames(test)[2:26]    <- features
colnames(test)[27:145]  <- returns[1:119]
#colnames(train)[206:207] <- dailyRets[1:2] # does not exist
test_rets   <- test[,27:145]
test_feats  <- test[,2:26]
#test_extras <- test[,206:207]

# Below is a demonstration of how slow finding the correlations would work in R
#c1 <- sapply(1:nrow(rets),function(j){ cor(t(rets[1,]),t(rets[j,])) })

# So we proceed to the fast c++ implementation interfaced to R with the proprietary clustcorr package
#require(clustcorr)
#setcores(4)

#cl <- cross.correlations(train_rets[1:10000,1:119],test_rets[1:10000,1:119])

#cl <- cluster.correlations(rets,0.5)

#df <- data.frame(ind=seq(1,length(cl)),cl)
#groupByCluster <- aggregate( df, by=list(label=cl), function(x){x} )
#hist( unlist(lapply(groupByCluster[,2],length)) )

