require(Amelia)

train <- read.csv(file='train.csv',header=T,sep=',')
returns <- c()
for(i in 2:180){ returns <- c( returns,paste('Ret_',i,sep='') ) }
dailyRets <- c( "Ret_MinusTwo", "Ret_MinusOne", "Ret_PlusOne", "Ret_PlusTwo", "Weight_Intraday", "Weight_Daily" )
features = c()
for(i in 1:25){ features <- c( features,paste('Feature_',i,sep='') ) }
feats <- amelia(train[,features],m=1,ords = c("Feature_1","Feature_5","Feature_10","Feature_13","Feature_16","Feature_20"))
for(i in 1:dim(train)[[1]]){
        if(any(is.na(train[i,returns]))){
            q <- data.frame( t(train[i,returns]) );
            q$time <- 2:180;
            w <- amelia(q, m=1, ts="time", splinetime = 2);
            e <- t(w$imputations[[1]][1])
        } else {
            e = train[i,returns];
        };
        write.csv( cbind(feats$imputations[[1]][i,],e,train[i,dailyRets]), file=paste("impute/row",i,".csv",sep="") );
    if(i %% 100 == 0){ print(i);}
}

#i=1; while [ $i -le 40000 ] ; do if [ -e "impute/row$i.csv" ] ; then  tail -1 impute/row$i.csv >> impTrain.csv ; fi; i=`expr $i + 1`; done; rm impute/* 

test <- read.csv(file='test.csv',header=T,sep=',')
returns <- c()
for(i in 2:120){ returns <- c( returns,paste('Ret_',i,sep='') ) }
dailyRets <- c( "Ret_MinusTwo", "Ret_MinusOne" )
features = c()
for(i in 1:25){ features <- c( features,paste('Feature_',i,sep='') ) }
feats <- amelia(test[,features],m=1,ords = c("Feature_1","Feature_5","Feature_10","Feature_13","Feature_16","Feature_20"))
for(i in 1:dim(test)[[1]]){
        if(any(is.na(test[i,returns]))){
            q <- data.frame( t(test[i,returns]) );
            q$time <- 2:120;
            w <- amelia(q, m=1, ts="time", splinetime = 2);
            e <- t(w$imputations[[1]][1])
        } else {
            e = test[i,returns];
        };
        write.csv( cbind(feats$imputations[[1]][i,],e,test[i,dailyRets]), file=paste("impute/row",i,".csv",sep="") );
    if(i %% 100 == 0){ print(i);}
}

#i=1; while [ $i -le 40000 ] ; do if [ -e "impute/row$i.csv" ] ; then  tail -1 impute/row$i.csv >> impTest.csv ; fi; i=`expr $i + 1`; done; rm impute/* 
