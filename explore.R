#tr <- read.csv(file='train.csv',header=T,sep=',')

any( round(tr[,"Weight_Intraday"]/tr[,"Weight_Daily"],8) != 0.8 )
any( is.na(tr[,"Ret_PlusOne"]) )

features = c()
for(i in 1:25){ features <- c( features,paste('Feature_',i,sep='') ) }

require(ggplot2)
for(i in 1:25){ ggplot(data=tr,aes_string(x=paste('Feature_',i,sep=''))) + geom_histogram(alpha=0.1,color='blue'); dev.copy(pdf,paste('Feature_',i,'.pdf',sep='')); dev.off(); }

pairs( ~ Feature_1 + Feature_2 + Feature_3 + Feature_4 + Feature_5 + Feature_6 + Feature_7 + Feature_8 + Feature_9 + Feature_10 + Feature_11 + Feature_12 + Feature_13 + Feature_14 + Feature_15 + Feature_16 + Feature_17 + Feature_18 + Feature_19 + Feature_20 + Feature_21 + Feature_22 + Feature_23 + Feature_24 + Feature_25, data = tr, pch=3, col="blue")

pairs(~ Ret_MinusTwo + Ret_MinusOne + Ret_PlusOne + Ret_PlusTwo, data = tr)

plot.ts( ts( t(as.matrix(tr[c(1,2,3,4),rets])), frequency=32 ) )

q <- data.frame( t(tr[c(1,2,3,4),rets]) )
q$time <- 2:180
w <- amelia(q, m=1, ts="time", splinetime = 2)
plot.ts( ts( w$imputations[[1]][,c(1,2,3,4)], frequency=1 ) )

plot( decompose(ts( w$imputations[[1]][,c(1,2)], frequency=20 ) ) )

require(Amelia)
m <- matrix(rep(0,179*179), ncol=179, nrow=179)
addOne <- function(x,y){ y = y + outer(as.numeric(x),as.numeric(x)) }
apply(tr,1,addOne,m)

require(caret)
part <- createDataPartition( runif(dim(tr)[[1]], 0.0, 2) >= 1, p = 3/4)[[1]]
suppressMessages( library(Amelia) )

feats <- amelia(tr[,features],m=1,ords = c("Feature_1","Feature_5","Feature_10","Feature_13","Feature_16","Feature_20"))

for(i in 1:dim(tr)[[1]]){
    if(i %in% part){
        if(any(is.na(tr[i,rets]))){
            q <- data.frame( t(tr[i,rets]) );
            q$time <- 2:180;
            w <- amelia(q, m=1, ts="time", splinetime = 2);
            e <- t(w$imputations[[1]][1])
        } else {
            e = tr[i,rets];
        };
        write.csv( cbind(feats$imputations[[1]][i,],e), file=paste("impute/row",i,".csv",sep="") );
    };
    if(i %% 100 == 0){ print(i);}
}

#i=1; while [ $i -le 40000 ] ; do if [ -e "impute/row$i.csv" ] ; then  tail -1 impute/row$i.csv >> imp.csv ; fi; i=`expr $i + 1`; done

#m <- matrix(rep(0,179*179), ncol=179, nrow=179)
#addOne <- function(x,y){ y = y + outer(as.numeric(x),as.numeric(x)) }
#apply(tr[,rets],1,addOne,m)
#ms <- apply(imp[,c(27:205)],2,mean)
#for(i in 1:30001){ m = m + outer(as.numeric(imp[i,c(27:205)]-ms),as.numeric(imp[i,c(27:205)]-ms)) }
##recursion <- function(from,till){ # assuming from < till, and till is next to the last element
##    if( till == from + 1 ){ 
##         outer(as.numeric(imp[from,c(27:205)]-ms),as.numeric(imp[from,c(27:205)]-ms))
###        A[from]
##    } else {
##        middle = from + ceiling((till-from)/2)
##        recursion(from,middle) + recursion(middle,till) 
##    }
##}
#m <- m/30001.

eigen( cov(imp[,c(2:25)]) )$values
prcomp(imp[,c(2:25)],scale = F)$sdev^2
plot( prcomp(imp[,c(2:25)],scale = T)$sdev )

# PCA explained:

# normalization:
stdimp <- scale(imp[,c(2:25)])
# is the same as
standardize<-function(x) {(x-mean(x))/sd(x)}
stdimp <- apply(imp[,c(2:25)], MARGIN=2, FUN=standardize)
# scores:
stdimp[c(1),] %*% eigen(cor(imp[,c(2:25)]))$vectors ## cov -> cor ?
# is the same as
prcomp(imp[,c(2:25)],retx=T,scale=T)$x[1,]

# navigate over datasets:
data(package="fma")
require("fma")
grep('emplo',data()$results[,"Title"])
data()$results[24,"Item"]

# Autocorrelation function:
acf( ts(t(imp[1,c(27:205)]),frequency=1) )


##################################################
# Information content above the 'base' (plain average) model:

# append weights:
wgt = data.frame( w=numeric() )
for(i in 1:dim(imp)[1]){ wgt <- rbind(wgt,tr[imp[i,"V1"],"Weight_Intraday"]) }
colnames(wgt) <- c("w")
imp <- cbind(imp,wgt)

# build an error function and plot the penalties for first 120 time components
err <- function(x){ sqrt( sum( (x[1:119]-mean(x[1:119]))^2 )/119 )*x[120] }
hist( apply(as.matrix(imp[,c(27:145,206)]), MARGIN=1, FUN=err) )
h <- data.frame( e=apply(as.matrix(imp[,c(27:145,206)]), MARGIN=1, FUN=err) )
ggplot(data=h,aes(x=e)) + geom_histogram(breaks=seq(0,50000,1000))

# plot penalties for the remaining 60 time component (aimed to be forecasted)
erf <- function(x){ sqrt( sum( (x[121:180]-mean(x[2:120]))^2 )/60 )*x[1] }
h2 <- data.frame( e=apply(as.matrix(imp[,c(206,27:205)]), MARGIN=1, FUN=erf) )
ggplot(data=h2,aes(x=e*2)) + geom_histogram(breaks=seq(0,50000,1000))

# compare the two distributions
errors <- data.frame( e=h$e, type=factor('train',levels=c('train','test')) )
errors <- rbind(errors, data.frame( e=h2$e, type=factor('test',levels=c('train','test')) ))
ggplot(data=errors,aes(x=e, fill=type)) + geom_histogram(breaks=seq(0,5000,100), alpha=.5, position="identity")

#t-test:
t.test(h$e,h2$e)

# i.e. there is ~10 sigma difference between the two -> some non-trivial information was not yet been used

##################################################
# See if predictors cluster together

dist2 = dist(imp[,c(2:25)])
hclust2 = hclust(dist2)
plot(hclust2)

pca <- prcomp(imp[,c(2:25)],retx=T,scale=T)
km1 <- kmeans(imp[,c(2:25)],centers=2)
plot(prcomp(imp[,c(2:25)],retx=T,scale=T)$x[,1], prcomp(imp[,c(2:25)],retx=T,scale=T)$x[,2])
points( predict(pca,km1$centers)[,"PC1"], predict(pca,km1$centers)[,"PC2"], col="red" )

# not really seen in just PC1:PC2 projection, do PC1:PC2 alone:
p12 <- data.frame(pc1=prcomp(imp[,c(2:25)],retx=T,scale=T)$x[,1], pc2=prcomp(imp[,c(2:25)],retx=T,scale=T)$x[,2])
km1 <- kmeans(p12,centers=10)
plot(p12$pc1, p12$pc2)
points( km1$centers, col="blue" )

# honest answer: there seem to be really no well separated clusters

##################################################
# We need some sort of smoothing to filter out random noise and have manageable number of predictors
#  let's try principal components of the time component

pcaT <- prcomp(imp[,c(27:205)],retx=T,scale=T)  #c(27:145) are the predictors
plot( pcaT$sdev ) # see quite some noise
# focus on 41th and 6th series:
t41 <- ts(as.vector(t(imp[41,c(27:205)])))
t6  <- ts(as.vector(t(imp[6,c(27:205)]))) 
plot(t6)
lines( ma(t6, order=30 ), col='red' )
which( pcaT$x[6,] == max( pcaT$x[6,] ) ) # turn out to be #147
lines( pcaT$rotation[,147]*pcaT$scale[147], col='blue' )
#
plot(t41)
lines( ma(t41, order=30 ), col='red' )
which( pcaT$x[41,] == max( pcaT$x[41,] ) ) # turn out to be #1
lines( pcaT$rotation[,1]*pcaT$scale[1], col='blue' )
# 1. seems like we are picking up mostly random noise
# 2. the series are not the same: 41 varies >> 6 -> we want to do a better job on 41!

# let's better try principal components on the smoothed time components
smth <- apply( ts(as.matrix(t(imp[,c(27:205)]))), 2, ma, 30)
pcaS <- prcomp(t(smth[c(16:164),]),retx=T,scale=T)
plot( pcaS$sdev )
# Wow, that looks much better!
approx <- function(x){ pcaS$rotation[,1:6] %*% x[1:6]*pcaS$scale[1:6] } # pcaS$x[41,1:6]
apr <- matrix(rep(0,30001*15), ncol=30001, nrow=15)
apr <- rbind(apr, apply(pcaS$x,1,approx) )
apr <- rbind(apr, matrix(rep(0,30001*15), ncol=30001, nrow=15) )
res <- imp[,27:205] - t(apr)

plot( smth[,41], col='blue' )
lines( apr[,41], col='red' )
# not bad at all, but noise is still a dominant contribution:
plot( ts(as.vector(t(imp[41,c(27:205)]))) )
points(as.matrix(res[41,])[1,], col='brown')
points(apr[,41],col="red")

# compare the two distributions
errors <- data.frame( e=sqrt(apply(imp[,(27+15):(205-15)]^2,1,sum)/(205+1-15-(27+15)) - apply(imp[,(27+15):(205-15)],1,mean)^2)*imp[,206], type=factor('base',levels=c('base','ma')) )
errors <- rbind(errors, data.frame( e=sqrt(apply(res[,15:(179-15)]^2,1,sum)/(179-15-(15)))*imp[,206], type=factor('ma',levels=c('base','ma')) ))
ggplot(data=errors,aes(x=e, fill=type)) + geom_histogram(breaks=seq(0,5000,100), alpha=.5, position="identity")
# so the ma model is only slightly better (p-val=15%) than the base model

# can we really forecast the 1/3 of the trend using PCA?
stdimp <- scale( imp[,c((27+15):(205-15))], center=pcaS$center, scale=pcaS$scale)
pcaDec <- stdimp[,1:104] %*% pcaS$rotation[1:104,1:6]
newDat <- matrix(rep(0,30001*15), ncol=30001, nrow=15)
newDat <- rbind(newDat, pcaS$rotation[,1:6] %*% t( pcaDec[,1:6] * pcaS$scale[1:6] ) )
newDat <- rbind(newDat, matrix(rep(0,30001*15), ncol=30001, nrow=15) )
plot( smth[,41], col='blue' )
lines(newDat[,41], col='red')
points(apr[,41],col='brown')
# surprisingly, it does work, but not that impressive

# interesting:
acf(t41)
# how to build a model with autocorrelation?

##################################################

# More smoothing
plot(ts(t(imp[41,c(27:205)])))
hw41 <- HoltWinters(ts(t(imp[41,c(27:205)])), beta=FALSE, gamma=FALSE, l.start=imp[41,27])
plot(hw41)
lines(apr[,41],col='brown')
# nothing close
apply(res[41,1:179]^2,1,sum)
apply((t(newDat[,41])-imp[41,27:205])^2,1,sum)
sum((t(hw41$fitted[,"xhat"])-imp[41,29:205])^2)
hw41$SSE
# so far my pca's win by alot even with +-15 points and unconstrained forecast

# Real forecasting
library("forecast")
hw41 <- HoltWinters(ts(t(imp[41,c(27:145)])), beta=FALSE, gamma=FALSE, l.start=imp[41,27])
fc41 <- forecast.HoltWinters(hw41, h=60)
plot(fc41)
# that sucks

##################################################

# So what does Arima has to tell us?
fit <- auto.arima(ts(t(imp[41,c(27:145)])),seasonal=F)
plot(forecast(fit,h=60),include=180)
# Wow, this one picks up the acf!!! But does it performs well compared to ets?

rets41 <- ts(t(imp[41,c(27:205)]),start=1)
ret41 <- window(rets41,start=1,end=119)
res41 <- window(rets41,start=120)
arimaFit <- auto.arima( ret41, seasonal=F)
accuracy(forecast(arimaFit,h=60),res41)
accuracy(ts(forecast(arimaFit,h=60)$mean,start=120),res41)
accuracy(forecast(arimaFit,h=60),res41)[2,"RMSE"]
accuracy(ts(forecast(arimaFit,h=60)$mean,start=120),res41)[,"RMSE"]
accuracy(forecast(ets(ret41),h=60),res41)[2,"RMSE"]
# hmm, ets seems better, but
#   1) ets fits 5 parameters and this auto.arima has only 2: "ARIMA(1,0,1) with zero mean"
#   2) the difference is small and may be not statistically significant, so let's check it:

rets <- ts(t(imp[,c(27:205)]),start=1)
ret  <- window(rets,start=1,end=119)
res  <- window(rets,start=120)
etsModels   <- lapply(ret,ets)
arimaModels <- lapply(ret,auto.arima,NA, NA, 5, 5, 2, 2, 5, 2, 1, 2, 2, 1, 1, F, F)
etsForecasts   <- lapply(etsModels,    forecast, h=60)
arimaForecasts <- lapply(arimaModels,  forecast, h=60)

df <- data.frame( t(res) )
df$cc <- etsForecasts
acc <- function(l){ return(accuracy(l[[61]],ts(unlist(l[1:60]),start=120))[2,"RMSE"]) }
accuracyEts    <- apply(df, 1, acc)
df$cc <- arimaForecasts
accuracyArima  <- apply(df, 1, acc)
ac <- data.frame( rmse=unlist(accuracyEts), type=factor('ets',levels=c('ets','arima')) )
ac <- rbind(ac, data.frame( rmse=unlist(accuracyArima), type=factor('arima',levels=c('ets','arima')) ) )
ggplot(data=ac,aes(x=rmse, fill=type)) + geom_histogram(breaks=seq(0,1,0.001), alpha=.5, position="identity")
t.test(unlist(accuracyEts), unlist(accuracyArima))
# and with the p-value=0.6584 the two plots are indistinguishable -> ets is no better than arima

##################################################
# I clearly need a better feature extraction technique

# plot( stl( ts(t(imp[41,27:205]),start=1,end=179,frequency=1), t.window=15, s.window=1, robust=TRUE) ) # this somehow doesn't work
# 
allts <- ts(t(imp[,27:205]),start=1,end=179,frequency=1)
skw <- lapply(allts,skewness)
krt <- lapply(allts,kurtosis)
ks <- data.frame(unlist(skw),unlist(krt))
colnames(ks)<-c("skew","kurt")
ggplot(data=ks,aes(x=kurt,y=skew)) + geom_point()
ks[ks$skew>5,]
plot(allts[,1099])
plot(allts[,6385])
plot(allts[,11372])
plot(allts[,12616])
plot(allts[,20471])
plot(allts[,24131])
plot(allts[,29668])
ks[abs(ks$skew)<5 & ks$kurt>60,]
# so, what are you going to do about these spikes?

##################################################
# Let's see if there are some types of the time series

# Source functions from http://robjhyndman.com/hyndsight/tscharacteristics/ and do
require(fracdiff)
require(tseries)
require(parallel)
options(mc.cores=4)
ms <- mclapply(rets,measures)
#ms <- apply(rets,2,measures)
mms  <- melt(ms)
meas <- aggregate( mms, by=list(Index=mms$L1), function(x){x} )
d    <- dist(meas[,2])
plot( hclust(d) )

distdex<-function(i,j,n) #given row, column, and n, return index
    n*(i-1) - i*(i-1)/2 + j-i

rowcol<-function(ix,n) { #given index, return row and column
    nr=ceiling(n-(1+sqrt(1+4*(n^2-n-2*ix)))/2)
    nc=n-(2*n-nr+1)*nr/2+ix+nr
    cbind(nr,nc)
}

max(d)
which(d>2.4)
rowcol(313501004,30000)
plot(ts(t(imp[13478,c(27:205)]),start=1,end=179))
plot(ts(t(imp[25985,c(27:205)]),start=1,end=179))
# those ts don't look that very different to me

mms <- meas[,2]
colnames(mms) <- c('frequency','trend','seasonal','autocorrelation','non-linear','skewness','kurtosis','Hurst','Lyapunov','dc autocorrelation','dc non-linear','dc skewness','dc kurtosis')
mms <- mms[,c(-1,-3)]
pairs( ~ ., data = mms)
# I don't understand what I am looking at :0(

# So what is non-linearity?
#tt <- lapply(rets,terasvirta.test)
ttt <- lapply(rets,function(x){ decomp.x <- decomp(x); adj.x <- decomp.x$x - decomp.x$trend + mean(decomp.x$trend, na.rm=TRUE); terasvirta.test(adj.x)$p.value})
qplot(unlist(ttt))
# so, ~10% of the series are non-linear. so what? the overal distribution looks ok. though
# ok, wrapping up with this exercise, there seem to be nothing in it: all the financial ts are alike

##################################################
# Ok, by now we have several models: decomp (from above), arima, pca_ma+arima; which one is better?
# Well, how about fourier transformation to do dimension reduction?
#  (note, indexes below are shifted by 1)

rets <- ts(t(imp[,c(27:205)]),start=1)
ret  <- window(rets,start=1,end=119)
res  <- window(rets,start=120)
smth <- apply( rets, 2, ma, 7)
plot(smth[c(4:176),1])
lines( Re(fft(fft(smth[c(4:176),1]), inverse = TRUE)/(176-4)) )
# strip the noise:
x <- fft(smth[c(4:176),1])
x[21:(173-21)] <- 0
plot(Mod(x))
plot(smth[c(4:176),1])
lines( Re(fft(x, inverse = TRUE)/(176-4)) )
# not bad at all; of course fft is not for forecasting but rather for smoothing
# now hardcore:
x <- fft(rets[,1])
x[(90-50):(91+50)] <- 0
plot(rets[,1])
lines( Re(fft(x, inverse = TRUE)/(179)), col='red' )
lines(c(0,0,0,smth[c(4:176),1],0,0,0),col='blue')
# excellent: with just 2*10 parameters we do as good and as close as with ma(7) that is >100 parameters
# moreover, we can judge the resemblance of the time series agnosticly of the phase shift!
spect <- apply(rets,2,fft)
spect[(90-50):(91+50),] <- 0 # filter out high-frequency noise
spect <- Mod(spect)
d <- dist(t(spect))
hc <- hclust(d)
quantile(d,probs=1-3/30000)
# max distance appears to be between 25865 and 22118:
plot(ts(t(imp[25865,c(27:205)]),start=1,end=179))
lines(ts(t(imp[22118,c(27:205)]),start=1,end=179))
# but 25865 is special in many cases (i.e. wrt many other ts), how about others?
rm(d)
d <- dist(t(spect[,-25865]))
hc <- hclust(d)
plot(hc)
plot(ts(t(imp[3644,c(27:205)]),start=1,end=179))
lines(ts(t(imp[22118,c(27:205)]),start=1,end=179))
# seems like we need transform the ts if we want to compare the shapes
nr <- apply(spect,2,sd)
d  <- dist(t(spect)/nr)
rowcol(which(d>16.),30000)
rawcol(387609422,30000)
plot( rets[,18831]/sd(rets[,18831]), col="red" )
lines( rets[,22118]/sd(rets[,22118]), col="blue" )
# the two are really different, how about similar distributions?
which(d<3)
rowcol(279640351,30000)
plot( rets[,11542]/sd(rets[,11542]), col="red" )
lines( rets[,25004]/sd(rets[,25004]), col="blue" )
# that is so cool!!! and more of the kind:
#plot( rets[,11711], col="red" ); lines( rets[,23037], col="blue" )
#plot( rets[,19222], col="red" ); lines( rets[,17801], col="blue" )
rowcol(72369376,30000)
plot( rets[,2519]/sd(rets[,2519]), col="red" )
lines( rets[,3316]/sd(rets[,3316]), col="blue" )
# and that is so bad, but it get the picture up to the phase, at least:
plot(spect[,2519]/sd(spect[,2519]),col="red")
points(spect[,3316]/sd(spect[,3316]),col="blue")
plot(rets[,2519])
x <- fft(rets[,2519])
x[(90-50):(91+50)] <- 0
lines( Re(fft(x, inverse = TRUE)/(179)), col='red' )
# ok, most of the fourier-close ts patterns turned out to be really close
ggplot(data=data.frame(mod=as.numeric(d[ sample(length(d), 1000000, replace = FALSE) ])),aes(x=mod)) + geom_histogram(breaks=seq(0,17,0.05))
# but their distribution doesn't suggest any pronounced clustering; how about high-frequency match (like it was for 11543-1 and 25005-1)?
spect <- apply(rets,2,fft)
spect[1:40,] <- 0; spect[141:179,] <- 0
nr <- apply(spect,2,sd)
d  <- dist(t(spect)/nr)
which(d<5)
# our friends 11543-1 and 25005-1 are close again, but mostly that filtered unrelated series with similar spikes like these:
plot(rets[,17407],col="blue")
lines(rets[,23159],col="red")

# another thing to laverage the white-noisiness (of fourier: acf is within bounds -> there is nothing to predict?); compare: ???
plot( acf(Mod(spect[,12615])/sd(Mod(spect[,12615]))) )
plot( acf(Mod(spect[,1097])/sd(Mod(spect[,1097]))) )
Box.test(Mod(spect[,2519])/sd(Mod(spect[,2519])))
Box.test(Mod(spect[,251])/sd(Mod(spect[,251])))
# a more regular approach:
bt <- apply(spect,2,function(x){ Box.test(Mod(x)/sd(Mod(x)))$p.value })
head(which(bt<0.000001))
plot(rets[,635])
# so, how do we build our model?
#  group white noise in one category and allow a constant (>sd/sqrt(179)) and a slope, and a trend?
#  group wierdos
# or look at corss correlation function
cf25005 <- apply(rets,2,ccf,rets[,25005])
cc <- lapply(cf25005,function(x){ any(abs(x$acf) > 4*1.96/sqrt(179)) })
which(unlist(cc))
# DWT as opposed to DFT (or FFT) has several advantages: http://people.cs.pitt.edu/~iyad/papers/ICMLA_2009.pdf
wt <- apply(rets,2,dwt,filter="haar")
wtV5 <- lapply(wt,function(x){ x@V[[5]] } )
wtW5 <- lapply(wt,function(x){ x@W[[5]] } )
wt5  <- melt(wtV5)
w5   <- aggregate( wt5, by=list(Index=wt5$L1), function(x){x} )
nr   <- apply(w5[,4],1,sd)
d    <- dist(w5[,4]/nr)
which(d<0.03)
plot(rets[,4939],col="blue")
lines(rets[,29195],col="red")
# nothing... maybe higher orders or not only scaling?
# well let's look at the DCT first:
spect2 <- apply(rets,2,dct)
nr2    <- apply(spect2,2,sd)
rm(d)
d      <- dist(t(spect2)/nr2)
c25 <- dct(rets[,25005])
c11 <- dct(rets[,11543])
dist( rbind(c11/sd(c11),c25/sd(c25)) )
c11 <- dct( window(rets[,11543],start=21,end=179) )
c25 <- dct( window(lag(rets[,25005],20),start=1) )
dist( rbind(c11/sd(c11),c25/sd(c25)) )
ccf(c11,c25,lag.max=25)
which(d<5)
# see all of our old friends

##################################################
impTest <- read.csv("impTest.csv",header=F,sep=',')

retsTest  <- ts(t(impTest[,c(27:145)]),start=1)
spectTest <- apply(retsTest,2,dct)

retsTrain  <- ts(t(impTrain[,c(27:145)]),start=1)
spectTrain <- apply(retsTrain,2,dct)

#nrTrain <- apply(retsTrain,2,sd)
#nrTest  <- apply(retsTest,2,sd)
#stdTrain <- t(retsTrain)/nrTrain
#stdTest  <- t(retsTest)/nrTest

nrTest  <- apply(spectTest,2,sd)
nrTrain <- apply(spectTrain,2,sd)

stdTest  <- t(spectTest)/nrTest
stdTrain <- t(spectTrain)/nrTrain

for(i in 1:60000){ for(j in 1:40000){ d <- dist( rbind(spectTest[,i]/sd(spectTest[,i]),spectTrain[,j]/sd(spectTrain[,j])) ); if( d < 10 ) print(paste("i=",i," j=",j," d=",d)) }; print(paste(" finised",i,"test")) }

#for(i in 1:60000){ d <- apply(stdTrain,1,function(x,y){ dist( rbind(x,y) ) },retsTest[,i]); if( d<10 ){ cf <- apply(retsTrain,2,ccf,retsTest[,i]); cc <- lapply(cf,function(x){ any(abs(x$acf) > 4*1.96/sqrt(179)) }); if( length(which(unlist(cc))) ){ k <- rbind(k,list(which(unlist(cc)))) } }; print(paste(" finised",i,"test")) }

k <- c()
for(i in 1:60000){ d <- apply(stdTrain,1,function(x,y){ dist( rbind(x,y) ) },retsTest[,i]); if( any(d<10) ){ k <- rbind(k,i) }; print(paste(" finised",i,"test")) }
# nothing

##################################################
# Training a network
#  starting from scratch

train <- read.csv(file='impTrain.csv',header=F,sep=',')
test  <- read.csv(file='impTest.csv', header=F,sep=',')

require(caret)
part <- createDataPartition( runif(dim(train)[[1]], 0.0, 2) >= 1, p = 3/4)[[1]]

require(e1071)
model <- svm(train[part,c(2:26,206,207)],train[part,208])
p <- predict(model,train[-part,c(2:26,206,207)])
ggplot(data=data.frame(delta=((train[-part,208] - p)/sd(train[-part,208]))),aes(x=delta)) + geom_histogram(breaks=seq(-1,1,0.01))

library(h2o)
localH2O <- h2o.init(nthreads = -1,max_mem_size = '5g')

trainTrain <- train[ part,c(2:26,206,207,208)]
trainValid <- train[-part,c(2:26,206,207,208)]

train.hex <- as.h2o(localH2O,trainTrain)
valid.hex <- as.h2o(localH2O,trainValid)

model <- h2o.randomForest(x=c(1:27),y = 28, training_frame = train.hex, validation_frame = valid.hex, mtries = 18, sample_rate = 0.5, ntree = 500)
test.hex <- as.h2o(localH2O,trainValid)

pred <- h2o.predict(model,test.hex)
p <- as.data.frame(pred)

sd((train[-part,208] - p[,1]))/sd(train[-part,208])
cor(p[,1],train[-part,208])
cor(p[,1],sample(train[-part,208]))

# so it looks like there is a fair amount of correlation that leads to a ~2% of variance explained by the predictors

ggplot(data=data.frame(x=p[,1],y=train[-part,208]),aes(x=x,y=y)) + geom_point()
# but we really need to do something to the outliers and standartize!

dl <- h2o.deeplearning(x = 1:27,y = 28, training_frame = train.hex, validation_frame = valid.hex, hidden=c(20,10), epochs=5)
pred <- h2o.predict(model,test.hex)
cor(p[,1],train[-part,208])

# ok, even if this work to 2% level, how do we extract and use ts features?
#  ARIMA(0,0,0) + 0 -> white noise
#  ARIMA(x,0,y) + Z
options(mc.cores=4)
arimaModelsShort <- mclapply(ret, auto.arima,NA, NA, 5, 5, 2, 2, 5, 2, 1, 2, 2, 1, 1, F, F)
arimaModelsFull  <- mclapply(rets,auto.arima,NA, NA, 5, 5, 2, 2, 5, 2, 1, 2, 2, 1, 1, F, F)
arimaArmaFull    <- mclapply(arimaModelsFull, function(x){ return(x$arma) })
arimaArmaShort   <- mclapply(arimaModelsShort,function(x){ return(x$arma) })
meltedArmaFull   <- melt(arimaArmaFull)
meltedArmaShort  <- melt(arimaArmaShort)
armaFull         <- aggregate( meltedArmaFull,  by=list(Index=meltedArmaFull$L1),  function(x){x} )
armaShort        <- aggregate( meltedArmaShort, by=list(Index=meltedArmaShort$L1), function(x){x} )
cor(armaShort[,2][,1],armaFull[,2][,1])
which(armaShort[,2][,1]>4 & armaFull[,2][,1]>4)
armaShort[4615,3]
plot(forecast(arimaModelsShort[[14150]],h=60),include=180)
lines(rets[,14150],col="red")

#########################################################
extend.series( as.matrix(impTrain[10923,27:205]), method="mean", length="powerof2", j=7)
#########################################################

y <- rep(0,256)
y[1:179] <- rets[,10923]
x <- (1:256)
plot(x, y, type="l", ylab="Bumps")
ywd <- wd(y,filter.number=1, family="DaubExPhase")
FineCoefs <- accessD(ywd, lev=7)
sigma <- mad(FineCoefs)
utDJ <- sigma*sqrt(2*log(256))
ywdT <- threshold(ywd, policy="manual", value=utDJ)
ywr <- wr(ywdT)
plot(x, ywr, type="l")
lines(x, y, col="red")
plot(x, y, col="red")
plot(x, rets[,10923], col="red")
plot(rets[,10923], col="red")

yi <- rep(0,128)
yf <- rep(0,64)
yi[1:119] <- rets[1:119,2520]
yf[1:60]  <- rets[120:179,2520]
yiwd <- wd(yi,filter.number=1, family="DaubExPhase")
yfwd <- wd(yf,filter.number=1, family="DaubExPhase")
yiwdT <- threshold(yiwd, policy="fdr")
yfwdT <- threshold(yfwd, policy="fdr")
yiwr <- wr(yiwdT)
yfwr <- wr(yfwdT)
plot(rets[1:119,2520])
plot(ts(rets[1:119,2520]))
lines(x[1:119], yiwr[1:119], type="l", col="blue")
plot(ts(rets[120:179,2520]))
lines(x[120:179]-119, yfwr[1:60], type="l", col="blue")

plot(ts(rets[1:116,2520]))
yi <- rep(0,128)
yi[1:116] <- rets[1:116,2520]
yiwdT <- threshold(yiwd, policy="fdr")
yiwr <- wr(yiwdT)
lines(x[1:116], yiwr[1:116], type="l", col="blue")
accessD(yiwd, lev=1)
# some smoothing and dimensionality reduction

library("EbayesThresh")
yiwdEBT <- ebayesthresh.wavelet(yiwd)
yiwrEB <- wr(yiwdEBT)
lines(x[1:116], yiwrEB[1:116], type="l", col="red")
accessD(yiwdEBT, lev=3)
# that looks even better

# One thing to try with the wavelets: loop over the basis level packets of NDWT and choose the winer of highest scalar product after denoseing:
ywst1 <- wst(rets[1:128,11543],filter.number=1, family="DaubExPhase")
ywst2 <- wst(rets[1:128,25005],filter.number=1, family="DaubExPhase")
FineWSTCoefs1 <- accessD(ywst1, lev=6)
FineWSTCoefs2 <- accessD(ywst2, lev=6)
sigmaWST1 <- mad(FineWSTCoefs1)
sigmaWST2 <- mad(FineWSTCoefs2)
utWSTDJ1 <- sigmaWST1*sqrt(2*log(128))
utWSTDJ2 <- sigmaWST2*sqrt(2*log(128))
ywstT1 <- threshold(ywst1, policy="manual", value=utWSTDJ1)
ywstT2 <- threshold(ywst1, policy="manual", value=utWSTDJ2)
yABuv1 <- AvBasis(ywstT1)
yABuv2 <- AvBasis(ywstT2)
plot(1:128,rets[1:128,11543],col="red",type="l")
lines(1:128,yABuv1)
lines(1:128,rets[1:128,25005],col="blue")
lines(1:128,yABuv2,col="brown")
for(i in 1:10) print( sqrt(sum((getpacket(ywstT2, level=1, index=3) - getpacket(ywstT1, level=1, index=i))^2)) )

### Ok, let's wrap it up already
train <- read.csv(file='impTrain.csv',header=F,sep=',')
test  <- read.csv(file='impTest.csv', header=F,sep=',')
require("parallel")
require("wavethresh")
options(mc.cores=4)
require("fpp")
wdStart          <- mclapply( ts(t(train[,27:145])),  function(x){ wd(c(x,rep(0, 9)), filter.number=1, family="DaubExPhase") } )
wdFinish         <- mclapply( ts(t(train[,146:205])), function(x){ wd(c(x,rep(0, 4)), filter.number=1, family="DaubExPhase") } )
require("EbayesThresh")
wdStartThr       <- mclapply( wdStart,      ebayesthresh.wavelet )
wdFinishThr      <- mclapply( wdFinish,     ebayesthresh.wavelet )
wdStartThrLev    <- mclapply( wdStartThr,   accessC, lev=0 )
wdFinishThrLev   <- mclapply( wdFinishThr,  accessC, lev=0 )
wdStartThrLev0   <- mclapply( wdStartThr,   accessD, lev=0 )
wdFinishThrLev0  <- mclapply( wdFinishThr,  accessD, lev=0 )
wdStartThrLev1   <- mclapply( wdStartThr,   accessD, lev=1 )
wdFinishThrLev1  <- mclapply( wdFinishThr,  accessD, lev=1 )
wdStartThrLev2   <- mclapply( wdStartThr,   accessD, lev=2 )
wdFinishThrLev2  <- mclapply( wdFinishThr,  accessD, lev=2 )
wdStartThrLev3   <- mclapply( wdStartThr,   accessD, lev=3 )
wdFinishThrLev3  <- mclapply( wdFinishThr,  accessD, lev=3 )

wdStartThrZero6  <- mclapply( wdStartThr,       putD, level=6, rep(0,64) )
wdFinishThrZero6 <- mclapply( wdFinishThr,      putD, level=5, rep(0,32) )
wdStartThrZero5  <- mclapply( wdStartThrZero6,  putD, level=5, rep(0,32) )
wdFinishThrZero5 <- mclapply( wdFinishThrZero6, putD, level=4, rep(0,16) )
wdStartThrZero4  <- mclapply( wdStartThrZero5,  putD, level=4, rep(0,16) )
wdFinishThrZero4 <- mclapply( wdFinishThrZero5, putD, level=3, rep(0,8) )

wrStartThrZero4  <- mclapply( wdStartThrZero4,  wr )
wrFinishThrZero4 <- mclapply( wdFinishThrZero4, wr )

plot(ts(t(train[25144,c(27:205)]),start=1)) #3354
lines(1:119,   wrStartThrZero4[[25144]][1:119], type="l", col="red")
lines(120:179, wrFinishThrZero4[[25144]][1:60], type="l", col="blue")

require(reshape)
wdStartThrLevmlt  <- melt(wdStartThrLev)
wdFinishThrLevmlt <- melt(wdFinishThrLev)
wdStartThrLev     <- aggregate( wdStartThrLevmlt,  by=list(Index=wdStartThrLevmlt$L1),  function(x){x} )
wdFinishThrLev    <- aggregate( wdFinishThrLevmlt, by=list(Index=wdFinishThrLevmlt$L1), function(x){x} )
wdStartThrLev0mlt  <- melt(wdStartThrLev0)
wdFinishThrLev0mlt <- melt(wdFinishThrLev0)
wdStartThrLev0     <- aggregate( wdStartThrLev0mlt,  by=list(Index=wdStartThrLev0mlt$L1),  function(x){x} )
wdFinishThrLev0    <- aggregate( wdFinishThrLev0mlt, by=list(Index=wdFinishThrLev0mlt$L1), function(x){x} )
wdStartThrLev1mlt  <- melt(wdStartThrLev1)
wdFinishThrLev1mlt <- melt(wdFinishThrLev1)
wdStartThrLev1     <- aggregate( wdStartThrLev1mlt,  by=list(Index=wdStartThrLev1mlt$L1),  function(x){x} )
wdFinishThrLev1    <- aggregate( wdFinishThrLev1mlt, by=list(Index=wdFinishThrLev1mlt$L1), function(x){x} )
wdStartThrLev2mlt  <- melt(wdStartThrLev2)
wdFinishThrLev2mlt <- melt(wdFinishThrLev2)
wdStartThrLev2     <- aggregate( wdStartThrLev2mlt,  by=list(Index=wdStartThrLev2mlt$L1),  function(x){x} )
wdFinishThrLev2    <- aggregate( wdFinishThrLev2mlt, by=list(Index=wdFinishThrLev2mlt$L1), function(x){x} )
wdStartThrLev3mlt  <- melt(wdStartThrLev3)
wdFinishThrLev3mlt <- melt(wdFinishThrLev3)
wdStartThrLev3     <- aggregate( wdStartThrLev3mlt,  by=list(Index=wdStartThrLev3mlt$L1),  function(x){x} )
wdFinishThrLev3    <- aggregate( wdFinishThrLev3mlt, by=list(Index=wdFinishThrLev3mlt$L1), function(x){x} )

which(wdFinishThrLev0$L1=="V25144")
plot(ts(t(train[25144,c(27:205)]),start=1))
recoStart <- wdStartThrZero4[[1]]
recoStart <- putC(recoStart, level=6, rep(0,64) )
recoStart <- putC(recoStart, level=5, rep(0,32) )
recoStart <- putC(recoStart, level=4, rep(0,16) )
recoStart <- putC(recoStart, level=3, rep(0,8) )
recoStart <- putC(recoStart, level=2, rep(0,4) )
recoStart <- putC(recoStart, level=1, rep(0,2) )
recoStart <- putC(recoStart, level=0, wdStartThrLev[16830,"value"] )
recoStart <- putD(recoStart, level=6, rep(0,64) )
recoStart <- putD(recoStart, level=5, rep(0,32) )
recoStart <- putD(recoStart, level=4, rep(0,16) )
recoStart <- putD(recoStart, level=3, wdStartThrLev3[16830,"value"])
recoStart <- putD(recoStart, level=2, wdStartThrLev2[16830,"value"])
recoStart <- putD(recoStart, level=1, wdStartThrLev1[16830,"value"])
recoStart <- putD(recoStart, level=0, wdStartThrLev0[16830,"value"])
recoFinish <- wdFinishThrZero4[[1]]
recoFinish <- putC(recoFinish, level=5, rep(0,32) )
recoFinish <- putC(recoFinish, level=4, rep(0,16) )
recoFinish <- putC(recoFinish, level=3, rep(0,8) )
recoFinish <- putC(recoFinish, level=2, rep(0,4) )
recoFinish <- putC(recoFinish, level=1, rep(0,2) )
recoFinish <- putC(recoFinish, level=0, wdFinishThrLev[16830,"value"] )
recoFinish <- putD(recoFinish, level=5, rep(0,32) )
recoFinish <- putD(recoFinish, level=4, rep(0,16) )
recoFinish <- putD(recoFinish, level=3, rep(0,8) )
recoFinish <- putD(recoFinish, level=2, wdFinishThrLev2[16830,"value"])
recoFinish <- putD(recoFinish, level=1, wdFinishThrLev1[16830,"value"])
recoFinish <- putD(recoFinish, level=0, wdFinishThrLev0[16830,"value"])
lines(1:119,   wr(recoStart)[1:119], type="l", col="red")
lines(120:179, wr(recoFinish)[1:60], type="l", col="blue")

wdFinishThrLev$L1 <- as.integer( sub("V","",wdFinishThrLev$L1) )
wdFinishThrLev    <- sort_df( wdFinishThrLev, vars="L1" )
wdFinishThrLev0$L1 <- as.integer( sub("V","",wdFinishThrLev0$L1) )
wdFinishThrLev0    <- sort_df( wdFinishThrLev0, vars="L1" )
wdFinishThrLev1$L1 <- as.integer( sub("V","",wdFinishThrLev1$L1) )[1:40000]
wdFinishThrLev1    <- sort_df( wdFinishThrLev1, vars="L1" )
wdFinishThrLev2$L1 <- as.integer( sub("V","",wdFinishThrLev2$L1) )[1:40000]
wdFinishThrLev2    <- sort_df( wdFinishThrLev2, vars="L1" )
wdFinishThrLev3$L1 <- as.integer( sub("V","",wdFinishThrLev3$L1) )[1:40000]
wdFinishThrLev3    <- sort_df( wdFinishThrLev3, vars="L1" )
wdStartThrLev$L1  <- as.integer( sub("V","",wdStartThrLev$L1) )
wdStartThrLev     <- sort_df( wdStartThrLev, vars="L1" )
wdStartThrLev0$L1  <- as.integer( sub("V","",wdStartThrLev0$L1) )
wdStartThrLev0     <- sort_df( wdStartThrLev0, vars="L1" )
wdStartThrLev1$L1  <- as.integer( sub("V","",wdStartThrLev1$L1) )[1:40000]
wdStartThrLev1     <- sort_df( wdStartThrLev1, vars="L1" )
wdStartThrLev2$L1  <- as.integer( sub("V","",wdStartThrLev2$L1) )[1:40000]
wdStartThrLev2     <- sort_df( wdStartThrLev2, vars="L1" )
wdStartThrLev3$L1  <- as.integer( sub("V","",wdStartThrLev3$L1) )[1:40000]
wdStartThrLev3     <- sort_df( wdStartThrLev3, vars="L1" )

require(h2o)
require(caret)
part <- createDataPartition( runif(dim(train)[[1]], 0.0, 2) >= 1, p = 3/4)[[1]]

trainTrain <- train[ part,2:26]
trainValid <- train[-part,2:26]

trainTrain <- cbind(trainTrain, data.frame(fi =wdStartThrLev [part,"value"]) )
trainTrain <- cbind(trainTrain, data.frame(mi0=wdStartThrLev0[part,"value"]) )
trainTrain <- cbind(trainTrain, data.frame(mi1=wdStartThrLev1[part,"value"]) )
trainTrain <- cbind(trainTrain, data.frame(mi2=wdStartThrLev2[part,"value"]) )
trainTrain <- cbind(trainTrain, data.frame(mi3=wdStartThrLev3[part,"value"]) )
trainTrain <- cbind(trainTrain, data.frame(ff =wdFinishThrLev [part,"value"]) )
trainTrain <- cbind(trainTrain, data.frame(mf0=wdFinishThrLev0[part,"value"]) )
trainTrain <- cbind(trainTrain, data.frame(mf1=wdFinishThrLev1[part,"value"]) )
trainTrain <- cbind(trainTrain, data.frame(mf2=wdFinishThrLev2[part,"value"]) )
trainTrain <- cbind(trainTrain, data.frame(mf3=wdFinishThrLev3[part,"value"]) )

trainValid <- cbind(trainValid, data.frame(fi =wdStartThrLev [-part,"value"]) )
trainValid <- cbind(trainValid, data.frame(mi0=wdStartThrLev0[-part,"value"]) )
trainValid <- cbind(trainValid, data.frame(mi1=wdStartThrLev1[-part,"value"]) )
trainValid <- cbind(trainValid, data.frame(mi2=wdStartThrLev2[-part,"value"]) )
trainValid <- cbind(trainValid, data.frame(mi3=wdStartThrLev3[-part,"value"]) )
trainValid <- cbind(trainValid, data.frame(ff =wdFinishThrLev [-part,"value"]) )
trainValid <- cbind(trainValid, data.frame(mf0=wdFinishThrLev0[-part,"value"]) )
trainValid <- cbind(trainValid, data.frame(mf1=wdFinishThrLev1[-part,"value"]) )
trainValid <- cbind(trainValid, data.frame(mf2=wdFinishThrLev2[-part,"value"]) )
trainValid <- cbind(trainValid, data.frame(mf3=wdFinishThrLev3[-part,"value"]) )

localH2O <- h2o.init(nthreads = -1,max_mem_size = '7g')
train.hex <- as.h2o(localH2O,trainTrain)
valid.hex <- as.h2o(localH2O,trainValid)
test.hex  <- as.h2o(localH2O,trainValid)

dllev <- h2o.deeplearning(x = 1:43,y = 44, training_frame = train.hex, validation_frame = valid.hex, hidden=c(20,10), epochs=5)
pred  <- h2o.predict(dllev,test.hex)
p     <- as.data.frame(pred)
cor(p[,1],trainValid[,44])
# shit doesn't do any good

#makewpstRO( c(as.numeric(train[25144,27:145]),rep(0,9)), c(as.numeric(train[25144,146:205]),rep(0,68)), filter.number=1, family="DaubExPhase")
#makewpstRO( as.numeric(train[25144,81:145]), c(as.numeric(train[25144,146:205]),rep(0,4)), filter.number=1, family="DaubExPhase")
ro25144 <- makewpstRO( as.numeric(train[25144,82:145])*train[25144,210], as.numeric(train[25144,142:205])*train[25144,210], filter.number=1, family="DaubExPhase", percentage=100 )
xnam  <- paste("X", 1:64, sep="")
fmla1 <- as.formula(paste("response ~ ", paste(xnam, collapse= "+")))
model <- lm(formula = fmla1, data = ro25144$df)
plot( predict( model, ro25144$df), type='l' )
lines( ro25144$df["response"], col="red")

dec3354 <- wpstREGR( as.numeric(train[3354,82:145])*train[3354,210] , ro25144)
plot( predict( model, dec3354), type='l' )
lines( as.numeric(train[3354,142:205])*train[3354,210]/10 , col="red")
# no good

# do the same on smoothed/reduced versions
wrStartThrZero4[[25144]][56:119]
wrFinishThrZero4[[25144]][1:60]

#c(65,73,81,89,97,105,113,121)

ro25144 <- makewpstRO(
    wrStartThrZero4 [[25144]][c( 1,17,33,49,65,81,97,113)] * train[25144,210],
    wrFinishThrZero4[[25144]][c( 1, 9,17,25,33,41,49, 57)] * train[25144,210],
    filter.number=1, family="DaubExPhase", percentage=100
)
# Haar is no good here

# Let's see if there are more explanatory wavelet packets we want to form our basis mainly out of those
require(plyr)
ro <- lapply( dlply(train[,c(1,27:205,210)],1,unlist), function(x){ makewpstRO( as.numeric(x[57:120])*as.numeric(x[181]), as.numeric(x[117:180])*as.numeric(x[181]), filter.number=1, family="DaubExPhase", percentage=200 ) } )
b <- lapply(ro,function(x){ order(x$ixvec) })
occ <- data.frame(t(matrix(unlist(b), nrow=128)))
hist(occ[,1])
# clearly, there is no best basis, so any _common_ ordering would do
oro <- lapply( ro, function(x){ df <- x$df[,c(1,(order(x$ixvec[1:126])+1))]; colnames(df) <- c("response",paste("X", 1:126, sep="")); return(df) } )
#xnam  <- paste("X", c(2:22,seq(26,62,4)), sep="")
xnam  <- paste("X", c(2,3,5:18), sep="")
fmla3 <- as.formula(paste("response ~ ", paste(xnam, collapse= "+")))
lmo <- lapply( oro, function(x){ lm(formula = fmla3, data = x)} )

require(reshape)
cff <- lapply(lmo,function(x){x$coeff})
cfm <- melt(cff)
cf <- aggregate( cfm,  by=list(Index=cfm$L1),  function(x){x} )
cf$L2 <- as.integer( cf$L1[,1] )[1:40000]
cf <- sort_df( cf, vars="L2" )
rm(cfm)
rm(cff)

plot( predict( lmo[[25144]], oro[[25144]]), type='l' )
lines( oro[[25144]]["response"], col="red")
# very mediocre fit
#ro25144 <- makewpstRO( as.numeric(train[25144,82:145])*train[25144,210], as.numeric(train[25144,142:205])*train[25144,210], filter.number=1, family="DaubExPhase", percentage=200 )
#df25144 <- ro25144$df[,c(1,(order(ro25144$ixvec[1:126])+1))]
#colnames(df25144) <- c("response",paste("X", 1:126, sep=""))
#xnam  <- paste("X", 1:64, sep="")
#fmla1 <- as.formula(paste("response ~ ", paste(xnam, collapse= "+")))
#model <- lm(formula = fmla1, data = df25144)
#plot( predict( model, df25144[]), type='l' )
#lines( ro25144$df["response"], col="red")

# also, not a stable fit;
lm( formula=fmla2, data=oro[[7342]])
# better regularized:
l1ce(fmla2, oro[[7342]], sweep.out = NULL, standardize = F, bound=1)
# so, what a heck?
cro <- lapply(oro[1:40000], function(x){cor(x[,paste("X",c(2,3,5:18),sep='')],x[,paste("X",c(2,3,5:18),sep='')])})
ccm <- melt(cro)
ccf  <- aggregate( ccm,  by=list(Index=ccm$L1),  function(x){x} )
ccf$L2  <- as.integer( ccf$L1[,1] )[1:40000]
ccf <- sort_df( ccf, vars="L2" )

ccc <- matrix( rep(F,16*16), ncol=16); ccc <- as.data.frame(ccc); colnames(ccc) <- paste("X",2,3,5:18,sep=''); rownames(ccc) <- paste("X",2,3,5:18,sep='')
for(i in 1:40000){ ccc <- ccc | (abs(matrix( ccf[i,"value"], ncol=16 )>0.8)) }
# ccc has lots of correlations!
ccc <- ifelse( abs(matrix( ccf[i,"value"], ncol=16 ))>0.999, T, F )
cases <- c()
for(i in 1:40000){ if( any( xor(ifelse(abs(matrix( ccf[i,"value"], ncol=16 )>0.6), T, F), ccc) ) ) cases <- rbind(cases,i) }
#length(cases) = 17172
max(cf[cases,"value"][,2])
cf[ cases[ which( cf[cases,"value"][,2] < min(cf[cases,"value"][,2]) + 1 ) ] ,"value"]
cor( as.numeric(oro[[19997]][,"X2"]), as.numeric(oro[[19997]][,"X4"]) )
cro[[19997]]
require(ggplot2)
ggplot(data=as.data.frame(cf[cases,"value"]),aes_string(x="V4")) + geom_histogram(breaks=seq(-250000,250000,10000)) + scale_y_log10()
sd(cf[ cases,"value"][,4])
sd(cf[-cases,"value"][,4])
# so, filtering the correlation early does make its job, but there are still same outliers screwing the NN later
which(is.na(cf[-cases,"value"][,16]))

############################################

x <- (1:256)
yi <- rep(0,128)
yi[1:119] <- as.numeric( train[25144,27:145] )
yf <- rep(0,64)
yf[1:60]  <- as.numeric( train[25144,146:205] )
yiwd <- wd(yi,filter.number=1, family="DaubExPhase")
yfwd <- wd(yf,filter.number=1, family="DaubExPhase")
yiwdEBT <- ebayesthresh.wavelet(yiwd)
yfwdEBT <- ebayesthresh.wavelet(yfwd)
yiwrEB <- wr(yiwdEBT)
plot(ts(t(train[25144,27:145])))
lines(x[1:116], yiwr[1:116], type="l", col="blue")
yfwrEB <- wr(yfwdEBT)
plot(ts(t(train[25144,146:205])))
lines(x[1:64], yfwr[1:64], type="l", col="blue")
ro25144 <- makewpstRO( yiwrEB[(116-63):116], yfwrEB[1:64], filter.number=1, family="DaubExPhase", percentage=100 )

############################################

require(h2o)
require(caret)
part <- createDataPartition( runif(dim(train)[[1]], 0.0, 2) >= 1, p = 3/4)[[1]]

#trainTrain <- train[ part,2:26]
#trainValid <- train[-part,2:26]
prep <- preProcess(train[ part,2:26], method=c('center','scale')) # c('BoxCox')
trainTrain <- predict(prep,train[ part,2:26])
trainValid <- predict(prep,train[-part,2:26])

require(reshape)
cff <- lapply(lmo,function(x){x$coeff})
cfm <- melt(cff)
cf  <- aggregate( cfm,  by=list(Index=cfm$L1),  function(x){x} )
trainTrain <- cbind( trainTrain, data.frame(coef = cf[ part,"value"]), data.frame(cff = jitter(cf[ part,"value"],factor=0.01)) )
trainValid <- cbind( trainValid, data.frame(coef = cf[-part,"value"]), data.frame(cff = jitter(cf[-part,"value"],factor=0.01)) )

localH2O <- h2o.init(nthreads = -1,max_mem_size = '7g')
train.hex <- as.h2o(localH2O,trainTrain)
valid.hex <- as.h2o(localH2O,trainValid)
test.hex  <- as.h2o(localH2O,trainValid)

dldec <- h2o.deeplearning(x = c(1:25,59), y = 27, training_frame = train.hex, validation_frame = valid.hex, hidden=c(20), epochs=5)
pred  <- h2o.predict(dldec,test.hex)
p     <- as.data.frame(pred)
cor(p[,1],trainValid[,27])


### JUNK BELOW ###
colnames(train) <- c("Id","Feature_1","Feature_2","Feature_3","Feature_4","Feature_5","Feature_6","Feature_7","Feature_8","Feature_9","Feature_10","Feature_11","Feature_12","Feature_13","Feature_14","Feature_15","Feature_16","Feature_17","Feature_18","Feature_19","Feature_20","Feature_21","Feature_22","Feature_23","Feature_24","Feature_25","Ret_2","Ret_3","Ret_4","Ret_5","Ret_6","Ret_7","Ret_8","Ret_9","Ret_10","Ret_11","Ret_12","Ret_13","Ret_14","Ret_15","Ret_16","Ret_17","Ret_18","Ret_19","Ret_20","Ret_21","Ret_22","Ret_23","Ret_24","Ret_25","Ret_26","Ret_27","Ret_28","Ret_29","Ret_30","Ret_31","Ret_32","Ret_33","Ret_34","Ret_35","Ret_36","Ret_37","Ret_38","Ret_39","Ret_40","Ret_41","Ret_42","Ret_43","Ret_44","Ret_45","Ret_46","Ret_47","Ret_48","Ret_49","Ret_50","Ret_51","Ret_52","Ret_53","Ret_54","Ret_55","Ret_56","Ret_57","Ret_58","Ret_59","Ret_60","Ret_61","Ret_62","Ret_63","Ret_64","Ret_65","Ret_66","Ret_67","Ret_68","Ret_69","Ret_70","Ret_71","Ret_72","Ret_73","Ret_74","Ret_75","Ret_76","Ret_77","Ret_78","Ret_79","Ret_80","Ret_81","Ret_82","Ret_83","Ret_84","Ret_85","Ret_86","Ret_87","Ret_88","Ret_89","Ret_90","Ret_91","Ret_92","Ret_93","Ret_94","Ret_95","Ret_96","Ret_97","Ret_98","Ret_99","Ret_100","Ret_101","Ret_102","Ret_103","Ret_104","Ret_105","Ret_106","Ret_107","Ret_108","Ret_109","Ret_110","Ret_111","Ret_112","Ret_113","Ret_114","Ret_115","Ret_116","Ret_117","Ret_118","Ret_119","Ret_120","Ret_121","Ret_122","Ret_123","Ret_124","Ret_125","Ret_126","Ret_127","Ret_128","Ret_129","Ret_130","Ret_131","Ret_132","Ret_133","Ret_134","Ret_135","Ret_136","Ret_137","Ret_138","Ret_139","Ret_140","Ret_141","Ret_142","Ret_143","Ret_144","Ret_145","Ret_146","Ret_147","Ret_148","Ret_149","Ret_150","Ret_151","Ret_152","Ret_153","Ret_154","Ret_155","Ret_156","Ret_157","Ret_158","Ret_159","Ret_160","Ret_161","Ret_162","Ret_163","Ret_164","Ret_165","Ret_166","Ret_167","Ret_168","Ret_169","Ret_170","Ret_171","Ret_172","Ret_173","Ret_174","Ret_175","Ret_176","Ret_177","Ret_178","Ret_179","Ret_180","Ret_MinusTwo", "Ret_MinusOne", "Ret_PlusOne", "Ret_PlusTwo", "Weight_Intraday", "Weight_Daily")

colnames(test) <- c("Id","Feature_1","Feature_2","Feature_3","Feature_4","Feature_5","Feature_6","Feature_7","Feature_8","Feature_9","Feature_10","Feature_11","Feature_12","Feature_13","Feature_14","Feature_15","Feature_16","Feature_17","Feature_18","Feature_19","Feature_20","Feature_21","Feature_22","Feature_23","Feature_24","Feature_25","Ret_2","Ret_3","Ret_4","Ret_5","Ret_6","Ret_7","Ret_8","Ret_9","Ret_10","Ret_11","Ret_12","Ret_13","Ret_14","Ret_15","Ret_16","Ret_17","Ret_18","Ret_19","Ret_20","Ret_21","Ret_22","Ret_23","Ret_24","Ret_25","Ret_26","Ret_27","Ret_28","Ret_29","Ret_30","Ret_31","Ret_32","Ret_33","Ret_34","Ret_35","Ret_36","Ret_37","Ret_38","Ret_39","Ret_40","Ret_41","Ret_42","Ret_43","Ret_44","Ret_45","Ret_46","Ret_47","Ret_48","Ret_49","Ret_50","Ret_51","Ret_52","Ret_53","Ret_54","Ret_55","Ret_56","Ret_57","Ret_58","Ret_59","Ret_60","Ret_61","Ret_62","Ret_63","Ret_64","Ret_65","Ret_66","Ret_67","Ret_68","Ret_69","Ret_70","Ret_71","Ret_72","Ret_73","Ret_74","Ret_75","Ret_76","Ret_77","Ret_78","Ret_79","Ret_80","Ret_81","Ret_82","Ret_83","Ret_84","Ret_85","Ret_86","Ret_87","Ret_88","Ret_89","Ret_90","Ret_91","Ret_92","Ret_93","Ret_94","Ret_95","Ret_96","Ret_97","Ret_98","Ret_99","Ret_100","Ret_101","Ret_102","Ret_103","Ret_104","Ret_105","Ret_106","Ret_107","Ret_108","Ret_109","Ret_110","Ret_111","Ret_112","Ret_113","Ret_114","Ret_115","Ret_116","Ret_117","Ret_118","Ret_119","Ret_120","Ret_MinusTwo", "Ret_MinusOne" )


# try Neural network that are agnostic to standartization/outliers?
fit  <- avNNet(Ret_PlusOne ~ Ret_MinusOne + Ret_MinusTwo + Feature_1 + Feature_2 + Feature_3 + Feature_4 + Feature_5 + Feature_6 + Feature_7 + Feature_8 + Feature_9 + Feature_10 + Feature_11 + Feature_12 + Feature_13 + Feature_14 + Feature_15 + Feature_16 + Feature_17 + Feature_18 + Feature_19 + Feature_20 + Feature_21 + Feature_22 + Feature_23 + Feature_24 + Feature_25,   data=train, repeats=25, size=20, decay=0.1, linout=T)
p2 <- predict(fit,train[-part,c(2:26,206,207)])
sd((train[-part,208] - p2))/sd(train[-part,208])
#  worked terrible

require(neuralnet)
fit  <- neuralnet(Ret_PlusOne ~ Ret_MinusOne + Ret_MinusTwo + Feature_1 + Feature_2 + Feature_3 + Feature_4 + Feature_5 + Feature_6 + Feature_7 + Feature_8 + Feature_9 + Feature_10 + Feature_11 + Feature_12 + Feature_13 + Feature_14 + Feature_15 + Feature_16 + Feature_17 + Feature_18 + Feature_19 + Feature_20 + Feature_21 + Feature_22 + Feature_23 + Feature_24 + Feature_25,   data=train, rep=10, hidden=c(30,10), linear.output=F)
compute(fit,train[-part,c(2:26,206,207)])$net.result
# complete crap 



imp <- read.csv("imp.txt",header=F,sep=',')

colnames(imp) <- c("Id","Feature_1","Feature_2","Feature_3","Feature_4","Feature_5","Feature_6","Feature_7","Feature_8","Feature_9","Feature_10","Feature_11","Feature_12","Feature_13","Feature_14","Feature_15","Feature_16","Feature_17","Feature_18","Feature_19","Feature_20","Feature_21","Feature_22","Feature_23","Feature_24","Feature_25","Ret_2","Ret_3","Ret_4","Ret_5","Ret_6","Ret_7","Ret_8","Ret_9","Ret_10","Ret_11","Ret_12","Ret_13","Ret_14","Ret_15","Ret_16","Ret_17","Ret_18","Ret_19","Ret_20","Ret_21","Ret_22","Ret_23","Ret_24","Ret_25","Ret_26","Ret_27","Ret_28","Ret_29","Ret_30","Ret_31","Ret_32","Ret_33","Ret_34","Ret_35","Ret_36","Ret_37","Ret_38","Ret_39","Ret_40","Ret_41","Ret_42","Ret_43","Ret_44","Ret_45","Ret_46","Ret_47","Ret_48","Ret_49","Ret_50","Ret_51","Ret_52","Ret_53","Ret_54","Ret_55","Ret_56","Ret_57","Ret_58","Ret_59","Ret_60","Ret_61","Ret_62","Ret_63","Ret_64","Ret_65","Ret_66","Ret_67","Ret_68","Ret_69","Ret_70","Ret_71","Ret_72","Ret_73","Ret_74","Ret_75","Ret_76","Ret_77","Ret_78","Ret_79","Ret_80","Ret_81","Ret_82","Ret_83","Ret_84","Ret_85","Ret_86","Ret_87","Ret_88","Ret_89","Ret_90","Ret_91","Ret_92","Ret_93","Ret_94","Ret_95","Ret_96","Ret_97","Ret_98","Ret_99","Ret_100","Ret_101","Ret_102","Ret_103","Ret_104","Ret_105","Ret_106","Ret_107","Ret_108","Ret_109","Ret_110","Ret_111","Ret_112","Ret_113","Ret_114","Ret_115","Ret_116","Ret_117","Ret_118","Ret_119","Ret_120","Ret_121","Ret_122","Ret_123","Ret_124","Ret_125","Ret_126","Ret_127","Ret_128","Ret_129","Ret_130","Ret_131","Ret_132","Ret_133","Ret_134","Ret_135","Ret_136","Ret_137","Ret_138","Ret_139","Ret_140","Ret_141","Ret_142","Ret_143","Ret_144","Ret_145","Ret_146","Ret_147","Ret_148","Ret_149","Ret_150","Ret_151","Ret_152","Ret_153","Ret_154","Ret_155","Ret_156","Ret_157","Ret_158","Ret_159","Ret_160","Ret_161","Ret_162","Ret_163","Ret_164","Ret_165","Ret_166","Ret_167","Ret_168","Ret_169","Ret_170","Ret_171","Ret_172","Ret_173","Ret_174","Ret_175","Ret_176","Ret_177","Ret_178","Ret_179","Ret_180")

require(fpp)
require(fracdiff)

source("func.R")

require(parallel)
options(mc.cores=4)

rets <- ts(t(imp[,c(27:205)]),start=1)
ms   <- mclapply(rets,measures)
require(reshape)
mms  <- melt(ms)
meas <- aggregate( mms, by=list(Index=mms$L1), function(x){x} )

require(caret)
part <- createDataPartition( runif(dim(imp)[[1]], 0.0, 2) >= 1, p = 3/4)[[1]]

trainTrain <- cbind(imp[ part,2:26], meas[ part,2])
trainValid <- cbind(imp[-part,2:26], meas[-part,2])
colnames(trainTrain)[26:36] <- names(ms[[1]])
colnames(trainValid)[26:36] <- names(ms[[1]])

# standardizing? 
prepTrain <- preProcess(trainTrain[,2:26], method=c('center','scale')) # c('BoxCox')
prepValid <- predict(prepTrain,trainValid[,2:26])

modelFit  <- train(trend ~ Feature_1 + Feature_2 + Feature_3 + Feature_4 + Feature_5 + Feature_6 + Feature_7 + Feature_8 + Feature_9 + Feature_10 + Feature_11 + Feature_12 + Feature_13 + Feature_14 + Feature_15 + Feature_16 + Feature_17 + Feature_18 + Feature_19 + Feature_20 + Feature_21 + Feature_22 + Feature_23 + Feature_24 + Feature_25, preProcess = c("center","scale"), data = trainTrain, method = "glm")

t <- predict(modelFit,trainValid)
sqrt( sum( (t - trainValid[,"trend"])^2 )/dim(trainValid)[1] )
#nnetFit <- train(TrainData, TrainClasses, method = "nnet", preProcess = "range", tuneLength = 2, trace = FALSE, maxit = 100)

library(h2o)
localH2O <- h2o.init(nthreads = -1,max_mem_size = '7g')

train.hex <- as.h2o(localH2O,trainTrain)
valid.hex <- as.h2o(localH2O,trainValid)

model <- h2o.randomForest(x=c(2:26),y = c(27:29), training_frame = train.hex, validation_frame = valid.hex, mtries = 18, sample_rate = 0.5, ntree = 500)

test.hex <- as.h2o(localH2O,trainValid)
pred <- h2o.predict(model,test.hex)
p <- as.data.frame(pred)
summary(p)

# what are the true features (as function of time) that we want to detect?
# for example in rets[,2520] or rets[,18832], or ? Seems like whenever it plunges, it will go up in the next lag
