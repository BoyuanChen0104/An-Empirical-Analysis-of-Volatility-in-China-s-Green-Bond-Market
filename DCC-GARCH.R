library(zoo)
library(readxl)
library(ggplot2)
Sys.setlocale("LC_TIME", "English")#Change the language system into English
data_xlsx<-read_excel("/Users/boyuanchen/Desktop/thesis/termpaper_data.xlsx")
time=as.Date(data_xlsx$'date',"%Y/%m/%d")
data_xlsx1=ts(data_xlsx[,2:4])#Import the data#
#new#
par(mfrow=c(1,1),oma=c(0.2,0.2,0.2,0.2)) 
plot(zoo(data_xlsxd,time),xlab="time",ylab="Index", 
     plot.type = "single",col=c("blue","yellow","green"),
     lty=1:3,main="index series presentation");
par(mfrow=c(1,3),oma=c(0.2,0.2,0.2,0.2))  
plot(zoo(data_xlsx1[,1],time),xlab="time",ylab="SP300",main="stock market performance")
plot(zoo(data_xlsx1[,2],time),xlab="time",ylab="stern",main="nyu")
plot(zoo(data_xlsx1[,3],time),xlab="time",ylab="questorm",main="bu")
#



par(mfrow=c(1,1),oma=c(0.2,0.2,0.2,0.2))
plot(zoo(data_xlsx1,time),xlab="time",
     ylab="Index",plot.type="single",col=c("red","black","yellow","green"),
     lty=1:3,main="index series");
#Combination picture of three series
par(mfrow=c(1,3),oma=c(0.2,0.2,0.2,0.2))
plot(zoo(data_xlsx1[,1],time),xlab="time",ylab="SP300",main="stock market performance")
plot(zoo(data_xlsx1[,2],time),xlab="time",ylab="SPbond",main="bond market performance")
plot(zoo(data_xlsx1[,3],time),xlab="time",ylab="greenbond",main="greenbond market performance")
library(xts)
hh<-xts(data_xlsx$gb, as.Date(data_xlsx$date, format='%Y/%m/%d'))
win.graph(width=8.5,height=6.5,pointsize=8)
plot(hh,type = 'l', main='greenbond')
#repeat the code above to see the big picture of series performance
p1cor=cor(data_xlsx1)
p1cor
library(e1071)
install.packages('DistributionUtils')
library(DistributionUtils)
#Descriptive Statistics of each series
data_outline=function(x){
  m=mean(x)
  d=max(x)
  xd=min(x)
  me=median(x)
  s=sd(x)
  kur=kurtosis(x)
  ske=skewness(x)
  R = max(x)-min(x)
  data.frame(Mean=m,Median=me,max=d,min=xd,std_dev=s,
             Skewness=ske,Kurtosis=kur, R=R)
}
for (i in 1:3){print(data_outline(data_xlsx1[,i]))}
par(mfrow=c(1,3))
hist(data_xlsx1[,1],main="equity",col="#009999",xlab="index")
hist(data_xlsx1[,2],main="bond",col="pink",xlab="index")
hist(data_xlsx1[,3],main="greenbond",col="#339966",xlab="index")

#3 series: first difference to make them stationary
data_xlsxd=diff(data_xlsx1)

#new (might be deleted in the future) this serve as an example when the data is 
#taken log difference
data_xlsxdnew=diff(log(data_xlsx1))
for (i in 1:3)print(shapiro.test(data_xlsxdnew[,i]))
for (i in 1:3)print(adf.test(data_xlsxdnew[,i],alt="stationary"))
armamodel1new=auto.arima(data_xlsxdnew[,1], allowmean=FALSE, allowdrift=FALSE, trace=TRUE)
armamodel2new=auto.arima(data_xlsxdnew[,2],ic=c("aicc", "aic", "bic"))
armamodel3new=auto.arima(data_xlsxdnew[,3],ic=c("aicc", "aic", "bic"))
checkresiduals(armamodel1new)
summary(armamodel1new)
checkresiduals(armamodel2new)#ARIMA becomes 5,0,2
summary(armamodel2new)
#new end



for (i in 1:3)print(shapiro.test(data_xlsxd[,i]))
library(tseries)
library(xts)
#take first difference in another way
gbd=diff(data_xlsx$greenbond)
rbond=diff(data_xlsx$SPChinaBondIndex)
ra300=diff(data_xlsx$SPChinaA300IndexCNY)
hh1<-xts(gbd, as.Date(data_xlsx$date[2:1198], format='%Y/%m/%d'))
plot(hh1,type = 'l', col=("grey38"),bg=("gray100"),main='greenbond market')
hh2<-xts(ra300, as.Date(data_xlsx$date[2:1198], format='%Y/%m/%d'))
plot(hh2,type = 'l', col=("grey38"),bg=("gray100"),main='equity market')
hh3<-xts(rbond, as.Date(data_xlsx$date[2:1198], format='%Y/%m/%d'))
plot(hh3,type = 'l',col=("grey38"),bg=("gray100"), main='bond market')
#or beautifying the graph
plot(hh3,type = 'l',col=("forestgreen"), bg=("azure"), main='bond market')
#Dicky-Fuller test
for (i in 1:3)print(adf.test(data_xlsxd[,i],alt="stationary"))
install.packages('stats')
library(stats)
for (i in 1:3){
  par(mfrow=c(1,2))
  acf(data_xlsxd[,i])
  pacf(data_xlsxd[,i])
}
install.packages('forecast')
#ARIMA Model identification
library(forecast)
ggAcf(data_xlsxd[,1])
ggPacf(data_xlsxd[,1],ylim=c(-0.1,0.1))
armamodel1=auto.arima(data_xlsxd[,1],ic='aic')
armamodel2=auto.arima(data_xlsxd[,2],ic='aic', trace = TRUE, stepwise = TRUE)
armamodel2new=Arima(data_xlsxd[,2], order = c(5,0,3))
armamodel3=auto.arima(data_xlsxd[,3],ic='aic')
armamodel4=Arima(data_xlsxd[,1],order=c(1,0,0))
armamodel5=auto.arima(ra300, ic='aic', start.P=2, start.Q=2, statioary = FALSE, trace = TRUE, stepwise = TRUE)
checkresiduals(armamodel4)
summary(armamodel4)
checkresiduals(armamodel1)
summary(armamodel1)
checkresiduals(armamodel2)
summary(armamodel2)
checkresiduals(armamodel3)
summary(armamodel3)
#Box test: ARCH effect testing
Box.test(residuals(armamodel1),lag=5,type="Ljung-Box")
Box.test(residuals(armamodel2),lag=5,type="Ljung-Box")
Box.test(residuals(armamodel3),lag=5,type="Ljung-Box")
Box.test(residuals(armamodel1)^2,lag=5)
Box.test(residuals(armamodel2)^2,lag=5)
Box.test(residuals(armamodel3)^2,lag=5)
Box.test(residuals(armamodel2new)^2,lag=5)
Box.test(residuals(armamodel4)^2,lag=5)
library(xts)
residsq1=resid(armamodel4)^2
plot(residsq1)
residsq2=resid(armamodel2new)^2
plot(residsq2, type = 'l',col=("forestgreen"), main='bond market')
residsq3=resid(armamodel3)^2
plot(residsq3)
hh4<-xts(residsq2, as.Date(data_xlsx$date[2:1198], format='%Y/%m/%d'))
plot(hh4,type = 'l',col=("grey38"), bg=("white"), main='bond market')
hh5<-xts(residsq3, as.Date(data_xlsx$date[2:1198], format='%Y/%m/%d'))
plot(hh5,type = 'l',col=("grey38"), bg=("white"), main='greenbond market')
hh6<-xts(residsq1, as.Date(data_xlsx$date[2:1198], format='%Y/%m/%d'))
plot(hh6,type = 'l',col=("grey38"), bg=("white"), main='equity market')
#The ARCH test is used to determine the dependence of the conditional variance of the perturbation term on the prior variance
install.packages('MTS')
library(MTS)
archTest(residuals(armamodel1),lag=5)
archTest(residuals(armamodel2),lag=5)
archTest(residuals(armamodel3),lag=5)
archTest(residuals(armamodel2new),lag=5)
#GARCH Model specification
install.packages('fGarch')
library(fGarch)
gfit1=garchFit(~garch(1,1),data=data_xlsxd[,1],include.mean = FALSE, trace = F, cond.dist="norm")
gfit1
gfit2=garchFit(~garch(1,1),data=data_xlsxd[,2],include.mean = FALSE, trace = F, cond.dist="norm")
gfit2
gfit3=garchFit(~garch(1,1),data=data_xlsxd[,3],include.mean = FALSE, trace = F, cond.dist="norm")
gfit3
#Parameter estimation preparation process
install.packages('rmgarch')
library(rmgarch)
meanSpec=list(armaOrder=c(0,0),include.mean=FALSE,archpow=1)
distSpec=c("mvnorm")
varSpec=list(model="sGARCH",garchOrder=c(1,1))
spec1=ugarchspec(mean.model=meanSpec,variance.model=varSpec)
mySpec=multispec(replicate(2,spec1))
mspec=dccspec(mySpec,VAR=F,robust=F,lag=1,lag.max=NULL,lag.criterion=c("AIC"),external.regressors=NULL, 
             robust.control=list(gamma=0.25,delta=0.01,nc=10,ns=500),distribution=distSpec,
             start.pars=list(),fixed.pars=list())
#DCC-GARCH estimation
fdcc12=dccfit(data=data_xlsxd[,c(1,2)],mspec,out.sample = 10,solver="solnp",solver.control = list(),
              fit.control = list(eval.se=TRUE, stationary=TRUE, scale=FALSE),parallel.control=list(pkg=c("multicore"),cores=2),
              fit=NULL,VAR.fit = NULL)
show(fdcc12)#Demonstrate whether the risk spillover (transfer) effect of equity market and bond market is significant
plot(fdcc12)
fdcc13=dccfit(data=data_xlsxd[,c(1,3)],mspec,out.sample = 10,solver="solnp",solver.control = list(),
              fit.control = list(eval.se=TRUE, stationary=TRUE, scale=FALSE),parallel.control=list(pkg=c("multicore"),cores=2),
              fit=NULL,VAR.fit = NULL)
show(fdcc13)
plot(fdcc13)
fdcc23=dccfit(data=data_xlsxd[,c(2,3)],mspec,out.sample = 10,solver="solnp",solver.control = list(),
              fit.control = list(eval.se=TRUE, stationary=TRUE, scale=FALSE),parallel.control=list(pkg=c("multicore"),cores=2),
              fit=NULL,VAR.fit = NULL)
show(fdcc23)
plot(fdcc23)
nisurface(fdcc13)
nisurface(fdcc23)
#extract the conditional correlation
correlation1=rcor(fdcc13)
cora300_gb=correlation1[2,1,]
correlation2=rcor(fdcc23)
corcb_gb=correlation2[2,1,]
hh7<-xts(cora300_gb, as.Date(data_xlsx$date[2:1188], format='%Y/%m/%d'))
plot(hh7,type = 'l',col=("lightsteelblue"), bg=("white"),main='Conditional Correlation between GB & A300')
hh8<-xts(corcb_gb, as.Date(data_xlsx$date[2:1188], format='%Y/%m/%d'))
plot(hh8,type = 'l',col=("lightsteelblue"), bg=("white"),main='Conditional Correlation between GB & CB')
summary(cora300_gb)
#extract the conditional covariance
covariance=rcov(fdcc13)
cova300_gb=covariance[2,1,]
covariance2=rcov(fdcc23)
covcb_gb=covariance2[2,1,]
hh9<-xts(cova300_gb, as.Date(data_xlsx$date[2:1188], format='%Y/%m/%d'))
plot(hh9,type = 'l',col=("lightseagreen"), bg=("white"),main='Conditional Covariance between GB & A300')
hh10<-xts(covcb_gb, as.Date(data_xlsx$date[2:1188], format='%Y/%m/%d'))
plot(hh10,type = 'l',col=("lightseagreen"), bg=("white"),main='Conditional Covariance between GB & CB')
#extract the conditional variance
vargb=covariance[2,2,]
var300=covariance[1,1,]
varcb=covariance2[1,1,]
hedgeratio300_gb=cova300_gb/var300
data_xlsx$time <- as.Date(data_xlsx$date, "%m/%d/%Y",)
plot.ts(hedgeratio300_gb, type="l", col=("darkseagreen3"), bg=("white"),main='Hedge Ratio between GB & Equity Market')
axis(1,data_xlsx$time,format(data_xlsx$time, "%d-%m-%Y"))#not efficient
hh11<-xts(hedgeratio300_gb, as.Date(data_xlsx$date[2:1188], format='%Y/%m/%d'))
plot(hh11,type = 'l',col=("darkseagreen3"), bg=("white"),main='Hedge Ratio between GB & Equity Market')
hedgeratiocb_gb=covcb_gb/varcb
hh12<-xts(hedgeratiocb_gb, as.Date(data_xlsx$date[2:1188], format='%Y/%m/%d'))
plot(hh12,type = 'l',col=("darkseagreen3"), bg=("white"),main='Hedge Ratio between GB & CB')
plot.ts(hedgeratiocb_gb, type="l")