######################################
## ECON 343 S26
## HW5 Key
#####################################

# get data
data<-read.csv("https://raw.githubusercontent.com/hegerty/ECON346/refs/heads/main/HW5_data.csv")
head(data)
tail(data)
data<-data[-c(562:565),] #clean it up
data$SR1503<-as.numeric(gsub(",","",data$SR1503))
tail(data)

#3
q3<-na.omit(cbind(data$MXCPI,data$SR1503))
cor(q3)[2]
plot(q3,xlab="SR1503",ylab="MXCPI",pch=20)

#4
data$RER<-data$USCPI*data$MXNUSD/data$MXCPI
tsdata<-ts(data[,-1],start=c(1979,1),frequency = 12)
head(tsdata)
ts.plot(tsdata[,9],lwd=3,main="Real Exchange Rate",ylab="",xlab="")

colnames(tsdata)
dlne<-diff(log(tsdata[,3]/lag(tsdata[,3],-12)))
dlnrer<-diff(log(tsdata[,9]/lag(tsdata[,9],-12)))
dlnreer<-diff(log(tsdata[,4]/lag(tsdata[,4],-12)))
tsdata2<-cbind(tsdata,dlne,dlnrer,dlnreer)

par(mfrow=c(1,3))
ts.plot(tsdata[,3],xlab="",ylab="",main="E")
ts.plot(tsdata[,9],xlab="",ylab="",main="RER")
ts.plot(tsdata[,4],xlab="",ylab="",main="REER")

#5
par(mfrow=c(1,1))
ts.plot(dlnrer,ylim=c(-0.5,1),xlab="",ylab="")
par(new=TRUE)
ts.plot(dlnreer,ylim=c(-0.5,1),col="red",xlab="",ylab="")
legend("top",legend=c("RER","REER"),lty=1,col=c("black","red"),bty="n")
plot(dlnrer,dlnreer,pch=20)
cor(na.omit(cbind(dlnrer,dlnreer)))[2]

#6
library(zoo)
rersd<-rollapply(data = dlnrer,FUN="sd",width=12,align="right")
plot(rersd,lwd=3,main="Volatility of Peso-Dollar Real Ex. Rate",xlab="",ylab="")
stats<-summary(rersd)[c(4,3,3,1,6)]
stats[3]<-sd(rersd)
names(stats)[3]<-"SD"
round(stats,3)
