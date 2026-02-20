##################################
## ECON 343 S26
## HW4 Key
#################################

# get data
data<-read.csv("https://raw.githubusercontent.com/hegerty/ECON343/refs/heads/main/ECON343_HW4_Data.csv")
head(data)
tail(data)
tsdata<-ts(data[,-1],start=c(1976,1),frequency = 12) # Make TS
head(tsdata)


infus12<-100*((tsdata[,1]/lag(tsdata[,1],-12))-1) # Make all variables
infus<-1200*((tsdata[,1]/lag(tsdata[,1],-1))-1)
infmx12<-100*((tsdata[,3]/lag(tsdata[,3],-12))-1)
infmx<-1200*((tsdata[,3]/lag(tsdata[,3],-1))-1)
infdiff<-infmx12-infus12
usrealr<-infus12-tsdata[,2]  # Even from later questions
mxrealr<-infmx12-tsdata[,4]
nomrdiff<-tsdata[,4]-tsdata[,2]
realrdiff<-mxrealr-usrealr

# One big database
tsdata2<-cbind(tsdata,infus12,infus,infmx12,infmx,infdiff,usrealr,mxrealr,nomrdiff,realrdiff)
colnames(tsdata2)[1:4]<-colnames(tsdata)
head(tsdata2)
tail(tsdata2)

# Plot the pairs
par(mfrow=c(2,2))
par(mar=c(2,2,1,1))
ts.plot(tsdata2[,c(2,4)],lwd=c(2,2),col=c("black","dark grey"),main="Nominal r")
legend("topright",legend=c("US","Mexico"),lwd=c(2,2),col=c("black","dark grey"),bty="n",cex=.5)
ts.plot(tsdata2[,c(5,7)],lwd=c(2,2),col=c("black","dark grey"),main="12-month inflation")
legend("topright",legend=c("US","Mexico"),lwd=c(2,2),col=c("black","dark grey"),bty="n",cex=.5)
ts.plot(tsdata2[,c(6,8)],lwd=c(2,2),col=c("black","dark grey"),main="1-month inflation")
legend("topright",legend=c("US","Mexico"),lwd=c(2,2),col=c("black","dark grey"),bty="n",cex=.5)
ts.plot(tsdata2[,c(7,8)],lwd=c(2,2),col=c("black","dark grey"),main="Mexican inflation rates")
legend("topright",legend=c("12-mo","1-mo"),lwd=c(2,2),col=c("black","dark grey"),bty="n",cex=.5)

par(mfrow=c(1,1))
par(mar=c(2,2,1,1))
ts.plot(tsdata2[,c(10,11)],lwd=c(2,2),col=c("black","dark grey"),main="Real interest rates")
legend("topright",legend=c("US","Mexico"),lwd=c(2,2),col=c("black","dark grey"),bty="n",cex=1)

ts.plot(tsdata2[,c(12,13)],lwd=c(2,2),col=c("black","dark grey"),main="Interest-rate differentials")
legend("topright",legend=c("Nominal","Real"),lwd=c(2,2),col=c("black","dark grey"),bty="n",cex=1)

#7 
par(mar=c(4,4,1,1))

plot(infdiff,realrdiff,pch=20)

cor(infdiff,realrdiff)
#8 Summary stats
ss1<-na.omit(tsdata2[,5:13])
ss1
statstable<-round(rbind(
apply(ss1,2,"mean"),
apply(ss1,2,"median"),
apply(ss1,2,"sd"),
apply(ss1,2,"min"),
apply(ss1,2,"max")
),3)
rownames(statstable)<-c("Mean","Median","SD","Min","Max")
statstable

## BONUS
# Split 1/1/2000
tsdata2<c(2000,1)

tsbefore<-tsdata2[time(tsdata2)<2000,]
tsafter<-tsdata2[time(tsdata2)>=2000,]
dim(tsdata2)
dim(tsbefore)+dim(tsafter)


ss1<-na.omit(tsbefore[,5:13])
ss1
statstable1<-round(rbind(
  apply(ss1,2,"mean"),
  apply(ss1,2,"median"),
  apply(ss1,2,"sd"),
  apply(ss1,2,"min"),
  apply(ss1,2,"max")
),3)
rownames(statstable1)<-c("Mean","Median","SD","Min","Max")

ss1<-na.omit(tsafter[,5:13])
ss1
statstable2<-round(rbind(
  apply(ss1,2,"mean"),
  apply(ss1,2,"median"),
  apply(ss1,2,"sd"),
  apply(ss1,2,"min"),
  apply(ss1,2,"max")
),3)
rownames(statstable2)<-c("Mean","Median","SD","Min","Max")
statstable1
statstable2
