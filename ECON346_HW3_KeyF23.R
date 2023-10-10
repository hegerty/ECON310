########################################
## ECON 346 HW3 Key F23
########################################

#Pull data
data<-read.csv("https://raw.githubusercontent.com/hegerty/ECON346/main/ECON346_GrowthRates.csv")
head(data)

#Here I just assumed digit = 0
# I could do this for all with more code (lists of numbers, colors, etc + a loop)

plot(data$G_MX,data$G_SA,pch=15,col="blue",xlab ="MX",ylab = "ZA",main="Growth Correlations")
legend("bottomright",legend=paste("Correlation = ",round(cor(data[,3],data[,4]),3),sep=""),box.lty = 0)

abline(lm(G_SA~G_MX,data=data),lwd=2,lty=6,col="dark grey")
#Add the years over the text
#text(ZA_G~MX_G,data=data,labels=data[,1],font=3,cex=1)

#No dots; only years
plot(data$G_MX,data$G_SA,pch=15,col="blue",xlab ="MX",ylab = "ZA",main="Growth Correlations",type="n")
legend("bottomright",legend=paste("Correlation = ",round(cor(data[,3],data[,4]),3),sep=""),box.lty = 0)
abline(lm(G_SA~G_MX,data=data),lwd=2,lty=6,col="dark grey")
text(G_SA~G_MX,data=data,labels=data[,1],font=3,cex=.8)
#Notice outlier = 2020 Alternatively, you could find: 
data[which.min(data$G_MX),c(1,3)]

