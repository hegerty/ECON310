#################################
# ECON 346 HW1 Key F2023
################################

## Read data from GitHub
data<-read.csv("https://raw.githubusercontent.com/hegerty/ECON346/main/ECON346_GrowthRates.csv",header=TRUE)
head(data)

# Apply means formula
means<-apply(data,2,mean)
means[-1]
## What does it mean to omit column 1, and why?

sd<-apply(data,2,sd)
sd[-1]

# Do it manually but just for Mexico
# Sum of squared deviation from mean
# One column is the same every year
# Remember to divide by N-1
data2<-data[,3]
data3<-cbind(data2,mean(data2),data2-mean(data2),(data2-mean(data2))^2)
sdmx<-sqrt(sum(data3[,4])/(nrow(data3)-1))
sdmx

#show the originals
head(data3)

# I asked for five column names, so here I add them--verbatim!
data3<-cbind(data2,mean(data2),data2-mean(data2),(data2-mean(data2))^2)
colnames(data3)<-c("mx_g","meanmx_g","X-meanmx_g","(X-meanmx_g)^2")
head(data3)


colnames(data3)<-c("mxgrowth","mean","dev","sqdev")
head(data3)  

#All in one line
sum(data2)/length(data2)
sqrt(sum((data2-mean(data2))^2)/(nrow(data3)-1))

