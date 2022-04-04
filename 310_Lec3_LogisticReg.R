###############################################
# Chicago Block Groups: Logistic Regression
# In class example, ECON 310 Spring 2022
###############################################
#install.packages("rms") <- This package gives pseudo-R2
library(rms)

# Get data
data<-read.csv("https://raw.githubusercontent.com/hegerty/ECON310/main/310_Lec3_Data.csv")
data<-na.omit(data)
head(data)
dim(data)

# examine binary variable
summary(data$BANKCOUNTB)
head(data$BANKCOUNTB,40)

# compare against count data
summary(data$BANKCOUNT) # you could use poisson, negative binomial
tail(data$BANKCOUNT,40)


# Get some statistics
colnames(data)
summary(data[,6:13])

# Run some logistic regressions:
# DV: whether there is at least one bank in a given block group
# Try some different explanatory variables
# Which are significant?

attach(data)
lr0<-glm(BANKCOUNTB~PERCVAC+PERC75K+PERC25K,family = "binomial")
summary(lr0)
# Use rms package for pseudo-R-squared
lrm0<-lrm(BANKCOUNTB~PERCVAC+PERC75K+PERC25K,data=data)
lrm0
lrm0$stats[10]

lr1<-glm(BANKCOUNTB~PERCVAC+PERC75K,family = "binomial")
summary(lr1)
lrm1<-lrm(BANKCOUNTB~PERCVAC+PERC75K,data=data)


lr2<-glm(BANKCOUNTB~PERCVAC+PERC25K,family = "binomial")
summary(lr2)
lrm2<-lrm(BANKCOUNTB~PERCVAC+PERC25K,data=data)

lr3<-glm(BANKCOUNTB~PERCVAC+PERCBLK,family = "binomial")
summary(lr3)
lrm3<-lrm(BANKCOUNTB~PERCVAC+PERCBLK,data=data)

lr4<-glm(BANKCOUNTB~PERCVAC+PERCWHT,family = "binomial")
summary(lr4)
lrm4<-lrm(BANKCOUNTB~PERCVAC+PERCWHT,data=data)

# Compare Pseudo-R2
pr2<-cbind(lrm0$stats[10],lrm1$stats[10],lrm2$stats[10],lrm3$stats[10],lrm4$stats[10])
colnames(pr2)<-0:4
round(pr2,4)
lrm3 # best choice?

# plot bank variable vs. vacancy rate
plot(PERCVAC,BANKCOUNTB,pch=20)
plot(PERCBLK,BANKCOUNTB,pch=20)



