########################################
# ECON 310: Nonparametric Data Example
# NEIU Spring 2022
#######################################

# Generate 250 observations of random data
set.seed(1)
x<-runif(250,5,10)
x
# y is a function of x
y<-2.5*x+rnorm(250,1,2)

# Plot and get Pearson and Spearman correlations
plot(x,y,pch=20)
cor(x,y)
cor(x,y,method="spearman")
cor(x,y,method="kendall")

# Now make an outlier
y2<-y
y2[max(y)]<-10*y[max(y)]
plot(x,y2,pch=20)
cor(x,y2)
cor(x,y2,method="spearman")
cor(x,y,method="kendall")
# Do the correlations change?

# Now generate some groups and make a dataset
data<-cbind(x,y,1,1,1)
head(data)

# Random assignment
for(i in 1:250){
  data[i,3]<-sample(c(1,2,3),1)
}
head(data)


# Are the means different?
plot(data[,3],data[,1],pch=20)
kruskal.test(x= data[,1],g = data[,3])

# Now split off the bottom 25%
qv<-as.numeric(quantile(data[,1],.25))
data[which(data[,1]<qv),4]<-2

plot(data[,4],data[,1])
kruskal.test(x= data[,1],g = data[,4])

# Now I introduce some randomness
qv<-as.numeric(quantile(data[,1],.5))


data[which(data[,1]<qv),5]<-2

for(i in which(data[,5]==2)){
  data[i,5]<-sample(c(1,2),1)
}
for(i in which(data[,4]==1)){
  data[i,5]<-sample(c(1,2,2),1)
}
#Plot and visually compare means
plot(data[,c(5,1)],pch=20,col=c("green","red"))
abline(h=mean(data[which(data[,5]==2),1]),col="red")
abline(h=mean(data[which(data[,5]==1),1]),col="green")
kruskal.test(x= data[,1],g = data[,5])


