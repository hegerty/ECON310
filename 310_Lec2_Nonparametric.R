########################################
# ECON 310: Nonparametric Data Example
# NEIU Spring 2022
#######################################

# Generate 250 observations of random data
set.seed(1)
n<-250
x<-runif(n,5,10)
x
# y is a function of x
y<-2.5*x+rnorm(n,1,2)

# Plot and get Pearson and Spearman correlations; also Kendall's tau
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

# Random assignment among 3 groups
for(i in 1:n){
  data[i,3]<-sample(c(1,2,3),1)
}
head(data)


# Are the means of x different?
plot(data[,3],data[,1],pch=20)
mean(data[data[,3]==1,1])
mean(data[data[,3]==2,1])
mean(data[data[,3]==3,1])
#not significant
kruskal.test(x= data[,1],g = data[,3])

# Now split off the bottom 25% into the second of two groups 
qv<-as.numeric(quantile(data[,1],.25))
data[which(data[,1]<qv),4]<-2

plot(data[,4],data[,1])
# They clearly differ
mean(data[data[,4]==1,1])
mean(data[data[,4]==2,1])
kruskal.test(x= data[,1],g = data[,4])

# Now I introduce some randomness
# Apply 1 and 2s
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
# Different results with different random data

##################################################
# Here is the Spearman correlation formula by hand
## Original x and y from above

# They match!
x1<-rank(x)
x1
# no ties here but check this out
rank(c(100,4,89,4,71,15))

y1<-rank(y)
D<-x1-y1
D
1-6*sum(D^2)/(n*(n^2-1))
cor(x,y,method="spearman")

########################################
# Compare KW by hand
kruskal.test(x= data[,1],g = data[,3])

# ranks and groups
R1<-mean(x1[data[,3]==1])
R2<-mean(x1[data[,3]==2])
R3<-mean(x1[data[,3]==3])
l1<-nrow(data[data[,3]==1,])
l2<-nrow(data[data[,3]==2,])
l3<-nrow(data[data[,3]==3,])
N<-l1+l2+l3
N
k<-3

H<-(12/(N*(N+1)))*(l1*R1^2+l2*R2^2+l3*R3^2)-3*(N+1)
H
2*dchisq(H,k-1)

