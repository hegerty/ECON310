################################
# ECON 310: Correlation formulas
# NEIU Spring 2022
################################

# Generate data
options(max.print=999999)
set.seed(5)
n<-1000
x<-round(100*rt(n,14),1)
x
y<-round(x+rnorm(n,1,200),1)
y
cbind(x,y)

plot(x,y,pch=20)

# Pearson vs. Spearman
# Formulas vs. program
cor(x,y)
cor(x,y,method = "spearman")

# Pearson
# Start with means, variances, sds
xbar<-mean(x)
ybar<-mean(y)
varx<-sum((x-xbar)^2)/(n-1)
vary<-sum((y-ybar)^2)/(n-1)
# compare!
varx
var(x)

sdy<-sqrt(vary)
sdx<-sqrt(varx)
(sum((x-xbar)*(y-ybar))/(n-1))/(sdx*sdy)
cor(x,y)

# Now do Spearman
x1<-rank(x)
x1
y1<-rank(y)
D<-x1-y1
D
1-6*sum(D^2)/(n*(n^2-1))
cor(x,y,method="spearman")


