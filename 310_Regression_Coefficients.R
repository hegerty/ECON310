##############################################
## ECON 310/318/343/346 NEIU
## Understanding Regression Coefficients
##############################################
# Generate data for regression
set.seed(1)
x<-rnorm(1000)
y<-12*x+rnorm(1000,0,130) # related a little to x
z<-rnorm(1000,5,2) # totally independent

# Run regression
reg<-lm(y~x+z)
summary(reg)
coeffs<-summary(reg)[4] # to use on graph
bx<-coeffs$coefficients[2,1] #beta (coefficient)
bz<-coeffs$coefficients[3,1]
sx<-coeffs$coefficients[2,2] # standard error
sz<-coeffs$coefficients[3,2]

# Plot t-distribution
x1<-seq(-4,4,length=1000)
df<-reg$df.residual
y1<-dt(x1,df)
q5<-qt(0.975,df) #critical value for beta/se at 5%
q5

plot(x1,y1,type = "l",lwd=2,ylab="Prob.",xlab="t") # basic curve
abline(v=qt(0.975,df),lty=2,lwd=2,col="dark gray") # +0.025
abline(v=qt(0.025,df),lty=2,lwd=2,col="dark gray") # -0.025

# test x: is significant
abline(v=bx/sx,col="red") # t-stat
bx/sx # (beta - zero)/se
2*(1-pt(bx/sx,df)) # p-value
coeffs$coefficients[2,] # show results to compare

# test z: is not significant
plot(x1,y1,type = "l",lwd=2,ylab="Prob.",xlab="t") # basic curve
abline(v=qt(0.975,df),lty=2,lwd=2,col="dark gray") # +0.025
abline(v=qt(0.025,df),lty=2,lwd=2,col="dark gray") # -0.025
abline(v=bz/sz,col="red")
bz/sz
2*(1-pt(bz/sz,df))
coeffs$coefficients[3,] # show results to compare

# also 95% Confidence Intervals
bands<-rbind(c(bx-q5*sx,bx+q5*sx),c(bz-2*sz,bz+2*sz))
rownames(bands)<-c("x","z")
colnames(bands)<-c("-2se","+2se")
round(bands,3) #not used that much

# Now look again at results
summary(reg)
# coeffs/se/t/p
# R-squared
# Everything else

# test Hypothesis that bx = 12
stat12<-(bx-12)/sx
stat12
2*(1-pt(stat12,df)) # not significant


# Interpret this!
z1<--1.5*y+rnorm(1000,0,400) # new z that is related
summary(lm(y~x+z1))

