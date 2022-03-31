#############################
## Freund, 5e Ch. 15 ANOVA
## pp. 548, ex. 15.1
## ALSO Kruskall-Wallis
#############################

## Simple data from book; make a set
A<-c(77,81,71,76,80)
B<-c(72,58,74,66,70)
C<-c(76,85,82,80,77)
c(mean(A),mean(B),mean(C))
ABC<-cbind(c(A,B,C),c(rep(1,5),rep(2,5),rep(3,5)))
ABC     
colnames(ABC)<-c("val","group")

n = length(A)
k = length(c("A","B","C"))

# Total, Treatment, Error Sums of Squares
SST<-sum(ABC[,1]^2)-(1/(n*k))*sum(ABC[,1])^2
SSTR<-(1/5)*(sum(A)^2+sum(B)^2+sum(C)^2)-(1/(n*k))*sum(ABC[,1])^2
SSE<-SST-SSTR

SST
SSTR
SSE

# degrees of freedom
dfTR<-k-1
dfE<-k*(n-1)
dfT<-k*n-1

# mean squares
MSTR<-SSTR/dfTR
MSTR
MSE<-SSE/dfE
MSE

# compare f to 1% critical value
f<-MSTR/MSE
f

# F with dfTR, dfE
df(f,dfTR,dfE)
qf(.01,dfTR,dfE,lower.tail = FALSE)

# Now do KW test
kruskal.test(ABC[,1],g=ABC[,2])

# ranks and groups
ABC<-cbind(ABC,rank(ABC[,1]))
R1<-mean(ABC[ABC[,2]==1,3])
R2<-mean(ABC[ABC[,2]==2,3])
R3<-mean(ABC[ABC[,2]==3,3])

N<-n*k
H<-(12/(N*(N+1)))*(length(A)*R1^2+length(B)*R2^2+length(C)*R3^2)-3*(N+1)
H
dchisq(H,k-1)
qchisq(.01,k-1,lower.tail = FALSE)
qchisq(.05,k-1,lower.tail = FALSE)

#adjust for ties (close but not quite)
ABC[,3]
# 3, 0, 3
a1<-2*(3^3-3)
adj<-1-a1/(N^3-1)
Hadj<-H/adj
Hadj
dchisq(Hadj,k-1)

