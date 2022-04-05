#########################################################
# Exchange Market Pressure Indices
# Principal Components Analysis
# In class example, ECON 310 Spring 2022
########################################################

# Generated EMP series (PCA) vs. s.d.-weighted indices
# Get data
# Set dates and examine
# extra data; also unlabeled
# 1995q2 - 2020q4, eight countries
# I set this just to click "Source"

data<-read.csv("https://raw.githubusercontent.com/hegerty/ECON310/main/310_Lec4_Data.csv")
head(data)
dim(data)
data<-data[1:103,]
colnames(data)
tail(data)

##########################################
## PRINCIPAL COMPONENTS ANALYSIS
## Much of this is extra!
## You need 1) pcev (eigenvalues), 
##          2) loadings (factor loadings)
##          3) pc1 (new series)
## You do not need a loop, table, etc.
##########################################

# set graphical parameters
par(mfrow=c(4,2))
par(mar=2*c(1,1,1,1))


# set values for loop
# 1-8 are original SD-weighted series; recalculated here
jlist<-seq(10,33,by=3) # 3 components j=10 #10, 13, etc.

#make some empty tables
cortab<-NULL
evtab<-NULL
loadingstab<-NULL

# loop for 8 pairs
for(i in 1:8){
s1<-data[,jlist[i]:(jlist[i]+2)] # 3 components here
# s1[,2]<--s1[,2] # to make 2nd component negative, but it doesn't help
emp1<-s1[,1]/sd(s1[,1])-s1[,2]/sd(s1[,2])+s1[,3]/sd(s1[,3]) # SD as alternate
colnames(s1)<-c("dlnE","delRES","delrdiff")

# do pca; here make tables manually
pca<-prcomp(s1,center = TRUE,scale. = TRUE) # s1 is the set of variables you want to use
#Eigenvalues
pcev<-sqrt(pca$sdev)
#Loadings: (will be 1 or 2)
pcl<-pca$rotation
loadings<-pcl[,1] # factor loadings
loadingstab<-cbind(loadingstab,loadings)
pcev # show eigenvalues
evtab<-rbind(evtab,pcev)
pcl[,1:2] # First two principal components
pc1<-pca$x[,1]
pc1<-ts(pc1,start=c(1995,2),frequency = 4)
pc2<-pca$x[,2]
pc1 # this is the generated series

# calculate and plot series
cor(pc1,emp1)
ts.plot(pc1,emp1,main=colnames(data)[i+1],col=c("blue","red"))
legend("bottomright",legend=c("PCA","SD"),lty=c(1,1),col=c("blue","red"),cex=.75,bty="n")
abline(h=0)
abline(h=1.5*sd(pc1),col="red",lty=2) # "Crisis" threshold, PC1

cortab<-rbind(cortab,round(cor(pc1,emp1),3))
}
par(mfrow=c(1,1)) # return to normal

# Clean up tables
rownames(cortab)<-colnames(data)[2:9]
colnames(cortab)<-"Corr."
colnames(loadingstab)<-rownames(cortab)
rownames(evtab)<-rownames(cortab)
colnames(evtab)<-c("PC1","PC2","PC3")

# Print results
print("Eigenvalues")
print(round(t(evtab),3)) #Eigenvalues (I decided to transpose this)
print("Factor Loadings")
print(round(loadingstab,3)) #factor loadings
print("Correlations: SD and PCA")
print(cortab) # correlation table

#################################################
## Just the main part
## CHANGE VARIABLE NAMES, DATES AS NEEDED
#################################################
# s1<-         # DEFINE YOUR SET OF VARIABLES
colnames(s1)<-c("dlnE","delRES","delrdiff") # change names as needed
pca<-prcomp(s1,center = TRUE,scale. = TRUE) # s1 is the set of variables you want to use
pcev<-sqrt(pca$sdev)
pcl<-pca$rotation # this gets overwritten
loadings<-pcl[,1]
pc1<-pca$x[,1]
pc1<-ts(pc1,start=c(1995,2),frequency = 4)  # TS only; change dates if needed
pc1 # this is the generated series
pcev
loadings
plot(pc1)  # works for time series
