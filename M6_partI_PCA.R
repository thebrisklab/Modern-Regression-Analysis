##############
# PCA
# generate bivariate normal data
# and compare linear regression to PCA


# Here, p=2
# Induce correlation:
rho = 0.8
R = matrix(c(1,rho,rho,1),nrow=2)
library(expm)
sqrtR = sqrtm(R)
sqrtR

N=10000
y = matrix(rnorm(2*N),ncol=2)

x = y%*%sqrtR
cor(x)

# center and scale variables exactly:
x = scale(x,center = TRUE,scale=TRUE)
cov(x)

plot(x[,2]~x[,1])

# Obtain principal components via SVD:
# This could also be done using EVD of the covariance matrix
svd.x = svd(t(x)) 

# these define the principal axes:
svd.x$u
svd.x$u[,1]

svd.x$d^2/N #PC variance; sums to same as the sum of covariances
# when highly correlated, we can capture most of the variance
# on the first PC
sum(svd.x$d^2/N)
sum(diag(cov(x)))
# NOTE: sum to total variance = 2

# Fun fact: the singular value squared divide by N is equal to 
# the correlation plus 1. This is the correlation:
svd.x$d[1]^2/N - 1
# size of the first eigenvalue is related to the correlation
# when uncorrelated, first eigenvalue  = second eigenvalue


# convert the direction to a slope:
pc.slope = svd.x$u[2,1]/svd.x$u[1,1]
pc.slope
abline(a=0,b=pc.slope,col='green',lwd=2)
# this is the direction of highest variance in this data.
# (note: it turns out this direction is always the same for p=2, 
# no matter correlation, and corresponds to a slope=1; 
# however, the amount of variance captured by this direction
# is determined by the strength of correlation. At extreme,
# x2=x1, and all variance is captured on this axis.)


#first pc:
#these are the pc scores
z1 = svd.x$v[,1]*svd.x$d[1]
# the ith score reflects the importance of the first 
# principal component for the ith observation
# it represents the coordinate of the data point
# along the first principle axis, i.e.,
# if you draw a line from 0,0 to the (x1,x2) projected onto U_1
# "longest" points, signs may be swapped
# note the sign is not identifiable, because 
# xhat = as.matrix(z1,ncol=1)%*%svd.x$u[1,] =(-1*as.matrix(z1,ncol=1))%*%(-1*svd.x$u[1,])
min(z1)
x[which.min(z1),]

max(z1)
x[which.max(z1),]

# reconstruct the data using first PC:
xhat = as.matrix(z1,ncol=1)%*%svd.x$u[1,]
points(xhat,col='green')

# the hypotenuse of the triangles equal to lengths in subject scores:
head(sqrt(xhat[,1]^2 + xhat[,2]^2))
head(abs(z1))


# using prcomp:
prcomp(x)
svd.x$u
# equivalent up to sign. 

# standard deviations using svd 
svd.x$d/sqrt(N-1)


#######
# Compare this to the least squares line of best fit:
lm.model = lm(x[,2]~x[,1])
abline(lm.model,col='red')
# this line changes as you change the correlation



########################
## Example of probabilistic pca model:
N=100
y = matrix(rnorm(2*N),ncol=2)
# create PC scores:
z = y%*%diag(c(4,2))

#mixing matrix for latent components:
# generate a matrix with orthonormal rows:
mytW = t(svd(matrix(rnorm(20),nrow=2))$v)
mytW
mytW%*%t(mytW)


# mix the latent components. This is the 
# signal portion of the data matrix:

latentmix = z%*%mytW
cor(latentmix)
library(corrplot)
corrplot(cor(latentmix))

# add isotropic noise:
noise = matrix(rnorm(N*10),ncol=10)
bigX = latentmix+noise

# prcomp is in base R
newdecomp = prcomp(bigX)

# create screeplot:)
plot(newdecomp$sdev^2,type='b')
newdecomp$sdev^2
plot(newdecomp$sdev[3:10]^2)

sum(newdecomp$sdev^2)
# signal plus noise variances
16+4+10*1

# estimation of directions:
cor(t(mytW),newdecomp$rotation[,1:2])
t(mytW)
newdecomp$rotation[,1:2]
# (NOTE: In practice, center and standardize X, but not done in this toy example for illustrating PPCA)



##########################
######################

# citation: https://www.r-bloggers.com/principal-component-analysis-in-r/
# http://archive.ics.uci.edu/ml/datasets/Wine
wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep=",")


# Name the variables
#colnames(wine) <- c("Cvs","Alcohol","Malic acid","Ash","Alkalinity", "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols", "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315 of diluted wines", "Proline")
colnames(wine) <- c("Cvs","Alcohol","Malic acid","Ash","Alkalinity", "Magnesium", "Phenols", "Flavanoids", "NonflavPhen", "Proantho", "Color", "Hue", "OD", "Proline")

# The first column corresponds to the cultivars (varieties)
wineClasses <- factor(wine$Cvs)
wine$Cvs = wineClasses

# Use pairs
pairs(wine[,-1], col = wineClasses, upper.panel = NULL, pch = 16, cex = 0.5)
legend("topright", bty = "n", legend = c("Cv1","Cv2","Cv3"), pch = 16, col = c("black","red","green"),xpd = T, cex = 2, y.intersp = 0.5)
cor(wine[,-1])
corrplot(cor(wine[,-1]))

# a fair amount of correlation, so we can reasonably hope to capture most of the informations with some $q < 13$
winePCA <- prcomp(wine[,-1],center = TRUE, scale. = TRUE)
summary(winePCA)
screeplot(winePCA)

# I prefer the plot with points:
#screeplot: the most important tool for selecting number of components
plot(winePCA$sdev^2,xlab='Rank of Eigenvalue',ylab='Eigenvalue',main='Screeplot for the Wine Dataset',type='b')

# here, 3 would be a good option since ith corresponds to the "elbow"
cumsum(winePCA$sdev^2)/sum(winePCA$sdev^2)
# 67% of variance with 3. 90% if often a rule of thumb for data compression; for latent variable representation, "elbow" is a useful heuristic


# the scores can often be useful for visualizing differences between groups
# plot the scores for the first two PCs and color the points by the categories:
par(mfrow=c(1,3))
plot(winePCA$x[,1:2], col = wineClasses)
plot(winePCA$x[,c(1,3)], col = wineClasses)
plot(winePCA$x[,c(2,3)], col = wineClasses)
# First two look seem to help the most with separating all three

# note the importance of standardization:
badwinePCA <- prcomp(wine[,-1])
plot(badwinePCA$x[,1:2], col = wineClasses)


# Additional plots that can be useful for inspecting which variables
# were most important in the PCs:
#source("https://bioconductor.org/biocLite.R")

#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install()
#library("BiocManager")

BiocManager::install("pcaMethods")
library(pcaMethods)
winePCAmethods <- pca(wine[,-1], scale = "uv", center = T, nPcs = 3, method = "svd")
slplot(winePCAmethods, pcs=c(1,2),scoresLoadings = c(T,T), scol = wineClasses,hotelling=FALSE)

slplot(winePCAmethods, pcs=c(2,3),scoresLoadings = c(T,T), scol = wineClasses,hotelling=FALSE)
slplot(winePCAmethods, pcs=c(1,3),scoresLoadings = c(T,T), scol = wineClasses,hotelling=FALSE)

# we can see that the loadings of phenols, 
winePCA$rotation[,1:2]
# The variable loadings are what prcomp calls "Rotation"
winePCA$rotation[apply(abs(winePCA$rotation[,1:2])>0.4,1,sum)>0,1:2]
# It is the magnitudes that matter.




###############
# Based on the example from 6.7, Lab 3, in James et al, ISL:
# this library contains datasets using in ISL:
library(ISLR) 
# this library contains some short-cut functions for PCR:
library(pls)
View(Hitters)
sum(is.na(Hitters$Salary))
dim(Hitters)

Hitters = na.omit(Hitters)
dim(Hitters)

pcr.fit = pcr(Salary~., data=Hitters, scale=TRUE)
summary(pcr.fit)
# NOTE: make sure to scale=TRUE -- defaults to false:(
# % variance explained: percent in X, second row is R^2 in PRC

set.seed(2) # for cross-validation to determine number of components
pcr.fit = pcr(Salary~., data=Hitters, scale=TRUE, validation='CV')
summary(pcr.fit)
validationplot(pcr.fit,estimate='all',legendpos='topright')

# by default, uses 10-fold CV. Doesn't provide clear guidance.
# The minimum occurs at 16, which is not helpful. However, 
# just using 1 component does pretty well. 

# Although the output nicely summarizes the predictive ability, we could also do it by hand:
# interestingly, pcr automatically converted categorical to dummies...

# create a matrix that can be used by prcomp:
hitters = model.matrix(Salary~.,data=Hitters)

# now use prcomp and then lm:
pca.hitters = prcomp(x=hitters[,-1],center = TRUE, scale. = TRUE)
summary(pca.hitters)
plot(pca.hitters$sdev^2,type='b')
# if we used the 95% rule of thumb, choose 9 components
# Let's try four, since these eigenvalues stand out

pcr.fit.v2 = lm(Hitters$Salary~pca.hitters$x[,1:4])
summary(pcr.fit.v2)

# look what happens with 16:
tempdata = data.frame('Salary'=Hitters$Salary,pca.hitters$x[,1:16])
pcr.fit.v3 = lm(Salary~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16,data=tempdata)
vif(pcr.fit.v3)

summary(pcr.fit.v3)
# some later components are highly significant

dataf = data.frame(Hitters$Salary,pca.hitters$x)
pcr.fit.v4 = lm(Hitters.Salary~.,data=dataf)
summary(pcr.fit.v4)
car::vif(pcr.fit.v4)
# VIF all equal 1 because orthogonal

plot(pca.hitters$rotation[,1])
# examine loadings for first PC:
pca.hitters$rotation[,1]
# career numbers have larger loadings than the season's, e.g., runs and rbi's;


ols.fit = lm(Salary~.,data=Hitters)
car::vif(ols.fit)
# vif's are huge, so p-values from ols are not very useful. The loadings are one alternative, more qualitative way to interpret importance
summary(ols.fit)

