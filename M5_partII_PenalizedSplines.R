##############################################
# Module 5 part II: Penalized and smoothing splines
##############################################

library (splines)

load ("~/Dropbox/EmoryCourses/BIOS_526/Materials_BRisk_2020/Data/NYC.RData")

str (health)

#Fig 1
boxplot(alldeaths~round(Temp), xlab = "Temperature (F)", ylab = "Death count", data=health)
par (mar = c(4,4,1,1))
plot(alldeaths~Temp, xlab = "Temperature (F)", ylab = "Death count", data=health)

health = health [order(health$Temp),] 

# Slide 11:
## Penalized Regression 
knots = seq(range(health$Temp)[1], range(health$Temp)[2], length.out = 40+2)
# place knots evenly on interior of the range of x
knots = knots[c(2:(length(knots)-1))]
X = cbind(rep(1,length(health$Temp)),health$Temp)
for (i in 1:length(knots)) {
  X = cbind(X,(health$Temp-knots[i])*(health$Temp>knots[i]))
}
B = diag(42)
B[1,1]=0
B[2,2]=0
dim (X); dim (B)

matplot(health$Temp,X,type='l')


# Slide 13
Y = health$alldeaths

lambda = 0 
beta = solve (t(X)%*%X + lambda*B) %*% t(X) %*% Y
H = X %*% solve (t(X)%*%X + lambda*B) %*% t(X) 		##Hat matrix, for lambda>0, "smoothing" matrix
Yhat = X%*%beta                                       ##Fitted values 
(GCV = mean ( (Y-Yhat)^2 ) / (1- mean (diag(H)))^2)
(C = t(beta)%*%B%*%beta)
# Shrinkage will correspond to values < C_{lambda=0}

# Slide 14
###Comparison of fit for different penalties
beta1 = solve (t(X)%*%X + 0*B) %*% t(X) %*% Y
beta2 = solve (t(X)%*%X + 5*B) %*% t(X) %*% Y
beta3 = solve (t(X)%*%X + 20*B) %*% t(X) %*% Y
beta4 = solve (t(X)%*%X + 50*B) %*% t(X) %*% Y
beta5 = solve (t(X)%*%X + 100*B) %*% t(X) %*% Y

plot(alldeaths~Temp, xlab = "Temperature (F)", ylab = "Death count", data=health, col = "grey")
lines(X%*%beta1~health$Temp, type = "l", lwd = 3, col =1 )
lines(X%*%beta2~health$Temp, type = "l", lwd = 3, col =2 )
lines(X%*%beta3~health$Temp, type = "l", lwd = 3, col =3 )
lines(X%*%beta4~health$Temp, type = "l", lwd = 3, col =4 )
title (main = "Penalized Linear Splines with 40 Knots")
legend ("topright", legend = c("lambda = 0", "lambda = 5", "lambda = 20", "lambda = 50"),
		col = 1:4, lwd = 3, bty = "n", cex = 1.2)


# slide 14: Effects of penalization
par(mar=c(4,4,1,1), cex.lab = 1)
betamat = cbind (beta1, beta2, beta3, beta4, beta5)[-1,]
plot (0,0, xlim = c(1,5), ylim = range (betamat), type = "n", xaxt ="n", bty="n", xlab = "", ylab = "Spline Regression Coefficient")

# Plot showing impact of penalty on betas.
# Notice shrinkage is not strictly proportional to size of coefficient (lines cross)

for (i in 1:nrow(betamat)){
 lines (betamat[i,]~c(1:5), col = "grey")
}
for (i in 1:nrow(betamat)){
 lines (betamat[i,]~c(1:5), col = c(2:6), type = "p", pch = 16)
}
abline (h = 0, lwd = 4)
legend ("bottomright", 
		legend = c("lambda = 0", "lambda = 5", "lambda = 20", "lambda = 50", "lambda = 100") ,
		col = c(2:6), pch =16, cex = 1, bty = "n" )
		




###Search Lambda
results = NULL
lambdalist = seq(0, 5000, 50)
for (l in lambdalist ){ print (l)
	beta = solve (t(X)%*%X + l*B) %*% t(X) %*% Y
	H = X %*% solve (t(X)%*%X + l*B) %*% t(X) 		##Hat matrix
	Yhat = X%*%beta                                       ##Fitted values 
	GCV = mean ( (Y-Yhat)^2 ) / (1- mean (diag(H)))^2
	C = t(beta)%*%B%*%beta
	effdf = sum (diag (H))
	results = rbind (results, c(l, GCV, C, effdf) )
}

# Effective DF and lambda: 
plot (results[,4]~results[,1], type = "l", xlab = expression(paste(lambda)), ylab = 'DF_eff', lwd = 3)
title (main = "Effective DF")

# look at value that minimizes GCV:
results[which.min(results[,2]),]
min.lambda = results[which.min(results[,2]),1]

par (mfrow = c(1,3))
plot (results[,2]~results[,1], type = "l", xlab = expression(paste(lambda)), ylab = "GCV", lwd = 3)
title (main = "GCV", cex.main = 1.5)
abline (v = results[which.min(results[,2]),1], col = 2, lwd = 3,lty = 3)

plot (results[,3]~results[,1], type = "l", xlab = expression(paste(lambda)), ylab = "GCV", lwd = 3)
title (main = "b'Bb", cex.main = 1.5)
abline (v = results[which.min(results[,2]),1], col = 2, lwd = 3,lty = 3)

plot (results[,4]~results[,1], type = "l", xlab = expression(paste(lambda)), ylab = 'DF_eff', lwd = 3)
title (main = "Effective DF", cex.main = 1.5)
abline (v = results[which.min(results[,2]),1], col = 2, lwd = 3,lty = 3)


### Confidence/Prediction Interval
beta = solve (t(X)%*%X + min.lambda*B) %*% t(X) %*% Y
S = X %*% solve (t(X)%*%X + min.lambda*B) %*% t(X) 		##Hat matrix
Yhat = X%*%beta   
df = sum (diag (S))

n = length (Y) 
sigma1 = sqrt ( sum ( (Y-Yhat)^2 ) / ( n - df) )

a = proc.time()
sigma2 = sqrt ( sum ( (Y-Yhat)^2 ) / ( n - 2*df + sum (diag( S%*%t(S)))  ) )
proc.time() - a

# computational shortcut:
a = proc.time()
biascorrection = 0
for (j in 1:nrow(S)) {
  biascorrection = biascorrection + t(S[,j])%*%S[,j]
}
sigma2_faster = sqrt ( sum ( (Y-Yhat)^2 ) / ( n - 2*df + biascorrection))
proc.time() - a

sigma2==sigma2_faster
sigma1; sigma2

vcov = sigma1^2 * solve (t(X)%*%X + min.lambda*B) %*% t(X) %*% X %*% solve (t(X)%*%X + min.lambda*B)


### Pointwise-CI of the "Temperature Effect"
pred.vcov = X %*% vcov %*% t(X)
Upper95.ci = Yhat  + 1.96* sqrt(diag (pred.vcov)) 
Lower95.ci = Yhat  - 1.96* sqrt (diag (pred.vcov))
par (mar = c(4,4,1,1) )
plot(Yhat~health$Temp, xlab = "Temperature", ylab = "Temperature Effect", 
     ylim =c(min(Lower95.ci), max(Upper95.ci)), type = "l", lwd = 3)
lines (Upper95.ci~health$Temp, type = "l", lwd = 3, col = 2, lty = 3)
lines (Lower95.ci~health$Temp, type = "l", lwd = 3, col = 2, lty =3)
abline(h = 0, col = "grey", lwd = 2)
legend ("topright", bty="n", legend = c("Temp Effect", "95% CI"), col=c(1,2),lwd=3,lty=c(1,3), cex = 1)


### Pointwise-Prediction Intervals

Upper95 = Yhat  + 1.96* (sigma1 + sqrt(diag (pred.vcov)) )
Lower95 = Yhat  - 1.96* (sigma1 + sqrt (diag (pred.vcov)) )
par (mar = c(4,4,1,1) )
plot(Y~health$Temp, xlab = "Temperature", ylab = "Death count", 
     ylim =c(min(Lower95), max(Upper95)), col='grey',type = "p")
lines (Yhat~health$Temp, col = "black", type = "l",lwd=3)
lines (Upper95~health$Temp, type = "l", lwd = 3, col = 2, lty = 3)
lines (Lower95~health$Temp, type = "l", lwd = 3, col = 2, lty =3)
abline(h = 0, col = "grey", lwd = 2)
legend ("topright", bty="n", legend = c("Fitted values", "95% prediction interval"), col=c(1,2),lwd=3,lty=c(1,3))





## Compare to unpenalized:
lambda = 0
beta = solve (t(X)%*%X + lambda*B) %*% t(X) %*% Y
S = X %*% solve (t(X)%*%X + lambda*B) %*% t(X) 		##Hat matrix
Yhat = X%*%beta   
df = sum (diag (S))

n = length (Y) 
sigma1 = sqrt ( sum ( (Y-Yhat)^2 ) / ( n - df) )
vcov = sigma1^2 * solve (t(X)%*%X + lambda*B) %*% t(X) %*% X %*% solve (t(X)%*%X + lambda*B)

pred.vcov = X %*% vcov %*% t(X)
Upper95 = Yhat  + 1.96* sqrt(diag (pred.vcov)) 
Lower95 = Yhat  - 1.96* sqrt (diag (pred.vcov))
par (mar = c(4,4,1,1) )
plot(Yhat~health$Temp, xlab = "Temperature", ylab = "Temperature Effect", 
     ylim =c(min(Lower95), max(Upper95)), type = "l", lwd = 3)
lines (Upper95~health$Temp, type = "l", lwd = 3, col = 2, lty = 3)
lines (Lower95~health$Temp, type = "l", lwd = 3, col = 2, lty =3)
abline(h = 0, col = "grey", lwd = 2)
legend ("topright", bty="n", legend = c("Temperature Effect", "95% CI"), col=c(1,2),lwd=3,lty=c(1,3), cex = 1.3)


### Confidence/Prediction Interval
Y = health$alldeaths
knots = seq(8.5, 92.5, length.out = 40+2)
knots = knots[c(2:(length(knots)-1))]
X = cbind(1, bs (health$Temp, knots = knots, degree = 3) )
B = cbind (0, rbind ( 0, diag(43)) )

lambda = 2.5
beta = solve (t(X)%*%X + lambda*B) %*% t(X) %*% Y
S = X %*% solve (t(X)%*%X + lambda*B) %*% t(X) 		##Hat matrix
Yhat = X%*%beta   
df = sum (diag (S))

n = length (Y) 
sigma1 = sqrt ( sum ( (Y-Yhat)^2 ) / ( n - df) )
sigma2 = sqrt ( sum ( (Y-Yhat)^2 ) / ( n - 2*df + sum (diag( S%*%t(S)))  ) )

sigma1; sigma2

vcov = sigma1^2 * solve (t(X)%*%X + lambda*B) %*% t(X) %*% X %*% solve (t(X)%*%X + lambda*B)

X.pred = cbind(1, bs ( seq(8,93, .2), knots = knots, degree = 3) )

fitted = X.pred%*%beta
TempEffect = X.pred[,-1]%*%beta[-1] #Drop the intercept

pred.vcov = X.pred[,-1] %*% vcov[-1,-1] %*% t(X.pred[,-1])
Upper95 = TempEffect  + 1.96* sqrt(diag (pred.vcov)) 
Lower95 = TempEffect  - 1.96* sqrt (diag (pred.vcov))
par (mar = c(4,4,1,1) )
plot(TempEffect~seq(8,93, .2), xlab = "Temperature", ylab = "Temperature Effect", 
               ylim =c(min(Lower95), max(Upper95)), type = "l", lwd = 3)
lines (Upper95~seq(8,93, .2), type = "l", lwd = 3, col = 2, lty = 3)
lines (Lower95~seq(8,93, .2), type = "l", lwd = 3, col = 2, lty =3)
abline(h = 0, col = "grey", lwd = 2)
legend ("topright", bty="n", legend = c("Temperature Effect", "95% CI"), col=c(1,2),lwd=3,lty=c(1,3), cex = 1.3)

#######################
# Introducing mgcv
#### GAM fit
library (mgcv)
fit1 = gam(alldeaths~s(Temp), data= health)
summary(fit1)
plot(fit1)

names(fit1$smooth[[1]])

  # you can check the defaults are equivalent to this:
  fit.checkdefault = gam(alldeaths~s(Temp,bs='tp',k=10),method="GCV.Cp", data= health)
  summary(fit.checkdefault)

par(mfrow=c(2,2))
gam.check(fit1)
#p-value is rough guide, suggests adequate. Here, we see edf is notably less than k', also suggest adequate.

fit.checkcubic = gam(alldeaths~s(Temp,bs='cr',k=10),method='GCV.Cp',data=health)
plot(fit.checkcubic)
# results from thin plate regression splines are very similar to cubic splines

fit2= gam (alldeaths~s(Temp, k = 40), data = health)
summary (fit2)
#  similar to k=10. Note EDF only increased by 0.2.

AIC(fit1)
AIC(fit2)
# equivocal evidence -- can use either.

## Fit model using mixed model smoothing
fit.reml = gam(alldeaths~s(Temp,bs='tp',k=10),method="REML", data= health)
summary(fit.reml)
plot(fit.reml)

# compare to previous:
summary(fit1)
# note edf is lower in REML than GCV

# Check slope at particular values:
# What is the slope at 40 degrees?
newd <- health[1, ] # grab any row; we are going to change temperature only
newd$Temp <- 40 - 1e-05 # subtract some small number
y1 <- predict(fit.reml, newd)
newd$Temp <- 40 + 1e-05 # add some small number
y2 <- predict(fit.reml, newd)
(y2 - y1)/2e-05
# visually check whether this is consistent with the plot

newd <- health[1, ] # grab any row; we are going to change temperature only
newd$Temp <- 90 - 1e-05 # subtract some small number
y1 <- predict(fit.reml, newd)
newd$Temp <- 90 + 1e-05 # add some small number
y2 <- predict(fit.reml, newd)
(y2 - y1)/2e-05
# visually check whether this is consistent with the plot



##### GAMM (here, Gaussian)
nepal = read.csv('~/Dropbox/EmoryCourses/BIOS_526/Materials_BRisk_2020/Data/Nepal.csv')
library(lme4)

nepal$arm[ nepal$arm == 99.9] = NA
nepal$arm[ nepal$arm == 88.8] = NA
nepal = subset (nepal, !is.na(arm))
nepal$id = as.factor(nepal$id)
nepal$ageC = nepal$age - 36
nepal$sex = as.factor(nepal$sex)

# Recall the model we fit in module 2:
fit = lmer (arm~ age + (1|id), data = nepal)
summary(fit)

# Now fit the model with possibly non-linear effects of age:
fit.gamm = gam(arm~s(age)+s(id,bs = 're'),data=nepal)
gam.check(fit.gamm)
summary(fit.gamm)


AIC(fit)
AIC(fit.gamm) # much better fit
plot(fit.gamm)


library(itsadug)
plot_smooth(fit.gamm,view='age',rm.ranef=TRUE)
# I like how this plot includes the intercept.


# plot the first few random effects alongside the mean trend:
myylim=c(9,18)
plot_smooth(fit.gamm.reml,view='age',cond=list(id=10),col='orange',ylim=myylim)
plot_smooth(fit.gamm.reml,view='age',cond=list(id=40),col='red',add=TRUE,ylim=myylim)
plot_smooth(fit.gamm.reml,view='age',cond=list(id=120),col='purple',add=TRUE,ylim=myylim)
plot_smooth(fit.gamm.reml,view='age',cond=list(id=50),col='turquoise',add=TRUE,ylim=myylim)
