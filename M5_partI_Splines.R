# Code supporting M5: Parameteric Splines and Cross Validation
load ("Data/GAbirth.RData")
library(splines)
str (dat)

# Slide 4
boxplot(dat$bw~dat$gw, xlab = "Gestational Week", ylab = "Birth weight")

# Slide 5
fit = lm (bw~gw, data = dat)
summary(fit)
plot (dat$bw~dat$gw, xlab = "Gestational Week", ylab = "Birth weight")
abline(fit, col=2, lwd = 3)

# Slide 6
par (mfrow = c(2,2)); plot (fit)

# Slide 10
# ordering by gw facilitates plotting:
dat = dat[order (dat$gw),]
dev.off() # clears the plotting environment
fit = lm (bw~gw, data = dat)
fit2 = lm (bw~gw+I(gw^2), data = dat)
fit3 = lm (bw~gw+I(gw^2) + I(gw^3), data = dat)
plot (dat$bw~dat$gw, xlab = "Gestational Week", ylab = "Birth weight")
lines (fit$fitted~dat$gw, col = 2, lwd = 3)
lines (fit2$fitted~dat$gw, col = 4, lwd = 3)
lines (fit3$fitted~dat$gw, col = 5, lwd = 3)
legend ("topleft", legend=c("Linear", "Quadratic", "Cubic"), col=c(2,4,5), lwd=3) 


# Slide 11
plot (dat$bw~dat$gw, xlab = "Gestational Week", ylab = "Birth weight")
abline (v=c(36.5, 41.5), col = 4, lwd = 3)


#Slide 13
Ind1 = dat$gw >= 37 & dat$gw <= 41
Ind2 = dat$gw >= 42

fit4 = lm (bw~gw*(Ind1+ Ind2), data = dat)

#the syntax I(gw^2) can be used to calculate gw^2 within lm:
# also note use of parentheses to enumerate all terms
fit5 = lm (bw~(gw+I(gw^2))*(Ind1+ Ind2), data = dat)
summary(fit5)

dontdothis = lm(bw~(gw+(gw^2))*(Ind1+ Ind2), data = dat)
summary(dontdothis)

fit6 = lm (bw~(gw+I(gw^2) + I(gw^3))*(Ind1+ Ind2), data = dat)

par (mar = c(4,4,1,1))
plot (dat$bw~dat$gw, xlab = "Gestational Week", ylab = "Birth weight")
abline (v=c(36.5, 41.5), col = 4, lwd = 3)
lines (fit4$fitted[Ind1]~dat$gw[Ind1], col = 2, lwd = 3)
lines (fit4$fitted[Ind2]~dat$gw[Ind2], col = 2, lwd = 3)
lines (fit4$fitted[!Ind1 & !Ind2]~dat$gw[!Ind1 & !Ind2], col = 2, lwd = 3)
lines (fit5$fitted[Ind1]~dat$gw[Ind1], col = 4, lwd = 3)
lines (fit5$fitted[Ind2]~dat$gw[Ind2], col = 4, lwd = 3)
lines (fit5$fitted[!Ind1 & !Ind2]~dat$gw[!Ind1 & !Ind2], col = 4, lwd = 3)
lines (fit6$fitted[Ind1]~dat$gw[Ind1], col = 5, lwd = 3)
lines (fit6$fitted[Ind2]~dat$gw[Ind2], col = 5, lwd = 3)
lines (fit6$fitted[!Ind1 & !Ind2]~dat$gw[!Ind1 & !Ind2], col = 5, lwd = 3)
legend ("topleft", legend=c("Linear", "Quadratic", "Cubic"), col=c(2,4,5), lwd=3) 


#Slide 14
#Piecewise splines (continuous)
Sp1 = (dat$gw - 36.5)*as.numeric(dat$gw >= 36.5 )
Sp2 = (dat$gw - 41.5)*as.numeric(dat$gw >= 41.5)
fit7 = lm (bw~ gw+Sp1 + Sp2, data = dat)
plot(dat$gw~dat$gw,type='l')
lines(Sp1~dat$gw,type='l',col='red')
lines(Sp2~dat$gw,type='l',col='blue')

morepoints = seq(26, 44, .1)
newdata = data.frame(gw=morepoints, Sp1= (morepoints-36.5), Sp2=(morepoints-41.5) )
# non-zero for values greater than 36.5 and greater than 41.5 only:
newdata[ newdata <0] = 0

plot(newdata[,1],newdata[,1],type='l',xlim=c(20,45),ylim=c(0,45))
lines(newdata[,2]~newdata[,1],type='l')
lines(newdata[,3]~newdata[,1],type='l')


plot (dat$bw~dat$gw, xlab = "Gestational Week", ylab = "Birth weight")
lines (fit4$fitted[Ind1]~dat$gw[Ind1], col = 2, lwd = 3)
lines (fit4$fitted[Ind2]~dat$gw[Ind2], col = 2, lwd = 3)
lines (fit4$fitted[!Ind1 & !Ind2]~dat$gw[!Ind1 & !Ind2], col = 2, lwd = 3)

lines (predict (fit7, newdata=newdata)~morepoints, col = 4, lwd = 3)
abline (v=c(36.5, 41.5), col = "grey", lwd = 3)
legend ("topleft", legend=c("Piecewise linear", "Linear splines"), col=c(2,4), lwd=3)

#Slide 17
# Coefficient interpretation
summary (fit7)

selectregion2 = c(0,1,1,0)
(slope_region2 = coef(fit7)%*%selectregion2)
vcovpl = vcov(fit7)
se_region2 = sqrt(t(selectregion2)%*%vcovpl%*%selectregion2)
slope_region2-2*se_region2
slope_region2+2*se_region2

selectregion3 = c(0,1,1,1)
(slope_region3 = coef(fit7)%*%selectregion3)
se_region3 = sqrt(t(selectregion3)%*%vcovpl%*%selectregion3)
slope_region3-2*se_region3
slope_region3+2*se_region3


# Slide 19
#Fig 8: cubic (polynomial) splines
# review on 4 november:
Sp1 = (dat$gw - 36.5)^3*as.numeric(dat$gw >= 36.5 )
Sp2 = (dat$gw - 41.5)^3*as.numeric(dat$gw >= 41.5)
fit8 = lm (bw~ gw+I(gw^2) + I(gw^3) + Sp1 + Sp2, data = dat)
# note: I() is necessary
summary(fit8)
# linear splines are interpretable, but cubic splines more confusing



# look at cubic splines:
# additional material not in slides:
# --->
dmat = model.matrix(fit8)
head(dmat)
plot(dmat[,4]~dat$gw,type='l',ylim=c(0,100000))
lines(dmat[,2]~dat$gw,col='blue')
lines(dmat[,3]~dat$gw,col='red')
lines(dmat[,5]~dat$gw,col='orange')
lines(dmat[,6]~dat$gw,col='purple')
# you can see how the numbers get very large,
# which can sometimes lead to numerical problems.

plot(dat$gw,dmat[,5],type='l')
lines(dat$gw,dmat[,6],type='l',col='red')
##<-------


# code for slide 19:
newdata1 = data.frame(gw=morepoints, Sp1= (morepoints-36.5), Sp2=(morepoints-41.5) )
newdata1[ newdata1 <0] = 0
newdata2 = data.frame(gw=morepoints, Sp1= (morepoints-36.5)^3, Sp2=(morepoints-41.5)^3 )
newdata2[ newdata2<0] = 0

plot (dat$bw~dat$gw, xlab = "Gestational Week", ylab = "Birth weight")
lines (predict (fit7, newdata=newdata)~morepoints, col = 4, lwd = 3)
abline (v=c(36.5, 41.5), col = "grey", lwd = 3)
lines (predict (fit7, newdata=newdata1)~morepoints, col = 4, lwd = 3)
lines (predict (fit8, newdata=newdata2)~morepoints, col = 2, lwd = 3)
legend ("topleft", legend=c("Linear splines", "Cubic splines"), col=c(4,2), lwd=3)

# slide 23
### Linear bsplines
fit9 = lm ( bw ~ bs(gw, knots = c(36.5, 41.5), degree = 1),  data = dat)

### Quadratic bsplines
fit10 = lm ( bw ~ bs(gw, knots = c(36.5, 41.5), degree = 2),  data = dat)

### Cubic bsplines
# slide 25
fit11 = lm ( bw ~ bs(gw, knots = c(36.5, 41.5), degree = 3),  data = dat)
# don't interpret coefficients
summary(fit11)

## bsplines are scaled between 0 and 1
## Their precise definition is complicated...
xmat = bs(morepoints, knots=c(36.5,41.5),degree = 1)
head(xmat)
tail(xmat)
summary(xmat)

# look at bsplines:
pdf(file = 'Figure_linearbsplines.pdf')
plot(xmat[,1]~morepoints,type='l',ylim=c(0,1))
lines(xmat[,2]~morepoints,type='l',col=2)
lines(xmat[,3]~morepoints,type='l',col=3)
dev.off()


xmat = bs(morepoints, knots=c(36.5,41.5),degree = 3)
#pdf('Figure_cubicbsplines.pdf')
plot(xmat[,1]~morepoints,type='l',ylim=c(0,1))
lines(xmat[,2]~morepoints,type='l',col=2)
lines(xmat[,3]~morepoints,type='l',col=3)
lines(xmat[,4]~morepoints,type='l',col=4)
lines(xmat[,5]~morepoints,type='l',col=5)
#dev.off()
cbind(morepoints,apply(xmat,1,sum))

#slide  26
plot (dat$bw~dat$gw, xlab = "Gestational Week", ylab = "Birth weight", col = "grey")
abline (v = c(36.5, 41.5), lty = 3, lwd = 2)
lines(fit9$fitted[Ind1]~dat$gw[Ind1], col=1, lwd=3)
lines(fit9$fitted[Ind2]~dat$gw[Ind2], col=1, lwd=3)
lines(fit9$fitted[!Ind1 & !Ind2]~dat$gw[!Ind1 & !Ind2], col=1, lwd=3)
lines (fit10$fitted~dat$gw, col = 4, lwd = 3)
lines (fit11$fitted~dat$gw, col = 5, lwd = 3)
legend ("topleft", legend=c("Linear splines", "Quadratic splines", "Cubic splines"), col=c(1,4, 5), lwd=3)




# the predictions with polynomials and b-splines are essentially equivalent:
sum((fit7$fitted-fit9$fitted)^2) # linear piecewise spline minus degree 1 (i.e., linear) b-spline
sum((fit8$fitted-fit11$fitted)^2) # cubic polynomial spline minus cubic b-spline




####################
###Mortality 
load ("NYC.RData")

plot (health$alldeaths~health$date, xlab = "Date", ylab = "Death")


fit1 = lm (alldeaths~bs (date, df = 10, degree = 1), data = health)

fit2 = lm (alldeaths~bs (date, df = 10, degree = 2), data = health)
fit3 = lm (alldeaths~bs (date, df = 10, degree = 3), data = health)


# NOTE: To see where knots are
temp = bs(health$date,df=10,degree=1) # allows 9 knots; bs does not create column of ones... 
bs(health$date,df=10,degree=2) # allows 8 knots
bs(health$date,df=10,degree=3) # allows 7 knots




plot (health$alldeaths~health$date, col = "grey", xlab = "Date", ylab = "Death")
lines (fit1$fitted~health$date, col = 2, lwd = 3)
lines (fit2$fitted~health$date, col = 4, lwd = 3)
lines (fit3$fitted~health$date, col = 5, lwd = 3)
legend ("topleft", legend=c("Linear splines", "Quadratic splines", "Cubic splines"), col=c(2,4, 5), lwd=3, cex = 1.3, bty= "n")


 
 # Slide 27:
fit1 = lm (alldeaths~bs (date, df = 20, degree = 1), data = health)
fit2 = lm (alldeaths~bs (date, df = 20, degree = 2), data = health)
fit3 = lm (alldeaths~bs (date, df = 20, degree = 3), data = health)

plot (health$alldeaths~health$date, col = "grey", xlab = "Date", ylab = "Death")
lines (fit1$fitted~health$date, col = 2, lwd = 3)
lines (fit2$fitted~health$date, col = 4, lwd = 3)
lines (fit3$fitted~health$date, col = 5, lwd = 3)
 legend ("topleft", legend=c("Linear splines", "Quadratic splines", "Cubic splines"), col=c(2,4, 5), lwd=3, cex = 1.3, bty= "n")


## Slide 32
### Knots
fit1 = lm (alldeaths~bs (date, df = 10), data = health)
fit2 = lm (alldeaths~bs (date, df = 20), data = health)
fit3 = lm (alldeaths~bs (date, df = 30), data = health)
fit4 = lm (alldeaths~bs (date, df = 50), data = health)


plot (health$alldeaths~health$date, col = "grey", xlab = "Date", ylab = "Death")
lines (fit1$fitted~health$date, col = 2, lwd = 3)
lines (fit2$fitted~health$date, col = 4, lwd = 3)
lines (fit3$fitted~health$date, col = 5, lwd = 3)
lines (fit4$fitted~health$date, col = 6, lwd = 3)
 legend ("topleft", legend=c("10 df", "20 df", "30 df", "50 df"), col=c(2,4, 5, 6), lwd=3, cex = 1.3, bty= "n")


## Slide 43
### AIC, CV, and GCV calculation examples:
fit = lm (alldeaths~bs (date, df = 10), data = health)

##AIC 
AIC (fit)

##GCV/CV
H =  hatvalues (fit)
CV = mean(  (fit$fitted - health$alldeaths)^2 / (1-H)^2 )
CV

GCV = mean ((fit$fitted - health$alldeaths)^2) / (1 - mean(H))^2
GCV



## Slide 44
#### Loop for Mortality
## Loop across different degrees of freedom
results = NULL
for (d in seq(5,200, by = 5) ){
	print (d)
	fit = lm (alldeaths~bs (date, df = d), data = health)
	H =  hatvalues (fit)
	results = rbind (results, 
			c(d,  	AIC (fit), 
			mean(  (fit$fitted - health$alldeaths)^2 / (1-H)^2 ),
			mean ((fit$fitted - health$alldeaths)^2) / (1 - mean(H))^2 ))
}

par (mfrow = c(1,3))
plot (results[,2]~results[,1], xlab = "# Knots", ylab = "AIC", main = "AIC")
plot (results[,3]~results[,1], xlab = "# Knots", ylab = "CV", main = "CV")
plot (results[,4]~results[,1], xlab = "# Knots", ylab = "GCV", main = "GCV")


results[which.min(results[,2]),1]
results[which.min(results[,3]),1]
results[which.min(results[,4]),1]
# here they agree

fit1 =  lm (alldeaths~bs (date, results[ which.min(results[,2]), 1]), data = health)
fit2 =  lm (alldeaths~bs (date, results[ which.min(results[,3]), 1]), data = health)
fit3 =  lm (alldeaths~bs (date, results[ which.min(results[,4]), 1]), data = health)

par (mfrow = c(1,1), mar = c(4, 4, 1, 1) )
plot (health$alldeaths~health$date, col = "grey", xlab = "Date", ylab = "Death")
lines (fit1$fitted~health$date, col = 2, lwd = 2)
lines (fit2$fitted~health$date, col = 4, lwd = 3)
lines (fit3$fitted~health$date, col = 5, lwd = 3)

## Slide 45
#### Loop for Birth Weight and Pregnancy length
## Here, we will look at WHERE to put one know

resultsLinear = resultsCubic = NULL
loc = seq(27, 43, by = 1)

for (loc.i in loc){ print (loc.i)

	Sp1 = (dat$gw - loc.i)*as.numeric(dat$gw >= loc.i)
	Sp2 = (dat$gw - loc.i)^3*as.numeric(dat$gw >= loc.i)
	fit1 = lm (bw~ gw+ Sp1, data = dat)
	fit2 = lm (bw~ gw+I(gw^2) + I(gw^3) + Sp2, data = dat)

	H =  hatvalues (fit1)
	resultsLinear = rbind(resultsLinear, 
			c(loc.i,  	AIC (fit1), 
			mean(  (fit1$fitted - dat$bw)^2 / (1-H)^2 ),
			mean ((fit1$fitted - dat$bw)^2) / (1 - mean(H))^2 ))

	H =  hatvalues (fit2)
	resultsCubic = rbind(resultsCubic, 
			c(loc.i,  	AIC (fit2), 
			mean(  (fit2$fitted - dat$bw)^2 / (1-H)^2 ),
			mean ((fit2$fitted - dat$bw)^2) / (1 - mean(H))^2 ))

}

par (mfrow = c(2,3), pch = 16)
plot (resultsLinear[,2]~resultsLinear[,1], xlab = "Knot Point", ylab = "AIC", main = "Linear AIC")
plot ((resultsLinear[,3])~resultsLinear[,1], xlab = "Knot Point", ylab = "CV", main = "Linear CV")
plot ((resultsLinear[,4])~resultsLinear[,1], xlab = "Knot Point", ylab = "GCV", main = "Linear GCV")

plot (resultsCubic[,2]~resultsCubic[,1], xlab = "Knot Point", ylab = "AIC", main = "Cubic AIC")
plot ((resultsCubic[,3])~resultsCubic[,1], xlab = "Knot Point", ylab = "CV", main = "Cubic CV")
plot ((resultsCubic[,4])~resultsCubic[,1], xlab = "Knot Point", ylab = "GCV", main = "Cubic GCV")

# Slide 46
# print out the minima:
(loc.i.cubic = resultsLinear[which.min(resultsLinear[,2]),1])
(loc.i.linear = resultsLinear[which.min(resultsCubic[,2]),1])


# Compare their AICs:
min(resultsLinear[,2])
min(resultsCubic[,2])
# Cubic has much lower AIC.
# Rule of thumb: More the 2 implies substantially better model fit. 


Sp1 = (dat$gw - loc.i.linear)*as.numeric(dat$gw >= loc.i.linear)
Sp2 = (dat$gw - loc.i.cubic)^3*as.numeric(dat$gw >= loc.i.cubic)
fit1 = lm (bw~ gw+ Sp1, data = dat)
fit2 = lm (bw~ gw+I(gw^2) + I(gw^3) + Sp2, data = dat)

morepoints = seq(26, 44, 0.1)
newdata1 = data.frame(gw=morepoints, Sp1= (morepoints-loc.i.linear) )
newdata1[ newdata1 <0] = 0
newdata2 = data.frame(gw=morepoints, Sp2= (morepoints-loc.i.cubic)^3 )
newdata2[ newdata2<0] = 0


plot(bw~gw,data=dat)
lines(predict(fit1,newdata1)~morepoints,col=2,lwd=3)
lines(predict(fit2,newdata2)~morepoints,col=4,lwd=3)


