# GAMS and bivariate splines
library (mgcv)

# Pre-term births with non-linear age effect
load('Data/PTB.RData')

fit = glm(ptb~age + male+tobacco, data = dat, family = binomial(link='logit'))
summary(fit)
par(mfrow=c(2,2))
plot(fit)


# inspect linearity of age:
ptb.age = tapply(dat$ptb, dat$age, mean)

plot(ptb.age~c(16:43), xlab="Maternal Age at Delivery", ylab="% of Preterm Birth", pch=16)
plot(log(ptb.age/(1-ptb.age))~c(16:43), xlab="Maternal Age at Delivery", ylab="Log odds of Preterm Birth", pch=16)

fit.gam = gam(ptb~s(age)+male+tobacco,family=binomial,data=dat)
plot(fit.gam)

gam.check(fit.gam)
# look at the printed output; default k is adequate
summary(fit.gam)
plot(fit.gam, shade = TRUE, seWithMean = TRUE, pch = 16, cex = 0.5)



fit.gam.reml = gam(ptb~s(age)+male+tobacco,family=binomial,method='REML',data=dat)
summary(fit.gam.reml)
plot(fit.gam.reml)
# similar, although surprisingly the edf actually went up; usually REML smooths more. 


#####################################
## Pm2.5 & Mortality Analysis in NYC


## Load Data ##
load('Data/NYC.RData')
health$date2=order(health$date)
str(health)
#dow: day of the week
#DpTemp: dew point temp
#rmTemp: running mean: moving average of current and two previous days

###
# create lag variable of pm25: used later on
health$pm25.lag1 = c(NA, health$pm25[1:1825])

# Explore data
##Plot
par (mfrow = c(3,1), mar = c(2,4,1,1),cex.lab = 1.4, cex.axis = 1.4)
plot (health$cr65plus~health$date,xlab = "Date", ylab = "Death", col = "grey")
legend ("topleft", legend=c("Daily Non-accidental Mortality Counts"), bty="n", text.font=2, cex =1.5)

plot (health$pm25~health$date,xlab = "Date", ylab = "PM2.5", col = "grey")
legend ("topleft", legend=c("Daily PM2.5 Concentrations (ug/m3)"), bty="n", text.font=2, cex =1.5)

plot (health$Temp~health$date,xlab = "Date", ylab = "Temperature", col = "grey")
legend ("topleft", legend=c("Daily Temperature (C)"), bty="n", text.font=2, cex =1.5)


### Unadjusted effects
fit1 = gam(log(cr65plus)~s(Temp), data = health)
summary(fit1)
par(mfrow=c(2,2))
gam.check(fit1) # use gam.check to get residuals versus fitted

fit.Temp = gam(cr65plus~s(Temp), data = health)
gam.check(fit.Temp)

# Additional info not in lecture notes:
smooth = fit1$smooth[[1]]
# look at design matrix:
xmat = predict(fit1,type='lpmatrix')
dim(xmat)
head(xmat)
plot(xmat[,1]~health$Temp,ylim=c(-2,2),pch='.')
points(xmat[,2]~health$Temp,pch='.')
points(xmat[,3]~health$Temp,pch='.')
points(xmat[,10]~health$Temp,pch='.')


# by default, mgcv:gam uses a lower-rank spline
# p-value is significant if there is a lack of fit, i.e., if 
# k is too small. For additional details, see ?gam.check()
fit1 = gam(log(cr65plus)~s(Temp,k=75), data = health)
gam.check(fit1)
summary(fit1)

fit2 = gam(log(cr65plus)~s(DpTemp), data = health)
fit3 = gam(log(cr65plus)~s(rmTemp), data = health)
fit4 = gam(log(cr65plus)~s(rmDpTemp), data = health)
par (mfrow = c(2,2))
plot (fit1, rug=T, main = "Same-day Temperature", shade=T);abline(h=0)
plot(fit2, rug=T, main = "Same-day Dew-point Temperature", shade=T);abline(h=0)
plot(fit3, rug=T, main = "Lagged Average Temperature", shade=T );abline(h=0)
plot (fit4, rug=T, main = "Lagged Average Dew-point Temperature", shade=T  );abline(h=0)

### Joint effects
fit = gam(log(cr65plus)~s(Temp) + s(DpTemp) + s(rmTemp) + s(rmDpTemp), data = health)
par (mfrow =c(2,2))
plot (fit, select=1, rug =T, main = "Same-day Temperature", shade=T);abline(h=0)
plot(fit,select=2,  rug=T, main = "Same-day Dew-point Temperature", shade=T);abline(h=0)
plot(fit, select=3, rug=T, main = "Lagged Average Temperature", shade=T );abline(h=0)
plot (fit, select=4, rug=T, main = "Lagged Average Dew-point Temperature", shade=T  );abline(h=0)

# slide 19
summary(fit)

# variables are highly correlated:
cor (health[c("Temp", "DpTemp", "rmTemp", "rmDpTemp")])
# you can look at variance inflation factors with an lm result object:
temp=lm(log(cr65plus)~Temp + DpTemp + rmTemp + rmDpTemp,data=health)
car::vif(temp)


### Temporal trends
fit = gam(log(cr65plus)~s(date2), data=health)
summary(fit)
plot(fit, rug=F, main = "Temporal Trends", shade = T)

# slide 21
gam.check(fit)

## WARNING:
# this indicates a lack of fit.
# Need to specify more knots


fit1 = gam(log(cr65plus)~s(date2, k = 20), data=health)
summary(fit1)

fit2 = gam(log(cr65plus)~s(date2, k = 50), data=health)
fit3 = gam(log(cr65plus)~s(date2, k = 100), data=health)
fit4 = gam(log(cr65plus)~s(date2, k = 200), data=health)


par (mfrow = c(2,2))
plot (fit1, rug =F, main = "k = 20; edf = 17.65", shade=T  );abline(h=0)
plot (fit2, rug =F, main = "k = 50; edf = 42.19", shade=T  );abline(h=0)
plot (fit3, rug =F, main = "k = 100; edf = 67.64", shade=T  );abline(h=0)
plot (fit4, rug =F, main = "k = 200; edf = 79.77", shade=T  );abline(h=0)


gam.check(fit1)
gam.check(fit2)
gam.check(fit3)
gam.check(fit4)

fit5 = gam(log(cr65plus)~s(date2, k = 400), data=health)
gam.check(fit5)


# slide 25
fit = gam(log(cr65plus)~s(date2, k=80, fx = TRUE), data=health)
summary(fit)
par(mfrow=c(1,1))
plot (fit, rug =F, main = "k = 80; df = 79", shade=T  );abline(h=0)
# fx: fixed d.f. regression spline
# similar to k=200 with penalty

### Full model
health$fdow = factor(health$dow)
health$fdow = relevel (health$fdow, ref = "Sunday")

# use a more intuitive reference level:
fit = gam(log(cr65plus)~pm25+fdow+s(date2, k = 100 )+s(Temp)+ s(DpTemp)+s(rmTemp)+s(rmDpTemp), data = health)
summary(fit)
gam.check(fit)
cor(health[,c("pm25","date2","Temp","DpTemp","rmTemp","rmDpTemp")])
temp=lm(log(cr65plus)~pm25+fdow+date2+Temp+DpTemp+rmTemp+rmDpTemp, data = health)
car::vif(temp)
# caution-- issue with collinearity. Note pm25 is okay, which is the focus here.


## check non-linear effect of pm25:
fit.sm.pm25 = gam(log(cr65plus)~s(pm25)+fdow+s(date2, k = 100 )+s(Temp)+ s(DpTemp)+s(rmTemp)+s(rmDpTemp), data = health)
summary(fit.sm.pm25)
dev.off()
plot(fit.sm.pm25,select=1)


#############
# what about a lag effect?
fit = gam(log(cr65plus)~pm25.lag1+fdow+s(date2, k = 100 )+s(Temp)+ s(DpTemp)+s(rmTemp)+s(rmDpTemp), data = health)
summary(fit)

fit = gam(log(cr65plus)~s(pm25.lag1)+fdow+s(date2, k = 100 )+s(Temp)+ s(DpTemp)+s(rmTemp)+s(rmDpTemp), data = health)
summary(fit)

#pdf(file='PM2pt5_gamcheck.pdf')
par(mfrow=c(2,2))
gam.check(fit)
#dev.off()

#pdf(file='PM2pt5_plotsOfSmooths.pdf')
par(mfrow=c(3,2))
plot(fit)
#dev.off()


library(corrplot)
#pdf(file='PM2pt5AndTemp.pdf')
temp = health[,c('pm25.lag1','date2','Temp','DpTemp','rmTemp','rmDpTemp')]
corrplot.mixed(cor(temp,use='complete.obs'))
#dev.off()
pdf(file='PM2pt5_acf.pdf')
acf(resid(fit))
dev.off()

## check slope of PM2.5:
newd <- health[1, ] # grab any row
newd$pm25.lag1 <- 15 - 1e-05 # subtract some small number
y1 <- predict(fit, newd)
newd$pm25.lag1 <- 15 + 1e-05 # add some small number
y2 <- predict(fit, newd)
beta1=(y2 - y1)/2e-05
# since edf approx 1, this is nearly constant across pm2.5

100*(exp(10*beta1)-1)


## for educational purposes, see what happens without date:
fit.nodate = gam(log(cr65plus)~s(pm25.lag1)+fdow+s(Temp)+ s(DpTemp)+s
                 (rmTemp)+s(rmDpTemp), data = health)

pdf(file='PM2pt5_acf_nodate.pdf')
acf(resid(fit.nodate))
dev.off()

summary(fit.nodate)



################################
#################################
###Temperature sensitivity
Results = NULL
for (df in 6:12){ print (df)
  fit = gam(log(cr65plus)~pm25.lag1+fdow+s(date2, k = 100 )+s(Temp)+
              s(DpTemp)+s(rmTemp, k = df, fx=TRUE )+s(rmDpTemp), data = health)
  
  #Extract PM2.5 point estimate and standard error
  est = c(summary(fit)$p.coef[2], summary(fit)$se[2])
  
  #Save results
  Results = rbind (Results, est)
}

Results=data.frame(Results)
Results$Upper95 = Results[,1]+1.96*Results[,2]
Results$Lower95 = Results[,1]-1.96*Results[,2]
y.lim = c(min( c(0,Results$Lower95)), max(Results$Upper95))

plot (Results[,1]~c(6:12), ylim = y.lim, ylab = "PM2.5 Effect", xlab = "DF for Lagged Temperature", pch = 16, cex=1.2)
for (i in 1:nrow(Results)){ lines(c(Results$Upper95[i],Results$Lower95[i])~rep( c(6:12)[i], 2), type = "l", lwd = 2) }
abline(h = 0)


###Temporal sensitivity
Results = NULL
dfs = seq(1, 100, by = 1)
for (df in dfs){ print (df)
  fit = gam(log(cr65plus)~pm25.lag1+fdow+s(date2, k = df, fx=TRUE )+s(Temp)+
              s(DpTemp)+s(rmTemp)+s(rmDpTemp), data = health)
  
  #Extract PM2.5 point estimate and standard error
  est = c(summary(fit)$p.coef[2], summary(fit)$se[2])
  
  #Save results
  Results = rbind (Results, est)
}

Results=data.frame(Results)
Results$Upper95 = Results[,1]+1.96*Results[,2]
Results$Lower95 = Results[,1]-1.96*Results[,2]
y.lim = c(min( c(0,Results$Lower95)), max(Results$Upper95))

par(mar=c(4,4,1,1), cex.lab = 1.2, cex.axis=1.2)
plot (Results[,1]~dfs, ylim = y.lim, ylab = "PM2.5 Effect", xlab = "DF for Date", pch = 16, cex=1.2)
for (i in 1:nrow(Results)){ lines(c(Results$Upper95[i],Results$Lower95[i])~rep( dfs[i], 2), type = "l", lwd = 2) }
abline(h = 0)



### Bivariate splines
### Thin-plate spline examples
par (mar = c(4,4,1,1), cex.lab = 1.2, cex.axis = 1.2)
plot (health$Temp~health$DpTemp, pch = 16, cex = .5, col = "grey", xlab = "Temperature", ylab = "Dew-point Temperature")
junk = lm (health$Temp~health$DpTemp)
x = c(1:4)*(max(health$Temp)-min(health$Temp))/5# quantile (health$Temp, c(.2,.4, .6, .8))
knots = cbind (x, coef(junk)[1] + coef (junk)[2]*x)
text (knots[,1]-4, knots[,2], 1:4, cex = 1.8, col = 4)
lines (knots, pch = 18, col = 4, cex = 2, type = "p")
legend ("bottomright", "Knot", pch = 16, col = 4, cex = 1.5, bty = "n")


### Set up prediction grid for 3-d plot
x = seq(0, 100, by = 2)
y = seq(0, 100, by = 5)
nrz <- length(x)
ncz <- length(y)
colors <- colorRampPalette( c("lightblue","blue") ) #Color scale
nbcol <- 100 #Number of color
color <- colors(nbcol) #Output color IDs
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
facetcol <- cut(zfacet, nbcol)

par (mfrow = c(2,2), mar=c(0,0,1,0) )

for (j in 1:4){
  
  #Calculate distance and thin-plate spline
  radbasis = function(x,y) {
    z = sqrt( (x- knots[j,1])^2 + (y-knots[j,2])^2) 
    z^2*log(z)
  }
  
  
  z = outer(x, y, radbasis)
  
  #Plot perspective
  persp (x, y, z, expand = .5, col = color[facetcol], xlab = "Temp", ylab = "DpTemp", theta = -20, phi = 50, border = "grey")
  title (main = paste("Spline at knot",j ,": (", paste(round(knots[j,]), collapse=", "), ")"))
}


# slide 40
beta = c(-2,1,1,-2)
z = matrix(0, ncol = length (y), nrow =length(x))
for (j in 1:4){
  junk = outer(x, y, function(x,y){ sqrt( (x- knots[j,1])^2 + (y-knots[j,2])^2) })
  z = z + junk^2*log(junk)*beta[j]
}
facetcol <- cut(zfacet, nbcol)
par (mar = c(0,0,1,0))
par (mfrow = c(1,1))
persp (x, y, z, expand = .5, col = color[facetcol], xlab = "Temp", ylab = "DpTemp", theta = -60, phi = 50, border = "grey")
title (main = "Basis coefficients = [-2,1,1,-2]")

beta = c(1,-1,-1,1)
z = matrix(0, ncol = length (y), nrow =length(x))
for (j in 1:4){
  junk = outer(x, y, function(x,y){ sqrt( (x- knots[j,1])^2 + (y-knots[j,2])^2) })
  z = z + junk^2*log(junk)*beta[j]
}
facetcol <- cut(zfacet, nbcol)
par (mar = c(0,0,1,0))
par (mfrow = c(1,1))
persp (x, y, z, expand = .5, col = color[facetcol], xlab = "Temp", ylab = "DpTemp", theta = -60, phi = 50, border = "grey")
title (main = "Basis coefficients = [1,-1,-1,1]")


##########################
#Tensor Product Spline Demo
##########################
# slide 43
x = seq(0, 100, by = 2)
y = seq(0, 100, by = 5)
nrz <- length(x); ncz <- length (y)
colors <- colorRampPalette( c("lightblue","blue") )
nbcol <- 100
color <- colors(nbcol)

#Create marginal splines
t1 = outer(x, y, function(x,y){ x })
# equivalently,
a = x%*%t(rep(1,length(y)))
all(a==t1)

t2 = outer(x, y, function(x,y){ x-42 })
t2[t2<0] = 0
h1 = outer(x, y, function(x,y){ y })
h2 = outer(x, y, function(x,y){ y-55 })
h2[h2<0] = 0

#Create interaction 
s11 = t1*h1
s12 = t1*h2
s21 = t2*h1
s22 = t2*h2

#Perspective plot for each basis function
par (mfrow = c(3,3))
plot (health$Temp~health$DpTemp, pch = 16, cex = .1, col = "grey", xlab = "Temperature (X)", ylab = "Humidity(Z)")
lines (42, 55, pch = 16, col = 4, cex =1.52, type = "p")
text (44, 60, "knot", cex = 2, col = 4)
par (mar = c(0,0,1,0))
#lines (15, 50, pch = 18, col = 4, cex = 2, type = "p")
facetcol <- cut(t1[-1, -1] + t1[-1, -ncz] + t1[-nrz, -1] + t1[-nrz, -ncz], nbcol)
persp (x, y, t1, expand = .5, col = color[facetcol], xlab = "Z", ylab = "X", theta = -60, phi = 50, border = "grey")
title(main = "X")
facetcol <- cut(t2[-1, -1] + t2[-1, -ncz] + t2[-nrz, -1] + t2[-nrz, -ncz], nbcol)
persp (x, y, t2, expand = .5, col = color[facetcol], xlab = "X", ylab = "Y", theta = -60, phi = 50, border = "grey")
title(main = "(X-42)+")
facetcol <- cut(h1[-1, -1] + h1[-1, -ncz] + h1[-nrz, -1] + h1[-nrz, -ncz], nbcol)
persp (x, y, h1, expand = .5, col = color[facetcol], xlab = "X", ylab = "Y", theta = -60, phi = 50, border = "grey")
title(main = "Z")
facetcol <- cut(h2[-1, -1] + h2[-1, -ncz] + h2[-nrz, -1] + h2[-nrz, -ncz], nbcol)
persp (x, y, h2, expand = .5, col = color[facetcol], xlab = "X", ylab = "Y", theta = -60, phi = 50, border = "grey")
title(main = "(Z-55)+")
facetcol <- cut(s11[-1, -1] + s11[-1, -ncz] + s11[-nrz, -1] + s11[-nrz, -ncz], nbcol)
persp (x, y, s11, expand = .5, col = color[facetcol], xlab = "X", ylab = "Y", theta = -60, phi = 50, border = "grey")
title(main = "XZ")
facetcol <- cut(s12[-1, -1] + s12[-1, -ncz] + s12[-nrz, -1] + s12[-nrz, -ncz], nbcol)
persp (x, y, s12, expand = .5, col = color[facetcol], xlab = "X", ylab = "Y", theta = -60, phi = 50, border = "grey")
title(main = "X(Z-55)+")
facetcol <- cut(s21[-1, -1] + s21[-1, -ncz] + s21[-nrz, -1] + s21[-nrz, -ncz], nbcol)
persp (x, y, s21, expand = .5, col = color[facetcol], xlab = "X", ylab = "Y", theta = -60, phi = 50, border = "grey")
title(main = "(X-42)+ Z")
facetcol <- cut(s22[-1, -1] + s22[-1, -ncz] + s22[-nrz, -1] + s22[-nrz, -ncz], nbcol)
persp (x, y, s22, expand = .5, col = color[facetcol], xlab = "X", ylab = "Y", theta = -60, phi = 50, border = "grey")
title(main = "(X-42)+ (Z-54)+")



###### Example
## Thin-plate spline is the default
fit.bivthinplate = gam (log(cr65plus) ~ s(date2, k = 100) + s(Temp, DpTemp, k = 20) , data = health)
summary (fit.bivthinplate)

xmat = predict(fit.bivthinplate,type='lpmatrix')
head(xmat) # 19 functions in bivariate smoothing spline 
dim(xmat)

par(mfrow=c(2,2))
gam.check(fit.bivthinplate)


#Contour plots
plot (fit.bivthinplate, select = 2, lwd =3 )
# vis.gam offers some plots that can aid interpretation

# additional code not in lecture notes:
# 3 plots to visualize predicted surface:
vis.gam(fit.bivthinplate) #defaults to first two variables
vis.gam(fit.bivthinplate,theta=90) 
# effect of temperature is the same as we move across different dates
# because we did not model an interation between date2 and Temp



vis.gam(fit.bivthinplate,view=c('Temp','DpTemp'),theta=0)
# Effect of temperature can now change for different dew point temperatures
?vis.gam
# read documentation for "cond"

vis.gam(fit.bivthinplate,theta=-45,view=c('Temp','DpTemp'),ticktype='detailed')
# ticktype: add axis labels

vis.gam(fit.bivthinplate,theta=30,view=c('Temp','DpTemp'),ticktype='detailed')

vis.gam(fit.bivthinplate,theta=-45,view=c('Temp','DpTemp'),ticktype='detailed', se =2)


vis.gam(fit.bivthinplate,theta=90,view=c('Temp','DpTemp'))
vis.gam(fit.bivthinplate,theta=90,view=c('Temp','DpTemp'),ticktype='detailed',se=2)
vis.gam(fit.bivthinplate,view=c('Temp','DpTemp'),plot.type='contour')


##Tensor product fit

# this takes a minute:
a = proc.time()
fit.tensorspline = gam (log(cr65plus) ~ s(date2, k = 100) + te(Temp, DpTemp, k = 20, bs = "cr") , data = health)
proc.time() - a
summary(fit.tensorspline)

xmat = predict(fit.tensorspline,type='lpmatrix')
head(xmat)
# NOTE! (20*20-1)

vis.gam(fit.tensorspline,theta=-45,view=c('Temp','DpTemp'),ticktype='detailed')


vis.gam(fit.tensorspline,theta=-45,view=c('Temp','DpTemp'),ticktype='detailed', se =2)

# Dew point temperature = 30
pred.temp = seq( 0, 95,  by = 2)
eff = predict (fit.tensorspline, data.frame(Temp=pred.temp, DpTemp = 30, date2=0), type="terms", se = T)
head(eff$fit)
head(eff$se.fit)
Est = eff$fit[,2]
Upper95 = Est + 1.96*eff$se.fit[,2]; 
Lower95 = Est - 1.96*eff$se.fit[,2] # NOTE: Here we grab only the standard error from the tensor spline. 

rangeTempDp2535 = range (health$Temp[ abs(health$DpTemp - 30)< 5])
# recall that DpTemp and Temp are correlated

par(mfrow=c(1,2))
plot (Est~pred.temp, ylim = c(-.3, .3), lwd = 2,xlab = "Temperature", ylab = "Effect", type="l")
lines (Upper95~pred.temp, col=2, lty = 3, lwd = 2)
lines (Lower95~pred.temp, col=2, lty = 3, lwd = 2)
abline(h = 0, col = "grey", lwd = 2)
legend ("topleft", legend = c("Estimated Effect", "95% Confidence Interval"), col=c(1,2), lwd = 3, lty=c(1,3), bty = "n", cex = 1)
title (main = "Dew Point Temperature = 30")

abline (v = rangeTempDp2535, col = 4, lty = 2)
text (mean(rangeTempDp2535), -.25, "Temperature range for \n dew point temperature 25-35", cex = .75, col = 4)


#Dew point temperature = 60
pred.temp = seq( 0, 95,  by = 2)
eff = predict (fit.tensorspline,data.frame(Temp=pred.temp, DpTemp = 60, date2=0), type="terms", se = T)
Est = eff$fit[,2]
Upper95 = Est + 1.96*eff$se.fit[,2]; Lower95 = Est - 1.96*eff$se.fit[,2]

plot (Est~pred.temp, ylim = c(-.3, .3), lwd = 2,xlab = "Temperature", ylab = "Effect", type="l")
lines (Upper95~pred.temp, col=2, lty = 3, lwd = 2)
lines (Lower95~pred.temp, col=2, lty = 3, lwd = 2)
abline(h = 0, col = "grey", lwd = 2)
legend ("topleft", legend = c("Estimated Effect", "95% Confidence Interval"), col=c(1,2), lwd = 3, lty=c(1,3), bty = "n", cex = 1)
title (main = "Dew Point Temperature = 60")

rangeTempDp5565 = range (health$Temp[ abs(health$DpTemp - 60)< 5])
abline (v = rangeTempDp5565, col = 4, lty = 2)
text (mean(rangeTempDp5565), -.25, "Temperature range for \n dew point temperature 55-65", cex = .75, col = 4)


## Plotting marginal effects with plot_smooth.
library(itsadug)
plot_smooth(fit.tensorspline,view='Temp',cond=list(DpTemp=30,date2=0),xlim=c(0,95)) 
# NOTE: Includes the variance from all model terms. 
eff = predict (fit.tensorspline,data.frame(Temp=pred.temp, DpTemp = 30, date2=0), type="link", se = T)
2*1.96*eff$se.fit # this should be equal to width of the confidence bands

plot_smooth(fit.tensorspline,view='Temp',cond=list(DpTemp=60,date2=0),xlim=c(0,95)) 
eff = predict (fit.tensorspline,data.frame(Temp=pred.temp, DpTemp = 60, date2=0), type="link", se = T)
2*1.96*eff$se.fit 

##########################
### Pm2.5 and temperature
# may need to re-run
#--------->
health$fdow = factor(health$dow)
health$fdow = relevel (health$fdow, ref = "Sunday")
#<-------------

fit = gam(log(cr65plus)~s(pm25.lag1, rmTemp)+fdow+s(date2, k = 100)+s(DpTemp)+s(rmDpTemp), data = health)
# note in slides:
par(mfrow=c(2,2))
gam.check(fit)
# residuals are okay.
# k is okay.

#pdf(file='~/Dropbox/EmoryCourses/BIOS_526/Materials_BRisk/Lectures/M4-GAM/Fig19_new.pdf')
vis.gam(fit,view = c('pm25.lag1','rmTemp'), theta = -45,ticktype='detailed')
#dev.off()
vis.gam(fit,view = c('pm25.lag1','rmTemp'),phi=0,ticktype='detailed')
vis.gam(fit,view = c('pm25.lag1','rmTemp'),theta=90,phi=0,ticktype='detailed')




##Marginal Effect
pred.pm = seq( 0, 60,  by = 1)

par (mfrow = c(2,2))

for (cut in c(41, 50, 70, 77)) {
  eff = predict(fit, data.frame(pm25.lag1 = pred.pm, rmTemp=cut,
                                DpTemp=0, rmDpTemp = 0, fdow = "Sunday", date2=0),type= "iterms", se=T)
  
  Est = eff$fit[,2]
  Upper95 = Est + 1.96*eff$se.fit[,2]; Lower95 = Est - 1.96*eff$se.fit[,2]
  
  range = range (health$pm25.lag1[ abs(health$rmTemp - cut)< 5])
  plot (Est~pred.pm, ylim = c(-.3, .3), lwd = 3,xlab = "Previous day's PM2.5 Level", ylab = "Effect", type="l")
  lines (Upper95~pred.pm, col=2, lty = 3, lwd = 3); lines (Lower95~pred.pm, col=2, lty = 3, lwd = 2)
  abline(h = 0, col = "grey", lwd = 2, lty = 3)
  legend ("top", legend = c("Estimated Effect", "95% Confidence Interval"), col=c(1,2), lwd = 3, lty=c(1,3), bty = "n", cex = 1 )
  title (main = paste("Temperature =", cut))
  
  abline (v = range, col = 4, lty = 2)
  text (mean(range), -.25, paste("Temperature range for \n temperature", cut-5, "-", cut+5), cex = 1, col = 4)
}


### Extract effect
# Slide 59
X1 = predict(fit, data.frame(pm25.lag1 = 40, rmTemp=70,
                         DpTemp=0, rmDpTemp = 0, fdow = "Sunday", date2=0),type= "lpmatrix")
X2 = predict(fit, data.frame(pm25.lag1 = 50, rmTemp=70,
                         DpTemp=0, rmDpTemp = 0, fdow = "Sunday", date2=0),type= "lpmatrix")
X.diff = X2 - X1
X.diff
dim (X.diff)

X2%*%coef(fit)


Est = X.diff %*% coef (fit)

# another approach (but won't extend to get CIs...):
predict(fit, data.frame(pm25.lag1 = 50, rmTemp=70,DpTemp=0, rmDpTemp = 0, fdow = "Sunday", date2=0)) - predict(fit, data.frame(pm25.lag1 = 40, rmTemp=70,DpTemp=0, rmDpTemp = 0, fdow = "Sunday", date2=0))


predict(fit, data.frame(pm25.lag1 = 50, rmTemp=50,DpTemp=0, rmDpTemp = 0, fdow = "Sunday", date2=0)) - predict(fit, data.frame(pm25.lag1 = 40, rmTemp=50,DpTemp=0, rmDpTemp = 0, fdow = "Sunday", date2=0))

# Estimate confidence intervals:
# vcov extracts full covariance matrix:
temp = vcov(fit)
temp
dim(temp)

# combine relevant variances. X.diff contains zeros for terms that do not change.
se = sqrt( X.diff %*% vcov (fit) %*% t(X.diff) ) 
Est; se

Est - 2*se
Est + 2*se


# repeat for PM 2.5 20 to 30
X1 = predict(fit, data.frame(pm25.lag1 = 20, rmTemp=70,
                             DpTemp=0, rmDpTemp = 0, fdow = "Sunday", date2=0),type= "lpmatrix")
X2 = predict(fit, data.frame(pm25.lag1 = 30, rmTemp=70,
                             DpTemp=0, rmDpTemp = 0, fdow = "Sunday", date2=0),type= "lpmatrix")
X.diff = X2 - X1
Est = X.diff %*% coef (fit)
se = sqrt( X.diff %*% vcov (fit) %*% t(X.diff) ) 
Est - 2*se
Est + 2*se

###### Testing significance of interaction:

# Let's return to the question of inference in gams.

# First, test significance of dow. This is a parametric term.
fit.full = gam(log(cr65plus)~s(pm25.lag1, rmTemp)+fdow+s(date2, k = 100)+s(DpTemp)+s(rmDpTemp), data = health)
summary(fit.full)
anova(fit.full)
# usual inference applies

  # Things are a little funny if you use this approach:
  fit.reduced = gam(log(cr65plus)~s(pm25.lag1, rmTemp)+s(date2, k = 100)+s(DpTemp)+s(rmDpTemp), data = health)
  anova(fit.reduced,fit.full,test='F')
  # Note the df is not equal to 6, as there is slightly different smoothing for the smoothed terms in the full and reduced. 
  # Use the previous approach with anova(fit.full)


# Next, assess approximate significance of smooth terms:
summary(fit.full)
# approximate p-values

# Test for whether the term with the bivariate smoother is better than two separate smoothers
fit.reduced2 = gam(log(cr65plus)~s(pm25.lag1)+s(rmTemp)+s(date2, k = 100)+fdow+s(DpTemp)+s(rmDpTemp), data = health)
summary(fit.reduced2)
anova(fit.reduced2,fit.full,test='F')  
# the "F-statistic" is negative
((27.354 - 27.428)/(1734.9 - 1733.3)) / (27.354 / 1734.9)

# Another note: the bivariate thin plate spline is not nested in the univariate thin plate splines (the design matrices are different). 
# an alternative is to fit the tensor, which should be nested:

## This is the preferred way to test for interactions in a tensor spline.
## The "ti()" basis creates a spline such that all "main effects" are in the null space of the ti() penalty,
## see pages 243 and the example on page 343-346.
fit.full.tensor = gam(log(cr65plus)~s(pm25.lag1,bs='cr')+s(rmTemp,bs='cr')+ti(pm25.lag1, rmTemp,bs='cr')+fdow+s(date2, k = 100)+s(DpTemp)+s(rmDpTemp), data = health)
summary(fit.full.tensor)
anova(fit.full.tensor)



# Anova can be used if you are removing a single smooth. Example using date2:
fit.nodate2 = gam(log(cr65plus)~s(pm25.lag1,rmTemp)+fdow+s(DpTemp)+s(rmDpTemp), data = health)
anova(fit.full,fit.nodate2,test='F')  
anova(fit.nodate2,fit.full,test='F')   
((34.854 - 27.428)/(1798.9-1733.3)) / (27.428 / 1733.3)

  
###########
# should I model pm25.lag1 as linear or non-linear?
fit.reduced3 = gam(log(cr65plus)~pm25.lag1+s(rmTemp)+s(date2, k = 100)+fdow+s(DpTemp)+s(rmDpTemp), data = health)
summary(fit.reduced3)
# it doesn't matter - here, pvalues are about the same with edf = 1
anova(fit.reduced2,fit.reduced3,test='F')  
###############  

# General Notes:
# anova sometimes works, but approximate:
# weird things can happen, like "more complicated" model has lower degrees of freedom due to gcv. 
# in this case, Wood recommends using "more complicated" model, since it is in some sense simpler. 
# However, I suggest you weigh that against the costs of interpretability.

# Another important measure is the R-squared. If you don't get much with the more complicated bivariate model, 
# and given it is more difficult to interpret, I suggest going with the simpler model.





