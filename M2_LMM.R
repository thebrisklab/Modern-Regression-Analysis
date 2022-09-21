setwd("./Data")
#setwd("C:/Users/bbr28/Dropbox/EmoryCourses/BIOS_526/Materials_BRisk_2019/Data")

pig = read.csv('pig.csv')[,2:4]
nepal = read.csv('Nepal.csv')
library(lme4)


#Pig growth
# slide 4
plot (pig$weight~pig$weeks, xlab = "Time (weeks)", ylab = "Weight (kg)", type ="n" )
title (main = "48 pigs with body weights measured at 9 successive weeks", cex = 1.5)
for (i in 1:48){
	y = pig$weight[pig$id == i]; x = pig$weeks[pig$id ==i]
	lines (y~x, type = "b", col = "grey", cex = .75, lwd = .5)
}
for (i in c(1, 10, 15, 20, 30, 35, 40) ){
  y = pig$weight[pig$id == i]; x = pig$weeks[pig$id ==i]
  lines (y~x, type = "b", col = i, cex = 1, lwd = 2)
}

# slide 5
pig[1:13,]

str(pig)
table(pig$id)

  
# slide 6
plot (pig$weight~pig$weeks, xlab = "Time (weeks)", ylab = "Weight (kg)", type ="n" )
title (main = "48 pigs with body weights measured at 9 successive weeks", cex = 1.5)
for (i in 1:48){
  y = pig$weight[pig$id == i]; x = pig$weeks[pig$id ==i]
  lines (y~x, type = "b", col = "grey", cex = .75, lwd = .5)
}
fit.incorrect = lm(pig$weight~pig$weeks)
abline (coef (fit.incorrect), lwd = 3, col = 2)
summary(fit.incorrect)



fit.fixedeffects = lm (weight~weeks + factor(id), data = pig)
plot (pig$weight~pig$weeks, xlab = "Time (weeks)", ylab = "Weight (kg)", type ="n" )
for (i in 1:48){
  y = pig$weight[pig$id == i]; x = pig$weeks[pig$id ==i]
  lines (y~x, type = "b", col = "grey", cex = .75, lwd = .5)
}
id.use = c(1, 10, 15, 20, 30, 35, 40)
for ( i in id.use){
	y = fit.fixedeffects$fitted[pig$id == i]; x = pig$weeks[pig$id ==i]
	lines(y~x, type = "b", col = i, cex = 1, lwd = 2)
}
abline (coef (fit.fixedeffects)[1:2], lwd = 3, col = 2)
summary(fit.fixedeffects)
# note: SE is different



#Nepalese Children

table(nepal$id)
# suggests 5 observations per subject, but...
par (mar = c(4, 4,2, 1))
plot (nepal$arm~nepal$age, col = "grey", xlab = "Child's age (months)", ylab = "Arm circumference" )
abline ( coef(lm (nepal$arm~nepal$age)), col = 2, lwd = 3)

# most of the arm circumferences are less than 20. 
# there appear to be many missing values coded as 99.9 or 88.8
sum(nepal$arm==99.9)
sum(nepal$arm==88.8)


# Important data cleaning:
nepal$arm[ nepal$arm == 99.9] = NA
nepal$arm[ nepal$arm == 88.8] = NA
nepal = subset (nepal, !is.na(arm))
table(nepal$id)
# not balanced

# slide 10
plot (nepal$arm~nepal$age, col = "grey", xlab = "Child's age (months)", ylab = "Arm circumference" )
abline ( coef(lm (nepal$arm~nepal$age)), col = 2, lwd = 3)


# slide 11
plot (nepal$arm~nepal$age, xlab = "Child's age (months)", ylab = "Arm circumference", type ="n" )
for (i in 1:200){
	y = nepal$arm[nepal$id == i]; x = nepal$age[nepal$id ==i]
	lines (y~x, type = "b", col = "grey", cex = 1, lwd = .5)
}
for (i in 1:15){
	y = nepal$arm[nepal$id == i]; x = nepal$age[nepal$id ==i]
	lines (y~x, type = "b", col = i, cex = 1, lwd = 2)
}
title (main = "Arm Circumference (cm) for 200 Nepalese Children", cex = 1.4)


# slide 12
# fit the model assuming data are independent:
fit.incorrect = lm (arm~age, data = nepal)
summary (fit.incorrect)

# fit the model controlling for dependence by estimating the mean structure:
nepal$fid = factor(nepal$id)
fit.fixedeffects = lm (arm~age+fid, data = nepal)
summary (fit.fixedeffects)
# note: SE is larger when accounting for subject


## ASIDE: note what happens to R-squared if you remove the intercept:
  temp = lm (arm~age+fid-1, data = nepal)
  summary(temp)
  # R-squared is approximately 1. Is this accurate? 
  # Are predictions the same?
  # Is SE the same?
  1-sum((nepal$arm - fitted(temp))^2)/sum((nepal$arm - mean(nepal$arm))^2)
  1-sum((nepal$arm - fitted(temp))^2)/sum((nepal$arm))^2


# slide 17 -- back to Pigs dataset
fit.randomeffects = lmer(weight~weeks+(1|id), data = pig)
summary(fit.randomeffects)


#note: fit with lmerTest for approximate inference
library(lmerTest)
fit.randomeffects = lmer(weight~weeks+(1|id), data = pig)
summary(fit.randomeffects)


random.eff = ranef(fit.randomeffects)$id[,1]
hist(random.eff)

# slide 18 -- Nepalese children
fit.re.arms = lmer(arm~age+(1|id), data = nepal)
random.eff.arms = ranef (fit.re.arms)$id[,1]
summary(fit.re.arms)
random.eff.arms
hist(random.eff.arms)

    # aside: fit the same lmm with mgcv::gam, which has weird syntax, will be explored in Module 5
    library(mgcv)
    nepal$id2 = as.factor(nepal$id)
    fit.re.arms.check = gam(arm~age+s(id2,bs = 're'),data=nepal,method='REML')
    summary(fit.re.arms.check)    
    # they are equivalent
    # one we learn about gams, we will examine a non-linear effect of age...     
    
    
# re-fit fixed effects:
fit.fe.arms = lm(arm~age+factor(id), data = nepal)
summary(fit.fe.arms)
# estimates differ for effect of age from the mixed model 
# data not balanced


# slide 22
fit = lmer(arm~age+lit+(1|id), data = nepal)
summary(fit)

fit = lmer(arm~age+lit+(1|id), data = nepal)
summary(fit)


fit = lmer(arm~age+lit+(1|id), data = nepal)
summary(fit)



##############
# Slide 29

# simulate data:
tau=4
sigma=2
n = 48
r=9
#generate random effects:

#pdf(file='~/Dropbox/EmoryCourses/BIOS_526/Materials_BRisk/Lectures/M5-RandomIntercept/ICCPig.pdf',width=8,height=5)
par(mfrow=c(1,3))
for (tau in c(0.5,4,16)) {
  thetai = rnorm(n,0,tau)
  thetailong = thetai%x%rep(1,r)
  head(thetailong)
  yij = 15+thetailong+6.2*pig$weeks+rnorm(n*r,0,sigma)
  
  plot(yij~pig$weeks, xlab = "Time (weeks)", ylab = "Weight (kg)", type ="n" )
  for (i in 1:48){
    y = yij[pig$id == i]; x = pig$weeks[pig$id ==i]
    lines (y~x, type = "b", col = "blue", cex = .75, lwd = .5)
  }
  icc = tau^2/(tau^2+sigma^2)
  title(paste('Tau =',tau,', ICC =',round(icc,2)))
} 
#dev.off()


# slide 30
#pdf(file='~/Dropbox/EmoryCourses/BIOS_526/Materials_BRisk/Lectures/M5-RandomIntercept/ICCPig2.pdf',width=8,height=5)
par(mfrow=c(1,3))
tau=4

for (sigmasq in c(0.5,4,16)) {
  thetai = rnorm(n,0,tau)
  thetailong = thetai%x%rep(1,r)
  head(thetailong)
  yij = 15+thetailong+6.2*pig$weeks+rnorm(n*r,0,sqrt(sigmasq))
  
  plot(yij~pig$weeks, xlab = "Time (weeks)", ylab = "Weight (kg)", type ="n" )
  for (i in 1:48){
    y = yij[pig$id == i]; x = pig$weeks[pig$id ==i]
    lines (y~x, type = "b", col = "blue", cex = .75, lwd = .5)
  }
  icc = tau^2/(tau^2+sigmasq)
  title(paste('Sigmasq =',sigmasq,', ICC =',round(icc,2)))
}
#dev.off()






###################
# Slide 46
fit.randomeffects = lmer(weight~weeks+(1|id), data = pig)
summary(fit.randomeffects)
random.eff = ranef(fit.randomeffects)$id[,1]
hist(random.eff)

#re-fit fixed effects model:
fit.fixedeffects = lm (weight~weeks+factor(id)-1, data = pig)
summary(fit.fixedeffects)
coef1 = coef(fit.fixedeffects)[-1]

# random effects (centered around zero) plus the fixed intercept 
# (alternative view of random effects from slide 16)
coef2 = unlist(ranef(fit.randomeffects)) + fixef(fit.randomeffects)[1]
par (mfrow = c(1,2) )
plot (coef2~coef1, cex = 2, xlim = range(coef1), ylim = range(coef1), xlab = "Pig-specific Intercepts (No Shrinkage)", ylab = "Pig-specific Intercepts (With Shrinkage)")
lines (coef2~coef1, type = "p", col = 4, pch = 16, cex = .5)
abline(0,1)
abline( h = fixef(fit.randomeffects)[1], lwd= 2, col = 2, lty = 2)
abline( v = fixef(fit.randomeffects)[1], lwd= 2, col = 2, lty = 2)
lines (fixef(fit.randomeffects)[1],fixef(fit.randomeffects)[1], pch = 16, col = 2, cex = 2, type = "p")


plot (0,0, type = "n", xlim = range(coef1) , ylim = c(.9,2.1),  yaxt = "n", xlab = "", ylab = "")
for (i in 1:length(coef2) ){ lines ( c(1,2)~c(coef2[i], coef1[i]), col = "grey", type = "l")}
abline (h = c(1,2))
lines (fixef(fit.randomeffects)[1],1, pch = 16, col = 2, cex = 2, type = "p")
text (mean(range(coef1)), .9, "Shrunken Estimates")
text (mean(range(coef1)), 2.1, "Raw Estimates")



## Slide 47
fit = lm (arm~age+factor(id)-1, data = nepal)
coef1 = coef(fit)[-1]
fit2 = lmer(arm~age+(1|id), data = nepal)
coef2 = unlist(ranef(fit2)) + fixef(fit2)[1]

#pdf(file='~/Dropbox/EmoryCourses/BIOS_526/Materials_BRisk/Lectures/M5-RandomIntercept/ShrinkNepal_v2.pdf',width=11,height=6)
par (mfrow = c(1,2))
plot (coef2~coef1, cex = 2, xlim = range(coef1), ylim = range(coef1), xlab = "Child-specific Intercepts (No Shrinkage)", ylab = "Child-specific Intercepts (With Shrinkage)")
lines (coef2~coef1, type = "p", col = 4, pch = 16, cex = .5)
abline(0,1)
abline( h = fixef(fit2)[1], lwd= 2, col = 2, lty = 2)
abline( v = fixef(fit2)[1], lwd= 2, col = 2, lty = 2)
lines (fixef(fit2)[1],fixef(fit2)[1], pch = 16, col = 2, cex = 2, type = "p")


plot (0,0, type = "n", xlim = range(coef1) , ylim = c(.9,2.1),  yaxt = "n", xlab = "", ylab = "")
for (i in 1:length(coef2) ){ lines ( c(1,2)~c(coef2[i], coef1[i]), col = "grey", type = "l")}
abline (h = c(1,2))
lines (fixef(fit2)[1],1, pch = 16, col = 2, cex = 2, type = "p")
text (mean(range(coef1)), .9, "Shrunken Estimates")
text (mean(range(coef1)), 2.1, "Raw Estimates")
#dev.off()



## Slide 48
# add more noise (i.e., measurement error)
set.seed(123)
nepal$arm2 = nepal$arm + rnorm (nrow(nepal), 0, sqrt(2))
fit = lm (arm2~age+factor(id)-1, data = nepal)
coef1 = coef(fit)[-1]
fit2 = lmer(arm2~age+(1|id), data = nepal)
coef2 = unlist(ranef(fit2)) + fixef(fit2)[1]
summary(fit2)

#pdf(file='~/Dropbox/EmoryCourses/BIOS_526/Materials_BRisk/Lectures/M5-RandomIntercept/ShrinkNepal_Noise_v2.pdf',width=11,height=6)

par (mfrow = c(1,2))
plot (coef2~coef1, cex = 2, xlim = range(coef1), ylim = range(coef1), xlab = "Child-specific Intercepts (No Shrinkage)", ylab = "Child-specific Intercepts (With Shrinkage)")
lines (coef2~coef1, type = "p", col = 4, pch = 16, cex = .5)
abline(0,1)
abline( h = fixef(fit2)[1], lwd= 2, col = 2, lty = 2)
abline( v = fixef(fit2)[1], lwd= 2, col = 2, lty = 2)
lines (fixef(fit2)[1],fixef(fit2)[1], pch = 16, col = 2, cex = 2, type = "p")

plot (0,0, type = "n", xlim = range(coef1) , ylim = c(.9,2.1),  yaxt = "n", xlab = "", ylab = "")
for (i in 1:length(coef2) ){ lines ( c(1,2)~c(coef2[i], coef1[i]), col = "grey", type = "l")}
abline (h = c(1,2))
lines (fixef(fit2)[1],1, pch = 16, col = 2, cex = 2, type = "p")
text (mean(range(coef1)), .9, "Shrunken Estimates")
text (mean(range(coef1)), 2.1, "Raw Estimates")

#dev.off()

######################


# Slide 49
fit = lm (arm~age+factor(id)-1, data = nepal)
coef1 = coef(fit)[-1]
fit2 = lmer(arm~age+(1|id), data = nepal)
coef2 = unlist(ranef(fit2)) + fixef(fit2)[1]

par (mfrow = c(1,2), mar = c(4,4,1,1))
n = tapply (!is.na(nepal$arm), factor(nepal$id), sum)
plot ( (coef1-coef2)/coef1~n[n!=0], cex = 2, xlab = "Sample Size per Child", ylab = "% Shrinkage")
abline( h = 0, col = 2, lwd = 3, lty = 3)

se = sqrt(diag(vcov (fit))[-1])
plot ( (coef1-coef2)/coef1~se, cex = 2, xlab = "Standard Error of Raw Estimates", ylab = "% Shrinkage")
abline( h = 0, col = 2, lwd = 3, lty = 3)

nepal$wt[ nepal$wt == 99.9] = NA
nepal$wt[ nepal$wt == 88.8] = NA

fit = lmer(arm~age+(1|id), data = nepal)
random.eff = ranef (fit)$id[,1]
summary(fit)
random.eff[1:2]

#############
# Slide 54
# default is REML:
fit.randomeffects = lmer(weight~weeks+(1|id), data = pig)
summary(fit.randomeffects)

# estimation with mle:
fit.randomeffects.mle = lmer(weight~weeks+(1|id), data = pig, REML = FALSE)
summary(fit.randomeffects.mle)

###########################
##############################
# Random slopes:
##############################
###########################

nepal$ageC = nepal$age - 36
nepal$sex = as.factor(nepal$sex)


# Slide 64:
fit = lmer (arm~ ageC + (ageC|id), data = nepal)
summary(fit)

# note: ageC is both fixed and random
# fixed effect is the population average trend
# random effects is the interaction between random intercept and fixed effect
# interaction between fixed and random is random

# NOTES on model specification in lme4:
# See table 2 in https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf 


fit.scale = lmer (arm~ scale(ageC) + (scale(ageC)|id), data = nepal)
summary(fit.scale)

#note how it differs without centering
temp = lmer(arm~age+(age|id),data=nepal)
summary(temp)
# what changes?
# NOTE: Who has a mac? Please run. Convergence warnings? Sometimes differ across
# platforms or versions

# Slide 67:    
fit = lmer (arm~ ageC + (ageC|id), data = nepal,REML=FALSE)
fit.randomintercept = lmer(arm~ageC+(1|id),data=nepal,REML=FALSE)
AIC(fit)
AIC(fit.randomintercept)
anova(fit.randomintercept,fit) 



# Slide 68:
fit.indep = lmer (arm~ ageC + (1|id) + (0+ageC|id), data = nepal)
summary(fit.indep)



    # model without random intercepts:
    # don't do this:
    fit.norandomint = lmer (arm~ ageC + (0+ageC|id), data = nepal)
    summary(fit.norandomint)
    fit.norandomint = lmer (arm~ ageC + (0+ageC|id), data = nepal,REML=FALSE)
    AIC(fit)
    AIC(fit.norandomint)

# refit the REML model for later comparison:
fit = lmer (arm~ ageC + (ageC|id), data = nepal)


#################################

# NOTE:
# alternatively, we could fit a model with an interaction between subject and slope
nepal$id = factor(nepal$id)
fit.fe.default = lm (arm~ ageC*id, data = nepal)
summary(fit.fe.default) 

# but in this model, the ageC slope corresponds to slope for id1
# recode using sum-to-zero contrasts:
contrasts(nepal$id) = contr.sum
fit.fe = lm (arm~ ageC*id, data = nepal)
summary(fit.fe)
# note: results in some singularities due to no repeated measurements for some subjects

# Since these children are a random sample, we can use 
# our scientific goals to guide our decision to use random effects:
# characterize the population of children in Nepal.


# what happens in balanced data:?
pig = read.csv('pig.csv')[,2:4]
pig$id = as.factor(pig$id)
contrasts(pig$id) = contr.sum
temp = lmer(weight~weeks+(weeks|id),data=pig)
summary(temp)

temp.fe = lm(weight~weeks*id,data=pig)
summary(temp.fe)
# slopes of week are equal, but SE are different. 
# unlike model with random intercepts only, inference is not equivalent. 

####


# code used later on, here makes the coefficients estimated the same, i.e.,
# only includes subjects with repeated measurements. We just do this to facilitate
# comparison of the fixed effect approach to the random effect approach, but in
# general, we can use all the data:)

use =  unique (nepal$id)[ tapply (!is.na(nepal$arm), factor(nepal$id), sum) > 1]
(p = length (use))
length(levels(nepal$id))
fit.fix = lm (arm~ id*ageC - ageC -1, data = subset(nepal, id %in% use) )
fit.re = lmer (arm~ ageC + (ageC|id), data = subset(nepal, id %in% use) )

# note: coef extract fixed + random
re.int = coef(fit)$id[,1] # extract intercepts

# coding tip: ranef extracts just random:
apply(coef(fit)$id,2,mean) #coef are centered around overall effects
apply(ranef(fit)$id,2,mean) # random effects are mean zero


re.slope = coef(fit)$id[,2]
sum(re.slope<0)

fix.int = coef(fit.fix)[1:p]
fix.slope = coef(fit.fix)[(p+1):(2*p)]
sum(fix.slope<0)

# Slide 70
par(mfrow=c(2,2))
hist(fix.int,main='Fixed Subj. Intercept + Main Intercept',breaks=15)
hist(fix.slope,main='Fixed Slope + Main Slope')


hist(re.int,main='Random  Intercept + Main Intercept')
hist(re.slope,main='Random Slope + Main Slope')





##########
# Slide 71: 
plot (nepal$arm~nepal$ageC, xlab = "Child's age - 36 (months)", ylab = "Arm circumference", type ="n" )
for (i in 1:200){
  y = nepal$arm[nepal$id == i]; x = nepal$ageC[nepal$id ==i]
  lines (y~x, type = "b", col = "grey", cex = 1, lwd = .5)
}

# Extract random effects for a few subjects:

# include two subjects that are extreme:
which.min(coef(fit)$id[,1])
which.max(coef(fit)$id[,2])

#use = c(1:15, 57, 103, 105)

use = c(57,105)
for (i in use ){
  x = range(nepal$ageC[nepal$id ==i])
  beta = as.numeric(coef(fit)$id[i,])
  y = beta[1] + beta[2]*x
  lines (y~x, type = "l", col = i+2, cex = 1, lwd = 2)
}

for (i in use){
  y = nepal$arm[nepal$id == i]; x = nepal$ageC[nepal$id ==i]
  lines (y~x, type = "p", col = i+2, cex = 1, lwd = 2)
}


abline( 13.9439, 0.032527, col=2, lwd = 4, lty = 2)
legend("bottomright", "Mean Trend", col = 2, lwd = 4, lty = 2, cex = 1.5, bty = "n")
text (-25, 9.5, "ID = 105", col = "red", font = 2)
text (-20, 12.6, "ID = 57", col = "red", font = 2)

# Slide 72
fit.subset = lmer (arm~ageC+(ageC|id), data = subset(nepal, id != 57 & id != 105) )
summary(fit.subset)

VarCorr(fit)
VarCorr(fit.subset)




#### Shrinkage

# again subset to observations where fe can be estimated:
use =  unique (nepal$id)[ tapply (!is.na(nepal$arm), factor(nepal$id), sum) > 1]
(p = length (use))
length(levels(nepal$id))

fit.fix = lm (arm~ id*ageC - ageC -1, data = subset(nepal, id %in% use) )
fit.re = lmer (arm~ ageC + (ageC|id), data = subset(nepal, id %in% use) )

# note: coef extract fixed + random
re.int = coef(fit)$id[,1]
re.slope = coef(fit)$id[,2]
fix.int = coef(fit.fix)[1:p]
fix.slope = coef(fit.fix)[(p+1):(2*p)]
fix.int.se = sqrt(diag(vcov(fit.fix))[1:p])
fix.slope.se = sqrt(diag(vcov(fit.fix))[(p+1):(2*p)])


pdf(file='Shrinkage_v2.pdf',width=8,height=5)
par (mfrow = c(1,2))

# Create plots for shrinkage of random intercepts:
plot (0,0, type = "n", xlim = range(fix.int) , ylim = c(.9,2.1),  yaxt = "n", xlab = "", ylab = "")
for (i in 1:p ){ lines ( c(2,1)~c(fix.int[i], re.int[i]), col = "grey", type = "l")}
abline (h = c(1,2))

# lines are shrunk towards main effects:
lines (fixef(fit.re)[1],1, pch = 16, col = 2, cex = 2, type = "p")

#use halfway point of range to locate text:
text (mean(range(fix.int)), .9, "Shrunken Intercept Estimates")
text (mean(range(fix.int)), 2.1, "Fixed Effect Intercept Estimates")


# Create plots for shrinkage of random slopes:
plot (0,0, type = "n", xlim = range(fix.slope) , ylim = c(.9,2.1),  yaxt = "n", xlab = "", ylab = "")
for (i in 1:p ){ lines ( c(2,1)~c(fix.slope[i], re.slope[i]), col = "grey", type = "l")}
abline (h = c(1,2))
lines (fixef(fit.re)[2],1, pch = 16, col = 2, cex = 2, type = "p")
text (mean(range(fix.slope)), .9, "Shrunken Slope Estimates")
text (mean(range(fix.slope)), 2.1, "Fixed Effect Slope Estimates")
dev.off()
####################
########
#



#### Simulated Curves ####
library (MASS)
Sigma = matrix (c(0.85^2,  0.0016, 0.0016, 0.021^2), ncol = 2)
plot (nepal$arm~nepal$ageC, xlab = "Child's age - 36 (months)", ylab = "Arm circumference", type ="n" )
x = seq(-36, 40, 1)
simcurve = NULL
for (i in 1:500){
  theta = mvrnorm(1, c(13.94, 0.0325), Sigma) 
  y = theta[1] + x*theta[2]
  simcurve = rbind (simcurve, y)
  lines (y~x,  col="#0000ff22", cex = 2, lwd = .5, type = "l")
}

# Slide 74
# approximate confidence intervals using simulated values:
lower = apply (simcurve, 2, quantile, 0.025)
upper = apply (simcurve, 2, quantile, 0.975)

abline (13.94, 0.0325, lwd = 5, col = "blue")
lines (lower~x, lwd = 3, col = "blue", lty = 3)
lines (upper~x, lwd = 3, col = "blue", lty = 3)
legend ("bottomright", c("Average Trend", "95% Prediction Interval"), lwd = c(3,5), lty = c(1, 3), bty = "n", cex = 1.2, col = 4)

title (main = "Predicted Growth Curves for 500 Children")



# Slide 80: 

# Don't use this model:
fit.sexwtlit = lmer (arm~sex+lit+sex*ageC + lit*ageC + wt+ (ageC|id), data = nepal)
summary(fit.sexwtlit)

# NOTE: Do not use if you have convergence issues.
# This can occur with random slopes, and scaling ageC will help.
# We also scale and center wt, which help for interpretability
fit.sexwtlit.scale = lmer (arm~sex+lit+sex*scale(ageC) + lit*scale(ageC) + scale(wt)+ (scale(ageC)|id), data = nepal)
summary(fit.sexwtlit.scale)
#NOTE: if there were no convergence issues, 

summary(fit.sexwtlit.scale)




# NOTE: The correlation output is  helpful
# We typically see how correlations with interactions reflects
# the general issue of decreased power with interactions
# based on the fitted model and thus reflect impacts on SE,
# different from naive correlations:
a = model.matrix(fit.sexwtlit.scale)
cor(a)
#(correlations with intercept are 0 because it is constant)


# note the variance components decrease a little bit relative to model without additional covariates:
VarCorr(fit)
VarCorr(fit.sexwtlit.scale)


## Alternative and equivalent specification:
fit.sexwtlit.scale.equivalent = lmer (arm~sex+lit+sex*scale(ageC) + lit*scale(ageC) + scale(wt) + (1+scale(ageC)|id), data = nepal)

summary(fit.sexwtlit.scale.equivalent)
summary(fit.sexwtlit.scale)



##############
## Extra material:
#### Pooling plot

use =  unique (nepal$id)[ tapply (!is.na(nepal$arm), factor(nepal$id), sum) > 1]
fit.fix = lm (arm~ factor(id)*ageC - ageC -1, data = subset(nepal, id %in% use) )
fit.re = lmer(arm~ ageC + (ageC|id), data = subset(nepal, id %in% use) )

summary(fit.re)
p = length (use)
re.int = coef(fit)$id[,1]
re.slope = coef(fit)$id[,2]
fix.int = coef(fit.fix)[1:p]
fix.slope = coef(fit.fix)[(p+1):(2*p)]
fix.int.se = sqrt(diag(vcov(fit.fix))[1:p])
fix.slope.se = sqrt(diag(vcov(fit.fix))[(p+1):(2*p)])

tausq.slope = unlist(VarCorr(fit.re))[4]

par (mfrow = c(2,2))
hist (re.int, main = "Fixed Intercepts", xlab = "")
hist (re.slope, main = "Fixed Slopes", xlab = "")
hist (fix.int, main = "Random Intercepts", xlab = "")
hist (fix.slope, main = "Random Slopes", xlab = "")

slope.up = fix.slope  + 1.96*fix.slope.se
slope.low = fix.slope  - 1.96*fix.slope.se

order.i = order (fix.slope.se, decreasing = T)

plot (fix.slope[order.i]~c(1:p), xlim =c(1, 220), ylim = c(-.2, .25) , ylab = "Age Slope", xlab = "", xaxt = "n", bty = "n", pch = 16)
for ( i in 1:p){	lines (c(slope.up[order.i][i], slope.low[order.i][i])~c(i,i), type="l")}
abline(h = 0,col = 2, lwd = 2)
abline (v = 195, lwd = 2, lty = 3)

se.1 = sqrt ( sum(fix.slope.se^2)/p )
se.2 = sqrt ( 1/ sum(1/(fix.slope.se^2)) )
se.3 = sqrt ( 1/ sum(1/(fix.slope.se^2+tausq.slope)))
lines (mean (fix.slope)~c(200), pch = 16, cex = 1.5, col = 2, type = "p")
lines (c(mean(fix.slope)-1.96*se.1, mean(fix.slope)+1.96*se.1)~c(200, 200), type="l", col=2, lwd = 2)

est=sum(fix.slope*(1/(fix.slope.se)^2))/sum((1/(fix.slope.se)^2)) 
lines (sum(fix.slope*(1/(fix.slope.se)^2))/sum((1/(fix.slope.se)^2)) ~c(210), pch = 16, cex = 1, col = "orange", type = "p")
lines (c(est-1.96*se.2, est+1.96*se.2)~c(210, 210), type="l", col = "orange", lwd = 2)

est=sum(fix.slope*(1/(fix.slope.se^2+tausq.slope)))/sum((1/(fix.slope.se^2+tausq.slope)))
lines (est~c(220), pch = 16, cex = 1, col = 4, type = "p")
lines (c(est-1.96*se.3, est+1.96*se.3)~c(220, 220), type="l", col = 4, lwd = 2)

legend ("bottom", legend = c("Simple Average: 0.029 (-0.062, 0.120)", "Inverse-variance Average: 0.031(0.026, 0.037)", 
                             "Normal Random Effect: 0.031(0.025, 0.038)"), 
        col = c("red","orange","blue"), lwd = 2, cex = 1.2, bty="n")
text(80, .25, "Individual Age Slope", font = 2, cex = 1.3)
text(210, .25, "Pooled \n Estimate", font = 2, cex = 1.3)


