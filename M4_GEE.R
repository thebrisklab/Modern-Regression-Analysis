#############
# Generalized Estimating Equations:

# note: there are two packages for gee: gee and geepack.
library(gee)
args(gee)
setwd('./Data')
dat = read.csv('pig.csv')


dat[1:3,]

# usual ols:
fit1 = gee(weight~weeks, id=id, data=dat, corstr='independence')
# also provides the sandwich estimates of SE
summary(fit1)
(lmfit1 = lm(weight~weeks,data=dat))
summary(lmfit1)


# what is the scale parameter in ols?
sum(lmfit1$residuals^2)/(length(lmfit1$residuals)-2)
# for ols, it is the residual variance


# exchangeable:
fit2 = gee(weight~weeks, id=id, data=dat, corstr="exchangeable")
summary(fit2)
# Note data are balanced with same covariate for each pig

  # estimates differ when not balanced or covariates differ
  dat3 = dat
  dat3$week = sample(c(1:9),size=nrow(dat),replace=TRUE)
  # data are still balanced
  table(dat3$id)
  
  summary(gee(weight~week, id=id, data=dat3, corstr='independence'))
  summary(gee(weight~week, id=id, data=dat3, corstr='exchangeable'))
  
                     
#AR-1
fit3 = gee(weight~weeks, id=id, data=dat, corstr="AR-M")
summary(fit3)

# Unstructured:
fit4 = gee(weight~weeks, id=id, data=dat, corstr="unstructured")
summary(fit4)
# can happen when you try to estimate too many parameters: 9 x 9 correlation matrix
# involves 9*8/2 = 36 parameters, which is a lot given we have only 48 pigs



library(lme4)
fit.mixed = lmer(weight~weeks+(1|id),data=dat)
summary(fit.mixed)
15.142/(15.142+4.395)


## NOTE on prediction and gee versus lmm:

#fit2 is exchangeable correlation in gee
# all pigs have the same prediction -- determine by the fixed effects
predict_gee_exchangeable = predict(fit2)

# compare this to lmer:
predict_lmer = predict(fit.mixed)

#pdf(file='~/Dropbox/EmoryCourses/BIOS_526/Materials_BRisk_2020/M4-GEE/PigPredictions.pdf')
par(mfrow=c(2,1))
plot(predict_gee_exchangeable~dat$weeks,main='GEE Predictions',ylab='Predicted value',xlab='week')
plot(predict_lmer~dat$weeks,main='LMM Predictions',ylab='Predicted value',xlab='week')
#dev.off()



plot(predict_gee_exchangeable,(predict_lmer - as.matrix(ranef(fit.mixed)$id)%x%rep(1,9)))
# prediction from gee are for the population effect

# random intercepts improve prediction:
sum((predict_gee_exchangeable - dat$weight)^2)
sum((predict_lmer - dat$weight)^2)



########
# 2 x 2 crossover trial

dat = read.table("2by2.txt",header=T)

### GLM 
fit.glm = glm (outcome~trt*period,data = dat, family = binomial)

### Marginal GEE model with ind corr
# note: set scale.fix=1 makes naive se equivalent to glm
fit.ind.scale.fix = gee::gee (outcome~trt*period, id = ID, data = dat,family = binomial (link = "logit"),  corstr = "independence", scale.fix = 1)

# the default is to estimate the shape parameter
fit.ind = gee (outcome~trt*period, id = ID, data = dat,family = binomial (link = "logit"),  corstr = "independence")
# scale parameter = overdispersion parameter


### Marginal GEE model with exch working corr
fit.exch = gee (outcome~trt*period, id = ID, data = dat,family = binomial (link = "logit"),  corstr = "exchangeable")


### GLMM (conditional) model with (exchangeable corr)
# (note: assumes scale parameter=1)
fit.re = glmer (outcome~trt*period+(1|ID), data=dat, family = binomial, nAGQ = 25)


summary(fit.glm)
summary(fit.ind)
summary(fit.exch)
summary(fit.re)
# Variance 24.4

# example: convert z score to pvalue
2*(1-pnorm(1.934)) # from working correlation = exchangeable with GEE


2*(1-pnorm(1.68)) # from glmm 

##################
#
summary(glmer (outcome~trt*period+(1|ID), data=dat, family = binomial, nAGQ = 1))
summary(glmer (outcome~trt*period+(1|ID), data=dat, family = binomial, nAGQ = 2))
summary(glmer(outcome~trt*period+(1|ID), data=dat, family = binomial, nAGQ = 5))
summary(glmer(outcome~trt*period+(1|ID), data=dat, family = binomial, nAGQ = 25))
summary(glmer(outcome~trt*period+(1|ID), data=dat, family = binomial, nAGQ = 50))

###############
# Indonesia
dat = read.table('Indonesia.txt',header=T)
dat[1:3,]

table(dat$id)
table(table(dat$id))

library(visdat)
vis_miss(dat) 


### GLM without over-dispersion
fit.glm = glm (response~time+sex+vit+age,family=binomial,data=dat)

### GLM with dispersion  ("quasi" option)
fit.glmquasi = glm (response~time+sex+vit+age,family=quasibinomial, data=dat)


fit.ind = gee (response~time+sex+vit+age, id=id, family=binomial, 
               corstr="independence", data=dat)


fit.uns = gee (response~time+sex+vit+age, id=id, family=binomial, 
               corstr="unstructured", data=dat)
summary(fit.uns)
# NOTE: Data must be correctly sorted for these to be meaningful


fit.exch = gee (response~time+sex+vit+age, id=id, family=binomial, 
                corstr="exchangeable", data=dat)

fit.ar1 = gee (response~time+sex+vit+age, id=id, family=binomial, 
               corstr="AR-M", Mv = 1, data=dat)

fit.ar3 = gee (response~time+sex+vit+age, id=id, family=binomial, 
               corstr="AR-M", Mv = 3, data=dat)
summary(fit.ar3)

fit.re = glmer (response~time+sex+vit+age + (1|id), family=binomial, data=dat)
summary(fit.re)

fit.re = glmer (response~time+sex+vit+age + (1|id), family=binomial, data=dat, nAGQ = 25)
summary(fit.re)
# results similar to without nAGQ, but not identical

### Extract correlation matrix
fit.uns$working.correlation
fit.exch$working.correlation
fit.ar1$working.correlation
# \theta_t = \phi theta_{t-1} + \epsilon_t

summary(fit.glm)
summary(fit.glmquasi)
summary(fit.ind)
summary(fit.uns)
summary(fit.exch)
summary(fit.ar1)
summary(fit.ar3)
summary(fit.re)


# CI for odds ratio for vitamin A deficiency in glmer:
exp(0.604)
exp(0.604-1.96*0.438)
exp(0.604+1.96*0.438)
##########################
#########################

# example of how conditional interpretation impacts the coefficients
# here we simulate the CONDITIONAL model with random effects and 
# examine what happens in GEE estimate, where GEE uses a marginal model.

# Poisson:
set.seed(1234)
nsubjects = 1000
nreps=5
beta0 = 0.5 # baseline counts
beta1 = 3 # make this 0 to relate to mean of y

tau = 1
ids = c(1:nsubjects)%x%rep(1,nreps)
randomeffects1 = rnorm(nsubjects,0,tau)%x%rep(1,nreps)
x1 = rnorm(nsubjects*nreps)

logEy = beta0+beta1*x1+randomeffects1
exp(logEy)
y = rpois(n = length(logEy),lambda = exp(logEy))
plot(y~exp(logEy))

# fit gee with exchangeable correlation
model.gee = gee(y~x1,id = ids,family=poisson,corstr = 'exchangeable')
summary(model.gee)

#when beta1=0
mean(y)
exp(tau^2/2)*exp(beta0) # mean of log normal; intercept should correspond to the log of this
tau^2/2+beta0 # the intercept should be approximately equal to this
coef(model.gee)[1]


# in particular, it does not approximate beta0!!!
summary(model.gee)
# for poisson, beta1 is fine.

model.glmer = glmer(y~x1+(1|ids),family=poisson)
# intercepts and slopes correspond:
summary(model.glmer)






### binomial with logistic:
set.seed(1235)
nsubjects = 1000
nreps=5
beta0 = 1 
beta1 = 3

tau = 2
ids = c(1:nsubjects)%x%rep(1,nreps)
randomeffects1 = rnorm(nsubjects,0,tau)%x%rep(1,nreps)
x1 = rnorm(nsubjects*nreps)

logitEy = beta0+beta1*x1+randomeffects1
y = rbinom(n = length(logitEy),size=1,p = exp(logitEy)/(1+exp(logitEy)))

# fit a marginal model:
model.gee = gee(y~x1,id=ids,family=binomial, corstr = 'exchangeable')
coef(model.gee) # coefficients and slopes do not approximate simulated values
summary(model.gee) #direction of coefficient in truth and gee are in agreement, but values are very different.


# fit glmm:
model.glmer = lme4::glmer(y~x1+(1|ids),family=binomial)
# intercepts and slopes correspond to the truth
summary(model.glmer)

# NOTE on selecting working correlation structure:
model.unstructured = gee(y~x1,id=ids,family=binomial, corstr = 'unstructured')
summary(model.unstructured) # qualitatitively assess working correlation assumption



## NOTES on fitting AR working correlation structure:
model.gee = gee(y~x1,id=ids,family=binomial, corstr = 'AR-M')
coef(model.gee) # coefficients and slopes do not approximate simulated values
summary(model.gee) #direction of coefficient in truth and gee are in agreement, but values are very different.
# with no missing data, estimates 5 x 5 correlation matrix

# gee does not like missing data with AR-M:
nalist = sample(1:length(y),10)
nax1=x1
nax1[nalist] = NA
gee(y~nax1,id=ids,family=binomial, corstr='AR-M')
# for some reason produces a 4x4 correlation matrix

nalist = sample(1:length(y),1000)
nax1=x1
nax1[nalist] = NA
gee(y~nax1,id=ids,family=binomial, corstr='AR-M')
naids = ids
naids[nalist] = NA
table(table(naids)) # count number of observations for each ID
# if any of the groups contains just one measurement, produces a fatal error:(

# geepack will work... (with ar1 structure)
geepack::geeglm(y~nax1,id=ids,family=binomial, corstr='ar1')
# geepack has its own nuances: pattern of missingness must correspond
nay = y
nay[nalist] = NA
head(nay)

model.geepack = geepack::geeglm(nay~nax1,id=naids,family=binomial, corstr='ar1')
summary(model.geepack)
  
# This fit the model, but how does it |j-j'|? Is it correctly obtaining the indices for the calculation of the correlations?

