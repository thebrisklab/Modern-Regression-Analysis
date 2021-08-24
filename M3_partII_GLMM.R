### Module 3: 
### Part II: glmms ###

setwd('~/Dropbox/EmoryCourses/BIOS_526/Materials_BRisk_2020/Data')

cbv = read.table ("2by2.txt", header = T)
cbv[1:5,]
table(cbv$group,cbv$period)
# group 1: corresponds to BA
# group 2: corresponds to AB

# reminder of logistic regression and overall
# proportion of "successes":
fit0 = glm(cbv$outcome~1,family=binomial(link='logit'))
summary(fit0)
1/(1+exp(-coef(fit0)))
mean(cbv$outcome)



#################


fit1 = glm (cbv$outcome~cbv$trt, family=binomial(link='logit'))
fit2 = glm (cbv$outcome~cbv$trt+cbv$period, family=binomial(link='logit'))
fit3 = glm (cbv$outcome~cbv$trt*cbv$period, family=binomial(link='logit'))

summary(fit1)
summary(fit2)
summary(fit3)


# calculate confidence intervals:
# note: exp is monotonic so transforming quantiles of a random variable is equivalent
# to the quantiles of the transformed random variable
# hence we can exponentiate CIs:
exp(1.11 - 1.96*0.57)
exp(1.11 + 1.96*0.57)

# We can not transform SEs. Estimating variances of transformed variable is more complex (one approach is the delta method).



library (lme4)

fit4 = glmer (outcome~trt*period+(1|ID), family=binomial(link='logit'), data = cbv)
summary(fit4)
# NOTE: Does not issue a convergence error.
# However, the variance of the ID is very large -- indication of an issue.

# Also note that by default, returns p-values (unlike lmer)


fit5 = glmer (outcome~trt*period+(1|ID), family=binomial(link='logit'), data = cbv, nAGQ = 2)
summary(fit5)
# Changes a lot -- this is troubling, particularly given there was no indication that the first solution was not at the optimum.
# May be related to only having 


fit6 = glmer (outcome~trt*period+(1|ID), family=binomial(link='logit'), data = cbv, nAGQ = 25)
summary(fit6)
# Still changing. Issues persist. 
# Random effects variance is large. 

fit7 = glmer (outcome~trt*period+(1|ID), family=binomial(link='logit'), data = cbv, nAGQ = 100)
summary(fit7)
# Seems to have stabilized. 

# random effects:
hist(ranef(fit7)$ID[,1])

coef(fit7)

plot(fit7)
# (meaningless for bernoulli data)


AIC(fit3) # fully crossed model with no random effects
AIC(fit7) # fully crossed model with random effects


# I speculate the issue may be related to having only
# two observations per subject. 
# It may be helpful to use GEEs in situations such as these (where the mle estimates seem numerically unstable). We will return to this topic later.


########################
########################

# example of how conditional interpretation impacts the coefficients
# here we simulate the CONDITIONAL model with random effects and 
# examine what happens in GLM estimate, where GLM uses a marginal model.
# (Note the inference in the GLM is incorrect due to correlation, but this
# is actually a different issue. Here we concern ourselves with how the interpretation
# of the betas.)
# simulate subjects
### binomial with logistic:
nsubjects = 2000
nrepsmax=10
beta0 = -1 
beta1 = 0 # you can experiment with values of beta1 as well. start with beta1=0
tau = 2

ids=NULL
randomeffects_short=NULL
randomeffects1=NULL
for (i in 1:nsubjects) { 
  r_i = rbinom(1,size=nrepsmax,prob=0.5) # pick some number of observations for ith subject
  ids = c(ids,rep(i,r_i))
  temp = rnorm(1,0,tau)
  #randomeffects_short=c(randomeffects_short,temp)
  randomeffects1=c(randomeffects1,rep(temp,r_i))
}

x1 = rnorm(length(ids)) # make some data for the covariate

# simulate the mixed model:
logitEy = beta0+beta1*x1+randomeffects1
y = rbinom(n = length(logitEy),size=1,p = exp(logitEy)/(1+exp(logitEy)))

# fit a marginal model:
model.glm = glm(y~x1,family=binomial)
# model.glm = glm(y~1,family=binomial)
coef(model.glm) # coefficients and slopes do not approximate simulated values
summary(model.glm)
exp(coef(model.glm)[1])/(1+exp(coef(model.glm)[1]))
#note:
# if beta1=0, the back-transformed intercept is equal to the mean of y: 
# (exactly equal if you don't estimate beta_1)
mean(y)

# fit glmm:
model.glmer = glmer(y~x1+(1|ids),family=binomial)
# intercepts and slopes roughly correspond to the truth:
summary(model.glmer)
exp(fixef(model.glmer)[1])/(1+exp(fixef(model.glmer))[1])
# compare this to mean(y) if beta=0, higher than mean(y)
mean(y)


  #########################
########################
## Poisson GLMM
cancer = read.table ("cancer.txt", header = T)

##Subset to 1980 + 
cancer = subset (cancer, year >= 13)

##Recode
cancer$year = cancer$year - 12 # year=0 is 1979
cancer$race[ cancer$race == 2] = 0 # simplify to white and non-white, 1=white
cancer$sex = cancer$sex-1 # female=1

fit = glmer (death~sex*race+year + (1|county), family = poisson, data = cancer)
summary (fit)


# check convergence:
fit.check = glmer (death~sex*race+year + (1|county), family = poisson, data = cancer,nAGQ = 25)
summary (fit.check)
# the original optimization is fine

# check goodness of fit:

pdf('~/Dropbox/EmoryCourses/BIOS_526/Materials_BRisk_2020/M3-GLM and GLMM/M3-GLMM/cancer_poisson_nooffset.pdf')
plot(fit)
dev.off()

# also see
# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#overdispersion
# caveats: expected counts per obs should be > 5
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(fit)



# The plot of the pearson residuals vs fitted looks terrible
# severe lack of fit


# compare to glm:  
fit.poisson.glm = glm(death~sex*race+year,family=poisson,data=cancer)
summary(fit.poisson.glm)


##########################
# Offsets!!!
# include offset: makes deaths per capita
# same as fixing slope at 1
# In model without offset, population differences lead to larger variance of random effects



par (mfrow = c(2,1))
hist (log(cancer$death), main = "Log Lung Cancer Deaths")
hist (log(cancer$pop), main = "Log Population")

cancer$logpop = log (cancer$pop)
fit.offset = glmer (death~offset(logpop) + sex*race+year + (1|county), family = poisson, data = cancer)
summary(fit.offset)


  # check optimizer:
  fit.check = glmer (death~offset(logpop) + sex*race+year + (1|county), family = poisson, data = cancer, nAGQ=25)
  summary(fit.check)
  # defaults appear to have converged

pdf('~/Dropbox/EmoryCourses/BIOS_526/Materials_BRisk_2020/M3-GLM and GLMM/M3-GLMM/cancer_poisson_offset.pdf')  
plot(fit.offset)
dev.off()
# The plot of the residuals is somewhat improved, but issues persist

overdisp_fun(fit.offset)

# random effects:
pdf('~/Dropbox/EmoryCourses/BIOS_526/Materials_BRisk_2020/M3-GLM and GLMM/M3-GLMM/cancer_randomeffects.pdf')
par(mfrow=c(2,2))
hist(ranef(fit)$county[,1],main='No offset')
hist(ranef(fit.offset)$county[,1],main='Offset')

hist(exp(ranef(fit)$county[,1]),breaks=30,main='No offset')
hist(exp(ranef(fit.offset)$county[,1]),breaks=30,main='Offset')
dev.off()


  # aside:
  # We will address overdispersion later with GEEs
  library(gee)
  fit.gee = gee(death~offset(logpop)+sex*race+year, id=county, data=cancer, corstr="exchangeable",family = 'poisson')
  summary(fit.gee)
  

par (mfrow = c(2,2))
hist (log(cancer$pop[cancer$race==1]), main = "(White) Log Lung Cancer Populations", xlab = "")
hist (log(cancer$pop[cancer$race==0]), main = "(Non-White) Log Lung Cancer Populations", xlab = "")

hist (log(cancer$death[cancer$race==1]), main = "(White) Log Lung Cancer Deaths", xlab = "")
hist (log(cancer$death[cancer$race==0]), main = "(Non-White) Log Lung Cancer Populations", xlab = "")

#############################
# Simulations examining marginal versus conditional in Poisson:
nsubjects = 2000
nreps=5
beta0 = 0.25 # baseline log counts
beta1 = 0.5 # make this 0 to examine how glm beta0 relates to mean of y

tau = 0.5
ids = c(1:nsubjects)%x%rep(1,nreps)
randomeffects1 = rnorm(nsubjects,0,tau)%x%rep(1,nreps)
x1 = rnorm(nsubjects*nreps)

logEy = beta0+beta1*x1+randomeffects1
y = rpois(n = length(logEy),lambda = exp(logEy))

model.glm = glm(y~x1,family=poisson)
summary(model.glm)
plot(model.glm)



#when beta1=0, the quantities are equal
mean(y)
exp(coef(model.glm)[1])

# for poisson, we can analytically calculate
# the relationship between the intercept on the log
# scale of the glmm and the mean count of the marginal
# model. It is related to the random effect variance:
# for poisson, we have
# E exp(\beta_0+\theta_i) = exp(\beta_0)*E exp(\theta_i})
# now exp(\theta_i) is a log-normal random variable
# with zero mean and var \tau^2, so
# we can look up its expectation in wiki.
# it is exp(\tau^2/2)

# thus for beta1=0, the mean(y) is approximately equal to
exp(tau^2/2)*exp(beta0) 

# in particular, the intercept
# does not approximate beta0
coef(model.glm)[1]
# instead, it approximates 
#log(exp(tau^2/2)*exp(beta0)), i.e.,
tau^2/2+beta0 #(this holds for beta1 not equal to zero too)

# for poisson, the betas in the marginal model
# still roughly approximate the betas in the conditional model
coef(model.glm)[2]

model.glmer = glmer(y~x1+(1|ids),family=poisson)
# intercepts and slopes correspond:
summary(model.glmer)

plot(model.glmer)
overdisp_fun(model.glmer)

############################
y2 = 2*y
model.glmer2 = glmer(y2~x1+(1|ids),family=poisson)
# intercepts and slopes correspond:
summary(model.glmer2)

plot(model.glmer2)
overdisp_fun(model.glmer2)

