

prob = seq(0,1,length=100)
logitprob = log(prob/(1-prob))
plot(logitprob~prob,type='l',col='blue',ylim=c(-8,8),ylab='logit(y)',xlab='y')

y=seq(-8,8,length=100)
logisticy = exp(y)/(1+exp(y))
plot(logisticy~y,type='l',col='red',xlab='x',ylab='exp(x)/(1+exp(x))')

### Change in intercept, logit
x = seq (-10, 10, by = .1)

b0 = 0; b1 = 1; eta = b0 + b1*x
mu = exp(eta)/(1 + exp(eta))
plot (mu~x, type = "l", lwd = 3, xlab = "Predictor X", ylab = "E[Y_i]")

b0 = 2; b1 = 1; eta = b0 + b1*x
mu = exp(eta)/(1 + exp(eta))
lines (mu~x, type = "l", col = 2, lwd = 3)

b0 = -2; b1 = 1; eta = b0 + b1*x
mu = exp(eta)/(1 + exp(eta))
lines (mu~x, type = "l", col = 4, lwd = 3)

b0 = -3; b1 = 1; eta = b0 + b1*x
mu = exp(eta)/(1 + exp(eta))
lines (mu~x, type = "l", col = 3, lwd = 3)

abline(v = 0, col = "grey", lwd = 2, lty = 4)
legend ("bottomright", c("beta0 = 0", "beta0 = 2", "beta0 = -2", "beta0 = -3"), lwd = 3, col = c(1,2,4,3), cex = 1.4, bty = "n")


### Changes in slope, logit
x = seq (-10, 10, by = .1)

b0 = 0; b1 = 0; eta = b0 + b1*x
mu = exp(eta)/(1 + exp(eta))
plot (mu~x, type = "l", lwd = 3, xlab = "Predictor X", ylab = "E[Y_i]", ylim = c(0, 1) )

b0 = 0; b1 = 1; eta = b0 + b1*x
mu = exp(eta)/(1 + exp(eta))
lines (mu~x, type = "l", lwd = 3, col = 2)

b0 = 0; b1 = 2; eta = b0 + b1*x
mu = exp(eta)/(1 + exp(eta))
lines (mu~x, type = "l", col = 4, lwd = 3)

b0 = 0; b1 = -1; eta = b0 + b1*x
mu = exp(eta)/(1 + exp(eta))
lines (mu~x, type = "l", col = 3, lwd = 3)

abline(v = 0, col = "grey", lwd = 2, lty = 4)
legend ("bottomright", c("beta1 = 0", "beta1 = 1", "beta1 = 2", "beta1 = -1"), lwd = 3, col = c(1,2,4,3), cex = 1.4, bty = "n")

# Changes in intercept: probit
x = seq (-10, 10, by = .1)

b0 = 0; b1 = 1; eta = b0 + b1*x
mu = pnorm (eta)
plot (mu~x, type = "l", lwd = 3, xlab = "Predictor X", ylab = "E[Y_i]")

b0 = 2; b1 = 1; eta = b0 + b1*x
mu =  pnorm (eta)
lines (mu~x, type = "l", col = 2, lwd = 3)

b0 = -2; b1 = 1; eta = b0 + b1*x
mu = pnorm (eta)
lines (mu~x, type = "l", col = 4, lwd = 3)

b0 = -3; b1 = 1; eta = b0 + b1*x
mu = pnorm (eta)
lines (mu~x, type = "l", col = 3, lwd = 3)

abline(v = 0, col = "grey", lwd = 2, lty = 4)
legend ("bottomright", c("beta0 = 0", "beta0 = 2", "beta0 = -2", "beta0 = -3"), lwd = 3, col = c(1,2,4,3), cex = 1.4, bty = "n")




#  Probit and logit

b0 = 0; b1 = 1; eta = b0 + b1*x
mu = pnorm (eta)
plot (mu~x, type = "l", lwd = 3, xlab = "Predictor X", ylab = "E[Y_i]")

b0 = 2; b1 = 1; eta = b0 + b1*x
mu =  pnorm (eta)
lines (mu~x, type = "l", col = 2, lwd = 3)

b0 = -2; b1 = 1; eta = b0 + b1*x
mu = pnorm (eta)
lines (mu~x, type = "l", col = 4, lwd = 3)

b0 = -3; b1 = 1; eta = b0 + b1*x
mu = pnorm (eta)
lines (mu~x, type = "l", col = 3, lwd = 3)

b0 = 0; b1 = 1; eta = b0 + b1*x
mu = exp(eta)/(1 + exp(eta))
lines (mu~x, type = "l", lwd = 3, lty=2 )

b0 = 2; b1 = 1; eta = b0 + b1*x
mu = exp(eta)/(1 + exp(eta))
lines (mu~x, type = "l", lwd = 3, col = 2, lty=2)

b0 = -2; b1 = 1; eta = b0 + b1*x
mu = exp(eta)/(1 + exp(eta))
lines (mu~x, type = "l", col = 4, lwd = 3, lty=2)

b0 = -3; b1 = 1; eta = b0 + b1*x
mu = exp(eta)/(1 + exp(eta))
lines (mu~x, type = "l", col = 3, lwd = 3,lty=2)

abline(v = 0, col = "grey", lwd = 2, lty = 4)
legend ("bottomright", c("beta0 = 0", "beta0 = 2", "beta0 = -2", "beta0 = -3","logit","probit"), lwd = 3, col = c(1,2,4,3,1,1), lty=c(1,1,1,1,2,1),cex = 1.4, bty = "n")

#dev.off()


# Probit slopes
x = seq (-10, 10, by = .1)

b0 = 0; b1 = 0; eta = b0 + b1*x
mu = exp(eta)/(1 + exp(eta))
plot (mu~x, type = "l", lwd = 3, xlab = "Predictor X", ylab = "E[Y_i]", ylim = c(0, 1) )

b0 = 0; b1 = 1; eta = b0 + b1*x
mu = exp(eta)/(1 + exp(eta))
lines (mu~x, type = "l", lwd = 3, col = 2)

b0 = 0; b1 = 2; eta = b0 + b1*x
mu = exp(eta)/(1 + exp(eta))
lines (mu~x, type = "l", col = 4, lwd = 3)

b0 = 0; b1 = -1; eta = b0 + b1*x
mu = exp(eta)/(1 + exp(eta))
lines (mu~x, type = "l", col = 3, lwd = 3)

abline(v = 0, col = "grey", lwd = 2, lty = 4)
legend ("bottomright", c("beta1 = 0", "beta1 = 1", "beta1 = 2", "beta1 = -1"), lwd = 3, col = c(1,2,4,3), cex = 1.4, bty = "n")



################
# changes in probability in logit model:

expit = function(x) exp(x)/(1+exp(x))
par(mfrow=c(3,1))

beta = 1
a = function(x) expit(beta*(x+1))-pnorm(beta*x)
curve(a,-4,4,main=expression(paste(beta,"=1")),ylab='Change in E[y]')

beta = 0.5
a = function(x) expit(beta*(x+1))-pnorm(beta*x)
curve(a,-4,4,main=expression(paste(beta,"=0.5")),ylab='Change in E[y]')


beta = -2
a = function(x) expit(beta*(x+1))-pnorm(beta*x)
curve(a,-4,4,main=expression(paste(beta,"=-2")),ylab='Change in E[y]')

#dev.off()


################
# changes in probability in probit model:

#pdf(file='~/Dropbox/EmoryCourses/BIOS_526/Materials_BRisk/Lectures/M7-GLM/Probit_unitincrease.pdf')
par(mfrow=c(3,1))

beta = 1
a = function(x) pnorm(beta*(x+1))-pnorm(beta*x)
curve(a,-4,4,main=expression(paste(beta,"=1")),ylab='Change in E[y]')

beta = 0.5
a = function(x) pnorm(beta*(x+1))-pnorm(beta*x)
curve(a,-4,4,main=expression(paste(beta,"=0.5")),ylab='Change in E[y]')


beta = -2
a = function(x) pnorm(beta*(x+1))-pnorm(beta*x)
curve(a,-4,4,main=expression(paste(beta,"=-2")),ylab='Change in E[y]')

#dev.off()




###########
# Logistic regression
# note: this dataset differs from the one used to generate the lecture slides, 
# but the results are in general similar:
load('./Data/PTB.RData')
dat$age = dat$age - 25 # change baseline to mother's at age 25
fit = glm(ptb~age + male+tobacco, data = dat, family = binomial(link='logit'))
# Wald statistics:
summary(fit)

anova(fit)
library(car)
# LRTs:
Anova(fit)

# car::Anova is equivalent to LRT for one variable at a time:
fitnotobacco = glm(ptb~age + male, data = dat, family = binomial(link='logit'))
anova(fitnotobacco,fit,test = 'LRT')

# Probit model:
fit2 = glm(ptb~age + male+tobacco, data = dat, family = binomial(link='probit'))
summary(fit2)

# probit versus logit: here, AIC doesn't distinguish
AIC(fit)
AIC(fit2)
# in practice, logit is more common.


# inspect linearity of age:
ptb.age = tapply(dat$ptb, dat$age, mean)
plot(log(ptb.age/(1-ptb.age))~c(16:43), xlab="Maternal Age at Delivery", ylab="Log odds of Preterm Birth", pch=16)


# conduct a goodness of fit test:

# goodness of fit test:
# for this dataset, deviance test is not helpful
# In particular, when the number of trials for each subject is equal to 1, 
# the deviance does not provide an assessment of the goodness of fit.
# A nice explanation is in Simon Sheather, "A Modern Approach to Regression with R"
#with(fit, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail=FALSE)))

#simplefit = glm(ptb~1, data = dat, family = binomial(link='logit'))
#with(simplefit, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail=FALSE)))
# residual deviance does not help
# You can use a Hosmer-Lemeshow test, but we will not discuss it here.

AIC(fit)
AIC(simplefit)
# AIC can be helpful


  
  

# This will be part of module 5:
  library(mgcv)
  fit3 = gam(ptb~s(age,k=-1) + male + tobacco, data = dat, family = binomial(link='logit'))
  summary(fit3)
  plot(fit3)



#######
# Poisson regression
# Information on colony data:
# 
#  A study investigated the occurrence of natural (spontaneous) mutation in wild-type E. coli in the presence of an antibiotic, novobiocin, which alters the integrity of bacterial DNA. The dataset contains three variables: (1) Colony, the number of ampicillin-resistant mutant colonies; (2) Conc, the concentration of novobiocin; (3) Media, the type of media used for bacterial growth. The experiment involved two media preparations (LB and M9), 5 concentrations of novobiocin, and 100 replicates for each media-concentration combination. TNTC (too numerous to count) were recorded when the number of colonies exceeded 300.

 
colonydata = read.csv('./Data/colony.csv')




head(colonydata)
sum(colonydata$Colony=='TNTC')
colonydata = colonydata[colonydata$Colony!='TNTC',]
sum(colonydata$Colony=='TNTC')

colonydata$Colony_numeric = as.numeric(as.character(colonydata$Colony))
colonydata$ConcByMedia = interaction(colonydata$Conc,colonydata$Media)


lm_colony = lm(Colony_numeric~factor(Conc)*Media,data=colonydata)
summary(lm_colony)
#pdf(file='~/Dropbox/EmoryCourses/BIOS_526/Materials_BRisk_2020/M3-GLM and GLMM/M3-GLM/colony_lm.pdf')
par(mfrow=c(2,2))
plot(lm_colony)
#dev.off()
# what issues do you see?

glm_colony = glm(Colony_numeric~factor(Conc)*Media,data=colonydata,family = "poisson")
summary(glm_colony)
#pdf(file='~/Dropbox/EmoryCourses/BIOS_526/Materials_BRisk_2020/M3-GLM and GLMM/M3-GLM/colony_glm.pdf')
par(mfrow=c(2,2))
plot(glm_colony)
#dev.off()
# note how residuals no longer have increasing variance


# what are residuals in glm?
residuals(glm_colony)
# here the residuals are by default the "deviance residuals" and they are standardized
# in the plot function. 
# The definition is a little complicated, e.g., https://www.stats.ox.ac.uk/pub/bdr/IAUL/ModellingLecture5.pdf p.7


 
glm_nomedia = glm(Colony_numeric~factor(Conc),data=colonydata,family = "poisson")

# overall effect of Media:
anova(glm_nomedia,glm_colony,test = 'LRT')

# goodness of fit test based on deviance:
with(glm_colony, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail=FALSE)))
# p>0.05 indicates no significant lack of fit.

# versus:
with(glm_nomedia, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail=FALSE)))
# issue with lack of fit.


# Poisson and overdispersion:
# In practice, I have found the Poisson often fits data poorly. 
# In particular, the variance is often greater than the mean. 
# Test for overdispersion:

#install.packages("AER")
library(AER)
dispersiontest(glm_colony)
# this is a test of the significance of the overdispersion term, which is 
# transformed to follow a standard normal distribution (i.e., z)
# The actual dispersion parameter is compared to 1: represents how 
# much the variances are scaled by

dispersiontest(glm_nomedia)

# the estimated overdispersion parameter will be approximately 1 if the model is a good fit to the data:
glm_colony_quasi = glm(Colony_numeric~factor(Conc)*Media,data=colonydata,family = "quasipoisson")
summary(glm_colony_quasi)


#  get bigger standard errors if overdispersion is violated:
glm_nomedia_quasi = glm(Colony_numeric~factor(Conc),data=colonydata,family = "quasipoisson")
summary(glm_nomedia_quasi)
summary(glm_nomedia)

sqrt(diag(vcov(glm_nomedia)*8.185913))

# This is the idea of how the scale parameter is calculated. This is the "Pearson" estimate in Wood 3.15
sum(((colonydata$Colony_numeric - fitted(glm_nomedia))^2)/fitted(glm_nomedia_quasi))/(glm_nomedia$df.residual)
sum(((colonydata$Colony_numeric - fitted(glm_colony))^2)/fitted(glm_colony))/glm_colony$df.residual


# if there is no lack of fit, quasi doesn't impact:
glm_colony_quasi = glm(Colony_numeric~factor(Conc)*Media,data=colonydata,family = "quasipoisson")
summary(glm_colony_quasi)

