library (lme4)
library (MCMCpack)


riwish(v = 2,S = diag(c(1,1)))
riwish(v = 20,S = diag(c(1,1)))
riwish(v = 200,S = diag(c(1,1)))

riwish(v = 2,S = 2*diag(c(1,1)))
riwish(v = 20,S = 20*diag(c(1,1)))
riwish(v = 200,S = 200*diag(c(1,1)))

# univariate case:
dinvgamma(2,shape=2,scale=2)
diwish(2,v = 4,S = 4)

dinvgamma(5,shape=2,scale=2)
diwish(5,v = 4,S = 4)


setwd("~/Dropbox/EmoryCourses/BIOS_526/Materials_BRisk_2020/Data")
dat = read.csv('pig.csv')[,2:4]
head(dat)

fit.lmer = lmer (weight~weeks + (1|id), data = dat)
summary(fit.lmer)

# one approach: set r to q, i.e., number of random components.
# then set R equal to lmer estimate.
# Since r is so small, this is an uninformative prior.


fit = MCMChregress (fixed = weight~weeks, random = ~ 1, group = "id", r = 1, R=15.1, data = dat)

post.samp = as.data.frame (fit$mcmc)
dim(post.samp)
# you get an mcmc sample for every piglet!
head(post.samp[,1:4])

post.samp[1:3, 49:53]



beta0 = post.samp[["beta.(Intercept)"]]
beta1 = post.samp[["beta.weeks"]]
tau2 = post.samp[["VCV.(Intercept).(Intercept)"]]
sigma2 = post.samp[["sigma2"]]

par (mfrow = c(2,2))
plot (beta0, type = "l")
plot (beta1, type = "l")
plot (tau2, type = "l")
plot (sigma2, type = "l")

par (mfrow = c(2,2))
hist (beta0); abline (v = mean (beta0), col =2 , lwd = 2)
abline (v = quantile(beta0,c(.025, .975)), col =4 , lwd = 2)
hist (beta1); abline (v = mean (beta1), col =2 , lwd = 2)
abline (v = quantile(beta1,c(.025, .975)), col =4 , lwd = 2)
hist (tau2); abline (v = mean (tau2), col =2 , lwd = 2)
abline (v = quantile(tau2,c(.025, .975)), col =4 , lwd = 2)
hist (sigma2); abline (v = mean (sigma2), col =2 , lwd = 2)
abline (v = quantile(sigma2,c(.025, .975)), col =4 , lwd = 2)

## Compare estimates:
# Note: object is different from MCMCregress
summary(fit$mcmc)
summary(fit$mcmc[,c(1:2,51,52)])
summary(fit.lmer)



##Intraclass Corr
icc = tau2/(tau2+sigma2)
quantile (icc, c(0.025, .5, .975))
hist (icc, main = "Intraclass Correlation"); abline (v = mean (icc), col =2 , lwd = 2)
abline (v = quantile(icc,c(.025, .975)), col =4 , lwd = 2)

##Extract the random intercepts
names (post.samp)
pig1 = post.samp[["b.(Intercept).1"]]
quantile (pig1, c(0.025, .5, .975))
pig2 = post.samp[["b.(Intercept).2"]]
quantile (pig2, c(0.025, .5, .975))

par (mfrow = c(1,2))
hist (pig1); hist (pig2)
table (pig2 > pig1)
par (mfrow = c(1,2))
plot (pig1, pig2, type = "p", col = 4, xlab = "Random Intercept for Pig 1", ylab = "Random Intercept for Pig 2", main = "Posterior Samples")
plot (pig1, beta0, type = "p", col = 4, xlab = "Random Intercept for Pig 1", ylab = "Fixed Effect Intercept", main = "Posterior Samples")

plot (pig2, beta0, type = "p", col = 4, xlab = "Random Intercept for Pig 1", ylab = "Fixed Effect Intercept", main = "Posterior Samples")



fit2 = MCMChregress (fixed = weight~weeks, random = ~ 1, group = "id", r = 10, R=15.1, data = dat)
fit3 = MCMChregress (fixed = weight~weeks, random = ~ 1, group = "id", r = 100, R=15.1, data = dat)
fit4 = MCMChregress (fixed = weight~weeks, random = ~ 1, group = "id", r = 1000, R=15.1, data = dat)

fit5 = MCMChregress (fixed = weight~weeks, random = ~ 1, group = "id", r = 1, R=5, data = dat)
fit6 = MCMChregress (fixed = weight~weeks, random = ~ 1, group = "id", r = 1000, R=5, data = dat)

fit7 = MCMChregress (fixed = weight~weeks, random = ~ 1, group = "id", r = 1, R=15.1, nu = 100000,  delta = (100000*4.39)/2, data = dat)


#### Random slope ####

fit.lmer = lmer (weight~weeks + (weeks |id), data = dat)

R.prior = matrix ( c(7, 0, 0, 0.4), ncol = 2)
fit = MCMChregress (fixed = weight~weeks, random = ~ 1+weeks, group = "id", r = 2, R=R.prior, data = dat)
dim(fit$mcmc)

summary(fit$mcmc[,c(1,2,99:103)])



