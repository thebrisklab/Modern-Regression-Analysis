library(MCMCpack)
set.seed(1234)
n = 100
X = 5*runif(n)

beta0 = 3
beta1 = 1.5 #value used in lecture slides
#beta1 = -0.05 # change to a number near zero for demonstration on transforming quantiles
# see notes at line 76

sigmasq = 1

Y = beta0+beta1*X+rnorm(n)
par(mar=c(2,2,1,1))
plot(Y~X,col='blue',pch=20)

?MCMCregress

fit = MCMCregress (Y~X, b0 = c(0,0), B0 = 0.001^2, c0=0.01, d0=0.01) 
plot(fit)

class(fit)
summary(fit)

class(fit)
?summary.mcmc


# Compare to frequentist:
fit.lm = lm(Y~X)
summary(fit.lm)
temp = vcov(fit.lm)
coef(fit.lm)[2] - 1.96*sqrt(temp[2,2])
coef(fit.lm)[2] + 1.96*sqrt(temp[2,2])

# Extract posterior sample:
post.samp = as.matrix(fit)
dim(post.samp)
post.samp[1:3,]

cor(post.samp)
pairs(post.samp,pch=".",col=4)

# compare to correlation between fixed effects
diag(1/sqrt(diag(temp)))%*%temp%*%diag(1/sqrt(diag(temp)))

  # Just for fun: centering removes correlation...
  centerX = X - mean(X)
  fit.lm = lm(Y~centerX)
  summary(fit.lm)
  temp = vcov(fit.lm)
  # correlation between fixed effects
  diag(1/sqrt(diag(temp)))%*%temp%*%diag(1/sqrt(diag(temp)))
  # intercept is now the effect at average X; 
  # intercept and centerX uncorrelated



library(GGally)
library(ggplot2)
ggpairs(data.frame(post.samp))


####
# Calculating means and credible intervals for transformed data are easy:
new.samp = post.samp[,1] + 2*post.samp[,2]
mean(new.samp) ###Point estimate
quantile(new.samp, c(.025, .975)) ### 95% posterior interval
par(mfrow=c(1,3)); hist(post.samp[,1]); hist(post.samp[,2]); hist(new.samp)
# Example with a non-linear, non-monotonic function: 
new.samp = exp(post.samp[,2]) - post.samp[,2]
mean (new.samp)
quantile (new.samp, c(0.025, .975)) # correct way to calcualte central 95% cred int
  
  # this is incorrect:
  exp(quantile(post.samp[,2],0.025)) - quantile(post.samp[,2],0.025)
  # if there are negative and positive values of post.samp[,2], these approaches will differ; 
  # change to beta1=-0.05 to see this

par(mfrow=c(1,2)); hist(post.samp[,2]); hist(new.samp)
  

# Transformations are really easy to do with a posterior sample!
# in the frequentist world, classic approach is to
# use the delta method



# another example:
# correct:
mean(exp(10*post.samp[,2]))
  # incorrect:
  exp(10*mean(post.samp[,2]))





# Test whether the ratio of the slope to intercept is greater than 1/2:
new.samp = post.samp[,2]/post.samp[,1]
# Hypothesis test:
# ratio of beta1/beta0 is greater than 0.5
mean ( new.samp > 0.5)

# probability that beta1 is greater than 0:
mean(post.samp[,2]>0)
# replaces a 1 - p-value; can directly test rather than use null
dev.off()
hist(new.samp)
abline(v=0.5,col='red')

# Posterior Predictive Distribution
# incorporates uncertainty in sigma2:

# Example: consider a subject with x = 2:
### Posterior samples of the predictive mean
new.mean = post.samp[,1] + 2*post.samp[,2]
### Generate normal random variables with different standard deviation.
# NOTE: Here a different sigma is used for every realization:
new.samp = rnorm (length(new.mean), new.mean, sqrt(post.samp[,3]))
mean(new.samp)
quantile(new.samp, c(.025, .975))
# generating a new epsilon_i for each sigma^{(k)}
par(mfrow=c(1,2)); hist(new.mean,xlim=c(3,10)); hist(new.samp)


# Effects of more MCMC samples:
fit = MCMCregress (Y~X,b0 = c(0,0), B0 = 0.001^2, c0=0.01, d0=0.01, mcmc = 100000)
summary(fit)


# Too few, also see effects of burnin:
### Beta starting values at (10,10)
### Run for 50 iterations with no burn-in
fit = MCMCregress (Y~X, b0 = c(0,0), B0 = 0.001^2, c0=0.01, d0=0.01,burnin = 0, mcmc = 50, beta.start = c(10,10) )
plot (fit)




### Beta starting values at (10,10)
### Run for 1000 iterations with no burn-in
### Non-convergence appears for the first few samples.
fit = MCMCregress (Y~X, b0 = c(0,0), B0 = 0.001^2, c0=0.01, d0=0.01,burnin = 0, mcmc = 1000, beta.start = c(10,10) ) 
plot (fit)


### Beta starting values at (10,10)
### Run for 1000 iterations with 100 burn-in samples
### Convergence Okay!
fit = MCMCregress (Y~X, b0 = c(0,0), B0 = 0.001^2, c0=0.01, d0=0.01,burnin = 100, mcmc = 1000, beta.start = c(10,10) ) 
plot (fit)


