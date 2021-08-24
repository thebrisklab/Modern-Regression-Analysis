#####################
#####################
## Posterior of \mu given known \sigma^2
n=10
sigma2 = 2
x = rnorm (n, 4, sqrt(sigma2))
tau2 = 1
mu0 = 0

post.var = 1/(n/sigma2+1/tau2)
post.mean = post.var*( (n/sigma2)*mean(x) + (1/tau2)*mu0)


x.grid = seq(-3, 7, by = 0.0005)
post.y = dnorm (x.grid, post.mean, sqrt(post.var))
prior.y = dnorm (x.grid, mu0, sqrt(tau2))
samp.y = dnorm(x.grid, mean(x), sqrt(sigma2/n) )

plot (post.y~x.grid, type = "l", xlim = range(x.grid), ylab = "Density", xlab = expression(mu), lwd = 3, cex.lab = 1.4, ylim = c(0, 1.1) )
lines (prior.y~x.grid, type = "l", col = 2, lwd = 3)
lines (samp.y~x.grid, type = "l", col = "blue", lwd = 3)
legend ("topleft", c("Prior", "Posterior", "Sampling"), lwd = 3, col = c(2,1, "blue"), cex=1.5, bty = "n")
abline (v = mean (x), lty = 3, lwd = 3, col = "blue")
text (mean(x), 0.9, paste("Sample Mean = ",round(mean(x),2)), col = "blue", font = 2, pos = 4)
abline (v = post.mean, lty = 3, lwd = 3)
text (post.mean, 1, paste("Posterior Mean = ",round(post.mean,2)), font = 2, pos = 2)


### Varying tau2
par (mfrow = c(2,2))
tau2s = c(0.5, 1, 10,100)
x.grid = seq(-1, 7, by = 0.0005)
for (i in 1:4){

	post.var = 1/(n/sigma2+1/tau2s[i])
	post.mean = post.var*( (n/sigma2)*mean(x) + (1/tau2s[i])*0)
	prior.y = dnorm (x.grid, mu0, sqrt(tau2s[i]))
	post.y = dnorm (x.grid, post.mean, sqrt(post.var))
	samp.y = dnorm(x.grid, mean(x), sqrt(2/n) )

	plot (post.y~x.grid, type = "l", xlim = range(x.grid), ylab = "Density", xlab = expression(mu), lwd = 3, cex.lab = 1.4, ylim = c(0, 1.3) )
	lines (prior.y~x.grid, type = "l", col = 2, lwd = 3)
	lines (samp.y~x.grid, type = "l", col = "blue", lwd = 3)
	legend ("topleft", c("Prior", "Posterior", "Sampling"), lwd = 3, col = c(2,1, "blue"), cex=1, bty = "n")
	abline (v = mean (x), lty = 3, lwd = 3, col = "blue")
	text (mean(x), 0.9, paste("Sample Mean = ",round(mean(x),2)), col = "blue", font = 2, pos = 4)
	abline (v = post.mean, lty = 3, lwd = 3)
	text (post.mean, 1.25, paste("Posterior Mean = ",round(post.mean,2)), font = 2, pos = 2)
	title (main = paste ("Prior Mean = 0;  Prior Variance = ", tau2s[i]) )
}


### Varying Sigma2
par (mfrow = c(2,2))
sigma2s = c(1, 2, 10, 100)
x.grid = seq(-1, 7, by = 0.0005)
for (i in 1:4){

	post.var = 1/(n/sigma2s[i]+1/tau2)
	post.mean = post.var*( (n/sigma2s[i])*mean(x) + (1/tau2)*0)
	prior.y = dnorm (x.grid, mu0, sqrt(tau2))
	post.y = dnorm (x.grid, post.mean, sqrt(post.var))
	samp.y = dnorm(x.grid, mean(x), sqrt(sigma2s[i]/n) )

	plot (post.y~x.grid, type = "l", xlim = range(x.grid), ylab = "Density", xlab = expression(mu), lwd = 3, cex.lab = 1.4, ylim = c(0, 1.5) )
	lines (prior.y~x.grid, type = "l", col = 2, lwd = 3)
	lines (samp.y~x.grid, type = "l", col = "blue", lwd = 3)
	legend ("topleft", c("Prior", "Posterior", "Sampling"), lwd = 3, col = c(2,1, "blue"), cex=1, bty = "n")
	abline (v = mean (x), lty = 3, lwd = 3, col = "blue")
	text (mean(x), 0.9, paste("Sample Mean = ",round(mean(x),2)), col = "blue", font = 2, pos = 4)
	abline (v = post.mean, lty = 3, lwd = 3)
	text (post.mean, 1.4, paste("Posterior Mean = ",round(post.mean,2)), font = 2, pos = 4)
	title (main = paste ("Population Variance =", sigma2s[i]) )
}

### Varying prior means 
tau2s = 1
mu0 = c(0, 1, 2.5, 4.08)
par (mfrow = c(2,2))
sigma2s = 2
x.grid = seq(-1, 7, by = 0.0005)
for (i in 1:4){

	post.var = 1/(n/sigma2+1/tau2)
	post.mean = post.var*( (n/sigma2s)*mean(x) + (1/tau2)*mu0[i])
	prior.y = dnorm (x.grid, mu0[i], sqrt(tau2))
	post.y = dnorm (x.grid, post.mean, sqrt(post.var))
	samp.y = dnorm(x.grid, mean(x), sqrt(sigma2s/n) )

	plot (post.y~x.grid, type = "l", xlim = range(x.grid), ylab = "Density", xlab = expression(mu), lwd = 3, cex.lab = 1.4, ylim = c(0, 1.5) )
	lines (prior.y~x.grid, type = "l", col = 2, lwd = 3)
	lines (samp.y~x.grid, type = "l", col = "blue", lwd = 3)
	legend ("topleft", c("Prior", "Posterior", "Sampling"), lwd = 3, col = c(2,1, "blue"), cex=1, bty = "n")
	abline (v = mean (x), lty = 3, lwd = 3, col = "blue")
	text (mean(x), 0.9, paste("Sample Mean = ",round(mean(x),2)), col = "blue", font = 2, pos = 4)
	abline (v = post.mean, lty = 3, lwd = 3)
	text (post.mean, 1.4, paste("Posterior Mean = ",round(post.mean,2)), font = 2, pos = 4)
	title (main = paste ("Prior Mean =", mu0[i]) )
}


lines (samp.y~x.grid, lty = 3, lwd = 3)
plot (post.y~x.grid, type = "l", xlim = range(x.grid), ylab = "Density", xlab = expression(mu), lwd = 3, cex.lab = 1.4, ylim = c(0, 1.1) )

#####################
#####################
# Posterior of sigma^2 given known \mu
### Invgamma
library (MCMCpack)

x.grid = seq(1, 20, by = .05)
r1 = dinvgamma (x.grid, 20, 100)
r2 = dinvgamma (x.grid, 10, 50)
r3 = dinvgamma (x.grid, 1, 5)
r4 = dinvgamma (x.grid, 0.01, 0.05)


plot (r1~x.grid, type = "l", lwd = 3, xlab = "X", ylab = "Density")
lines (r2~x.grid, type = "l", col = "2", lwd = 3)
lines (r3~x.grid, type = "l", col = "3", lwd = 3)
lines (r4~x.grid, type = "l", col = "4", lwd = 3)

legend ("topright", legend = c("a = 20, b = 100", "a = 10, b = 50", "a = 1, b = 5", "a = 0.01, b = 0.05"), col = 1:4, lwd=3, bty = "n", cex  = 1.5)

y = c(4.2, 6.6, 5.1, 2.0, 2.8, 3.2, 4.7, 4.1, 7.3, 0.8)

rss = sum( (y - 4)^2 )
n= 10
mle = rss/10 # corresponds to the mle estimate of sigma^2


p1 = dinvgamma (x.grid, 10/2 + 20, rss/2 + 100)
p2 = dinvgamma (x.grid, 10/2 + 10, rss/2 + 50)
p3 = dinvgamma (x.grid, 10/2 + 1, rss/2 + 5)
p4 = dinvgamma (x.grid, 10/2 + .01, rss/2 + 0.05)
sampling.distribution = dinvgamma(x.grid,10/2,rss/2)

dev.off()

plot (r1~x.grid, type = "l", lwd = 3, xlab = "X", ylab = "Density", lty = 3, ylim = c(0, .5), xlim = c(1, 10) )
lines (r2~x.grid, type = "l", col = "2", lwd = 3, lty = 3)
lines (r4~x.grid, type = "l", col = "4", lwd = 3, lty = 3)

lines (p1~x.grid, type = "l", col = "1", lwd = 3)
lines (p2~x.grid, type = "l", col = "2", lwd = 3)
lines (p4~x.grid, type = "l", col = "4", lwd = 3)
lines (sampling.distribution~x.grid, type = "l", col = "purple", lwd = 3)

# MLE sampling mean: alpha0 = beta0 = 0
(sampling.mean = (rss/2)/(n/2 - 1)) # corresponds to the expected value of \sigma^2 
# distributed as inverse gamma with alpha=n/2 and beta=rss/2
# with such a small sample, this is different from mle due to -1. 
legend ("topright", legend = c(paste("Prior: ",c("a = 20, b = 100", "a = 10, b = 50", "a = 0.01, b = 0.05")), "Sampling Distribution")
 , col = c(1,2,4, "purple"), lwd=3, lty = c(3,3,3,1), bty = "n", cex  = 1)

legend ("right", legend = c(paste("Posterior Mean: ", c(4.91, 4.85, 4.49)), "Sampling Mean = 4.47"), col = c(1,2,4, "purple"), lwd=3, lty = 1, bty = "n", cex  = 1)

abline(v = mle, col = "forestgreen", lwd = 4, lty = 2)
text(mle, .5, pos = 4, "MLE = 3.57", col = "forestgreen", font = 2)



#### Gibbs

set.seed(123)
n.burnin = 500
n.iter = 5500
y = c(4.2, 6.6, 5.1, 2.0, 2.8, 3.2, 4.7, 4.1, 7.3, 0.8)
# toggle this on to experiment with large n
#y = rnorm(n=1000,mean = 4,sd=2)

n= length(y)

sigmasq.save = rep (NA, n.iter)
mu.save = rep(NA, n.iter)

# hyperparameters for mu:
mu0=0
tausq= 25^2

# hyperparameters for sigmasq:
#a=1000*2 #if these values are large, become informative. prior mean may be approximately equal, but large difference in effects...
#b=1000*1
a=2
b=1
b/(a-1) # prior mean for sigmasq
# note: small values are "uninformative", but still impact estimate for small n

#initial values
mu = 0
sigmasq = 10

for (i in 1:n.iter){
	#update mu
  # calculate conditional dist of mu given sigmasq and data
  # update variance:
  sigmasqk = 1 / (n/sigmasq+1/tausq)
  # update mean:
  meank = (mean(y)/(sigmasq/n) + mu0/tausq)*sigmasqk
	mu = rnorm (1, meank, sqrt(sigmasqk))
	mu.save[i] = mu 

	# update sigmasq
	# calculate conditional dist of sigmasq given mu and data
	RRR = y - mu
	sigmasq = rinvgamma(1,shape=n/2+a,scale=sum(RRR^2)/2 + b) # note: the shape parameter does not change; see slide 33
	sigmasq.save[i] = sigmasq
}

# estimate of posterior mean for mu:
mu.sample = mu.save[(n.burnin+1):n.iter]
sigmasq.sample = sigmasq.save[(n.burnin+1):n.iter]

mean(mu.sample)
mean(y) # compare to mle for mu

# estimate of posterior mean for sigmasq
mean(sigmasq.sample)
sum((y - mean(y))^2)/n # compare to sample variance, even with uninformative priors, will be different when n is small


par (mfrow = c(2,1))
plot (mu.save, type = "l", xlab = "Iteration",  ylab = expression(mu), cex.lab = 1, col = "blue", main = expression(paste("Population Mean   ", mu)))
plot (sigmasq.save, type = "l", xlab = "Iteration",  ylab = expression(sigma^2), cex.lab = 1, col = "blue", main =  expression(paste("Population Variance   ", sigma^2)) )





scatterhist = function(x, y, xlab="", ylab=""){
  zones=matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
  layout(zones, widths=c(4/5,1/5), heights=c(1/5,4/5))
  xhist = hist(x, plot=FALSE)
  yhist = hist(y, plot=FALSE)
  top = max(c(xhist$counts, yhist$counts))
  par(mar=c(3,3,1,1))
  plot(x,y,col = "#0000ff22")
  par(mar=c(0,3,1,1))
  barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0)
  par(mar=c(3,0,1,1))
  barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE)
  par(oma=c(3,3,0,0))
  mtext(xlab, side=1, line=1, outer=TRUE, adj=0, 
    at=.8 * (mean(x) - min(x))/(max(x)-min(x)))
  mtext(ylab, side=2, line=1, outer=TRUE, adj=0, 
    at=(.8 * (mean(y) - min(y))/(max(y) - min(y))))
}

scatterhist (mu.sample, sigmasq.sample, xlab =  expression(paste("Population Mean   ", mu)), ylab = expression(paste("Population Variance   ", sigma^2)))





