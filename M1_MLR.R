####################################
## Code for LMR review
## BIOS 526
#####################################

setwd("~/Dropbox/EmoryCourses/BIOS_526/Materials_BRisk_2020/Data/")

# Datasets used in lectures and assignments are on Canvas/Data


dat = read.csv ("testscore.csv")

# Examine structure of dataset:
str(dat)
summary(dat)

# Exploratory plots:
#set-up plot to have 1 row and two colums:
par (mfrow =c(1,2))

# basic scatterplot:
plot (score~age, data = dat, xlab="Age",ylab="Score")

#boxplot also uses formula notation:
boxplot (score~edu, xlab = "Education", data = dat)


#Slide: "OLS calculations in R"
# Regression "by hand":
X = cbind (1, dat$age)
Y = dat$score
dim (X); dim (Y)

# Matrix multiplication: %*%
# Transpose: t(X)
# note that solve() in R computes the inverse:
invXtX = solve(t(X)%*%X)
(beta = invXtX %*% t(X) %*% Y)
# including parentheses is a trick to print the output beta

plot (score~age, data = dat, xlab="Age",ylab="Score")
abline (coef=beta, lwd = 3, col = 2)

# Slide: "lm() and Residual Error"
# Regression by R:
fit = lm (score~age, data = dat)
summary(fit)



# Estimate coefficients:
invXtX = solve(t(X)%*%X)
beta = invXtX %*% t(X) %*% Y
# Estimate covariance matrix of beta0 and beta1:
sigmahat = sum((Y - X%*%beta)^2) / (length(Y)-2)
V = sigmahat*invXtX




# This is equivalent:
Va = vcov (fit)
# confirm equivalent:
all(Va==V) # false due to machine precision
sum(abs(Va - V)) # differences negligible

# Create a design matrix for ages 17:30:
X = cbind(1, 17:30)
#note how R "recycles" 1 to be length 14

Est = X %*% beta
SE = sqrt( diag(X%*%V%*%t(X)) )
Upper95 = Est + 1.96*SE
Lower95 = Est - 1.96*SE
Upper95 = Est + qt(0.975,df=398)*SE
Lower95 = Est + qt(0.025,df=398)*SE


qt(0.975,df=10)
qt(0.975,df=100)
qt(0.975,df=398)
qt(0.975,df=1000)
qnorm(0.975)

# Print 
cbind(Est, Lower95, Upper95)[1:3,]

# Method 2:
ConfInt = predict (fit, newdata = data.frame(age=17:30), interval="conf")
ConfInt[1:3,]


# "Confidence Interval for the Estimate of the Mean"
plot (score~age, data = dat, xlab="Age",ylab="Score")
lines (Est~c(17:30), col = 2, lwd =3)
lines (Upper95~c(17:30), col = 4, lwd = 2)
lines (Lower95~c(17:30), col = 4, lwd = 2)
legend ("topleft", legend = c("Fitted value", "95% confidence interval"), col = c(2,4), bty="n", lwd=3)
mean(dat$age)



#  "Prediction Interval for New Observation, iii"
PredInt = predict (fit, newdata = data.frame(age=17:30), interval="pred")

plot (score~age, data = dat, xlab="Age",ylab="Score")
lines (Est~c(17:30), col = 2, lwd =3)
lines (ConfInt[,2]~c(17:30), col = 4, lwd = 2)
lines (ConfInt[,3]~c(17:30), col = 4, lwd = 2)
lines (PredInt[,2]~c(17:30), col = "orange", lwd = 2)
lines (PredInt[,3]~c(17:30), col = "orange", lwd = 2)
legend ("topleft", legend = c("Fitted value", "95% confidence interval", "95% prediction interval"),  col = c("red", "blue", "orange"), bty="n", lwd=3)


# "Model Diagnostics"
par(mfrow=c(2,2))
plot(fit)
hist(resid(fit))


###################################
# Extra not in slides: 
# See quiz 1 questions
set.seed(123)
a = seq(1,10,length=1000)
b = a+2*a*rnorm(1000)
  
# Which assumption is violated?
##################################




# "Confounding, i" slide:
boxplot(age~edu,data=dat)

# "Average score by maternal education, i"
# Indicators for Education

# Create dummy variables manually:
E1 = as.numeric(dat$edu == 1)
E2 = as.numeric(dat$edu == 2)
E3 = as.numeric(dat$edu == 3)
E4 = as.numeric(dat$edu == 4)
fit = lm (dat$score ~ E1 + E2 + E3 + E4 - 1)
summary (fit)
tapply(dat$score,INDEX = dat$edu,FUN = mean)



# "Average score by Maternal Education, ii"
dev.off()
plot (score~jitter(edu), data = dat, xlab="Education Group",ylab="Score")
points (coef(fit)~c(1,2,3,4), col = 'red', cex=1.5, pch=16, type = "p")
legend ("bottomright", legend=c("Fitted value"), pch=16, col = 2, cex=1.2, bty="n")






# 'Difference in Average Score w.r.t Edu=1'
# By default, R codes the alpha-sorted level as the reference level:

dat$FactorEdu = factor(dat$edu) #
fit = lm(score~FactorEdu,data=dat)
summary(fit)

# Equivalently:
fit2 = lm (dat$score ~ E2 + E3 + E4 )
summary (fit2)


#  'Adjusting for confounding, ii'
fit = lm (score ~ FactorEdu+age,data=dat)
summary(fit)
# age no longer significant




# create plots of age for each education category
par (mfrow = c(2,2))
plot (score~age, data = subset (dat, edu == 1) , ylim = c(20, 160), main = "Edu = 1" )
lines( predict(fit,newdata=data.frame(age=17:30,FactorEdu='1'))~c(17:30), col = 2, lwd = 2)

plot (score~age, data = subset (dat, edu == 2) , ylim = c(20, 160), main = "Edu = 2"  )
lines( predict(fit,newdata=data.frame(age=17:30,FactorEdu='2'))~c(17:30), col = 2, lwd = 2)

plot (score~age, data = subset (dat, edu == 3) , ylim = c(20, 160), main = "Edu = 3" )
lines( predict(fit,newdata=data.frame(age=17:30,FactorEdu='3'))~c(17:30), col = 2, lwd = 2)

plot (score~age, data = subset (dat, edu == 4) , ylim = c(20, 160), main = "Edu = 4"  )
lines( predict(fit,newdata=data.frame(age=17:30,FactorEdu='4'))~c(17:30), col = 2, lwd = 2)


# "Variance Inflation Factors"
library(car)
vif(fit)

temp = vif(fit)

temp[,3]^2

temp[,3]^2<5

# these look fine -- we know age and factor edu are dependent, but 
# it is sufficiently small, i.e. multicollinearity is not a big issue.
# a very rough rule of thumb: is the vif > 5, or here, (gvif^(1/2/df))^2 > 5? if so,
# you may want to consider dropping one of the variables. You can tolerate
# more vif if you have a larger sample size, since the decrease in se due to 
# large n can help offset the increase due to multicollinearity

# This is in the quiz:
n=20 
n=10000
a = rnorm(n) # defaults to standard normal, i.e., variance = 1
b = 0.1*rnorm(n)+a


d = a - b + rnorm(n) # \epsilon_i \sim n(0,1)
# true slopes are 1 and -1
e = lm(d~a+b)
summary(e)
vif(e)
1/(1-cor(a,b)^2)
1/(1-summary(lm(a~b))$r.squared)

###########################



### Effect Modification: examining interactions
fit = lm (score~FactorEdu*age, data = dat)
summary(fit)


vif(fit)
# some evidence that slopes change
# some issues with variance inflation. This generally happens
# with interaction effects and can make testing them challenging,
# as it implies reduced statistical power. 


library(emmeans)
#pdf(file='~/Dropbox/EmoryCourses/BIOS_526/Materials_BRisk_2020/M1-MLR/Emtrendsplot.pdf')
emmip(fit, FactorEdu ~ age, cov.reduce = range,CIs = TRUE)
#dev.off()

# means:
emmip(fit, FactorEdu ~ age, CIs = TRUE)

# examine the CI at age = 0:
emmip(fit, FactorEdu ~ age, at = list(age=c(0,30)),CIs = TRUE)


#The average score for a child whose mother is age 22.8 and whose education level is # 2 is 11.8 points higher than a child whose mother is age 22.8 with education level 1. 



par (mfrow = c(2,2))
plot (score~age, data = subset (dat, edu == 1) , ylim = c(20, 160), main = "Edu = 1" )
lines( predict(fit,newdata=data.frame(age=17:30,FactorEdu='1'))~c(17:30), col = 2, lwd = 2)

plot (score~age, data = subset (dat, edu == 2) , ylim = c(20, 160), main = "Edu = 2"  )
lines( predict(fit,newdata=data.frame(age=17:30,FactorEdu='2'))~c(17:30), col = 2, lwd = 2)

plot (score~age, data = subset (dat, edu == 3) , ylim = c(20, 160), main = "Edu = 3" )
lines( predict(fit,newdata=data.frame(age=17:30,FactorEdu='3'))~c(17:30), col = 2, lwd = 2)

plot (score~age, data = subset (dat, edu == 4) , ylim = c(20, 160), main = "Edu = 4"  )
lines( predict(fit,newdata=data.frame(age=17:30,FactorEdu='4'))~c(17:30), col = 2, lwd = 2)

# F-test of interaction effect
fit_nointer = lm(score~FactorEdu+age,data=dat)
anova(fit_nointer,fit)

    
# linear combinations of coefficients
(Est = coef(fit)[5] + coef(fit)[7])
(SE = sqrt ( vcov(fit)[5,5] + vcov(fit)[7,7] + 2*vcov(fit)[5,7]))




# Effect of centering:
dat$ageC = scale(dat$age,center=TRUE,scale=FALSE)
mean(dat$ageC)

fit_inter_ageC = lm(score~FactorEdu*ageC,data=dat)
summary(fit_inter_ageC)

summary(fit)

# What happens to variance inflation factors?
vif(fit) #not centered
vif(fit_inter_ageC) #centering tends to improve VIFs in models with interactions. 


fit_ageC = lm(score~ageC,data=dat)
anova(fit_ageC,fit_inter_ageC)
#overall effect of education

fit_age = lm(score~age,data=dat)
anova(fit_age,fit)
#overall effect of education


fit_edu = lm(score~FactorEdu,data=dat)
anova(fit_edu,fit_inter_ageC)
anova(fit_edu,fit)


fit_age = lm(score~age,data=dat)
anova(fit_age,fit)

# Compare slopes, no correction for multiple comparisons
emtrends(fit,pairwise~FactorEdu,var="age",adjust='none')

# Compare slopes, correct for multiple comparisons
emtrends(fit,pairwise~FactorEdu,var="age",adjust='bonferroni')

#  Same output for centered age:
emtrends(fit_inter_ageC,pairwise~FactorEdu,var="ageC",adjust='bonferroni')

