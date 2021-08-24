########################
# Ridge Regression, Lasso, and Elastic Net


plot(model.ridge)# this library contains datasets used in ISL:
library(glmnet)
# These data contain gene expression for 3,571 genes from 72 subjects
#https://www.jstatsoft.org/article/view/v033i01
load('~/Dropbox/EmoryCourses/BIOS_526/Materials_BRisk_2019/Data/Leukemia.RData')


# ridge regression:
x = Leukemia$x
y = Leukemia$y
table(y)
# 47 control, 25 with leukemia

image(t(x))
# each row is a subject
#Plot in lecture notes:
#library(r.jive)
#show.image(x[order(y),],ylab='Subject')
#abline(h=47)



# in glmnet, alpha = 0 : ridge regression
# by default, glmnet standardizes variables
model.ridge = glmnet(x,y,alpha=0,family = 'binomial')
print(model.ridge)
dim(coef(model.ridge))
coef(model.ridge)[1:10,1:5]
# by default, fit with 100 values of lambda
# note here, the intercept is also penalized

# in this plot, it says "3571" because the ridge penalty does not 
# induce sparsity, so ALL variables are selected
# Note axes...

# To perform cross validation, we need to estimate the MSE for each lambda. 
# By default, cv.glmnet uses 10-fold CV, such that for each lambda, we fit
# a ridge model 10 times:
set.seed(123)
cv.model.ridge = cv.glmnet(x,y,alpha=0,family='binomial')
plot(cv.model.ridge)
# choose the smallest value of lambda considered
cv.model.ridge$lambda


lambda = exp(seq(4,-10,length=100))
cv.model.ridge = cv.glmnet(x,y,alpha=0,lambda = lambda,family='binomial')
plot(cv.model.ridge)
# not much guidance from CV here.
# the penalty chosen is rather small (little shrinkage)
# note the axes

cv.model.ridge$lambda.min
cv.model.ridge$lambda.1se


# extract the mse for the optimal tuning parameter:
findindexlmin = which(cv.model.ridge$lambda == cv.model.ridge$lambda.min)
(ridge.mse.min <- cv.model.ridge$cvm[findindexlmin])
# there's also a lot of uncertainty in the cv estimate


ridge.estimates = coef(cv.model.ridge,s=cv.model.ridge$lambda.min)
plot(ridge.estimates[2:3572])

ridge.estimates.1se = coef(cv.model.ridge,s=cv.model.ridge$lambda.1se)
points(ridge.estimates.1se[2:3572],col='green')

# can also look at classification accuracy using logistic regression:
predict.y.ridge <- c(predict(cv.model.ridge,x,s = cv.model.ridge$lambda.min,type = "class"))
print(table(true = y, pred = predict.y.ridge))

predict.y.ridge <- c(predict(cv.model.ridge,x,s = cv.model.ridge$lambda.min,type = "class"))
print(table(true = y, pred = predict.y.ridge))

predict.y.ridge <- c(predict(cv.model.ridge,x,s = cv.model.ridge$lambda[1],type = "class"))
print(table(true = y, pred = predict.y.ridge))

predict.y.ridge <- c(predict(cv.model.ridge,x,s = cv.model.ridge$lambda[100],type = "class"))
print(table(true = y, pred = predict.y.ridge))


#############################
## For Lasso (L1 penalty), 
# figure out which penalties to use.
cv.model.lasso = cv.glmnet(x,y,alpha=1,family='binomial')
plot(cv.model.lasso)

# run it a few times to illustrate how much variability there is:
set.seed(123)
lambda = exp(seq(2,-15,length=100))
cv.model.lasso = cv.glmnet(x,y,alpha=1,lambda=lambda,family='binomial')
plot(cv.model.lasso)

lambda = exp(seq(2,-15,length=100))
cv.model.lasso = cv.glmnet(x,y,alpha=1,lambda=lambda,family='binomial')
plot(cv.model.lasso)


# extract the mse for the optimal tuning parameter:
findindexlmin = which(cv.model.lasso$lambda == cv.model.lasso$lambda.min)
(lasso.mse.min <- cv.model.lasso$cvm[findindexlmin])
# caution: refit a few times (different partitions of the data) and you 
# will see this estimate is not very stable. This is not surprising
# since we only have 72 observations. 



# run with glmnet to create coefficient path plots:
colnames(x) = paste0('v',c(1:ncol(x)))
model.lasso = glmnet(x,y,alpha=1,lambda=lambda,family='binomial')
plot(model.lasso)
# note: this is deterministic
# note numbers at the top


# plot coefficients for lambda.min
lasso.estimates = coef(cv.model.lasso,s=cv.model.lasso$lambda.min)
# note on sparse matrices: stores an object with indices and numbers;
# all indices not include are equal to zero. saves memory.
lasso.estimates

plot(lasso.estimates[2:3572])
sum(lasso.estimates!=0)

predict.y.lasso <- c(predict(cv.model.lasso,x,s = cv.model.lasso$lambda.min,type = "class"))
table(true = y, pred = predict.y.lasso)

predict.y.lasso <- c(predict(cv.model.lasso,x,s = cv.model.lasso$lambda[15],type = "class"))
table(true = y, pred = predict.y.lasso)
sum(coef(cv.model.lasso,s=cv.model.lasso$lambda[15])!=0)
# still do very well with just 7 predictors

###################
# Elastic net

model.elnet = glmnet(x,y,alpha=0.5,lambda=lambda,family='binomial')
plot(model.elnet)

set.seed(123)
cv.model.elnet = cv.glmnet(x,y,alpha=0.5,lambda=lambda,family='binomial')
# extract the mse for the optimal tuning parameter:
findindexlmin = which(cv.model.elnet$lambda == cv.model.elnet$lambda.min)
(elnet.deviance.min <- cv.model.elnet$cvm[findindexlmin])
  
elnet.estimates = coef(cv.model.elnet,s=cv.model.elnet$lambda.min)
model.elnet = glmnet(x,y,alpha=0.5,lambda=lambda,family='binomial')

par(mfrow=c(1,2))
plot(cv.model.elnet)
plot(model.elnet)
abline(v=sum(abs(elnet.estimates)),lty=2)


plot(elnet.estimates[2:3572])
sum(elnet.estimates!=0)

predict.y.elnet <- c(predict(cv.model.elnet,x,s = cv.model.elnet$lambda.1se,type = "class"))
table(true = y, pred = predict.y.elnet)


# may want to try less ridge / more lasso to get greater sparsity:

set.seed(1)
cv.model.elnet = cv.glmnet(x,y,alpha=0.95,lambda=lambda,family='binomial')
plot(cv.model.elnet)
elnet.estimates = coef(cv.model.elnet,s=cv.model.elnet$lambda.min)
elnet.estimates

plot(elnet.estimates[2:3572])

# create GLM regression to reduce the bias of the estimators: (p-values are incorrect)
which(abs(elnet.estimates[2:3572])!=0)

# still too many variables because we only have 72 observations!
model.elnet = glmnet(x,y,alpha=0.95,lambda=lambda,family='binomial')
plot(model.elnet)
lambda
sum(coef(model.elnet,s=lambda[35])!=0) #intercept
sum(coef(model.elnet,s=lambda[36])!=0)


elnet.estimates = coef(cv.model.elnet,s=lambda[36])
xselected = which(abs(elnet.estimates[2:3572])>0)
subdata = scale(x[,xselected])

model.glm.smallsubset = glm(y~subdata,family=binomial)
summary(model.glm.smallsubset)

vif(model.glm.smallsubset)
subdata = data.frame(subdata)
subdata$y = y
model.glm.smallsubset = glm(y~.,data=subdata,family=binomial)
summary(model.glm.smallsubset)
vif(model.glm.smallsubset)


summary(glm(y~X1,data=subdata,family=binomial))
summary(glm(y~X2,data=subdata,family=binomial))
summary(glm(y~X3,data=subdata,family=binomial))
# surprising how much the p-values change in univariate
# versus multiple logistic regression given the VIFs 
# look fine. 


 
# note: we can't trust the p-values because we've 
# used all the other variables to choose these variables,
# so in some respected we've done 3571 tests.
# These are not huge t-statistics and in the multivariate model, noting
# passes
0.05/3571
# although in the univariate models, some variables do pass.

