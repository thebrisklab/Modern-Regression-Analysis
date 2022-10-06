
###################
# Module 3 Part III: Hierarchical models
##################
setwd('./Data')

dat = read.csv("achievement.csv")

# check the data were read-in correctly:
head(dat)

summary(dat)

# check how many children appear in the dataset:
length(unique(dat$child))

# check how many schools:
length(unique(dat$school))


summary(is.na(dat))
# it would appear we have no missing data... but see below

# check how many observations per child:
table(table(dat$child))
#5 have 2 obs, 542 have 3 obs, 328 have 4 observations, etc. 
# thus we have lots of missing data, as children have differing numbers of observations. 




# check whether child is nested by school
# counts whether a child is found at more than one school:
# you can see if a factor is nested by using a cross tabulation table
# and seeing if the level appears in only one level of the other factor
table(dat$child,dat$school)

# it's a bit hard to see with so many children and school,
# so this code checks that each child is only at one school:
apply(table(dat$child,dat$school)>0,1,sum)

all(apply(table(dat$child,dat$school)>0,1,sum)==1)
# the children are nested in school
# can not examine whether there is an interaction between child and school


# examine retained (whether a student repeats a grade) variable:
table(dat$retained)
table(dat$child,dat$retained)
# retained appears more than once for some children
# in theory, we could look at the interaction between child and retention,
# i.e., does the effect of retention differ between individuals. 
# In the code below, we do not evaluate this possible interaction. 

# In general, we can look at interaction between factors that are crossed
# We can tell if two factors are crossed because a cross tabulation 
# produces a table with non-zero entries at all combinations of factor levels.
# E.g., sex and race
table(dat$female, dat$black)


# Fit multi-level mixed model:
library(lmerTest)
fit = lmer (math ~ year + retained + female + black + hispanic + size + lowinc + (1|school) + (1 | child), data = dat)
summary(fit)

car::vif(fit)
  
# NOTE ON NESTING:
  # imagine a dataset in which the child id was numbered from 1 to n_i for each school, such that
  # child 1 and school 1 was different from child 1 at school 2, yet labeled the same.
  # Then the NESTING structure matters, and you should do this to construct the correct
  # random effects design matrix:
  fit2 = lmer (math ~ year + retained + female + black + hispanic + size + lowinc + (1|school) + (1 | school:child), data = dat)
  summary(fit2)
  # in this dataset, each child id is unique, so these are equivalent

  # TIP: check output to see if number of children is correct


#NOTE: Intercept is not meaningful: for a school with 0 students, 0 low income... out-of-sample estimate
mean(dat$lowinc)
mean(dat$size)
fit.centered = lmer (math ~ year + retained + female + black + hispanic + I(scale(size,center = TRUE,scale = FALSE)) + I(scale(lowinc,center=TRUE,scale=FALSE)) + (1|school) + (1 | child), data = dat)
summary(fit.centered)
# note: intercept estimate and SE changed, others are equal





### Looks at other models:
fit2 = lmer (math ~ year + retained + female + black + hispanic + size + lowinc + (1|school) , data = dat)
summary(fit2)
anova(fit2,fit) # not a good test, but I don't have a better solution, 



fit3 = lmer (math ~ year + retained + female + black + hispanic + size + lowinc + (1|child) , data = dat)
summary(fit3)
anova(fit3,fit)

# don't do this:
# arr R:
anova(fit2,fit3)
# should return an error but does not.

pchisq(100,0) # mass at 0
pchisq(1,0)
pchisq(1e-10,0)

######################
######
# Guatemalan data:
# generalized linear mixed model 
dat = read.csv ("guatemalan.csv")

length (unique (dat$mom)) #Total number of mothers
length (unique (dat$cluster)) #Total number of communities
table ( table (dat$mom)) #Number of children per mom

# check whether mother id is nested by community:
table (dat$mom, dat$cluster)

# note there are not many mothers in the dataset with multiple children:
apply(table (dat$mom, dat$cluster),1,sum)
table(apply(table (dat$mom, dat$cluster),1,sum))

# should all be equal to 1:
apply(table (dat$mom, dat$cluster)!=0,1,sum)

all(apply(table (dat$mom, dat$cluster)!=0,1,sum)==1)


fit = glmer(immun ~ kid2p + momEdPri + momEdSec + husEdPri + husEdSec + rural + pcInd81 + (1|mom) + (1|cluster), family = binomial, data = dat)
summary(fit)
# Issues with convergence

fit = glmer(immun ~ kid2p + momEdPri + momEdSec + husEdPri + husEdSec + rural + pcInd81 + (1|mom) + (1|cluster), family = binomial, data = dat, glmerControl(optimizer='Nelder_Mead'))
summary (fit)
# Issues with convergence

# NOTE: nAGQ does not work here:( Only works for random intercept.
fit = glmer(immun ~ kid2p + momEdPri + momEdSec + husEdPri + husEdSec + rural + pcInd81 + (1|mom) + (1|cluster), family = binomial, data = dat, nAGQ=5)


# Lots of options in glmerControl, see
?glmerControl


# this seems to work
fit = glmer(immun ~ kid2p + momEdPri + momEdSec + husEdPri + husEdSec + rural + pcInd81 + (1|mom) + (1|cluster), family = binomial, data = dat, glmerControl(optimizer="bobyqa"))
summary(fit)

#Code to increase number iterations
#fit = glmer(immun ~ kid2p + momEdPri + momEdSec + husEdPri + husEdSec + rural + pcInd81 + (1|mom) + (1|cluster), family = binomial, data = dat,glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

beta0 = -0.7625
se0 = 1.32
exp(beta0-2*se0)/(1+exp(beta0-2*se0))
exp(beta0+2*se0)/(1+exp(beta0+2*se0))

# percent decrease in odds ratio due to 10% increase
# in indigenous population:
1 - exp(-0.98*0.1)

