getwd()
setwd("~/module2_R_biostats-master/")
# REGRESSION ANALYSIS
fevdat = read.table("~/module2_R_biostats-master/Datasets/fev_dataset.txt", header = T)
str(fevdat)
# first model: analyse association between height and age
plot(y=fevdat$Ht, x=fevdat$Age)
# another way to plot the same
# usung formula
plot(Ht~Age, data = fevdat)
# same as when a boxplot is plotted of sungroups
boxplot(Ht~Age, data = fevdat)
# to get a linear model: function lm()
lm(Ht~Age, data = fevdat)
 #output model is 
# Ht = 45.958 + 1.529 * Age
abline(a=45.958, b= 1.529, col="purple")
# lm() produces much more than the best estimate values for the regression parameters
model_1 = lm(Ht~Age, data = fevdat)
class(model_1)
model_1$coefficients # to access the coefficient
# abline also accepts to get as input an lm object
abline(model_1, col= 'red')
summary(model_1)
# check the mean residual = 0 
mean(model_1$residuals)
hist(model_1$residuals)
# the framework of the least squares regression is such that in most datasets are normally distributed when 
# the number of observations is large enough
# the (multiple) R-squared vale reps the fraction of total variance explained by the variance: its = ratio 
# between the predicted outcomes ("fitted.values" slot in lm object) and variance of the set of the observed outcomes.
var(model_1$fitted.values)/ var(fevdat$Ht)

# to show a "very wrong" model, ad a column to our dataframe that will be made up of gaussian noise (centred 
# at 100 and sd = 8 )and try to explain the same outcome (height) as a outcome of this prepostrous predictor
fevdat$absurd = rnorm(nrow(fevdat), mean = 100, sd = 8)
plot(Ht ~ absurd,  data = fevdat)
lm(Ht ~ absurd,  data = fevdat) -> absurd_model
abline(absurd_model, col='red', lty=2)
summary(absurd_model)
plot(Ht ~ absurd,  data = fevdat,xlim=c(0,140))
abline(absurd_model, col='red', lty=2)
mean(fevdat$Ht)
# that was a very bad regression model.
# influence of adding predictors
names(fevdat)
# we have a new model with two independent variables [predictors]; Age and Smoke
lm(Ht ~ Age + Smoke,  data = fevdat)-> model_2
# this model is: Ht = beta_0 + (beta_1 * Age) + (beta_2 * Smoke)
model_0 = lm(Ht ~ 1, data = fevdat)
summary(model_0) # zero-predictor model: Ht = beta_0
# Multiple R-squared:  0.0000,	Adjusted R-squared:  0.0000
summary(model_1) # one-predictor model: Ht = beta_0 + (beta_1 * Age)
# Multiple R-squared:  0.6272,	Adjusted R-squared:  0.6266
summary(model_2) # two-predictor model :Ht = beta_0 + (beta_1 * Age) + (beta_2 * Smoke)
# Multiple R-squared:  0.6291,	Adjusted R-squared:  0.6279
# zero-predictor model
 
# if we try to perdict the growth between 4 and 10, we will have a bettern linear model
fevdat_age = subset(fevdat, Age >=4 & Age <=10)
model_age = lm(Ht~ Age, data = fevdat_age)
plot(Ht ~ Age, data=fevdat_age)
abline(model_age, col= "red")
summary(model_age)

# EXERCISE
# using the tutuorial_dataset, try to predict diabetes out of predictors 
tutorialdat = read.csv("~/module2_R_biostats-master/Datasets/tutorial_data.csv", sep = ";", dec = ",", header = TRUE)
#  1. all the other variables in the dataset
model_All = lm(DIABETES ~ SEX + AGE + SYSBP + CURSMOKE + BMI + educ + CIGPDAY + DEATH, data = tutorialdat)
# 2. Systolic blood pressure and cu smoke
model_sybpsmoke = lm(DIABETES ~ SYSBP + CURSMOKE, data = tutorialdat)
# 3. age and educ level
model_ageeduc = lm(DIABETES ~ AGE + educ, data = tutorialdat)
# compare the 3 above, optionally other models and give take on which predictor(s) is/are relevant
######
# low p-value is an indication to reject H_0
# Correlation 
# correlation calculation
# by hand:
m_x = mean(fevdat$Ht)
m_y = mean(fevdat$Age)
numerator = sum((fevdat$Ht - m_x)*(fevdat$Age - m_y))
ss_x = sum((fevdat$Ht - m_x)^2)
ss_y = sum((fevdat$Age - m_y)^2)
pearson_cor = numerator / sqrt(ss_x)/sqrt(ss_y) # 1
pearson_cor = numerator / (sqrt(ss_x)*sqrt(ss_y)) # 2
# now using the r function
cor(fevdat$Ht, fevdat$Age)
# it is symmetric
# there is an associated test with H_0 being "pearson_cor between underlying population ==0":
cor.test(fevdat$Ht, fevdat$Age)
# since the p-value is so low (below 2.2e-16), we reject H_0
# to create articially a situation where there is null correlation
# we will create an artificial vector of value drawn uniformly between -1e-2 and 0
randomvec = runif(n= nrow(fevdat), min =-1e-2, max=0)
# plot the absence of correlation
plot(x=fevdat$Age, y= randomvec)
# to perform the cor.test
cor.test(x=fevdat$Age, y= randomvec)
# we don't reject H_0

# the pearson correlation assumes normality.There is another correlation measure well suited for samples
# departing from that assumption, and calculated based on comparisons betweeen within-vector ranks 
# The Spearman correlation co-efficient.
# The default on cor() and cor.test() is to use Pearson correlation, though one cal specify Spearman's instead
cor.test(fevdat$Ht, fevdat$Age, method = "spearman")
cor.test(x=fevdat$Age, y= randomvec, method = "spearman")
# Spearman's correlation should be prefered when when one of the two vectors doe not originate from a 
# distribution thought *not to be* normal

# !WARNING! sample correlation = 0, or even true correlation =0,  DOES NOT entail independence of the variables 
# Logical implication 
# X and Y => cor(X,Y) = 0
# BUT _NOT_:
# cor(X,Y) = X and Y independent # THIS ASSERTION IS FALSE

# Exercise: se what is the correlation between X and Y=X^2, for X values at random between -2 and 2.
# Test also when ranges between -1 and 1
# Exercise 2: try to test (X,Y) values describing the unit circle centred around (0,0) and test correlation
# between X and Y

##### TESTING NORMALITY
# How to assess the likelihood a sample originates from a normal population (i.e normal distribution)

# Shapiro-Wick or Wick-Shapiro normality test:
# H_0 is "the underlying population is normal"
hist(fevdat$FEV)
shapiro.test(fevdat$FEV)# rejection of H_0
# by construction, S-W is known to be very strick on the hypothesis of normality: it tends to reject H_0
# as soon as it sees even a faint signal of non-normality

# create artificially situation where W-S should accept H_O
shapiro.test(rnorm(10,3,10)) # high p-values: acceptance of H_0
shapiro.test(fevdat$Age) # rejection of H_0
shapiro.test(fevdat$Ht) # rejection of H_0, even though weaker
hist(fevdat$Ht)
# Other test are not so strick

## TESTING SIMILARITY BETWEEN TWO DISTRIBUTIONS
# The kolmogorov-Smirnov test is based on the comprison between two probablity function
# To test normality on one sample, we test with ks.test against the pnorm function wth parameters given 
# by the samples estimates (mean and sd) on our sample of interest
ks.test(fevdat$Ht, "pnorm", mean(fevdat$Ht), sd(fevdat$Ht)) # acceptance of H-0
ks.test(randomvec, "pnorm", mean(randomvec), sd(randomvec))# rejection of H_0

# The kolmogorov-Smirnov test is less "picky" on what it accepts as a sample possibility coming from a 
# normal distribution
 
##### 
# QQ_plots are a nice way to visually compare two distribution
# each point in a quantile-quantile plot is (x,y) wit x being a given quantile value for the first population
# and y being the *same* quantile for the other sample.

# we are now comapring distributions. Samples can have different sizes

sample1 = rnorm(n=10000, mean=10, sd=2) # norm distribution
sample2 = runif(n=20000, min = 20, max = 200) # uniform distribution

# for each I will calculate all the quantilees separated for 1%:
# the min, the 0.01 quantile, the 0.02 quantile etc until the 0.99 quantile and max
quantiles_sample1 = quantile(x=sample1, probs = seq(0,1, 0.01))
quantiles_sample2 = quantile(x=sample2, probs = seq(0,1, 0.01))
head(quantiles_sample1)

# check the median
quantiles_sample1["50%"] # 1
quantiles_sample1[51] # 2
quantiles_sample2["50%"]

# my happy manual quantile vs quantile plotting
plot(quantiles_sample1, quantiles_sample2, xlab = "Quantiles of a norm. dist. (10,2)", ylab = "Quantiles of unif. dist. (20,200)")
# now to compare 2 normal distributions.
sample3 = rnorm(n=20000, mean=20, sd=0.200) # norm distribution
quantiles_sample3 = quantile(x=sample3, probs = seq(0,1, 0.01))
plot(quantiles_sample1, quantiles_sample3, xlab = "Quantiles of a norm. dist. (10,2)", ylab = "Quantiles of norm. dist. (20,0.200)")
# we can obtain these automatically
qqplot(quantiles_sample1,quantiles_sample3)
# a straight line in qq plot indicates samples origination from same type of distribution (either norm or unif, etc)
qqnorm(quantiles_sample1) # a normal sample
qqnorm(quantiles_sample2) # not norm but unif
qqnorm(fevdat$Ht)
