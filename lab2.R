##############
library(nlme)
##view summary of Soybean dataset
str(Soybean)
##plot of outer
plot(Soybean,outer=~Year) 
plot(Soybean,outer=~Variety)
##create nlslist called fm1 containing
##weight data of soybean dataset, with numeric paremeters representing asymptote, 
#x value at inflection point of the curve and a sclae paremeter.
fm1 <- nlsList(weight ~ SSlogis(Time, Asym, xmid, scal),
               data=Soybean)
##fit fm1 nlslist into a nonlinear mixed-effects model and create a new nlme called fm1.nlme
fm1.nlme <- nlme(fm1)
#summary of fm1.nlme 
summary(fm1.nlme)
#plot of fm1.nlme
plot(fm1.nlme)
#update and re-fit the fml.nlme with weight as the varpower, a power variance funtion 
#structure and creat a new nlme calles fm2.nlme
fm2.nlme <- update(fm1.nlme, weights=varPower())
#plot of fm2.nlme
plot(fm2.nlme)
#carry out anova on fm1.nlme, fm2.nlme to determine to compare them and determine if they significantly differ 
anova(fm1.nlme,fm2.nlme)
#plot the extracted random effects of fm2.nlme, with the returned dataframe being ugmented with variables, 
#with the x axis being the product of year and Variety and the layout being divided into 3 colums and 1 row
plot(ranef(fm2.nlme,augFrame=T),form=~Year*Variety,layout=c(3,1))
#extract fixed effects of fm2.nlme into a numeric fm2.fix
fm2fix <- fixef(fm2.nlme)
#update and refit the fm2.nlme, with Asym, xmid and scal remaining fixed in relation 
#to year and starting with the concantenated first 3 columns of fm2fix numeric
fm3.nlme <- update(fm2.nlme, fixed=Asym+xmid+scal ~ Year,
                   start=c(fm2fix[1],0,0,fm2fix[2],0,0,fm2fix[3],0,0))
#carry out anova on fm3.nlme
anova(fm3.nlme)



########################
#set number of observation to be 1000
obs<-1000
#create a 4*4 matrix where variable one and variable 2 have an effect of 0.9 on each other 
#between variable3 and variable4 hane an effect of 0.45 on each other(the other covariances0)
mat<-matrix(c(1, 0.9, 0, 0,
              0.9, 1, 0, 0,
              0, 0, 1, 0.45,
              0, 0, 0.45, 1), nrow=4, ncol=4)
#compute the choleski decomposition on mat and save it as dec
dec=chol(mat)
#retrieve the first dimension of dec into nwars
nwars=dim(dec)[1]
#transpose the matrix dec
t(dec)
#matrix multiplication between dec and transposed dec
t(dec) %*% (dec)
#create data dataset from matix multiplicaton of transposed dec and the matrix of normalised 
#random values of multipilcation of nwars and the number of observations, whose mean is 0, number
#of row equal to nwars and the number of columns equal to obs
data=t(dec) %*% matrix(rnorm(nwars*obs, mean=0), nrow=nwars, ncol=obs)
#view the contents of data
data
class(data)
summary(data)

#transpose the data matrix and replace the original data
data=t(data)
#view the contents of the new data
data
summary(data)
#coerce the data matrix into a data frame names datafr
datafr=as.data.frame(data)
#add names, "var1", "var2", "var3", "var4" into the datafr dataframe
names(datafr)<-c("var1", "var2", "var3", "var4")
#load the lattice library 
library(lattice)
#generate conditional scatter Plot matrices and parallel coordinate Plots of datafr
splom(datafr)
#compote the covariance of datafr
cor(datafr)
