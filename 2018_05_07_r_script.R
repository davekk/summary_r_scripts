list(5, 3, "lello", TRUE, 9.5) -> mylist
mylist[1]
mylist[[1]]
qnorm(1-0.05/2,mean=22.5, sd=0.2) # 
qnorm(0.05/2, mean=22.5, sd=0.2) #
qnorm(1-0.01/2,mean=22.5, sd=0.2) #
qnorm(0.01/2,mean=22.5, sd=0.2) #
# drawt(overlay) on [-5,5], density for d.f = 2,5,10,100 ans also draw N(0,1)
dt(df=c(2,5,10,100),x=seq(-5,5, length.out = 101))
plot(function (x) dt(df=100,x=seq(-5,5, length.out = 101)),xlab = "t", ylab = "non-central parameter", col="blue", xlim = c(-5,5), ylim=c(0,0.5), add=T)
curve(expr = dt(x,df=2), from = -5, to = 5, col = "red")
# if we want to generate a vector of simulated observations
students_vec= rt(n=1000, df=2)
hist(students_vec,freq = F,breaks = 100)
hist(students_vec,freq = F,breaks = 1000, xlim = c(-5,5))
# the other solution
curve(expr = dt(x,df=2), from = -5, to = 5, col = "red", ylim = c(0, 0.45)) # add=T for the other df
# t.test whether in fev dataset, if the height of kids (male and female), between 6,10 (inch)have mean height 90 inches.
read.table("fev_dataset.txt", header = T) -> fev_dat
str(fev_dat)
dim(fev_dat)
subset(fev_dat, Age >= 6 & Age <= 10, select = Ht)
subset(fev_dat, Age >= 6 & Age <= 10)$Ht -> height # or )[["Ht"]]
# we perform the two tailed test corresponding to H_o being true mean of the distribution = 80, then 60 inches
t.test(x=height, mu= 80)
#######
# exercise, calculate the t statistic manually
######## 2018/05/08
#give critical values for the t distribution under type 1 error at 0.01
# give the r expression you would use to calculate the p-value asociated with this test
# reproduce 1-3 for the tests with  mu==80 & mu == 60
mean(height)
sd(height)
(mean(height)-0)/(sd(height)/length(height)^0.5) -> t_value_0
(mean(height)-80)/(sd(height)/length(height)^0.5) -> t_value_80
(mean(height)-60)/(sd(height)/length(height)^0.5) -> t_value_60
# critical value under alpha = 0.01
crit_val_min = qt(0.005, df =length(height)-1)
crit_val_pos = qt(1-0.005, df =length(height)-1)
# p-value calculation
# because of the symmetry around the y-axis, the p-value is equal to twice the AUC on the right side of the test statistic
2*(1-pt(t_value_60, df = 350)) # approximated to 0
# same critical values -2.59 and 2.59
2*(pt(c(t_value_0, t_value_60,t_value_80), df = 350))
# performming the test with R
# H_o is the 0
t.test(x=height)
## H_o is the 80
t.test(x=height, mu= 80)
# we are still in the rejection region though the p-value is significantly larger
# H_o is the 60
t.test(x=height, mu= 60)
#critical value still the same since the same alpha is used
### Two-sample t-test: true difference in means between the heights of boys and girls aged between 6 and 10 (inc.) under alpha = 0.05
subset(fev_dat, Age >= 6 & Age <= 10 & Gender == 0)$Ht->height_girls
subset(fev_dat, Age >= 6 & Age <= 10 & Gender == 1)$Ht->height_boys
t.test(x=height_boys, y=height_girls, alternative = "less", conf.level = 0.95, var.equal = T)
t.test(x=height_boys, y=height_girls, alternative = "greater", conf.level = 0.95, var.equal = T)

## 
library(readxl)
cooking_beans = read_xlsx("/Users/kaimenyi/Documents/shared_ubuntu/Cooking Time Data.xlsx", na= c("", ","," "))
ncol(cooking)

names(cooking_beans)
# different values of countries of origin
# with counts
table(cooking_beans$'Country of Origin')
# Beware table() silently ingores na
# without counts
unique(cooking_beans$'Country of Origin')
# how many NA in that column
sum(is.na(cooking_beans$'Country of Origin'))
# to merge n america with N. Am.
cooking_beans[cooking_beans$'Country of Origin'="N America", 'Country of Origin'] <- "N. Am"
# now transform into a factor
cooking_beans$'Country of Origin' = factor(cooking_beans$'Country of Origin')
str(cooking_beans)
# means of cooking time  per country
cooking_beans %>% group_by('Country of Origin') %>% summarize(mean_cooking_time = mean("Cook time(min), na.rm"))
# remaning new column
mean(cooking_beans[cooking_beans$'Country of Origin'=="Carribean", 'Cook time(min)'])


