library("ggplot2")
library("readxl")
library("magrittr")
library("tidyr")
library("forcats")
library("dplyr")
library("multcomp")
getwd()
senesence <- read.csv("~/module2_R_biostats-master/Datasets/senescence_data.csv")
wheat_barley_r <- read.csv("~/module2_R_biostats-master/Datasets/Fig1 Data_for_figure_wheat_barley_rice_numbers.csv")
wheatbarice <- gather(wheat_barley_r, value = "value",key = "species", Wheat, Barley, Rice)
ggplot(data= wheatbarice, aes(x=fct_rev(Subfamily), y=value))+ geom_bar(stat="identity")+ facet_wrap(~fct_relevel(species, "A"))+coord_flip()+ theme_minimal()+ labs(x="Family",y="Number of genes")+ theme(axis.line = element_line(color = "black"),strip.text = element_text(hjust = 0))
levels(wheatbarice$species) 
wheatbarice$species<- factor(wheatbarice$species, levels=c("Wheat", "Barley", "Rice"), labels = c("A","B","C"))

##
pdf(file = "TF_fact.pdf")
ggplot(data= wheatbarice, aes(x=Subfamily, y=value))+ geom_bar(stat="identity")+ facet_wrap(~fct_relevel(species, "A"))+coord_flip()+scale_x_discrete(name="Family",limits=rev(levels(as.factor(wheatbarice$Subfamily))))+ theme_minimal()+ theme(axis.line = element_line(color = 'black'), strip.text = element_text(hjust = 0))+labs(x="Family",y="Number of genes")+ scale_y_continuous(name="Number of genes", expand = c(0,25))
dev.off()
## 
# Working workthrough

melted_data <- data %>%
  gather(key= species,value= TF, 2:4) 

#OR
melted_data <- data %>%
  gather(key= species,value= TF, Wheat, Barley, Rice) # it is good practise to write the column names and then later when you come back you will know which columns you chose
colnames(melted_data) <- c("Subfamily", "Species", "Number")
ggplot(data=melted_data, aes(x=Subfamily, y=Number)) + 
  geom_bar(stat="identity") + # plot the actual values (no statistical transformation)
  facet_grid(~ Species) +  # separate panel per species
  coord_flip()  # add flip coordinates to make it vertical

ggplot(data=melted_data, aes(x=Subfamily, y=Number)) +geom_bar(stat="identity") + facet_grid(~ Species) + coord_flip() +scale_x_discrete(limits = rev(levels(as.factor(data$Subfamily)))) # reorder factors within scale_x_disrete

#edit the theme
ggplot(data=melted_data, aes(x=Subfamily, y=Number)) +geom_bar(stat="identity") + facet_grid(~ Species) + coord_flip() +scale_x_discrete(limits = rev(levels(as.factor(data$Subfamily)))) +# reorder factors within scale_x_disrete
  theme_minimal() +  # use minimal theme
  theme(axis.line= element_line(color="black")) # add black axis 


levels(melted_data$Species) # we want to order as wheat, barley then rice and rename as A, B, C
melted_data$Species <- factor(melted_data$Species,levels=c("Wheat","Barley","Rice"),labels=c("A","B","C"))

pdf(file="TF_graph.pdf", width=10, height=12) # set width and height
ggplot(data=melted_data, aes(x=Subfamily, y=Number)) + geom_bar(stat="identity") + facet_grid(~ Species) + coord_flip() +scale_x_discrete(name="Family",limits = rev(levels(as.factor(data$Subfamily)))) +# reorder factors within scale_x_disrete
  scale_y_continuous(name="Number of genes")+ # rename axis
  theme_minimal() + theme(axis.line= element_line(color="black")) + # add black axis 
  theme(strip.text = element_text(hjust=0.05))  # move A, B, C labels to left of each panel
dev.off()
# now add in columns of ratios of wheat/rice and wheat/barley
head(data)
data$wheatbarley <- data$Wheat/data$Barley #calculate ratio of wheat to barley
data$wheatrice <- data$Wheat/data$Rice # calculate ratio of wheat to rice
head(data)
data_ratio <- data[,c(1,5,6)] # just select ratio columns

#make data wide to long
melted_data <- data_ratio %>%
  gather( species, TF, 2:3) 
colnames(melted_data) <- c("Subfamily", "Species", "Ratio")#rename to match rest of script
head(melted_data)

pdf(file="TF_per_species_ratio.pdf", width=4, height=12) # print to pdf
ggplot(data=melted_data, aes(x=Subfamily, y=Ratio, group=Species, color=Species)) + # need to add group to get the geom_line to plot correctly
  geom_point() +  # addd scatter points
  geom_line() + # add lines between scatter points
  coord_flip()  + # swap x and y axis
  scale_y_continuous(limits = c(0, 10), breaks=c(0,2,4,6,8,10))+ #set breaks on ratio axis
  scale_x_discrete(name="Family",limits = rev(levels(as.factor(data$Subfamily)))) +#reorder names of subfamily
  theme_minimal() +  #set theme
  theme(axis.line= element_line(color="black")) + #add axis line
  geom_abline(intercept = 3, slope = 0) + # add line at ratio=3
  theme(legend.position ="none")# add in a line at x =3
dev.off() # close the pdf file

# senesence data
senesence$daytoflag <- as.numeric(as.Date(senesence$Flag.leaf.senescence.date, "%d/%m/%Y") - as.Date(senesence$Heading.date, "%d/%m/%Y"))
senesence$Daytopedun <- as.numeric(as.Date(senesence$Peduncle.senescence.date, "%d/%m/%Y") - as.Date(senesence$Heading.date, "%d/%m/%Y"))
senesence %>% gather(key=Genotype, value = daytoflag) 

ggplot(data=senesence, aes(y=daytoflag, x=Genotype))+ geom_boxplot()+ facet_wrap(~block)+theme(axis.text.x=element_text(angle=45)) # plot boxplots, separated by blocks, with x labels tilted
senesence$Genotype <- factor(senesence$Genotype, levels=c("Kronos WT","WT WT","hom WT","WT hom","hom hom"))
ggplot(data=senesence, aes(y=daytoflag, x=Genotype, fill=Genotype))+ geom_boxplot()+ facet_wrap(~block)+theme_minimal()+theme(axis.text.x=element_text(angle=45),strip.text = element_text(hjust=1)) + scale_fill_manual(values = c("white", "green", "green", "green", "green")) #color the fills     
## q3
subset(senesence, subset = block=="NAM2 block 4") -> newsendat
ggplot(data= newsendat, aes(y=daytoflag, x=Genotype))+ geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+ facet_wrap(~subblock)+theme_minimal()+theme(axis.text.x=element_text(angle=45),strip.text = element_text(hjust=0.05)) #Violin plots with quantiles
## q4
anova(lm(daytoflag~Genotype, data=senesence))# anova of the linear model 
aov(daytoflag~Genotype, data=senesence) -> senaov #do the analysis of variance of entire dataset
summary(senaov) #show the summary table
TukeyHSD(senaov)
boxplot(daytoflag~Genotype, data=senesence)#graphical summary
#get means of enotypes day to flg snnce
senesence %>% group_by(block, Genotype) %>% summarise(mean_geno = mean(daytoflag))
#do the analysis of variance per block
aov(daytoflag~Genotype, data=subset(senesence, block=="NAC1"))->nac1_aov
aov(daytoflag~Genotype, data=subset(senesence, block=="NAM2 block 4"))->nam2b2_aov
aov(daytoflag~Genotype, data=subset(senesence, block=="NAM2 block 5"))->nam2b5_aov
aov(daytoflag~Genotype, data=subset(senesence, block=="NAM2 block 6"))->nam2b6_aov
# use a post-hoc test to show which genotypes are different from each other for the most significant block
# which genotypes are different from each other in this block according to the post-hoc test?
# show the summary table per block
summary(nac1_aov)
summary(nam2b2_aov)
summary(nam2b5_aov)
summary(nam2b6_aov)
# post-hoc test per block 
TukeyHSD(nac1_aov)
TukeyHSD(nam2b2_aov)
TukeyHSD(nam2b5_aov)
TukeyHSD(nam2b6_aov)
# which genotypes are different from each other 
##
# 1) calculate the number of days from Heading.date to Flag.leaf.senescence.date for all plants # HINT: convert these columns into dates recognised by R

setwd("C:\\Users\\borrillp\\Documents\\Rgraphics\\dataSets\\")
sen_data <- read.csv(file="senescence_data.csv")
head(sen_data)

# convert dates into date format recognised by R
sen_data$Heading <- as.Date(sen_data$Heading.date, "%d/%m/%Y")
sen_data$Flag.leaf.senescence <- as.Date(sen_data$Flag.leaf.senescence.date, "%d/%m/%Y")

head(sen_data)
sen_data$days.to.flag <- sen_data$Flag.leaf.senescence - sen_data$Heading
head(sen_data$days.to.flag)

# 2) use ggplot2 to plot boxplots of the number of days from heading to flag leaf senescence (the values you calculated in #1). 
# Each boxplot should show one genotype, and different blocks should be split into different panels of the graph.
# You may need to convert the newly created days from heading to flag leaf senescence into a numerical column
# The genotypes should be in the order left to right: "Kronos WT", "WT WT", "hom WT", "WT hom", "hom hom"
# Colour the boxes for "Kronos WT" in white, and all other boxes in green
# Rotate the x-axis labels so they are at 45 degrees

sen_data$days.to.flag <- as.numeric(sen_data$days.to.flag) # convert to numeric

library("ggplot2")
# change order of levels
levels(sen_data$Genotype)
sen_data$Genotype <- factor(sen_data$Genotype,
                            levels=c("Kronos WT", "hom hom", "hom WT","WT hom", "WT WT"))
levels(sen_data$Genotype) # check levels now correct

ggplot(sen_data, aes(x=Genotype, y=days.to.flag)) + geom_boxplot(aes(fill=Genotype)) +scale_fill_manual(values= # scale_fill_manual adjusts colours of boxplots
                      c("Kronos WT"="white","hom hom"="green","hom WT" = "green","WT hom" = "green",
                        "WT WT" = "green"))+facet_wrap(~block)+theme(axis.text.x=element_text(angle=45, vjust=0.4)) # change angle of x axis text and move it down a bit
# you could set hjust =1

# 3) use ggplot2 to plot violin plots for block "NAM2 block 4", split the different sublocks into separate panels, and make a separate violin plot for each genotype
# add lines showing the quantiles (25 %, 50% and 75 %) to the violin plots

ggplot(subset(sen_data,block=="NAM2 block 4"), aes(x=Genotype, y=days.to.flag)) +
  geom_violin(aes(fill=Genotype), draw_quantiles = c(0.25, 0.5, 0.75)) + # use geom_violinplot and add quantiles using draw_quantiles
  scale_fill_manual(values= # scale_fill_manual adjusts colours of boxplots
                      c("Kronos WT"="white","hom hom"="green","hom WT" = "green","WT hom" = "green",
                        "WT WT" = "green"))+facet_wrap(~subblock)+theme(axis.text.x=element_text(angle=45,hjust=1)) # change angle of x axis text and move it down a bit

# 4) calculate whether the genotypes "hom hom", "hom WT", "WT hom" and "WT WT" are statistically 
# first remove KronosWT
sen_data_no_Kronos <- sen_data[sen_data$Genotype!="Kronos WT",]
head(sen_data_no_Kronos)

install.packages("broom")
library("broom")
library("tidyr")
library("dplyr")
Genotype <- "NAC1" # set genotype as an example
aov(lm(days.to.flag ~ Genotype, data = sen_data_no_Kronos)) # anova on its own
tidy(aov(lm(days.to.flag ~ Genotype, data = sen_data_no_Kronos)))

stat_result <- sen_data_no_Kronos %>% group_by(block) %>% do(tidy(aov(lm(days.to.flag ~ Genotype, data = .)))) %>%filter(term=="Genotype")

as.data.frame(stat_result)
(stat_result$p.value)

# illegal for loop method:
for (block in unique(sen_data$block)) {sel_data <- sen_data[sen_data$block ==block,]
  linear.model <- lm(days.to.flag ~ Genotype, data=sel_data[sel_data$Genotype == "WT WT" |
                                                              sel_data$Genotype == "WT hom" | 
                                                              sel_data$Genotype == "hom WT" | 
                                                              sel_data$Genotype == "hom hom",])
  print(block)
  print(summary(aov(linear.model)))
}

#now tukey HSD post hoc test for NAM2 block 5
block <- "NAM2 block 5"
sel_data <- sen_data[sen_data$block ==block,] # select block 5 data
linear.model <- lm(days.to.flag ~ Genotype, data=sel_data[sel_data$Genotype == "WT WT" |
                                                            sel_data$Genotype == "WT hom" | 
                                                            sel_data$Genotype == "hom WT" | 
                                                            sel_data$Genotype == "hom hom",])
print(block)
print(summary(aov(linear.model)))
TukeyHSD(aov(linear.model)) # run a tukey HSD test on the anova result

# p<0.05 for hom WT vs hom hom, WT hom vs hom WT, WT WT vs hom WT
# non significant for WT hom vs hom hom, WT WT vs hom hom and WT WT vs WT hom
# 5) BONUS: Is the number of days from heading to flag leaf senescence related to the number of days from heading to peduncle senescence? 
sen_data$Peduncle.senescence <- as.Date(sen_data$Peduncle.senescence.date, "%d/%m/%Y")
sen_data$days.to.peduncle <- as.numeric(sen_data$Peduncle.senescence - sen_data$Heading) # calculate the number of days from heading to peduncle senescence and save as numerical variable

head(sen_data)
# make a linear model relating days to flag with days to peduncle senescence
sen_model <- lm(days.to.peduncle ~ days.to.flag, data=sen_data)
summary(sen_model) # look at the summary

tidy(anova(sen_model)) # could use broom package to get tidy output from anova
anova(sen_model)$'Pr(>F)'[1]

summary(sen_model)$r.squared
rsq <- summary(sen_model)$r.squared # save the rsquared as an object
pval <- anova(sen_model)$'Pr(>F)'[1] # save the pvalue as an object
rsq
pval

ggplot(sen_data, aes(x=days.to.flag, y=days.to.peduncle)) +geom_point() +geom_smooth(method="lm",se=F) + # fit line and remove standard error
  annotate("text",x=30, y=70, # add text at position x=30, y=70
           label=paste0("rsquared=",round(rsq,2), " p-value=",round(pval,2))) # paste together the text rsquared with the value for rsq, text for p-value and the value for pval


# now do it for each block separately, I will use a for loop
# make a linear model relating days to flag with days to peduncle senescence

for (block in unique(sen_data$block)) {
  sel_data <- sen_data[sen_data$block == block,] # select the  block
  sen_model <- lm(days.to.peduncle ~ days.to.flag, data=sel_data)
  summary(sen_model) # look at the summary
  
  rsq <- summary(sen_model)$r.squared # save the rsquared as an object
  pval <- anova(sen_model)$'Pr(>F)'[1] # save the pvalue as an object
  rsq
  pval
  
  pdf(file=paste0(block,"linearmodel.pdf")) # write to pdf
  myplot <- ggplot(sel_data, aes(x=days.to.flag, y=days.to.peduncle)) +geom_point() +geom_smooth(method="lm",se=F) +annotate("text",x=0.5*max(sel_data$days.to.flag), y=0.99*max(sel_data$days.to.peduncle), # add text at position x and y (they are a proportion of the maximum for that axis)
             label=paste0("rsquared=",round(rsq,2), " p-value=",round(pval,2))) # paste together the text rsquared with the value for rsq, text for p-value and the value for pval
  print(myplot) # print the plot into the pdf (otherwise it won't print in a loop)
  dev.off()
}

# NAC1 has a weaker relationship (r2 = 0.2) compared to the other blocks but it is still highly significant