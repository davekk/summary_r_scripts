# A %>% B() ->A = A %<>% B() - this mutates A with B() function.
# cookbook-r.com
install.packages("ggrepel") 
library("magrittr")
library("ggplot2")
library("ggrepel")
getwd()
housing<-read.csv("~/Rgraphics/dataSets/landdata-states.csv")
hist(housing$Home.Value) # base histogram plot
plot(Home.Value ~ Date, data=subset(housing, State == "MA"))
points(Home.Value ~ Date, col="red", data=subset(housing, State == "TX")) 
legend(1975, 400000, c("MA", "TX"), title="State",col=c("black", "red"), pch=c(1, 1))
ggplot(data=housing, aes(x=Home.Value)) + geom_histogram()
ggplot(data=housing, aes(x=Home.Value, col= "red")) + geom_histogram() # border colors
ggplot(data=housing, aes(x=Home.Value, fill= "red")) + geom_histogram() # fill color
ggplot(data=housing, aes(x=Home.Value)) + geom_histogram(col="green", fill= "blue") # border colors
## now what
ggplot(subset(housing, State %in% c("MA", "TX")), aes(x=Date, y= Home.Value, color= State)) + geom_point() # color by state
ggplot(subset(housing, State %in% c("MA", "TX")), aes(x=Date, y= Home.Value)) + geom_point() # non-color coded
ggplot(subset(housing, State %in% c("MA", "TX")), aes(x=Date, y= Home.Value, col=State)) + geom_point()+ scale_color_manual(values =c("green", "purple")) # manual color
hp2001Q1 <- subset(housing, Date == 2001.25) 
ggplot(hp2001Q1, aes(y = Structure.Cost, x = Land.Value)) + geom_point() # since the points are far scattered, log transform the x values to highlight the associations
ggplot(hp2001Q1, aes(y = Structure.Cost, x = log(Land.Value))) + geom_point() # x_values are log transformed,#1
ggplot(hp2001Q1, aes(y = Structure.Cost, x = Land.Value)) + geom_point() + scale_x_log10() # 2 way to log transform
hp2001Q1$pred.SC <- predict(lm(Structure.Cost ~ log(Land.Value), data = hp2001Q1)) # create a linear model column in the dataset
p1 <- ggplot(hp2001Q1, aes(x = log(Land.Value), y = Structure.Cost)) # create a 
p1 + geom_point(aes(color = Home.Value)) +  geom_line(aes(y = pred.SC))
p1 + geom_point(aes(color = Home.Value)) + geom_smooth()
p1 + geom_text(aes(label=State), size = 3)
p1 + geom_point() + geom_text_repel(aes(label=State), size = 3)
dat <- read.csv("~/Rgraphics/dataSets/EconomistData.csv")
head(dat)
ggplot(dat, aes(x = CPI, y = HDI, size = HDI.Rank)) + geom_point()
# Create a scatter plot with CPI on the x axis and HDI on the y axis.
ggplot(data = dat, aes(x= CPI, y=HDI)) + geom_point()
#Color the points blue
ggplot(data = dat, aes(x= CPI, y=HDI)) + geom_point(col= "blue")
# Map the color of the the points to Region
ggplot(data = dat, aes(x= CPI, y=HDI, col=Region)) + geom_point()
#Make the points bigger by setting size to 2
ggplot(data = dat, aes(x= CPI, y=HDI, col=Region)) + geom_point(size=2)
# Map the size of the points to HDI.Rank
ggplot(data = dat, aes(x= CPI, y=HDI, col=Region, size=HDI.Rank)) + geom_point()
p2 <- ggplot(housing, aes(x = Home.Value))
p2 + geom_histogram()
p2 + geom_histogram(stat = "bin", binwidth=4000)
housing.sum <- aggregate(housing["Home.Value"], housing["State"], FUN=mean)
rbind(head(housing.sum), tail(housing.sum))
ggplot(housing.sum, aes(x=State, y=Home.Value)) + geom_bar(stat="identity")
# Re-create a scatter plot with CPI on the x axis and HDI on the y axis (as you did in the previous exercise)
ggplot(data=dat, aes(x= CPI, y=HDI)) ->p_1 
p_1 + geom_point(col="green")
# Overlay a smoothing line on top of the scatter plot using geom_smooth
p_1 + geom_point(col="green") + geom_smooth()
# Overlay a smoothing line on top of the scatter plot using geom_smooth, but use a linear model for the predictions. Hint: see  ?stat_smooth
p_1 + geom_point() + geom_smooth(method = "lm", se=FALSE)
# Overlay a smoothing line on top of the scatter plot using geom_line. Hint: change the statistical transformation
p_1 + geom_point() + geom_line(stat = "smooth", method = "loess")
# BONUS: Overlay a smoothing line on top of the scatter plot using the default loess method, but make it less smooth. Hint: see ?loess
p_1 + geom_point() + geom_smooth(method = "loess", span = 0.4) + theme_light()
# change background color with + theme
# scale and themes
p3 <- ggplot(housing,aes(x = State, y = Home.Price.Index)) + theme(legend.position="top",axis.text=element_text(size = 6))
# 
(p4 <- p3 + geom_point(aes(color = Date),alpha = 0.5,size = 1.5,position = position_jitter(width = 0.25, height = 0)))
p4 + scale_x_discrete(name="State Abbreviation") + scale_color_continuous(name="",breaks = c(1976, 1994, 2013),labels = c("'76", "'94", "'13"))
p4 +scale_x_discrete(name="State Abbreviation") +scale_color_continuous(name="",breaks = c(1976, 1994, 2013), labels = c("'76", "'94", "'13"),low = "blue", high = "red")
library(scales)
p4 +scale_color_continuous(name="",breaks = c(1976, 1994, 2013),labels = c("'76", "'94", "'13"),low = muted("blue"), high = muted("red"))
p4 +scale_color_gradient2(name="",breaks = c(1976, 1994, 2013),labels = c("'76", "'94", "'13"),low = muted("blue"),high = muted("red"),mid = "gray60",midpoint = 1994)
# Create a scatter plot with CPI on the x axis and HDI on the y axis. Color the points to indicate region
ggplot(data=dat, aes(x= CPI, y=HDI, col = Region)) + geom_point()
# Modify the x, y, and color scales so that they have more easily-understood names (e.g., spell out “Human development Index” instead of “HDI”).
ggplot(data=dat, aes(x= CPI, y=HDI, col = Region)) + geom_point() + scale_x_continuous(name="Corruption Perception Index") + scale_y_continuous(name="Human development Index")+ scale_color_manual(name="World region",values = c("blue", "green", "grey", "purple", "violet", "red"))
# Modify the color scale to use specific values of your choosing. Hint: see ?scale_color_manual
ggplot(data=dat, aes(x= CPI, y=HDI, col = Region)) + geom_point() + scale_x_continuous(name="Corruption Perception Index") + scale_y_continuous(name="Human development Index")+ scale_color_manual(name="World region",values = c(SSA="blue", 'EU W.Europe' = "green", 'East EU Cemt Asia' = "grey", 'Asia Pacific' =  "purple", Americas = "violet", MENA = "red")) # this specifies the colo per region.
p5 <- ggplot(housing, aes(x = Date, y = Home.Value))
p5 + geom_line(aes(color = State))  
(p5 <- p5 + geom_line() +facet_wrap(~State, ncol = 10))
p5 + theme_linedraw()
p5 + theme_light()
p5 + theme_minimal() +theme(text = element_text(color = "turquoise"))
theme_new <- theme_bw() +theme(plot.background = element_rect(size = 1, color = "blue", fill = "black"), text=element_text(size = 12, family = "Serif", color = "ivory"),axis.text.y = element_text(colour = "purple"),axis.text.x = element_text(colour = "red"),panel.background = element_rect(fill = "pink"),strip.background = element_rect(fill = muted("orange")))
p5 + theme_new

housing.byyear <- aggregate(cbind(Home.Value, Land.Value) ~ Date, data = housing, mean)
ggplot(housing.byyear,aes(x=Date)) +geom_line(aes(y=Home.Value), color="red") +geom_line(aes(y=Land.Value), color="blue") # this is wrong
library(tidyr)
home.land.byyear <- gather(housing.byyear,value = "value",key = "type",Home.Value, Land.Value)
ggplot(home.land.byyear,aes(x=Date,y=value,color=type)) +geom_line() # the right way to do it
# recreating magazine graph
ggplot(data=dat, aes(x= CPI, y=HDI)) + geom_point(aes(color=Region), size = 2, shape= 1, stroke = 1.25)+ geom_smooth(method = "lm", formula = y~x + log(x), se = FALSE, color = "red", mapping = aes(linetype= "r2"))+ geom_text_repel(aes(label = Country),color = "gray20",data = subset(dat, Country %in% pointsToLabel)) + scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10=least corrupt)",limits = c(.9, 10.5),breaks = 1:10) +scale_y_continuous(name = "Human Development Index, 2011 (1=Best)",limits = c(0.2, 1.0),breaks = seq(0.2, 1.0, by = 0.1)) +scale_color_manual(name = "",values = c("#24576D","#099DD7","#28AADC","#248E84","#F2583F","#96503F")) +ggtitle("Corruption and Human development") + theme_minimal() + theme(text = element_text(color = "gray20"), legend.position = c("top"), legend.direction = "horizontal",legend.justification = 0.1,legend.text = element_text(size = 11, color = "gray10"),axis.text = element_text(face = "italic"),axis.title.x = element_text(vjust = -1),axis.title.y = element_text(vjust = 2),axis.ticks.y = element_blank(),axis.line = element_line(color = "gray40", size = 0.5),axis.line.y = element_blank(), panel.grid.major = element_line(color = "gray50", size = 0.5),panel.grid.major.x = element_blank())


levels(dat$Region)
dat$Region <- factor(dat$Region, levels = c("EU W. Europe", "Americas", "Asia Pacific", "East EU Cemt Asia", "MENA","SSA"), c("OECD", "Americas", "Asia &\nOceana", "Central &\nEastern Europe", "Midle East &\nnorth Africa","Sub-Saharan \nAfrica"))
## A look at all 25 symbols
df2 <- data.frame(x = 1:5 , y = 1:25, z = 1:25)
s <- ggplot(df2, aes(x = x, y = y))
s + geom_point(aes(shape = z), size = 4) + scale_shape_identity()
## While all symbols have a foreground colour, symbols 19-25 also take a
## background colour (fill)
s + geom_point(aes(shape = z), size = 4, colour = "Red") +
  scale_shape_identity()
s + geom_point(aes(shape = z), size = 4, colour = "Red", fill = "Black") +
  scale_shape_identity()
# shpwn tutorial
pc1 <- ggplot(dat, aes(x = CPI, y = HDI, color = Region))
pc1 + geom_point()
pc2 <- pc1 +geom_smooth(mapping = aes(linetype = "r2"),method = "lm",formula = y ~ x + log(x), se = FALSE, color = "red")
pc2 + geom_point()
pc2 +geom_point(shape = 1, size = 4)
(pc3 <- pc2 + geom_point(shape = 1, size = 2.5, stroke = 1.25))
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan","Afghanistan", "Congo", "Greece","Argentina", "Brazil","India", "Italy", "China", "South Africa", "Spane","Botswana", "Cape Verde","Bhutan", "Rwanda", "France","United States", "Germany", "Britain", "Barbados", "Norway", "Japan","New Zealand", "Singapore")
(pc4 <- pc3 +geom_text(aes(label = Country),color = "gray20",data = subset(dat, Country %in% pointsToLabel)))
pc4$data <- dat
pc4
library(grid)
(pc5 <- pc4 +scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10=least corrupt)",limits = c(.9, 10.5),breaks = 1:10) +scale_y_continuous(name = "Human Development Index, 2011 (1=Best)",limits = c(0.2, 1.0),breaks = seq(0.2, 1.0, by = 0.1)) +scale_color_manual(name = "",values = c("#24576D","#099DD7","#28AADC","#248E84","#F2583F","#96503F")) +ggtitle("Corruption and Human development"))
(pc6 <- pc5 +
  theme_minimal() + # start with a minimal theme and add what we need
  theme(text = element_text(color = "gray20"),
        legend.position = c("top"), # position the legend in the upper left 
        legend.direction = "horizontal",
        legend.justification = 0.1, # anchor point for legend.position.
        legend.text = element_text(size = 11, color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1), # move title away from axis
        axis.title.y = element_text(vjust = 2), # move away for axis
        axis.ticks.y = element_blank(), # element_blank() is how we remove elements
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank()
        ))
mR2 <- summary(lm(HDI ~ CPI + log(CPI), data = dat))$r.squared
mR2 <- paste0(format(mR2, digits = 2), "%")

# final command 
png(file = "images/econScatter10.png", width = 800, height = 600)

p <- ggplot(dat,mapping = aes(x = CPI, y = HDI)) +geom_point(mapping = aes(color = Region),shape = 1,size = 4,stroke = 1.5) +geom_smooth(mapping = aes(linetype = "r2"),method = "lm",formula = y ~ x + log(x), se = FALSE,color = "red") +geom_text_repel(mapping = aes(label = Country, alpha = labels),data = transform(dat,labels = Country %in% c("Russia","Venezuela","Iraq","Mayanmar","Sudan","Afghanistan","Congo","Greece","Argentinia","Italy","Brazil","India","China","South Africa","Spain","Cape Verde","Bhutan","Rwanda","France","Botswana","France","US","Germany","Britain","Barbados","Japan","Norway","New Zealand","Sigapore"))) +scale_x_continuous(name = "Corruption Perception Index, 2011 (10=least corrupt)",limits = c(1.0, 10.0),breaks = 1:10) +scale_y_continuous(name = "Human Development Index, 2011 (1=best)",limits = c(0.2, 1.0),breaks = seq(0.2, 1.0, by = 0.1)) +scale_color_manual(name = "",values = c("#24576D","#099DD7","#28AADC","#248E84","#F2583F","#96503F"),guide = guide_legend(nrow = 1)) +scale_alpha_discrete(range = c(0, 1),guide = FALSE) +scale_linetype(name = "",breaks = "r2",labels = list(bquote(R^2==.(mR2))),guide = guide_legend(override.aes = list(linetype = 1, size = 2, color = "red"))) +ggtitle("Corruption and human development") +theme_bw() +theme(panel.border = element_blank(),panel.grid = element_blank(),panel.grid.major.y = element_line(color = "gray"),axis.line.x = element_line(color = "gray"),axis.text = element_text(face = "italic"),legend.position = "top",legend.direction = "horizontal",legend.box = "horizontal",legend.text = element_text(size = 12),plot.title = element_text(size = 16, face = "bold"))
p
dev.off()
# working with faceted plots
ggplot(data=dat, aes(x= CPI, y=HDI)) + geom_point(aes(color=Region), size = 2, shape= 1, stroke = 1.25)+ geom_smooth(method = "lm", formula = y~x + log(x), se = FALSE, color = "red", mapping = aes(linetype= "r2"))+ geom_text_repel(aes(label = Country),color = "gray20",data = subset(dat, Country %in% pointsToLabel))+ theme_gray() +theme(legend.position="top",strip.background = element_rect(fill = "blue"),strip.text = element_text(color = "white", size = 10, face = "bold"), plot.title = element_text(size = 12, face = "italic")) +labs (title = "plot of HDI against CPI", color = "red")+ facet_wrap(~Region)
