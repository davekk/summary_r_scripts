library(tidyverse)

#read in the input data
Data <- read.table(file="Pithy_sig.txt", header = T)
View(Data)

#here I find the -log10 of the p values 
Data$log <- -log10(data$P_value)

#I re-arrange the data by chr and position number
Data <- arrange(Data, Chr, Position)

#here I calculate a whole genome position for the locus instead of the chromosome position. This is important to plot the data in a continuous linear format
Data$wg_pos <- cumsum(Data$Position)

#Here I make a dataframe defining the limit of the rectangles that will be drawn over the plot
rect <- data.frame(ystart = seq(0,2,1), yend = seq(1,3,1), col = c("GL", "GW", "HGW"))

#Here I make the plot with ggplot
#the plot contain 2 geom objetcs
  #geom_jitter (or geom_point) with the aes set such that the color represent the traits and the size of the point represent the -log10 value 
  #geom_rect - i drew three rectangle whose boundaries have been set in the rect dataframe. The trick here is to also offset the boundaries by 0.5
ggplot(data=Data)+geom_jitter(aes(x=Data$wg_pos, y = Data$Trait, size = Data$log, fill = Trait), shape = 21 )+
  geom_rect(data = rect, aes(xmin=0, xmax=1000, ymin=ystart+0.5, ymax=yend+0.5, fill =col), alpha =0.2)+
  scale_x_continuous(expand = c(0,0))+scale_y_discrete(expand = c(0,0))


