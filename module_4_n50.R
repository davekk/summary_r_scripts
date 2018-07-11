library(dpylr)
library(ggplot2)
setwd("~/module2_R_biostats-master/module_4/trinity/")
lenght<- read.table("length.txt", header = F, sep= "\t", stringsAsFactors = T) 
# read the tsv
colnames(lenght) <- c("Contig_Name", "Contig_length")
# add header to contig file
new<- lenght[order(as.numeric(-lenght$Contig_length)),]
mutate(new, cum_length=cumsum(as.numeric(new$Contig_length)))->new_length                     
plot(as.numeric(new_length$cum_length)) # the plot of cummulative sum
plot(as.numeric(new_length$cum_length),ylab="Size of genome in bp", col="blue", cex=0.5) #histogram of contig length
ggplot(data=new_length, aes(x=new_length$cum_length)) + geom_histogram(col="green")
new_length[new_length$cum_length > max(new_length$cum_length)/2,][1,] # calculates the contig length at N50
# saving the histogram of contig length
jpeg(file="Histogram of contig length.jpeg", height=1000, width=1000)
par(mar=c(20,11,4,3)+0.1,mgp=c(6,1,0))
hist(as.numeric(new_length$Contig_length),breaks=101, border =  "blue", xlab = "Contig_length", ylab="Frequency", ylim=c(0, 1500), main = "Histogram of contig cummulative frequency")
dev.off()