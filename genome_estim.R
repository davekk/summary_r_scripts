setwd("~/module2_R_biostats-master/histo/")

df_set_mer <- read.table(file="51mer_out.histo", sep=" ")
#"15mer_out.histo" "21mer_out.histo" "27mer_out.histo" "33mer_out.histo" "41mer_out.histo" "45mer_out.histo" "51mer_out.histo"
plot(df_set_mer[4:100,], type="l") # plot line graph starting at pont 4 to avoid the bias at point 1
points(df_set_mer[4:100,]) #plot the data points from 2 through 100

sum(as.numeric(df_set_mer[4:nrow(df_set_mer),1]*df_set_mer[4:nrow(df_set_mer),2])) # calculate the total k-mers in the distribution

df_set_mer[4:20,] #examine the actual data points between 5 - 20

ggplot(data=df_set_mer[4:100,], aes(x=V1, y=V2))+ geom_line()+xlab("Kmer")+ylab("Count")+ggtitle("Plot of Kmer distribution")+scale_x_continuous(breaks = seq(0,100, by=10))+ geom_vline(xintercept = 6) # create plot 
ggsave("51mer.pdf") #save said plot

sum(as.numeric(df_set_mer[4:nrow(df_set_mer),1]*df_set_mer[4:nrow(df_set_mer),2]))/6 # Genome Size can be estimated
#with the new data
# for 15 kmer = 751347808 ~ 751.3 Mb
# for 21 kmer = 927890484 ~ 927.8 Mb
# for 27 kmer = 1257253815 ~ 1.25 Gb
# for 33 kmer = 1212445995 ~ 1.21 Gb
# for 41 kmer = 1145986301 ~ 1.14 Gb
# for 45 kmer = 1047778851 ~ 1.04 Gb
# for 51 kmer = 995833396 ~ 995.8 Mb
