setwd("~/module2_R_biostats-master/histo/")
library(ggplot2)
df_set_mer <- read.table(file="99mer_out.histo", sep=" ")

plot(df_set_mer[4:100,], type="l") # plot line graph starting at pont 4 to avoid the bias at point 1

points(df_set_mer[4:100,]) #plot the data points from 2 through 100

sum(as.numeric(df_set_mer[4:nrow(df_set_mer),1]*df_set_mer[4:nrow(df_set_mer),2])) # calculate the total k-mers in the distribution

df_set_mer[4:20,] #examine the actual data points between 5 - 20

ggplot(data=df_set_mer[4:100,], aes(x=V1, y=V2))+ geom_line()+xlab("Kmer")+ylab("Count")+ggtitle("Plot of Kmer distribution")+scale_x_continuous(breaks = seq(0,100, by=10))+ geom_vline(xintercept = 6) # create plot 
ggsave("99mer.pdf") #save said plot

sum(as.numeric(df_set_mer[4:nrow(df_set_mer),1]*df_set_mer[4:nrow(df_set_mer),2]))/6 # Genome Size can be estimated
# with 2 runs
# for 15 kmer = 751347808 ~ 751.3 Mb --total k-mers in d = 7513478077
# for 21 kmer = 927890484 ~ 927.8 Mb --total k-mers in d = 7423123869
# for 27 kmer = 942940361 ~ 942.9 Mb  --total k-mers in d = 7543522887
# for 33 kmer = 909334496 ~ 909.3 Mb  --total k-mers in d = 7274675967
# for 41 kmer = 1145986301 ~ 1.14 Gb --total k-mers in d = 6875917806
# for 45 kmer = 1047778851 ~ 1.04 Gb --total k-mers in d = 6286673106
# for 51 kmer = 995833396 ~ 995.8 Mb  --total k-mers in d = 5975000000
# with all 3 runs
# for 19 kmer = 749650655 ~ 749.6 Mb --total k-mers in d = 11244759825
# for 29 kmer = 836361131 ~ 836.3 Mb --total k-mers in d = 10872694707
# for 39 kmer = 848090355 ~ 848 Mb  --total k-mers in d = 10177084262
# for 49 kmer = 855516842 ~ 855.5 Mb  --total k-mers in d = 9410685263
# for 59 kmer = 865103095 ~ 865.1 Mb  --total k-mers in d = 8651030945
# for 69 kmer = 879932280 ~ 879.9 Mb  --total k-mers in d = 7919390520
# for 79 kmer = 902192820 ~ 902.1 Mb  --total k-mers in d = 7217542562
# for 89 kmer = 934758336 ~ 934.7 Mb  --total k-mers in d = 6543308355
# for 99 kmer = 982633213 ~ 989.6 Mb  --total k-mers in d = 5895799278
