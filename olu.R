library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
# read the data for both wild and mutant bin
read.delim("~/module2_R_biostats-master/Datasets/mutant_bin_count.text", header = F) -> mutant_bin
read.delim("~/module2_R_biostats-master/Datasets/wild-type_bin_count.text", header = F) -> wild_type_bin

colnames(mutant_bin)
glimpse(playdata)
# rename the columns of the new variable
colnames(mutant_bin) <- c("Chromosome_part","start","stop","read_count_mut")
colnames(wild_type_bin) <- c("Chromosome_part","start","stop","read_count_wild")

# join both dataet based on identical columns
new_bin <- full_join(mutant_bin, wild_type_bin, by=c("Chromosome_part","start","stop"),copy=FALSE)

# test separation of the first column
separate(mutant_bin, "Chromosome_part", sep ="_",into = c("Chromosome_geno", "part"), remove = FALSE)

# actual separation keeping the raw column and overwrite the original df
new_bin %>% separate("Chromosome_part", sep ="_",into = c("Chromosome_geno", "part"), remove = FALSE) ->new_bin

# separation of Chromosome_geno keeping the raw column and overwrite the original df
new_bin %>% separate("Chromosome_geno", sep ="(?<=[[:digit:]])",into = c("Chromosome", "Genome"), extra = "merge",remove = FALSE)->new_bin

# sneak peek into unique value in columns
unique(c(new_bin$Genome,new_bin$part,new_bin$Chromosome))

#convert all column with integer into numerics
new_bin$start <- as.numeric(new_bin$start)
new_bin$stop <- as.numeric(new_bin$stop)
new_bin$read_count_mut <- as.numeric(new_bin$read_count_mut)
new_bin$read_count_wild <- as.numeric(new_bin$read_count_wild)

# calculate the cumulative sums of start position and end position and append them the df
# duplicate the df
new_bin -> playdata

# cumulative sums of only those with non-na at values used to creat new tibble
playdata %>% filter(is.na(Genome)) %>% mutate(act_stat = cumsum(start))-> playdata_only_chrun

# cumulative sums of only those with na values used to creat new tibble
playdata %>% filter(is.na(Genome)) %>% mutate(act_stat = cumsum(start))-> playdata_only_chrun

# bind the two tibbles into a new tibble
bind_rows(playdata_no_chrun,playdata_only_chrun) ->playdata_new

# calculate the cummulative sum of all stop position irrespective of na values 
playdata_new %>% mutate(actual_stop=cumsum(stop)) -> playdata

# normalize the read count
sum_mut <- sum(playdata$read_count_mut) 
sum_wild <- sum(playdata$read_count_wild)
playdata %>% mutate(norm_count_mut =read_count_mut*10000/sum_mut)  -> playdata
playdata %>% mutate(norm_count_wild =read_count_wild*10000/sum_wild)  -> playdata

# collpased view of the tibble 
glimpse(playdata)
# create a new tibble with no NA value in genome column
playdata %>% filter(!is.na(Genome)) -> playdata_no_chrun
# running ggplot

ggplot(data = playdata, aes(x=actual_stop, y=norm_count_mut-norm_count_wild)) +geom_point()

ggplot(data = playdata, aes(x=actual_stop, y=norm_count_mut-norm_count_wild,color=Chromosome)) +geom_point()+theme_minimal()

ggplot(data = playdata_no_chrun, aes(x=actual_stop, y=norm_count_mut-norm_count_wild,color=Chromosome)) +geom_point()+theme_minimal() 

# a code to review
ggplot(don, aes(x=BPcum, y=-log10(P))) +
  
  # Show all points
  geom_point( aes(color=as.factor(CHR)), alpha=0.8, size=1.3) +
  scale_color_manual(values = rep(c("grey", "skyblue"), 22 )) +
  
  # custom X axis:
  scale_x_continuous( label = axisdf$CHR, breaks= axisdf$center ) +
  scale_y_continuous(expand = c(0, 0) ) +     # remove space between plot area and x axis
  
  # Custom the theme:
  theme_bw() +
  theme( 
    legend.position="none",
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
