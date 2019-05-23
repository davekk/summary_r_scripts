library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library('magrittr')
# read the data for both wild and mutant bin
read.delim("G:/knowlegde/R_stuff/Datasets/mutant_bin_count.text", header = F) -> mutant_bin
read.delim("G:/knowlegde/R_stuff/Datasets/wild-type_bin_count.text", header = F) -> wild_type_bin

colnames(mutant_bin) # column names
glimpse(wild_type_bin) #dimensions and reduced view
# rename the columns of the new variable
colnames(mutant_bin) <- c("Chromosome_part","start","stop","read_count_mut")
colnames(wild_type_bin) <- c("Chromosome_part","start","stop","read_count_wild")

# join both dataet based on identical columns
new_bin <- full_join(mutant_bin, wild_type_bin, by=c("Chromosome_part","start","stop"),copy=FALSE)

# test separation of the first column
separate(mutant_bin, "Chromosome_part", sep ="_",into = c("Chromosome_geno", "part"), remove = FALSE)

# actual separation keeping the raw column and overwrite the original df
new_bin %<>% separate("Chromosome_part", sep ="_",into = c("Chromosome_geno", "part"), remove = FALSE) 
unique(new_bin$Chromosome_geno) # unique chromosome with gnome
# separation of Chromosome_geno keeping the raw column and overwrite the original df
new_bin %<>% separate("Chromosome_geno", sep ="(?<=[[:digit:]])",into = c("Chromosome", "Genome"), extra = "merge",remove = FALSE)
unique(new_bin$Chromosome) # unique chromosomes
unique(new_bin[4])
unique(new_bin$Genome) # unique genomes
# sneak peek into unique value in columns
unique(c(new_bin$Genome,new_bin$part,new_bin$Chromosome))

#convert all column with integer into numerics
new_bin$start <- as.numeric(new_bin$start)
new_bin$stop <- as.numeric(new_bin$stop)
new_bin$read_count_mut <- as.numeric(new_bin$read_count_mut)
new_bin$read_count_wild <- as.numeric(new_bin$read_count_wild)

# calculate the cumulative sums of start position and end position and append them the df

# cumulative sums of only those with na at genome colum values used to create new tibble
new_bin %>% filter(is.na(Genome)) %>% mutate(act_stat = cumsum(start))-> playdata_only_chrun
unique(playdata_only_chrun$Genome)
# cumulative sums of only those with non-na values  in genome column used to create new tibble
new_bin %>% filter(!is.na(Genome)) %>% mutate(act_stat = cumsum(start))-> playdata_no_chrun
unique(playdata_no_chrun$Genome)
# bind the two tibbles into a new tibble
bind_rows(playdata_no_chrun,playdata_only_chrun) ->playdata_new

# calculate the cummulative sum of all stop position irrespective of na values 
playdata_new %>% mutate(actual_stop=cumsum(stop)) -> playdata

# normalize the read count
sum_mut <- sum(playdata$read_count_mut) 
sum_wild <- sum(playdata$read_count_wild)
playdata %<>% mutate(norm_count_mut =read_count_mut*10000/sum_mut)
playdata %<>% mutate(norm_count_wild =read_count_wild*10000/sum_wild)

# collpased view of the tibble 
glimpse(playdata)

# running ggplot

ggplot(data = playdata, aes(x=actual_stop, y=norm_count_mut-norm_count_wild)) +geom_point()

ggplot(data = playdata, aes(x=actual_stop, y=norm_count_mut-norm_count_wild,color=Chromosome)) +geom_point()+theme_minimal()
# plot on each dataset (mutant)
ggplot(data = playdata, aes(x=actual_stop, y=-log10(norm_count_mut),color=Chromosome)) +geom_point()+theme_minimal()
# plot(wildtype)
ggplot(data = playdata, aes(x=actual_stop, y=-log10(norm_count_wild),color=Chromosome)) +geom_point()+theme_minimal()



# a code to review
ggplot(playdata, aes(x=actual_stop, y=-log10(norm_count_mut-norm_count_wild))) +
  
  # Show all points
  geom_point(aes(color=as.factor(Chromosome)), alpha=0.8, size=1.3) +
#  scale_color_manual(values = rep(c("grey", "skyblue"), 22 )) +
  
  # custom X axis:
  scale_x_continuous( label = playdata$Chromosome, breaks= playdata$act_stat ) +
  scale_y_continuous(expand = c(0, 0) ) +     # remove space between plot area and x axis
  
  # Custom the theme:
  theme_bw() +
  theme( 
    legend.position="none",
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
