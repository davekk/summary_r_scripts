getwd()
setwd("~/limidb_data/")
# read the microbiome data 
otu_dense <- read.table("./chick_micr_feature-table.tsv", 
                        sep="\t", numerals = "no.loss", header=TRUE)
otu_sparse <- read.table("chick_micr_feature-table_melted.tsv",
                         sep="\t", numerals = "no.loss", header=TRUE)
taxonomy <- read.table("chick_micr_taxonomy.tsv", sep="\t", 
                       numerals = "no.loss", header=TRUE)
metadata <- read.table("chick_micr_metadata.tsv", sep="\t", 
                       numerals = "no.loss", header=TRUE)
#install and load needed 
install.packages('GGally')
install.packages('ggparallel')
require(GGally); require(tidyverse);require('ggparallel')

paste0("otu",c(1:nrow(otu_dense))) ->otu_dense$sid # give a shorter otu id
otu_dense%>% select(OTU.ID,sid) -> temp_names # get the id and temp id
names(taxonomy) <- c("OTU.ID","Taxon","Confidence") # rename the colunms
full_join(taxonomy,temp_names, by="OTU.ID") ->new_tax # assign new names to tax tibble
full_join(otu_sparse,temp_names, by="OTU.ID") ->new_otu_sp # assign new names to otu tibble

# drop the looong otu id
dplyr::select(new_otu_sp, -'OTU.ID') ->final_otu_sp
dplyr::select(new_tax, -'OTU.ID') ->final_tax
dplyr::select(otu_dense, -'OTU.ID') ->final_otu_d

# rename the sid 
names(final_otu_sp)[3] <- 'OTU.ID'
names(final_tax)[3] <- 'OTU.ID'
names(final_otu_d)[37] <- 'OTU.ID'

# re-arrange the columns
final_otu_sp[,c("OTU.ID","Sample.ID","abundance")] ->final_otu_sp
final_tax[,c("OTU.ID","Taxon","Confidence")] ->final_tax
final_otu_d[,c(37,1:36)] ->final_otu_d

## SUBSET A SMALL DF
final_otu_d[1:nrow(final_otu_d),c(1,2,5,6)] -> small_df

## from https://www.r-bloggers.com/parallel-coordinate-plots-for-discrete-and-categorical-data-in-r-a-comparison/
# modified as needed
small_df %>% mutate(count=chicken_gut_meta_1+chicken_gut_meta_12+
                  chicken_gut_meta_13) -> small_df # count the otu per row in new df
small_df %>%arrange(desc(count)) ->small_df # rearrange the df by the count column i descending order
 small_df[1:20,] ->df1 # subset a small df of 20 rows
log(df1[,2:ncol(df1)])->df1[,2:ncol(df1)] # natural log transform the df
small_df[181:200,] ->df2
log(df2[,2:ncol(df2)])->df2[,2:ncol(df2)]

#plot the 2 dfs separately
ggparcoord(df1, columns = 2:4,groupColumn = 'OTU.ID', scale = 'globalminmax')

ggparallel(list("chicken_gut_meta_1","chicken_gut_meta_12","chicken_gut_meta_13"),
           df2, order=0)
new_df2<- as.data.frame(df2)
ggparallel(list("chicken_gut_meta_1","chicken_gut_meta_12","chicken_gut_meta_13"),
           new_df2, order=0, weight = 'count')
#######
# scale it for the bigger dataset
log(small_df[,2:ncol(small_df)])->small_df[,2:ncol(small_df)]
ggparcoord(small_df, columns = 2:4,groupColumn = 'OTU.ID', scale = 'globalminmax')+
  theme(legend.position="none") # drop the key as it is taking up the too much space

ggparallel(list("chicken_gut_meta_1","chicken_gut_meta_12","chicken_gut_meta_13"),
           small_df, order=0, weight = 'count')+theme(legend.position="none")

##
# tests done with reduced dataset
# onto the entire dataset
final_otu_d %>% mutate(count=chicken_gut_meta_1+chicken_gut_meta_10+chicken_gut_meta_11
                       +chicken_gut_meta_12+chicken_gut_meta_14+chicken_gut_meta_15+chicken_gut_meta_16
                      +chicken_gut_meta_13+chicken_gut_meta_17+chicken_gut_meta_18+chicken_gut_meta_19+
                     chicken_gut_meta_2+chicken_gut_meta_20+chicken_gut_meta_21+chicken_gut_meta_22
                     +chicken_gut_meta_23+chicken_gut_meta_24+chicken_gut_meta_25+
                       chicken_gut_meta_26+chicken_gut_meta_27+chicken_gut_meta_28+chicken_gut_meta_29+
                       chicken_gut_meta_3+chicken_gut_meta_31+chicken_gut_meta_32+chicken_gut_meta_33+
                       chicken_gut_meta_34+chicken_gut_meta_35+chicken_gut_meta_35+chicken_gut_meta_36+
                       chicken_gut_meta_4+chicken_gut_meta_5+chicken_gut_meta_6+chicken_gut_meta_7+
                       chicken_gut_meta_8+chicken_gut_meta_9) -> large_df 

large_df %>%arrange(desc(count)) ->large_df

large_df[1:20,] ->df3 
log(df3[,2:37])->df3[,2:37]

ggparcoord(df3, columns = 2:37,groupColumn = 'OTU.ID', scale = 'globalminmax')+
           theme(axis.text.x = element_text(angle = 90, hjust = 0.5))+
  theme(legend.position="none")

ggsave("network_plot.pdf")

log(large_df[,2:ncol(large_df)])->large_df[,2:ncol(large_df)]
ggparcoord(large_df, columns = 2:37,groupColumn = 'OTU.ID',scale = 'globalminmax')+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))+
  theme(legend.position="none")+ labs(x='Sample',y='Log transformed OTU counts')
ggsave("~/module2_R_biostats-master/network_plot_full.pdf")
ggsave("~/module2_R_biostats-master/network_plot_full.png")
