getwd()
setwd("~/Documents/Davies_2018/limidb_project/")
# read the microbiome data 
otu_dense <- read.table("./chick_micr_feature-table.tsv", 
                        sep="\t", numerals = "no.loss", header=TRUE)
otu_sparse <- read.table("chick_micr_feature-table_melted.tsv",
                         sep="\t", numerals = "no.loss", header=TRUE)
taxonomy <- read.table("chick_micr_taxonomy.tsv", sep="\t", 
                       numerals = "no.loss", header=TRUE)
metadata <- read.table("chick_micr_metadata.tsv", sep="\t", 
                       numerals = "no.loss", header=TRUE)
library(GGally)
library(tidyverse)
library('ggparallel')

paste0("otu",c(1:nrow(otu_dense))) ->otu_dense$sid # give a shorter otu id
otu_dense%>% select(OTU.ID,sid) -> temp_names
names(taxonomy) <- c("OTU.ID","Taxon","Confidence")
full_join(taxonomy,temp_names, by="OTU.ID") ->new_tax
full_join(otu_sparse,temp_names, by="OTU.ID") ->new_otu_sp

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
                      chicken_gut_meta_13) -> small_df 
small_df %>%arrange(desc(count)) ->small_df
 small_df[1:20,] ->df1
small_df[181:200,] ->df2

ggparcoord(df2, columns = 2:4,groupColumn = 'OTU.ID', scale = 'globalminmax')

ggparallel(list("chicken_gut_meta_1","chicken_gut_meta_12","chicken_gut_meta_13"),
           df2, order=0)
new_df2<- as.data.frame(df2)
ggparallel(list("chicken_gut_meta_1","chicken_gut_meta_12","chicken_gut_meta_13"),
           new_df2, order=0, weight = 'count')
#######
ggparcoord(small_df, columns = 2:4,groupColumn = 'OTU.ID', scale = 'globalminmax')

ggparallel(list("chicken_gut_meta_1","chicken_gut_meta_12","chicken_gut_meta_13"),
           small_df, order=0, weight = 'count')
###
library(MASS)
mycols<- colours()[as.numeric(as.factor(new_df2$OTU.ID))]
parcoord(new_df2[,c(2:4)], col = mycols)
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
subset(large_df,count<1000, select = 1:ncol(large_df)) ->df_reduced
subset(df_reduced,count>10, select = 1:ncol(df_reduced)) ->df_reduced2
df_reduced2[1:20,] ->df3
df_reduced2[181:200,] ->df4
df_reduced2[850:872,]-> df5
ggparcoord(df3, columns = 2:38,groupColumn = 'OTU.ID', scale = 'globalminmax')+
           theme(axis.text.x = element_text(angle = 90, hjust = 0.5))

ggsave("network_plot.pdf")
#ggparallel(list("chicken_gut_meta_1","chicken_gut_meta_12","chicken_gut_meta_13"),
        #   df2, order=0)

#new_df2<- as.data.frame(df2)
#ggparallel(list("chicken_gut_meta_1","chicken_gut_meta_12","chicken_gut_meta_13"),
           new_df2, order=0, weight = 'count')
#ggparallel(list(names(df3)[2:37]),
           df3, order=0, weight = 'count')
