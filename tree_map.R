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
#install and load needed 
library(tidyverse)

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
##
# re-arrange the columns
final_otu_sp[,c("OTU.ID","Sample.ID","abundance")] ->final_otu_sp
final_tax[,c("OTU.ID","Taxon","Confidence")] ->final_tax
final_otu_d[,c(37,1:36)] ->final_otu_d
red_sp_otu <- final_otu_sp %>% filter(abundance >0)
final_otu_sp %>% filter(abundance >0,OTU.ID %in% c('otu1','otu3','otu4','otu6','otu10'))->d
# treemap
require(treemap)
# test with small dataset
treemap(d,
        index = c(names(d)[1:2]),
        vSize = names(d)[3],
        type = "index",
        palette = 'Greens',
        title = "Small dataset treemap")
# with all data
treemap(red_sp_otu,
        index = c(names(red_sp_otu)[1:2]),
        vSize = names(red_sp_otu)[3],
        type = "index",
        palette = 'Reds',
        title = "Small dataset treemap")
ggsave('treemap.pdf')

# create a new column with limits of 500
red_sp_otu$new_class<- ifelse(red_sp_otu$abundance<=500, "less than 500",
       ifelse(red_sp_otu$abundance > 500 & red_sp_otu$abundance<=1000, "500 to 1000",
              ifelse(red_sp_otu$abundance > 1000 & red_sp_otu$abundance<=2000,
                     "1000 to 2000","more than 2000")))

red_sp_otu %>% filter(OTU.ID %in% unique(red_sp_otu$OTU.ID)[1:8])->d

treemap(d,
        index = c(names(d)[1:2]),
        vSize = names(d)[3],
        vColor = names(d)[4],
        type = "categorical",
        palette = 'Set1',
        title = "Small dataset treemap")  
ggsave('small_dataset_treemap.pdf')
# with all dataset
treemap(red_sp_otu,
        index = c(names(red_sp_otu)[1:2]),
        vSize = names(red_sp_otu)[3],
        vColor = names(red_sp_otu)[4],
        type = "categorical",
        palette = 'Set1',
        title = "Limidb dataset treemap") 
ggsave("limidb_treemap.pdf")
 ######
#making it interactive
library(devtools)
install_github("d3treeR/d3treeR")
library(d3treeR)
d3t <- treemap(d,
        index = c(names(d)[1:2]),
        vSize = names(d)[3],
        vColor = names(d)[4],
        type = "categorical",
        palette = 'Set1',
        title = "Small dataset treemap")  
# make it interactive ("rootname" becomes the title of the plot)
inter<-d3tree2( d3t ,  rootname = "Small dataset treemap" )
inter_better <- d3tree( d3t ,  rootname = "Small dataset treemap" )
inter
inter_better
##
all_dat <- treemap(red_sp_otu,
        index = c(names(red_sp_otu)[1:2]),
        vSize = names(red_sp_otu)[3],
        vColor = names(red_sp_otu)[4],
        type = "categorical",
        palette = 'Set1',
        title = "Limidb dataset treemap") 
big_inter<-d3tree2( d3t ,  rootname = "Small dataset treemap" )
big_inter_better <- d3tree( d3t ,  rootname = "Small dataset treemap" )
