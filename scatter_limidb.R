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

# subset and try to test with a small dataset
final_otu_sp %>% filter(abundance >0,OTU.ID %in% c('otu1','otu3','otu4','otu6','otu10'))->d

## plot it
ggplot(d, aes(x = factor(OTU.ID), y = Sample.ID, colour = factor(abundance)))+
  geom_point()

# pick only values with non zero value coloured with sharp contrast
red_sp_otu <- final_otu_sp %>% filter(abundance >0) 
ggplot(red_sp_otu, aes(x = factor(OTU.ID), y = Sample.ID, colour = factor(abundance)))+  geom_point()+theme(axis.text.x = element_text(angle = 90, hjust = 0.5))+
  labs(y='Sample',x='OTU id')+theme(legend.position = "none") #+coord_flip()
ggsave('scatter_plot.pdf')
