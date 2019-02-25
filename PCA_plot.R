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
library(devtools)
install_github("vqv/ggbiplot")
install.packages('caret', dependencies=TRUE)
install.packages('e1071', dependencies=TRUE)
require(tidyverse); require(ggbiplot); 

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

big_df <-final_otu_d
log(big_df[,2:ncol(big_df)])->big_df[,2:ncol(big_df)]

# big_df[mapply(is.infinite, big_df)] <- 0 # this is a bias introduced to the data
##
otu.pca <- prcomp(final_otu_d[,2:ncol(final_otu_d)], center = TRUE,scale. = TRUE)
#otu_log.pca <- prcomp(big_df[,2:ncol(big_df)], center = TRUE,scale. = TRUE)
autoplot(prcomp(final_otu_d[,2:ncol(final_otu_d)]), scale=0)
#view the summary
summary(otu.pca)
## see the pca with most variance
pdf('pca_example.pdf')
plot(otu.pca, type='l')

# plot the pca
ggbiplot::ggbiplot(otu.pca)+ scale_color_discrete(name = '')+ theme(legend.direction = 'horizontal',legend.position = 'top')
biplot(otu.pca, scale = 0)
##
#install.packages("FactoMineR")
library("FactoMineR")

new_pca <- PCA(final_otu_d[,2:ncol(final_otu_d)])# does a nice pca 

dev.off()
