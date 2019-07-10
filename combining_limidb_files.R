getwd()
setwd('./Limidb/limidb_data/')
require('readr');require('magrittr');require('tidyr');require('dplyr');require('data.table') # load necessary libraries

# read all metadata files
files <- list.files(path = "./", pattern = "*metadata.tsv", full.names = T)
metadata_tbl <- sapply(files, read.delim, simplify=FALSE) %>% 
  bind_rows(.id = "id")

# read taxonomy files
files2 <- list.files(path = "./", pattern = "*taxonomy.tsv", full.names = T)
taxonomy_tbl <- sapply(files2, read.delim, simplify=FALSE) %>% 
  bind_rows(.id = "id")

# read otu count files
files3 <- list.files(path = "./melted/", pattern = "*melted.tsv", full.names = T)
feature_table_tbl <- sapply(files3, read.delim, simplify=FALSE) 
class(feature_table_tbl) # check type of object; should ideally be tibble or dataframe
df<- rbindlist(feature_table_tbl) # convert the list to dataframe
df[1:13,1:3] # see the first few rows
new_feature <- dcast(df,OTU.ID ~ Sample.ID ) # unmelt it

# shortening the id
paste0("otu",c(1:nrow(new_feature))) ->new_feature$sid # give a shorter otu id
new_feature %>% select(OTU.ID,sid) -> temp_names # get the id and temp id
names(taxonomy_tbl)[2] <- "OTU.ID" # rename the colunms in taxonomy
full_join(taxonomy_tbl,temp_names, by="OTU.ID") ->new_tax # assign new names to taxonomy tibble

# drop the looong otu id
select(new_tax, -'OTU.ID') ->taxonomy_tbl_final
select(new_feature, -'OTU.ID') ->feature_table_tbl_final

# rename the sid, which is the last column in the dataframe 
names(taxonomy_tbl_final)[length(taxonomy_tbl_final)] <- 'OTU.ID'
names(feature_table_tbl_final)[length(feature_table_tbl_final)] <- 'OTU.ID'

# remove extra file names
metadata_tbl$id <-substring(metadata_tbl$id, 3,nchar(metadata_tbl$id)-4)
taxonomy_tbl_final$id <-substring(taxonomy_tbl_final$id, 3,nchar(taxonomy_tbl_final$id)-4)
#write out the files
write.table(taxonomy_tbl_final,file = "../combined all/full_taxonomy_all.tsv",sep = "\t", quote = F, row.names = F, dec=".", eol= "\n")
write.table(feature_table_tbl_final,file = "../combined all/full_feature_table_all.tsv",sep = "\t", quote = F, row.names = F, dec=".", eol= "\n")
write.table(metadata_tbl,file = "../combined all/full_metadata_all.tsv",sep = "\t", quote = F, row.names = F, dec=".", eol= "\n")

## clear r environment
rm(list=ls())
