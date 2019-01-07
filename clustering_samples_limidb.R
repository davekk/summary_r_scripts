library("phyloseq")
# library(microbiome, quietly = TRUE)  
getwd()
setwd("~/data-shell/")
otu_dense <- read.table("./chick_micr_feature-table.tsv", sep="\t", numerals = "no.loss", header=TRUE)
otu_sparse <- read.table("chick_micr_feature-table_melted.tsv", sep="\t", numerals = "no.loss", header=TRUE)
taxonomy <- read.table("chick_micr_taxonomy.tsv", sep="\t", numerals = "no.loss", header=TRUE)
metadata <- read.table("chick_micr_metadata.tsv", sep="\t", numerals = "no.loss", header=TRUE)
# scale the dataset
rownames(otu_dense) <- otu_dense$OTU.ID # rename the rowID
otu_dense <- otu_dense[,2:ncol(otu_dense)] # drop the rowID ro ensure uniformity of data
otu_new <- scale(otu_dense) #  transform the data with default settings 
otu_dense <- t(otu_new) # transpose the df
d <- dist(otu_dense, method = "euclidean") # calculate the "euclidean" distance matrix 

# Hierarchical clustering using Ward's method
res.hc <- hclust(d, method = "ward.D2" )

# Plot the obtained dendrogram
plot(res.hc, cex = 0.6, hang = -1)
# Hierarchical clustering plot (write into png file)

plot(res.hc, hang=-1, main = paste("Hierarchical clustering", sep = ""), las = 1, xlab = "")

