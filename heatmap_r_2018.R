source("https://bioconductor.org/biocLite.R")
biocLite("RColorBrewer")
biocLite("NMF")
library(gplots)
library(RColorBrewer)
#1) plot a heatmap for the bottom 100 expressed genes in euploid shoots on chr1A
# is the pattern similar to for the top 100 expressed genes?
dat<-read.table("~/module2_R_biostats-master/Datasets/heatmap_data.txt",sep="\t",header=T)  
dat[1:5,1:5]
rownames(dat) <- dat[,1]
dat <- dat[,-1]
head(rownames(dat))
dim(dat)
dat <- dat[rowSums(dat) != 0,]
dim(dat)
colnames(dat) 
# selecting euploid shoots
euploid_shoots <- dat[,c(1,3:6)]
head(euploid_shoots)

# plot gene expression for genes on chromosome 1 in group1A + euploid
# select only genes on chrom 1A and in nullichrom1A_shoots
chrom1A_genes_eupl <- as.matrix(euploid_shoots[(grep("Traes_1A[L|S]_*",row.names(euploid_shoots))),])
head(chrom1A_genes_eupl)
tail(chrom1A_genes_eupl)
# draw boxplot of these allgenes_exprWT_chrom1A
jpeg("all_genes_exprWT_chrom1A_boxplot.jpg", height=300, width=300)
par(mar=c(10,6,4,3)+0.1,mgp=c(4,1,0))
boxplot.matrix(chrom1A_genes_eupl, las=2, ylim=c(0,10), ylab ="Expression TPM", main ="All expressed chrom1A genes in shoots")
dev.off()
# choose only the genes which are the top 100 most expr in WT
bottom100exprWT_chrom1A <- tail(chrom1A_genes_eupl[order(chrom1A_genes_eupl[,1],decreasing=TRUE),],100)

head(bottom100exprWT_chrom1A)
dim(bottom100exprWT_chrom1A)

jpeg(file="100_bottom_genes_exprWT_chrom1A_heatmap.jpg", height=1000, width=1000)
par(mar=c(20,11,4,3)+0.1,mgp=c(6,1,0))

heatmap.2(bottom100exprWT_chrom1A, 
          col=rev(heat.colors(75)), 
          Rowv=FALSE, # turn off clustering rows
          Colv=FALSE, # turn off clustering columns
          dendrogram= "none", # turn off dendrogram
          key=TRUE, # add key
          keysize=0.5, # make key smaller
          trace="none",  # turn off blue line showing expression level
          scale="row", # scales data for each row to be equal to the same sum
          margins = c(15,10), #set margin
          main= "Bottom 100 expressed chrom1A genes in shoots", # add title
          density.info="none") # get rid of blue line on legend
dev.off()

#2) plot a heatmap for both roots and shoots for chr1A, 
# use the top 100 expressed genes in euploid shoots
# add a dendrogram for the columns. You will need to make the columns re-order using colv
# which samples are more related?
colnames(dat) 
# selecting euploid shoots
euploid_shoots_root_1A <- dat[,c(1:4,21,22)]
head(euploid_shoots_root_1A)

# plot gene expression for genes on chromosome 1 in group1A + euploid
# select only genes on chrom 1A and in nullichrom1A_shoots
chrom1A_shoot_root_genes_eupl <- as.matrix(euploid_shoots_root_1A[(grep("Traes_1A[L|S]_*",row.names(euploid_shoots_root_1A))),])
head(chrom1A_shoot_root_genes_eupl)
tail(chrom1A_shoot_root_genes_eupl)
##
jpeg("all_genes_exprWT_chrom1A_shoot_root_boxplot.jpg", height=300, width=300)
par(mar=c(10,6,4,3)+0.1,mgp=c(4,1,0))
boxplot.matrix(chrom1A_shoot_root_genes_eupl, las=2, ylim=c(0,10), ylab ="Expression TPM", main ="All expressed chrom1A genes in shoots and roots")
dev.off()
# choose only the genes which are the top 100 most expr in WT
top100exprWT_chrom1A_shoot_root <- head(chrom1A_shoot_root_genes_eupl[order(chrom1A_shoot_root_genes_eupl[,1],decreasing=TRUE),],100)

head(top100exprWT_chrom1A_shoot_root)
dim(top100exprWT_chrom1A_shoot_root)

jpeg(file="100_bottom_genes_exprWT_chrom1A_heatmap.jpg", height=1000, width=1000)
par(mar=c(20,11,4,3)+0.1,mgp=c(6,1,0))

heatmap.2(top100exprWT_chrom1A_shoot_root, 
          col=rev(heat.colors(75)), 
          Rowv=FALSE, # turn off clustering rows
          Colv=FALSE, # turn off clustering columns
          dendrogram= "none", # turn off dendrogram
          key=TRUE, # add key
          keysize=0.5, # make key smaller
          trace="none",  # turn off blue line showing expression level
          scale="row", # scales data for each row to be equal to the same sum
          margins = c(15,10), #set margin
          main= "Bottom 100 expressed chrom1A genes in shoots", # add title
          density.info="none") # get rid of blue line on legend
dev.off()

#3) CHALLENGE plot the heatmap from #2) using the "aheatmap" function in NMF
# can you add on an annotation column showing which samples are roots, and which are shoots?
# HINT: you will need to make an annotation dataframe and pass it to annCol
# http://nmf.r-forge.r-project.org/vignettes/heatmaps.pdf part 1.4 shows an example

