getwd()
setwd("C:/Users/kaimenyi/Documents/module2_R_biostats-master/module_4/Hisat_28_june/")
library(devtools)
library(ballgown)
library(RSkittleBrewer)
library(genefilter)
library(dplyr)
library(ggplot2)
library(cowplot)

# read csv file
pheno_data = read.csv("pheno.csv")
dim(pheno_data)
print(pheno_data)
# Read in the expression data that was calculated by StringTie.
bg = ballgown(dataDir = "Ballgown_str_full_genome/", samplePattern = "1_", pData=pheno_data)
# filter to remove low-abundance genes with zero counts
bg_filt = subset(bg,"rowVars(texpr(bg)) >0",genomesubset=TRUE)
# view how many transcripts are in the Ballgown objects
bg
bg_filt
# make a table containing the information from the filtered dataset
bg_filt_table = texpr(bg_filt, 'all')
# basic summary of file
summary(bg_filt_table)
dim(bg_filt_table)
head(bg_filt_table)
# identify transcripts that show statistically significant differences between resistance and susceptible
results_transcripts <- stattest(bg_filt, feature="transcript", covariate="Susceptibility", getFC=T, meas = "FPKM")
# identify genes that show statistically significant differences between groups
results_genes <- stattest(bg_filt, feature="gene", covariate="Susceptibility", getFC=T, meas = "FPKM")
# add in the gene names and ids to results_transcripts
results_transcripts = data.frame(geneNames=ballgown::geneNames(bg_filt),
                                 geneIDs=ballgown::geneIDs(bg_filt), results_transcripts)
# results_transcripts <- arrange(results_transcripts, pval)
results_transcripts <- arrange(results_transcripts, pval)
results_genes <- arrange(results_genes, pval)
# count the number of transcripts with a p-value <= 0.05
table(results_transcripts$pval <= 0.05)
table(results_genes$pval <=0.05)
# Write the results to a .csv file
write.csv(results_transcripts, "spike_root_ballgown_results_trans.csv", row.names=F, quote=F)
write.csv(results_genes, "spike_root_ballgown_results_genes.csv", row.names=F, quote=F)
# Identify transcripts and genes with a p-value <=0.05
significant_results_transcripts <- subset(results_transcripts,results_transcripts$pval<=0.05)
significant_results_genes <- subset(results_genes,results_genes$pval<=0.05)
# How many 'low abundance' genes with variance in expression less than 1 FPKM, were removed?
4063-3463
# How many transcripts were identified as showing statistically significant differences between spike and root?
3463
# And how many of these transcripts had a p-value <=0.05?
224
# How many genes were identified as showing statistically significant differences between spike and root?
2474
# And how many of these genes had a p-value <=0.05?
236
# Ballgown make the results easier to view and compare expression data. First, we specify the colour palette
tropical=c('darkorange', 'dodgerblue','hotpink', 'limegreen', 'yellow')
palette(tropical)
#plot the distribution of gene abundances (measured as FPKM values) across samples, colored by tissue type
# we need to extract the expression measurements from the ballgown object bg_filt
fpkm = texpr(bg_filt, meas="FPKM")
# transform the FPKM data using a log2 transformation that adds one to all FPKM values because log2(0) is undefined
fpkm = log2(fpkm+1)
# create the plot:
boxplot(fpkm, col=as.numeric(pheno_data$tissue), las=2,ylab='log2(FPKM+1)')
# make plots of individual transcripts across samples eg a plot for the 1948th transcript in the dataset
ballgown::transcriptNames(bg_filt)[1948]
plot(fpkm[1948,] ~ pheno_data$tissue, border=c(1,2), main=ballgown::transcriptNames(bg)[1948], 
     pch=19, xlab="tissue", ylab='log2(FPKM+1)')
points(fpkm[1948,] ~ jitter(as.numeric(pheno_data$tissue)), col=as.numeric(pheno_data$tissue))
# plot the average expression levels for all transcripts of a gene within different groups using the plotMeans by specifying which gene to plot
plotMeans('mrg.1188', bg_filt, groupvar='tissue', legend=T)
# create an MA plot, with DE expressed genes (p<=0.05) highlighted in red
results_transcripts$mean <- rowMeans(texpr(bg_filt))
ggplot(results_transcripts, aes(log2(mean), log2(fc), colour = pval<=0.05)) +
  scale_colour_manual(values=c("#999999", "#FF0000")) + geom_point() +
  geom_hline(yintercept=0)

### sleuth workthrough
# specify where the kallisto results are stored
sample_id <- dir(file.path(".", "Kallisto/"))
# Check you have entered the correct file path 
sample_id
# list of paths to the kallisto results indexed by the sample IDs 
kal_dirs <- file.path(".", "Kallisto", sample_id)
kal_dirs
# read tsv
s2c <- read.table(file.path(".", "experiment_data.tsv"), header=TRUE, stringsAsFactors = FALSE)
s2c <- dplyr::mutate(s2c, path = kal_dirs)
# the "sleuth object (so)" is constructed
so <- sleuth_prep(s2c, ~condition)
# normalizing est_counts
# 2290 targets passed the filter
# normalizing tpm
# merging in metadata
# summarizing bootstraps

# the models are fitted
so <-sleuth_fit(so)
# fitting measurement error models
# shrinkage estimation
# 1 NA values were found during variance shrinkage estimation due to mean observation values outside of the range used for the LOESS fit.
# The LOESS fit will be repeated using exact computation of the fitted surface to extrapolate the missing values.
# These are the target ids with NA values: TraesCS6A01G192100.1
# computing variance of betas

so <-sleuth_fit(so, ~1, 'reduced')
# fitting measurement error models
# shrinkage estimation
# 1 NA values were found during variance shrinkage estimation due to mean observation values outside of the range used for the LOESS fit.
# The LOESS fit will be repeated using exact computation of the fitted surface to extrapolate the missing values.
# These are the target ids with NA values: TraesCS6A01G192100.1
# computing variance of betas

so <-sleuth_lrt(so, 'reduced', 'full')
# collate the results onto a table
results_table <- sleuth_results(so, 'reduced:full', test_type = 'lrt')
# create a table of significantly differentially expressed transcripts
results_table_significant <- dplyr::filter(results_table, pval <= 0.05)
# How many transcripts are significantly differentially expressed?
# visualize the results
sleuth_live(so)
# to visualise an MA plot, run the 'Wald test'
so <- sleuth_wt(so, 'conditionspike')

sleuth_live(so)
