source("https://bioconductor.org/biocLite.R")
biocLite()
# if any package can't be installed, close Rstudio and run it as administrator
# also consider installing failed dependecny independently, before retriying instalation of main package
biocLite("GO.db")
biocLite("Hmisc") # dependancy for DESeq2
biocLite("DESeq2")
biocLite("foreach")
library(GO.db)
getwd()
setwd("~/module2_R_biostats-master/data_deseq2-excercise/")
allfiles= dir(pattern="v1.tophat.dedup_sorted.bam.count")
# load all the count file
for(filename in allfiles) {
  tmpfilename = strsplit(x = filename, 
                         split=".",
                         fixed = T)
  objname = strsplit(tmpfilename[[1]][1], 
                     split="-", 
                     fixed=T)
  newobjname=paste(objname[[1]], collapse="_")
  assign(newobjname, 
         read.table(filename, row.names = 1)
  )
}
# combine all the variables into one dataframe.
allcounts = cbind(k12_1,k12_2,k12_3,k48_1,k48_2,k48_3,
                  kx0_1,kx0_2,kx0_3,kx12_1,kx12_2,kx12_3,
                  kx48_1,kx48_2,kx48_3,
                  m12_1,m12_2,m12_3,m48_1,m48_2,m48_3,
                  mx0_1,mx0_2,mx0_3,mx12_1,mx12_2,mx12_3,
                  mx48_1,mx48_2,mx48_3)
# assign the column names
colnames(allcounts)
colnames(allcounts)=c("k12_1","k12_2","k12_3","k48_1","k48_2","k48_3",
                      "kx0_1","kx0_2","kx0_3","kx12_1","kx12_2","kx12_3",
                      "kx48_1","kx48_2","kx48_3",
                      "m12_1","m12_2","m12_3","m48_1","m48_2","m48_3",
                      "mx0_1","mx0_2","mx0_3","mx12_1","mx12_2","mx12_3",
                      "mx48_1","mx48_2","mx48_3")
# remove all lines that start with "__"
allcounts_stats = allcounts[-grep("__", row.names(allcounts)),]
allcounts = allcounts[grep("__", row.names(allcounts),invert=TRUE),]
# Perform some quality control on the readcounts, this is optional as DESeq also has some filtering criteria
expgroup = c("k12","k12","k12","k48","k48","k48",
             "kx0","kx0","kx0","kx12","kx12","kx12",
             "kx48","kx48","kx48",
             "m12","m12","m12","m48","m48","m48",
             "mx0","mx0","mx0","mx12","mx12","mx12",
             "mx48","mx48","mx48")


medianCountByGroup = t(apply(allcounts, 1, tapply,
                             expgroup, median))

maxMedian=apply(medianCountByGroup, 1, max)
# remove all genes where the median of the max samples is <10
allcounts[maxMedian>=10,]->allcounts_filtered
# Plot the histogram before and after filtering
pdf("SampleMedianByGroup.pdf")
par(mfrow=c(2,1))
hist(log2(maxMedian), xlim=c(0,20), col = "gray")
hist(log2(maxMedian[maxMedian>=10]), xlim=c(0,20),
     col="gray")
dev.off()
###
# Define the experimental design of the study described
gen_group = factor(c("k","k","k","k","k","k",
                     "k","k","k","k","k","k",
                     "k","k","k",
                     "m","m","m","m","m","m",
                     "m","m","m",
                     "m","m","m","m","m","m"))

time_group = c("12","12","12","48","48","48",
               "0","0","0","12","12","12",
               "48","48","48",
               "12","12","12","48","48","48",
               "0","0","0","12","12","12",
               "48","48","48")

treatment_group = c("ctl","ctl","ctl","ctl","ctl","ctl",
                    "ctl","ctl","ctl","trt","trt","trt",
                    "trt","trt","trt",
                    "ctl","ctl","ctl","ctl","ctl","ctl",
                    "ctl","ctl","ctl","trt","trt","trt",
                    "trt","trt","trt")
expdesign = data.frame(genotype=gen_group,
                       time=time_group,
                       treatment=treatment_group)

rownames(expdesign)=colnames(allcounts_filtered)

install.packages("stringi",type="win.binary")
library(stringi)
library(DESeq2)
# The model definition is important Below we anlayse time 12 for both genotypes
time12counts = allcounts_filtered[,time_group=="12"]
time12expdesign = expdesign[time_group=="12",]

time12ddsMat <- DESeqDataSetFromMatrix(countData = time12counts,
                                       colData = time12expdesign,
                                       design = ~ genotype + treatment + genotype:treatment)

time12ddsMat <- DESeq(time12ddsMat)

res12Wald<-results(time12ddsMat)

plotMA(time12ddsMat,ylim=c(-2,2),main="DESeq2")

resultsNames(time12ddsMat)
# To look up a specific gene
res12Wald["GSMUA_Achr7G18930_001",]
# Look at the Top 20 significant genes
time12_top20_gentrt<-head(res12Wald[order(res12Wald$padj),], 20)
# ##
# This is the get annotation function
library(GO.db)
getannotation<-function(annofile, genelist) {
  anno<-read.table(annofile, 
                   header=FALSE, 
                   sep="\t", 
                   quote="",
                   comment.char="")
  
  justAnno<-anno[as.character(anno[,2]) %in% row.names(genelist), c(2,10:16)]
  gene_name=character()
  go_annot=character()
  arth_gene=character()
  arth_alias=character()
  arth_description=character()
  rice_gene=character()
  rice_alias=character()
  rice_description=character()
  
  for (i in 1:nrow(justAnno)) {
    gene_name[i]=as.character(justAnno[i,1])
    go=unlist(strsplit(as.character(justAnno[i,2]),","))
    
    go_annot[i] = paste(names(Term(go)),Term(go), collapse=" ; ")
    
    arth_gene[i]=as.character(justAnno[,3])[i]
    arth_alias[i]=as.character(justAnno[,4])[i]
    arth_description[i]=as.character(justAnno[,5])[i]
    rice_gene[i]=as.character(justAnno[,6])[i]
    rice_alias[i]=as.character(justAnno[,7])[i]
    rice_description[i]=as.character(justAnno[,8])[i]
  }
   return(data.frame(gene_name,
                    log2fc=genelist$log2FoldChange,
                    padj=genelist$padj,
                    go_annot,
                    arth_gene,
                    arth_alias,arth_description,
                    rice_gene,rice_alias,
                    rice_description))
}
# This step utilises the file downloaded from Phytozome and annotates it
time12_top20_gentrt_annotation<-getannotation("./Macuminata_304_v1.annotation_info.txt", time12_top20_gentrt)
write.csv(time12_top20_gentrt_annotation,file="./time12_all_top20_gentrt_annotation.csv")
# Here's a function to filter DESeq2 results based on a adjusted p-value of 0.05 and a 2 fold change
filterDeseq2<-function(res) {
  
  filtpadj = (!is.na(res[,"padj"]) & res[,"padj"] < 0.05)
  filtfoldchange = (res[,"log2FoldChange"] <= -1 | res[,"log2FoldChange"] >= 1)
  filtlogic = filtpadj & filtfoldchange
  newres = res[filtlogic,]
  return(newres)
}
res12Wald_filtered<-filterDeseq2(res12Wald)
res12Wald_filtered_annotation<-getannotation("./Macuminata_304_v1.annotation_info.txt", res12Wald_filtered)
write.csv(res12Wald_filtered_annotation,file="./time12_final_filtered_gentrt_annotation.csv")
##
library(tidyr)
library(dplyr)
# filters out those without Arabidopsis gene for Mapman analysis
res12Wald_filtered_annotation_ath<-res12Wald_filtered_annotation[!res12Wald_filtered_annotation$arth_gene=="",]
res12Wald_filtered_annotation_ath$arth_gene<-gsub("\\.*","",res12Wald_filtered_annotation_ath$arth_gene)
res12Wald_filtered_annotation_ath_final<-res12Wald_filtered_annotation_ath %>% group_by(res12Wald_filtered_annotation_ath$arth_gene) %>% mutate(Sum = cumsum(log2fc))
write.table(summarise(res12Wald_filtered_annotation_ath_final,log2fc=mean(log2fc),count=n()),"./res12Wald_filtered_annotation_ath_final.txt", sep = "\t", row.names = F)

##
# for time 0
# The model definition is important Below we analyse time 0 for both genotypes
time0counts = allcounts_filtered[,time_group=="0"]
time0expdesign = expdesign[time_group=="0",]

time0ddsMat <- DESeqDataSetFromMatrix(countData = time0counts,
                                      colData = time0expdesign,
                                      design = ~ genotype)

time0ddsMat <- DESeq(time0ddsMat)

res0Wald<-results(time0ddsMat)

plotMA(time0ddsMat,ylim=c(-2,2),main="DESeq2")
resultsNames(time0ddsMat)
# To look up a specific gene
res0Wald["GSMUA_Achr7G18930_001",]
# Look at the Top 20 significant genes
time0_top20_gentrt<-head(res0Wald[order(res0Wald$padj),], 20)

# This step utilises the file downloaded from Phytozome and annotates it
time0_top20_gentrt_annotation<-getannotation("./Macuminata_304_v1.annotation_info.txt", time0_top20_gentrt)
write.csv(time0_top20_gentrt_annotation,file="./time0_all_top20_gentrt_annotation.csv")

res0Wald_filtered<-filterDeseq2(res0Wald)
res0Wald_filtered_annotation<-getannotation("./Macuminata_304_v1.annotation_info.txt", res0Wald_filtered)
write.csv(res0Wald_filtered_annotation,file="./time0_final_filtered_gentrt_annotation.csv")

# filters out those without Arabidopsis gene for Mapman analysis
res0Wald_filtered_annotation_ath<-res0Wald_filtered_annotation[!res0Wald_filtered_annotation$arth_gene=="",]
res0Wald_filtered_annotation_ath$arth_gene<-gsub("\\.*","",res0Wald_filtered_annotation_ath$arth_gene)
res0Wald_filtered_annotation_ath_final<-res0Wald_filtered_annotation_ath %>% group_by(res0Wald_filtered_annotation_ath$arth_gene) %>% mutate(Sum = cumsum(log2fc))
write.table(summarise(res0Wald_filtered_annotation_ath_final,log2fc=mean(log2fc),count=n()),"./res0Wald_filtered_annotation_ath_final.txt", sep = "\t", row.names = F)
# The model definition is important Below we analyse time 48 for both genotypes
time48counts = allcounts_filtered[,time_group=="48"]
time48expdesign = expdesign[time_group=="48",]

time48ddsMat <- DESeqDataSetFromMatrix(countData = time48counts,
                                       colData = time48expdesign,
                                       design = ~ genotype + treatment + genotype:treatment)

time48ddsMat <- DESeq(time48ddsMat)

res48Wald<-results(time48ddsMat)

plotMA(time48ddsMat,ylim=c(-2,2),main="DESeq2")
resultsNames(time48ddsMat)

# Look at the Top 20 significant genes
time48_top20_gentrt<-head(res48Wald[order(res48Wald$padj),], 20)

# This step utilises the file downloaded from Phytozome and annotates it
time48_top20_gentrt_annotation<-getannotation("./Macuminata_304_v1.annotation_info.txt", time48_top20_gentrt)
write.csv(time48_top20_gentrt_annotation,file="./time48_all_top20_gentrt_annotation.csv")

res48Wald_filtered<-filterDeseq2(res48Wald)
res48Wald_filtered_annotation<-getannotation("./Macuminata_304_v1.annotation_info.txt", res48Wald_filtered)
write.csv(res48Wald_filtered_annotation,file="./time48_final_filtered_gentrt_annotation.csv")
# filters out those without Arabidopsis gene for Mapman analysis
res48Wald_filtered_annotation_ath<-res48Wald_filtered_annotation[!res48Wald_filtered_annotation$arth_gene=="",]
res48Wald_filtered_annotation_ath$arth_gene<-gsub("\\.*","",res48Wald_filtered_annotation_ath$arth_gene)
res48Wald_filtered_annotation_ath_final<-res48Wald_filtered_annotation_ath %>% group_by(res48Wald_filtered_annotation_ath$arth_gene) %>% mutate(Sum = cumsum(log2fc))
write.table(summarise(res48Wald_filtered_annotation_ath_final,log2fc=mean(log2fc),count=n()),"./res48Wald_filtered_annotation_ath_final.txt", sep = "\t", row.names = F)

              
