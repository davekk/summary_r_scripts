read.table("~/Desktop/GIT_repos/module2_R_biostats/Datasets/fev_dataset.txt", header = TRUE) -> fevdat
str(fevdat)
summary(fevdat) # 
summary(fevdat$Ht) 
quantile(fevdat$Ht, probs = c(0.1,0.9))
quantile(fevdat$Ht, probs = seq(0.1,1, length.out = 10)) 
# 10% quantile refers to the named vector at the 10% datapoint in the sample
# 90% quantile refers to the named vector at the 90% datapoint in the sample
# Using the same dataset, draw boxplots representing the distribution of heights for all 
# the ages between 6 and 10 inclusive (one boxplot per age, all on the same plot).
boxplot(Ht ~ Age, data = subset(fevdat, Age >= 6 & Age <= 10), col=c(2:7), 
        main= "Plot of height with age", xlab= "Age", ylab="Height")
boxplot(Ht ~ Age, data = fevdat, subset = Age >= 6 & Age <= 10, col=c("3", "2", "6", "blue", "8"), 
        main= "Plot of height with age", xlab= "Age", ylab="Height")
# On the same dataset, add a column called Gender2 being the translation the column 
# Gender into character values "Boy" (when Gender contains 1) and "Girl" 
# (when Gender contains 0).
factor(fevdat$Gender) -> tempofact
levels(tempofact)
levels(tempofact)<- c("girl", "boy") # 0 becomes girl, 1 becomes boy 
levels(fevdat) <- c(levels(fevdat), "Gender2")
fevdat$V6 = tempofact
names(fevdat)
names(fevdat) <- c("Age","FEV","Ht","Gender","Smoke","Gender2")
# or
names(fevdat[6] ) <- "Gender2"
fevdat[6] = "Girl"
names(fevdat)<-c("Age","FEV","Ht","Gender","Smoke","Gender2")
fevdat[fevdat$Gender==1, "Gender2"] = "Boy"
# or define a conversion function
gender_conversion_func = function (genderCode) if (genderCode==1) "Boy" else "Girl"
# return statements are implicit
gender_conversion_func(0)
gender_conversion_func(1)
gender_conversion_func(101)
gender_conversion_func(c(1:5))
# careful with size of the object given as input
sapply(c(1:5, 0:1, 1, 0), gender_conversion_func)
# simplified apply is used to apply the same function to all elements of a vector
# now we have a one line sol for the last question
fevdat$gender3 = sapply(fevdat$Gender, gender_conversion_func) # creating a new column
# Functional flavour, ifelse()
if (4==5) print("a") else print("b")
ifelse(4==5, "a", "b") #similar to if above
ifelse(c(1,3,4,5,6,6,7,8,9,10,4,5)==5, "a", "b")
ifelse(fevdat$Gender==1, "Boy", "Girl")
# better to use ifelse compared to if
# redefine the conversion function
gender_conversion_func = function (genderCode) ifelse(genderCode==1,"Boy","Girl")
# works on vectors
# could work without being defined
ifelse(fevdat$Gender==1, "Boy", "Girl")
fevdat$gender3 = ifelse(fevdat$Gender==1, "Boy", "Girl")
# other solution using dplyr mutate
fevdat %>% mutate(gender4 = gender_conversion_func(Gender) )

#########
#  part 2
#######
library(readxl)
ssrdata =read_xlsx("ssr_data.xlsx", na = c("", " "), col_names = T)
str(ssrdata)
library(stringr)
library(dplyr)
select(ssrdata, -contains("X")) ->Ssrdata # select all columns except the columns with 'X'
ssrdata %>% select(-starts_with("X__"))
# You can see that in the original .xlsx file, several columns bear the same name 
# e.g. "P1"). They pertain to the 4 alleles of the same marker. Using the appropriate string 
# manipulation functions, rename these columns "automatically" so that e.g. the four columns 
# corresponding to the marker P1 are named "P1_Allele1", "P1_Allele2", "P1_Allele3" and 
# "P1_Allele4". And likewise for the other markers.
# str_replace(colnames(Ssrdata),"__", "_Allele" ) # replace, in theory the given matching aspect 
# in the string provided with the last string in the function
names(Ssrdata) # the col names
names(Ssrdata)<- c("ID", paste0("P", rep(c(1:6, 8:13, 15, 16), each=4), "_Allele", 1:4)) 
# append the new names to the col names
# rep(obj, times=n): n times the object 
# rep(obj, each=n): n times the obj in sequence
# Using functions from the package dplyr, transform your dataset so that we get 
# one allele observation per row. The final columns in your dataset must be: "Sample", 
# "Marker", "Allele_number" and "Allele_length".
library("tidyr")
Ssrdata -> newdat
62*14*4
Ssrdata %>% gather(Marker, Allele_length, P1_Allele1:P16_Allele4)->dirty
dirty %>% separate("Marker", c("Marker", "Allele"), "_Allele", extra="warn")->clean_ssr
names(clean_ssr)[1]<- "Sample"
length(as.data.frame(dirty)[(dirty==0)])
clean_ssr %>% mutate_all(funs(ifelse(.==0, NA, .))) ->cleaned_ssr # replace all 0 with NA
write.table(cleaned_ssr, file = "mod_ssr.csv", sep= ",",row.names = F) 
# save the tibble as a csv in the cwd
rm(clean_ssr, dirty, dirt_na,ssrdata,newdat)
# 8.The most basic measure of genetic diversity for a marker is simply the number of distinct 
# alleles observed for that marker. Calculate a table of genetic diversity per marker.
cleaned_ssr %>% group_by(Marker, Sample) %>% summarise(dist = n_distinct(Allele_length, na.rm = TRUE)) -> genetic_div
# we have to count within each marker, the number of samples with 2 or more alleles: these are the heterozygous samples for that marker. we divide by the total number of observed samples for that marker to get the observed heterozygocity.
# check do you have any zero
unique(genetic_div$dist)
subset(genetic_div, subset = dist==0)
genetic_div %>% filter(dist==0)
# the data at this stage is grouped by marker. the number of observations (rows) in each group is the number of samples for which we have a non-NA, non-0 value for the number of distinct alleles

# how to get counts per group
genetic_div %>% summarise(sum=n()) # all our groups are same size
# count of number of observation >=2 for each group
genetic_div %>% filter(dist>=2) %>% summarise(sum=n())
heterozygous = function(inputvec) {sum(inputvec >=2)}
# the function below accomodates also input vectors containing NA's
# calculated ratios: number of observations larger than 2 divided by the number of (non-NA) observations
heterozygous_ratio = function(inputvec) {sum(inputvec >=2, na.rm = TRUE)/sum(!is.na(inputvec))}
# observed heterozygocity per marker.
genetic_div %>% summarise(num_hetero = heterozygous_ratio(dist))
# At this stage, the genetic_div contained gruoping by marker 
genetic_div %>% group_by(Sample) %>% summarise(counts=n()) %>% '[['('counts') %>% unique()
# the functional version "[[..]]" that extracts an element from a list. (remember: a dataframe is a list of column vectors)
genetic_div%>% group_by(Sample) %>% summarise(obs_hetero = heterozygous_ratio(dist))
# this is the table of observed heterozygocity value
# Q10 . a new measure of heterozygosity per sample and marker:
# 1 allele seen =>0/3
# 2 allele seen =>1/3
# 3 allele seen =>2/3
# 4 allele seen =>3/3
num_allele_to_het = function(n) {ifelse(n==1, 0, ifelse(n==2, 1/3, ifelse(n==3,2/3,1))) } # works but ugly
num_allele_to_het = function(n) {(n-1)/3} # simple function
num_allele_to_het = function(n) {c(0,1/3,2/3,1)[n]} # simple function
num_allele_to_het(c(1,2,5,4,3,2,1,0,1,4)) # testing the function works
# similar function using match() function
genetic_div %>% transmute(Sample = Sample,fancy_het_measure = num_allele_to_het(dist)) %>% group_by(Marker) %>% summarise(fancy_het_measure = mean(fancy_het_measure))
# table per marker
genetic_div %>% mutate(fancy_het_measure = num_allele_to_het(dist)) %>% group_by(Sample) %>% summarise(fancy_het_measure = mean(fancy_het_measure))
# table per sample
14*62
# calculate allelle frequency out of observed counts of the  alleles across 62 samples
# (1) when a allelle was seen in a given sample it gives a count of 4 observations of tha allelles
# (2) when a 2 allelles are seen it gives a count for each of these two allelle
# (3) when 3 allelles are seen in agiven sample, it gives a count of 4/3 of these allelles 
# (4) when 4 allelles are seen in a given sample, it gives a coun of 1 of each of these allelle 
# Then for eah marker you calculate the frequencies of each allelle using the counts above


# q 1
chickengtf <- read.table("/Users/kaimenyi/Documents/module2_R_biostats-master/Datasets/ref_Gallus_gallus-5.0_top_level_chr1.gtf", sep = "\t")
gtf <- read.table("/Users/kaimenyi/Documents/module2_R_biostats-master/Datasets/ref_Gallus_gallus-5.0_top_level_chr1.gtf", skip= 3,sep = "\t", header = F, stringsAsFactors = F)
names(chickengtf) <-c("Seqname","Source","Feature", "Start", "End", "Score", "Strand", "Frame", "Atrribute")
names(gtf) <-c("Seqname","Source","Feature", "Start", "End", "Score", "Strand", "Frame", "Atrribute")
# q 2
# Group the gtf file by the gene id (GeneID). `You will have to extract the GeneID from the atribute
# column using stringr functions`.
str_extract(gtf$Atrribute, "GeneID:[:digit:]+")
str_replace(gtf$Atrribute[2], ".*GeneID:([:digit:]+).*", "\\1")
gtf$GeneID <- str_replace(gtf$Atrribute, ".*GeneID:([:digit:]+).*", "\\1")
gtf$GeneID <- ifelse(gtf$Feature=="region", "chromosome1", str_replace(gtf$Atrribute, ".*GeneID:([:digit:]+).*", "\\1"))
# q3 
unique(chickengtf$Feature)
sum(data =  chickengtf$Feature == "gene")
gtf%>% group_by(GeneID) -> gtf
table(gtf$Feature)
#q4
sum(data =  chickengtf$Feature == "ncRNA")
#q5
sum(data =  chickengtf$Feature == "gene" & chickengtf$Strand == "+")
sum(data =  chickengtf$Feature == "gene" & chickengtf$Strand == "-")
nrow(filter(gtf, Feature == "gene" & Strand == "+"))
# q6 Find the longest and shortest genes on chromosome 1 of chicken
gtf$length <- gtf$End - gtf$Start
head(sort(gtf$length, decreasing = T))
max(gtf$length[gtf$Feature=="gene"])
subset(gtf, length =="996100")
gtf[gtf$length=="996100"]
 # for the longest
gtf[gtf$length == max(gtf$length[gtf$Feature=="gene"] ) & gtf$Feature =="gene", "GeneID"] # sol 1
subset(gtf, length =="996100" & Feature =="gene")[["GeneID"]] # sol 2
subset(gtf, length ==max(gtf$length[gtf$Feature=="gene"]) & Feature =="gene")$GeneID # sol 3 
 # for the shortest
gtf[gtf$length == min(gtf$length[gtf$Feature=="gene"] ) & gtf$Feature =="gene", "GeneID"] # sol 1
subset(gtf, length =="53" & Feature =="gene")[["GeneID"]] # sol 2
subset(gtf, length ==min(gtf$length[gtf$Feature=="gene"]) & Feature =="gene")$GeneID 
 # q7 What is the average length of all genes on this chromosome
mean(subset(gtf, length  & Feature =="gene")$length) # 1
mean(gtf[gtf$length & gtf$Feature =="gene", "length"]) # 2
 # q8 What is the number of exons per gene
gtf %>% filter(Feature == "exon") %>% summarise(count_exon=n()) -> sum_exon_count
merge(x=gtf, y=sum_exon_count, by=intersect(names(GeneID))) #to be fixed
 # q9 Find the average length of exons per gene
gtf %>% filter(Feature == "exon") %>% summarise(mean_exon=mean(length)) ->mean_exon_len
 # q10 Using the plot function in R, plot the gene density distribution of genes on chicken chromosome 1
hist(subset(gtf, subset = Feature =="gene")$Start, breaks = 197, xlab = "Start of Gene", border = "3",  main = "Histogram of freq of gene start position")
max_pos <- max(gtf$Start[gtf$Feature == "gene"])
seq(0,round(max_pos, -8),1000000) ->gtf_breaks
hist(subset(gtf, subset = Feature =="gene")$Start, breaks = gtf_breaks, xlab = "Start of Gene", border = "3", ylim = c(0,50), main = "Histogram of freq of gene start position")
#####
 # b
library("readxl")
setwd("~/module2_R_biostats-master/Datasets/")
read_xlsx("novel_gbs_snp_chicken.xlsx", col_names = T ) -> novel_chicksnp

novel_chicksnp %>% separate(Uploaded_variation, into = c("Chr", "Position", "SNP"), remove = F, sep = "_")-> novel_chicksnp
novel_chicksnp$Position <-as.numeric(novel_chicksnp$Position)
transitions <- c("G/A", "A/G","C/T", "T/C")
ifelse(novel_chicksnp$SNP %in% transitions, "Transition", "Transversion") -> novel_chicksnp$SNP_ty
sum(novel_chicksnp$SNP_ty=="Transition")/sum(novel_chicksnp$SNP_ty=="Transversion")

# q4 The consequence of each snp i.e genic (within genes) or non-genic (outside gene).
novel_chicksnp$Exactpos = str_replace(novel_chicksnp$Location, "1:([:digit:]+)-([:digit:]+)", "\\2")
outer(as.numeric(gtf$Start[gtf$Feature=="gene"]), as.numeric(novel_chicksnp$Exactpos), "<=") ->lesstab
outer(as.numeric(gtf$End[gtf$Feature=="gene"]), as.numeric(novel_chicksnp$Exactpos), ">=") ->bitab
colnames(lesstab) <- novel_chicksnp$Exactpos
colnames(bitab) <- novel_chicksnp$Exactpos
rownames(lesstab) <- gtf$Start[gtf$Feature=="gene"]
rownames(bitab) <- gtf$End[gtf$Feature=="gene"]
colSums(lesstab == TRUE & bitab == TRUE) -> tab_we_want
novel_chicksnp$isGene <- tab_we_want
novel_chicksnp$isGene <- ifelse(novel_chicksnp$isGene == 0, "non-gene", "gene")
##
gene_start <- gtf$Start[gtf$Feature=="gene"]
gene_end <- gtf$End[gtf$Feature=="gene"]
consequence <- function(z, x=gene_start, y=gene_end) any (z>=x & z<=y)
novel_chicksnp$conseq <- ifelse(sapply(novel_chicksnp$Position, FUN = consequence), "Genic", "Non-genic")
novel_chicksnp <- novel_chicksnp[order(novel_chicksnp$Position),]

