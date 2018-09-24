library(polysat)
library(tidyverse)
library(ape) # for the NJ tree

setwd("/home/jbde/Trainings/Phylogeny_BixCoP_2018/Practicals/SSR_Data/")
# reding the input csv file

rawdat = read.table(file="sheep_SSR_data.csv", sep=";", skip = 2, header = T, stringsAsFactors = F)

# sanity checks on input data
str(rawdat)
names(rawdat)[1:2] <- c("Pop", "Sample")
dim(rawdat) # 22 markers, 118 samples in 4 populations
unique(rawdat$Pop)
length(unique(rawdat$Sample)) == nrow(rawdat) # should be TRUE

# check for missing data (0 or NA, or other unexpected symbols)
c(0,"-")
typeof(rawdat[[4]])
sapply(rawdat[-c(1,2)], typeof) -> m
names(m) <- NULL
sum(m != "integer") # should be 0
# so we have no unexpected characters, OK
sum(is.na(rawdat)) # zero NA values
sum(rawdat == 0) # 10 "0" values, that we want to replace with NA's
replace_zeros_with_NAs = function(vec) { vec[vec==0] <- NA ; vec }
as.data.frame(lapply(rawdat, replace_zeros_with_NAs)) -> clean_dat


# data wrangling to put this in the appropriate GeneMapper format
# first unite the first two columns
clean_dat %>% unite(Sample, Pop, Sample) %>% gather(key = locus, value = length, -1) %>% separate(locus, c("Marker","Allele_num"), sep="\\.") -> tmp
str(tmp)
# at this stage, modify the allele numbers that appear as "1" into "2" and the NAs into 1:
tmp[!is.na(tmp$Allele_num) & tmp$Allele_num == "1", "Allele_num"] <- "2"
tmp[is.na(tmp$Allele_num), "Allele_num"] <- "1"
## OR, maybe better:
reassign_allele_num = function(vec) { ifelse(is.na(vec), "1", "2") }
tmp$Allele_num <- reassign_allele_num(tmp$Allele_num)

# finally we spread
tmp %>% spread(key = Allele_num, value = length, sep=".") -> tmp2
names(tmp2)[c(1,3:4)] <- c("Sample.Name", "Allele.1", "Allele.2")

# we check that we never have two rows with the same combination (Sample, Marker):
tmp2 %>% group_by(Sample.Name, Marker) %>% summarise(groupsize = n()) %>% ungroup() %>% select(groupsize) %>% range()

# now we save into a file, with GeneMapper format
tmp2$Allele.1[is.na(tmp2$Allele.1) & is.na(tmp2$Allele.2)] <- -9
write.table(x=tmp2, file="sheepdata_GeneMapper.txt", row.names = F, sep="\t", quote = F)


# FINALLY! importing into a genambig object (with polysat)
genambig_object = read.GeneMapper("sheepdata_GeneMapper.txt")
summary(genambig_object)

# now we decorate and add information to our object:
Loci(genambig_object)
viewGenotypes(genambig_object, samples = "Shinile_E06")
Description(genambig_object) <- "Data from Helen's sheep"
length(Loci(genambig_object))
Loci(genambig_object)
Ploidies(genambig_object) <- 2 # all samples are diploid

# FOLLOWING LINE TO BE UPDATED AFTER DOUBLE-CHECKING:
Usatnts(genambig_object) <- 2 # could be a vector of size 22 (the number of Loci)

Samples(genambig_object)
PopNames(genambig_object) <- c("Babile", "Harshin", "Jijiga", "Shinile")
PopInfo(genambig_object) <- c(rep(1,24), rep(2, 33), rep(3, 33), rep(4, 28))
summary(genambig_object)

Samples(genambig_object, populations = "Jijiga")
# ALL OK

distmat = meandistance.matrix(genambig_object)
class(distmat)
bionj(distmat) -> nj_tree
plot(nj_tree)
class(nj_tree)
