getwd()
setwd('~/module2_R_biostats-master/ssr_helen/')
# install.packages('polysat')
require('polysat')
library('tidyr')   # for more data frame manipulation
library('dplyr')   # these two are very convenient for a number of data transformations
library('stringr')
library('ape')
# read ssr data
#sheep_ssr <- read.csv('sheep_SSR_data.csv', header = F,sep = ";")

# head(sheep_ssr) # see content of ssr
# colnames(sheep_ssr) # column names
# rownames(sheep_ssr)  # rownames

# new_ssr <- sheep_ssr[3:nrow(sheep_ssr),] # copy dataset

# colnames(new_ssr) <- c("Pop", "Individual", paste0(as.character(unlist(sheep_ssr[3,3:ncol(sheep_ssr)])), '.', rep(1:2, n=44)))# set column names as the first row from the 3rd column
# new_ssr  <-  new_ssr[-1, ] # remove the first row
sheep_ssr <- read.table(file='sheep_SSR_data.csv', sep = ";", skip=2, header =T )

# new_ssr[new_ssr==0] <- NA # change all 0 to NA since they are missing data,
# colnames(new_ssr)
names(sheep_ssr)[1:2] <- c("Pop", "Individual")
dim(sheep_ssr ) # 22 markers 118 samples
unique(sheep_ssr$Pop)
length(unique(sheep_ssr$Individual)) == nrow(sheep_ssr)
# colnames(sheep_ssr)
# check if there are any missing data  (0 or NA or other unexpected symbol)
#c(0, "-")

typeof(sheep_ssr[[4]])

sapply(sheep_ssr[-c(1,2)], typeof, USE.NAMES=F) ->m
names(m) <- NULL
# unname(m) # second option
m
sum(m != "integer")# should be 0
sum(is.na(sheep_ssr)) # should be 0

sum(sheep_ssr==0) # check presence of any 0, that we want to replace with NA
replace_zero_with_na = function(vec) {vec[vec==0] <- NA ; vec}
as.data.frame(lapply(sheep_ssr, replace_zero_with_na))->clean_ssr
# sheep_ssr[sheep_ssr==0] <- NA
# Problem: "*.1" and "*.2" are now the two alleles corresponding to the same locus. We will use tidyr to transform (reshape) our dataset
new_ssr %>% gather(key = Locus, value = Length, -c(1,2)) %>% separate(col = Locus, into = c("Marker", "Allele")) %>% spread(key = Allele, value = Length, sep = ".") -> clean_ssr
# first unite the first 2 columns

clean_ssr %>% unite(Sample = Pop, Individual) %>% gather(key = Locus, value = Length, -1) %>% separate(col = Locus, into = c("Marker", "Allele_num"), sep = "\\.") -> tmp

# modify the allele numbers that appear as 1 into 2 and na into 1

tmp[!is.na(tmp$Allele_num) & tmp$Allele_num=="1", "Allele_num"] <- "2"
tmp[is.na(tmp$Allele_num), "Allele_num"] <- "1"
# second method 
reassign_allele_num = function(vec){ifelse(!is.na(vec), "1,","2")}
tmp$Allele_num <- reassign_allele_num(tmp$Allele_num)
# 
tmp %>% spread(key = Allele_num, value = Length, sep = ".") ->tmp2

colnames(clean_ssr)
# Last thing: we combine "Landraces" and "pop" into a single column:
clean_ssr %>% unite(Sample.Name, Pop, Individual, sep="_") -> clean_ssr

# we delete the rows with two NAs as their two alleles:
clean_ssr %>% filter(!(is.na(Allele.1) & is.na(Allele.2))) -> clean_ssr

# We put an -9 in Allele 1 where we have no observation at all (no Allele 1, no Allele 2):
clean_ssr$Allele.1[is.na(clean_ssr$Allele.1)] = -9

# Now we have finally reached the right input format for polysat. We write that tibble/data frame into a tab-delimited text file
write.table(x = clean_ssr, file = "sheep_genemapper.txt", sep = "\t", quote = F, row.names = F)
write.table(x=tmp2, file="sheep_genemapper.txt", row.names = F, sep = "\t", quote = F)
# and we import the data into R/polysat:
x = read.GeneMapper("sheep_genemapper.txt")
summary(x)
Description(x) <- "Sheep SSR genotyping dataset (Hellen Nigussie)"
# now we update the info about the populations and the various Usatnts:
# First we need to give the populations as integers from 1 to 5, in agreement with the different names for the 5 populations (factor-like thing)
Samples(x) -> samples

popnames = factor(str_sub(samples, start = 1L, end = -5L))

PopNames(x) <- levels(popnames)    # names names
PopInfo(x) <- as.integer(popnames) # integers from 1 to 5

Samples(x,populations = c("Babile","Harshin")) # to check
# adding the info about the nucleotide repeats (di- or tri-):
Loci(x)
#  "P1"  "P11" "P12" "P13" "P15" "P2"  "P4"  "P6"  "P7"  "P8"  "P9"  "P10"
# Usatnts(x) <- c(3,2,2,3,2,2,2,2,3,2,2,2)
#"P1"  "P10" "P11" "P12" "P13" "P15" "P2"  "P6"  "P7"  "P8"  "P9"  "P4"
# Usatnts(x) <- c(2,2)
#  "P1"  "P10" "P11" "P12" "P13" "P15" "P2"  "P4"  "P6"  "P7"  "P8"  "P9" 
# Usatnts(x) <- c(3,2,2,2,3,2,2,2,2,3,2,2)

# to check:
Loci(x,usatnts = 2)

# Ploidy is constant, equal to 2:
Ploidies(x) <- 2


# sanity checking our dataset:
Samples(x)
Samples(x, populations = "Babile")
Loci(x,usatnts=2)
viewGenotypes(x, loci = "BM1329", samples = Samples(x, populations = "Harshin"))
Genotypes(x)


alleleDiversity(x,alleles = F) # allele diversity at each loci, for each population
# West Gurage seems the most diverse population, followed by East Gurage

simpleFreq(x) -> allele_freqs # getting allele frequencies for each population and each marker
PIC(allele_freqs, bypop = T, overall = T) -> PIC_statistics

PIC_statistics
write.csv(PIC_statistics,file = "PIC_statistics.csv")
genotypeDiversity(x) -> shannon_genotype_diversity # uses Lynch distance by default, and calculates Shannon indices
write.csv(shannon_genotype_diversity,file = "genotype_diversity_Shannon.csv")

# calculate genetic distances between individuals using Lynch distance
distances = meandistance.matrix(x,distmetric = Lynch.distance)

pca <- cmdscale(distances, k=10, eig=T) # Principal Coordinates Analysis
plot(x=pca$points[,1],y=pca$points[,2],col=PopInfo(x),pch=16)


# build a Neighbour-Joining tree

# now we want to use the matrix to get a tree built with NJ
mytree = nj(distances)
identical(mytree$tip.label,Samples(x)) # ok, same order

mypalette = brewer.pal(n=5, "Set1")

# tweaking the popnames so that we can use shorter names to plot in the tree
levels(popnames)
levels(popnames) <- c("Bb","Hs", "Jj", "Sn")
distances_with_shortnames = distances
dimnames(distances_with_shortnames) <- list(as.character(popnames),as.character(popnames))
mytree = nj(distances_with_shortnames)
pdf("complete_NJ_tree_vertical.pdf", width=6,height=10)
plot.phylo(mytree, type="phylogram", cex = 0.8, tip.color = mypalette[PopInfo(x)]) # y.lim = c(2,50) # would trim the tree
axisPhylo()
legend("topright", cex=0.8, legend = PopNames(x), text.col = mypalette[1:length(PopNames(x))], bty="n")
dev.off()

# a horizontal tree now
pdf("complete_NJ_tree_horizontal.pdf", width=10,height=6)
plot.phylo(mytree, type="phylogram", cex = 0.8, tip.color = mypalette[PopInfo(x)],direction='downwards') # y.lim = c(2,50) # would trim the tree
#add.scale.bar()
axisPhylo(side=2)
legend("bottomleft", cex=0.8, legend = PopNames(x), text.col = mypalette[1:length(PopNames(x))], bty="n")
dev.off()
## 
# new dist mat
# calculating pairwise distance
distance_sheep <- meandistance.matrix(x, all.distances = F, distmetric = Bruvo.distance, progress = T)

distance_sheep
pca2 <- cmdscale(distance_sheep, k=10, eig=T) # Principal Coordinates Analysis
plot(x=pca2$points[,1],y=pca2$points[,2],col=PopInfo(x),pch=16)
# nj tree
mytree2 = nj(distance_sheep)
identical(mytree2$tip.label,Samples(x)) # ok, same order
levels(popnames)
levels(popnames) <- c("Bb","Hs", "Jj", "Sn")
distances_with_shortnames2 = distance_sheep
dimnames(distances_with_shortnames2) <- list(as.character(popnames),as.character(popnames))
mytree3 = nj(distances_with_shortnames2)
pdf("complete_NJ_tree_vertical2.pdf", width=6,height=10)
plot.phylo(mytree3, type="phylogram", cex = 0.8, tip.color = mypalette[PopInfo(x)]) # y.lim = c(2,50) # would trim the tree
axisPhylo()
legend("topright", cex=0.8, legend = PopNames(x), text.col = mypalette[1:length(PopNames(x))], bty="n")
dev.off()

# a horizontal tree now
pdf("complete_NJ_tree_horizontal2.pdf", width=10,height=6)
plot.phylo(mytree3, type="phylogram", cex = 0.8, tip.color = mypalette[PopInfo(x)],direction='downwards') # y.lim = c(2,50) # would trim the tree
#add.scale.bar()
axisPhylo(side=2)
legend("bottomleft", cex=0.8, legend = PopNames(x), text.col = mypalette[1:length(PopNames(x))], bty="n")
dev.off()