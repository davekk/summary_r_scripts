setwd("/home/jbde/Trainings/Phylogeny_BixCoP_2018/Practicals/ApesData/")
library(ape)
read.FASTA("mito_sequences.fasta") -> seqs
sub(" .*","",names(seqs)) -> accessions

# generating trees
trees = rmtree(N=100, n=7, rooted=F, tip.label = accessions, br = 0.1)
write.tree(phy = trees, file = "hundred_trees.nh")


# then after the calculations have been performed with PhyML...

results = read.table(sep='\t', file="all_results_JC69.txt", header=T)
str(results)
hist(results$parsimony, breaks=20)
hist(results$logLk, breaks=20)
plot(logLk ~ parsimony, data=results)

# let's see the tree size:
plot(logLk ~ tree_size, data=results)
plot(parsimony ~ tree_size, data=results)
cor(results$parsimony, results$tree_size, method="spearman")


