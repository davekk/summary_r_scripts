require(ape)
getwd()
# read.FASTA('~/mito_Seq.fasta') -> ape
#sub("\\..*","",names(ape)) -> accesions
tree = rmtree(N=100,n=7, rooted = F, tip.label = NULL,br=0.1)
write.tree(phy=tree, file= "hundred_trees.nh")
