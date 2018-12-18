library("phyloseq")
getwd()
setwd("~/data-shell/")
otu_dense <- read.table("./chick_micr_feature-table.tsv", sep="\t", numerals = "no.loss", header=TRUE)
otu_sparse <- read.table("chick_micr_feature-table_melted.tsv", sep="\t", numerals = "no.loss", header=TRUE)
taxonomy <- read.table("chick_micr_taxonomy.tsv", sep="\t", numerals = "no.loss", header=TRUE)
metadata <- read.table("chick_micr_metadata.tsv", sep="\t", numerals = "no.loss", header=TRUE)
## done loaded data into r
arrange(otu_dense, OTU.ID) -> otu_dense
arrange(taxonomy,Feature.ID) ->taxonomy
rownames(otu_dense) <- otu_dense$OTU.ID
subset(otu_dense, select = -OTU.ID) ->new_otu_dense
subset(otu_dense, select = name_otu[1:10]) ->new_otu_dense
taxonomy%>%separate(col=Taxon, into=c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep = ";" ,remove = TRUE) ->new_taxonomy
rownames(new_taxonomy) <- new_taxonomy$Feature.ID
subset(new_taxonomy, select = -Feature.ID) ->taxonomy
OTU = otu_table(new_otu_dense, taxa_are_rows = TRUE)
TAX = tax_table(taxonomy)

estimate_richness(OTU,measures = c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher")) ->richness_stats
plot_richness(OTU,measures = c("Observed", "Chao1",  "Shannon", "Simpson", "Fisher") )+scale_color_manual(randomcoloR::distinctColorPalette(length(new_otu_dense)))


