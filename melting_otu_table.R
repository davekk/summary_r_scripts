library(reshape2)
getwd()
setwd("~/data-shell/")
can <- read.table("./sheep_wafeature-table.tsv", sep="\t", numerals = "no.loss", header=TRUE)
can[1:5,1:5]
new_can <- melt(can, id="OTU.ID")
new_can[1:5,]
colnames(new_can) <- c("OTU.ID", "Sample.ID", "abundance")
write.table(new_can,file = "sheep_wa_feature-table_melted.tsv",sep = "\t", quote = F, row.names = F, dec=".", eol= "\n")
##
