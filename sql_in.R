# install.packages("RMySQL")
require(RMySQL)
require(ggplot2)
require(dplyr)
mydb = dbConnect(MySQL(), user='root', password='', dbname='limidb', host='localhost') # be careful as the name of db is limidb not limidb2 or limidb3
dbListTables(mydb) # see the tables in the object
dbListFields(mydb, 'otutbl') # see the column headers of specified table
# rs = dbSendQuery(mydb, "SELECT * FROM otutbl JOIN p_taxonomy ON otutbl.otu = p_taxonomy.out_id JOIN p_metadata ON otutbl.sample = p_metadata.sample") # entire dataset from sql
# data = fetch(rs, n=-1) # entire dataset from sql
rs = dbSendQuery(mydb, "SELECT otutbl.sample, otutbl.abundance, p_taxonomy.Phylum FROM otutbl JOIN p_taxonomy ON otutbl.otu = p_taxonomy.out_id") # selected columns in the dataset from sql
data_red = fetch(rs, n=-1) # get everything as a df
data_red$abundance <- as.numeric(data_red$abundance) # change class of abundance to numeric
data_red <- aggregate(abundance~Phylum+sample,data=data_red,FUN=mean) # remove duplicated rows based of the Phylum+sample columns and calculate the mean of abundance for duplicated rows
data_red[,c("sample","Phylum","abundance")]-> data_red # reorder the d.f
data_red %>% mutate(rescale=sqrt(abundance)) ->data_red # calculate a normalized count column based on sqrt of abundance 

#ggplot(data_red, aes(Phylum, sample, fill=abundance)) + geom_tile(aes(fill=abundance), colour= "white")
ggplot(data_red, aes(Phylum, sample))+ geom_tile(aes(fill = rescale),colour = "white") + # heatmap based on rescale column and white background
  scale_fill_gradient(low = "white",high = "green") + # fill the tiles from white to green
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + theme(legend.position = "none",axis.ticks = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1,size=5),axis.text.y = element_text(size=5)) # scale for x and y away from plot and edges, with the y tilted at 45 for better readibility       
###
library(randomcoloR)

ggplot() + geom_bar(aes(y = abundance, x = sample, fill = Phylum), data = data_red,  stat="identity",position="fill") +scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + theme(legend.position="right", legend.direction="vertical",legend.title = element_blank(),axis.ticks = element_blank(),axis.text.x = element_text(angle = 45, hjust = 1,size=5),axis.text.y = element_text(size=5)) +scale_fill_manual(values=distinctColorPalette(length(unique(data_red$Phylum))))# + scale_fill_manual(values=getPalette(colour))
  #scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Accent"))(colourCount)) + scale_fill_manual(values = colorRampPalette(brewer_pal(30, "Accent")))# +scale_fill_brewer(palette = "jco") 

