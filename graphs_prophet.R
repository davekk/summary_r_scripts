getwd()
setwd("./Documents/Davies_2018/R_stuff/") # set where all my data is and where i save my output
library("tidyverse") # general data maniputation & read excel files
library(scales)
labo <- read_xlsx("./Laboratory mosquito analysis_WHO TDR.xlsx", col_names = T, skip=3) # import the actual file
View(labo) # chect the data
class(labo) # type of data object
lab<-labo[2:nrow(labo),2:12] # remove extra information, to remain with the dataframe
length(unique(lab$`Sample ID`)) == nrow(lab) # verify the sample id unique. They must match
length(unique(lab$Item)) == nrow(lab) # verify item column is unique
rownames(lab) <- lab$`Sample ID` # set rownames as sample id column
lab <- select(lab, -`Sample ID`) # drop the sample id column
# drop NA from species pcr
lab <-lab[!(is.na(lab$`Species PCR`)),]
# rename stuff
lab$`Species PCR`[lab$`Species PCR` %in% "An squa"] <- "A. squamosus"
lab$`Species PCR`[lab$`Species PCR` %in% "An. riv" ] <-"A. rivolurum"
lab$`Species PCR`[lab$`Species PCR` %in% "An. a" ] <-"A. arabiensis"
lab$`Species PCR`[lab$`Species PCR` %in% "An. fun s.s" ] <-"A. funestus ss"
lab$`Species PCR`[lab$`Species PCR` %in% "An. prot" ] <-"A. protoriensis"
lab$`Species PCR`[lab$`Species PCR` %in% "An. g s.s" ] <-"A. gambiae ss"
lab$`Species PCR`[lab$`Species PCR` %in% "An. cos" ] <-"A. coustani"
lab$`Species PCR`[lab$`Species PCR` %in% "An. mou" ] <-"A. moucheti"
lab$`Species PCR`[lab$`Species PCR` %in% "An. leesoni" ] <-"A. leesoni"
lab %>% count(`Species PCR`) %>%mutate(perc=n/sum(n) *100) %>% 
  ggplot(aes(y=perc,x=`Species PCR`)) + geom_bar(stat = "identity") +
  labs(title = "Species of mosquitos and percentage occurence", 
       y= "Counts in Percentage", x="Species") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.5)) + 
  theme_minimal() 
ggsave("graph.pdf", device = "pdf", width = 20, height=30, units = "cm")
# group by the species id, calculated percentages of the count
# plot percentage against species id
#
# species id by count/percentage
unique(lab$`species by morphology`)
unique((lab$`Species PCR`))

