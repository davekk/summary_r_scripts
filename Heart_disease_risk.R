# q1
getwd()
setwd("~/module2_R_biostats-master/kenny_work/") # set the working directory (where the file is and where to save the output)

library(readxl) # the package for specifically reading excel files
disease <- read_xlsx("./20190108024644heart_disease_risk.xlsx") # read the excel file into r and save it into a variable
sum(is.na(disease)) # check if there is any NA value in dataset
summary(disease) # get a basic summary of the data
set.seed(20190801) # to make sure the data is reproducable because initial cluster assignments are random.





#q2
kmeans_disease <- kmeans(disease, centers = 4, nstart=50) # generate 4 clusters of the whole dataset and try 50 different random starting assignments and then select the one with the lowest within cluster variation

kmeans_disease # print the kmeans object
## summary of the work above
 #a cluster os a group sharing similar characteristics. It is more of a discovery than prediction and as such the algorithm searches for similarity in the dataset
# The kmeans algorithm tries to find randomly selected groups or "cluster" via minimizing distances between observations (individuals or rows). Groups are initially chosen at random and the distance between the cluster center and the different observations (centroid) minimized, resultings in groups with observations. The initial centroid is shifted to the group co-ordinate mean and the distance minimized according to the new centroid which creates new boundaries of the groups, moving some observations between different groups. This is repeated until no changes are observed in the groups
# for the results;
#The list kmeans_disease contains seven interesting elements:
#kmeans_disease$cluster: Indicates the cluster of each observation
#kmeans_disease$centers: The cluster centres
#kmeans_disease$totss: The total sum of squares
#kmeans_disease$withinss: Within sum of square. The number of components return is equal to `4`
#kmeans_disease$tot.withinss: Sum of withinss
#kmeans_diseasebetweenss: Total sum of square minus Within sum of square
#kmeans_disease$size: Number of observation within each cluster

# In this dataset, 4 clusters were generated and randomly iterated 50 times to get the lowest within cluster variation. The first cluster has 211 obseravtions, while the smallest, cluster 3 has 142 observations. The clusters are not homogenous, and the centroid value is used to take a deeper look at the data.
centre <- kmeans_disease$centers
centre
#The rows refer to the centriod of the cluster and the columns are the variables used by the algorithm. The values are the means by each cluster for the given column. Positive values indicate the z-score for a given cluster is above the overall mean.

# q3
new_disease <- cbind(disease, cluster = kmeans_disease$cluster) # dataset is combined with each individual's cluster number in a new data frame
# this dataframe can be used to compute the mean of each variables by clusters using the original data, to assess how distinct our 4 clusters are
aggregate(new_disease, by=list(new_disease$cluster), mean)

# q4
# Cluster 1 has above average choresterol, anxiety and weight levels, below average stress management levels. Cluster 2 has above average stress management levels and below average weight and choresterol levels but the lowest anxiety levels in all clusters. Cluster 3 has the highest weight and choresterol average  and lowest stress management among all the clusters. Cluster 4 the highest stress management levels, below average anxiety levels and the lowest weight and choresterol levels in all clusters. Therefore; 
# 1="high", 2="moderate", 3="critical", 4="low"

new_disease$cluster <- factor(new_disease$cluster) # change the column into a factor with levels
levels(new_disease$cluster) <-c("high","moderate","critical", "low")
#factor(c("4"="low","2"="moderate","1"="high","3"="critical")) # The four clusters are correctly classified into low, moderate, high and critical classification


# q5
# One correct patient recommendation is given that is based on data in the k-means analysis
new_disease[11,]
# individual '11' weighs '66' units, choresterol levels of '220' units, '0' stress management and '60' anxiety points is classified as 'critical'

#q6
# The relationship between gender and heart disease risk cluster is correctly identified and explained using data in the k-means analysis results
 library(ggplot2)
ggplot(data = new_disease, aes(x = new_disease$Gender, y = new_disease$Cholesterol,  color=new_disease$cluster)) + geom_point() + labs(x="Gender", y= "Choresterol level") # the plot show that the most individuals in the gender assigned "1 are at a lower risk of heart disease as they fall in the low and moderate clusters while more than average individuals in the gender asigned "2" are at a higher heart disease risk as they fall in the high and critical clusters

# ggsave("name.pdf") # if you need to save it