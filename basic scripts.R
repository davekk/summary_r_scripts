x
a
letters
names(a)<-c(LETTERS
            )
a
a_subset<-a(a<15 & a>10
            )
x[2,3]
x[2,]
x[,c(1,
     2)]
x["row1
  "]
x["row1",]
x["row1"]
x["row1","col2"]
y<-matrix(c(10,20,30,40), nrow=2, ncol=2, byrow=TRUE)
y
cbind(x1,y)
class(x
      )
typeof(x)
nrow(x
     )
ncol(y)
length(x
       )
x<-matrix(1:50, ncol = 5, nrow = 10
byrow=TRUE          )
x1<-matrix(1:50, ncol=5, nrow=10, byrow= TRUE
           )
x1
x[2:5
  ]
x1[2:5]
my_list<-list(a_subset, c, x2, 20)
my_list
my_list[[1]][2]="m"
a_subset
ls(x2)
ls
ls()
my_list
df1<-data.frame(x2,x3,x4,x5)
df1
df1[4,4]
nrow(df1)
ncol(df1)
dimnames(df1)<-list(c("row1","row2","row3","row4"), c("col1","col2","col3","col4"))
df1$col4
df1["row1",]
df1["col2"]
df<- data.frame(first=c('Davies'), last=c('Kaimenyi'), lucky_number=c(635), stringsAsFactors=FALSE )
df
df<-rbind(df, list('topi', 'Kale', 78)) # add row in df
df
df<-cbind(df, cofeetime= c(TRUE,FALSE,TRUE)) #add column in df
df
df2<-df[,-3]
df2
df[,c(1,2)]
#ignore text
###################################################################
v1<-c(1,1,1,2,2,2,2,3,3,3,3,3,3)
levels=c(1,2,3)
labels=c("red","blue","green")
f_v1<-factor(v1,levels,labels)
f_v1
typeof(df1)
class()
length(f_v1)
for(i in 1:10){print(i)}
i
m<-c(1:10)
for(i in m){print(paste("ocuments/day1 feb")
mainDir<-("C:/Users/kaimen5*",i,"=",5*i))}
getwd()
setwd("C:/Users/kaimenyi/Dyi/Documents/day1 feb")
data_import<-read.table("C:Users/kaimenyi/Downloads/gapminder-FiveYearData.csv", sep=",", header=TRUE)
data_import2<-read.table("C:\\Users\\kaimenyi\\Downloads\\clinic_file.txt", sep=",", header=TRUE)
mainDir <- "C:/Users/kaimenyi/Dyi/Documents/day1 feb"
subDir <- "outputDirectory"
if (file.exists(subDir)){setwd(file.path(mainDir, subDir))} else {dir.create(file.path(mainDir, subDir)) setwd(file.path(mainDir, subDir))} 
if (file.exists(subDir)){setwd(file.path(mainDir, subDir))
} else {dir.create(file.path(mainDir, subDir)) setwd(file.path(mainDir, subDir))}
tail(data_import
     )
tail(data_import2)

summary(c(data_import, data_import2
          ))
summary(data_import2)
str(c(data_import, data_import2))
gapminder_FiveYearData<-data_import
clinic_file<-data_import2
nrow(clinic_file)
nrow(clinic_file[clinic_file$BPressure=="high" | clinic_file$BPressure=="pre-high", ])
clinic_file
nrow(clinic_file[clinic_file$Sex=="female" & clinic_file$BGlucose>=7, ])
clinic_file[as.numeric(clinic_file$BCholesterole)
            
            )
clinic<-read.table("clinic_file", sep=(;), header=TRUE)
clinic<-read.table("C:\\Users\\kaimenyi\\Downloads\\clinic_file.txt", sep=";", header=TRUE)
nrow(clinic[clinic$BPressure=="high" | clinic$BPressure=="pre-high", ])
nrow(clinic)
nrow(clinic[clinic$Sex=="female" & clinic$BGlucose>=7, ])
clinic[clinic$BCholesterol>190,"Name"]
clinic[clinic$BPressure=="high" & clinic$BCholesterol>190, c("Name","Phone")]
clinic[(clinic$BPressure=="high" & clinic$BCholesterol>190) | (clinic$BPressure=="high" & clinic$BGlucose>=7), "Name"]
download.file("http://bioinformatics.hgen.slu.se/SLUBIOINFO2017/SLUBIOINFO2017/Tutorials_files/One%20day%20R-Exercises%20Bioinformatics_Course%20.pdf", destfile = "gapminder-FiveYearData.csv")
download.file("https://www.youtube.com/watch?v=FklUAoZ6KxY", destfile = "teen_spirit_parody")
ls()
gapminder<-read.csv("C:\\Users\\kaimenyi\\Documents\\day1 feb\\gapminder-FiveYearData.cvs")
head(gapminder
     )
head(gapminder_FiveYearData)
gapminder<-gapminder_FiveYearData
str(gapminder)
summary(gapminder)
typeof(gapminder$year)
length(gapminder)
nrow(gapminder)
NCOL(gapminder)
ncol(gapminder)
dim(gapminder)
colnames(gapminder)
row.names(gapminder)
gapminder_small<- gapminder[c(1:9, 19:23),]
gapminder_small
summary(gapminder_small)
gapminder2<-read.csv("http://bioinformatics.hgen.slu.se/SLUBIOINFO2017/SLUBIOINFO2017/Tutorials_files/One%20day%20R-Exercises%20Bioinformatics_Course%20.pdf")
head(gapminder2)
write.table(gapminder_small,file="gapminder_out.txt",sep="\t",quote=FALSE,row.names=FALSE)
write.table(gapminder_small,file="gapminder_out2.txt",sep="\t",quote=FALSE,col.names=NA)
if(nrow(gapminder[(gapminder$year==2002),]) >=1){print("Record(s) for the year 2002 found.")}
if(nrow(gapminder[(gapminder$year == 2002),]) >= 1){print("Record(s) for the year 2002 found.") }

paste0(LETTERS)
paste0(LETTERS, sep="", collapse=NULL)
df1<-data.frame(Chr=paste0('chr',1:9), Gene=paste0('gene', 19:11))
df1
df2<-data.frame(Chr=paste0('chr',2:10), Position=paste0('Pos',22:30))
df2
df3<-merge(df1, df2, by="Chr")
df3
colnames(df2)[1] <- "Chr_ID"
df4<- merge(df1, df2, by.x="Chr", by.y="Chr_ID")
df4
x<-rnorm(50)
x
hist(x)
hist(x,main="title of histogram")
hist(rnorm(20))
x
?hist
hist(x,main="Histogram of 50 random numbers")

hist(x, main = "Histogram of 50 random numbers", col = "red")dev.off()
x
pdf(file="my_histogram.pdf")
png(file="my_histogram.png")
dev.off()
q()
