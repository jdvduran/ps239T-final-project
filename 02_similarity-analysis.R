#######################################
#This script takes the text files found in python
#and determines teh dissimilarity between the 
#first and last texts of each bill. Determining 
#which bills are actually gut and amended is 
#done by hand. Hopefully this data can later be used 
#for machine learning to expedite the process.
#######################################



#install packages
packages <- c("tm", "RTextTools", "lsa", "cluster", "fpc", "rio", "plyr")
install.packages(packages)


#load pacages
library(tm)
library(RTextTools)
library(lsa)
library(cluster)
library(fpc)
library(rio)
library(plyr)


 #load in csv from jupyter collectio
intro = read.csv("introtext.csv")
last = read.csv("lasttext.csv")

+#make numbered rows for each of the two data frames in order t match them 
intro$ID<-seq.int(nrow(intro))
last$ID<-seq.int(nrow(last))

#merge them by ID created above
alltext <- merge(intro,last,by="ID")


#function gets dissimilarity measure in decimals
func <- function(banana){
  #makes two row corpus out of one row of two columns in df
  corp <- Corpus(VectorSource(banana))
  #makes dtm out of that corpus
  dtm <- DocumentTermMatrix(corp,
                            control = list(stopwords = T,
                                           tolower = TRUE,
                                           removeNumbers = TRUE,
                                           removePunctuation = TRUE,
                                           stemming=TRUE))
  #turns dtm into matrix
  dtm.m <- as.matrix(dtm)
  #repositions the matrix for cosine eval  by column
  dtm.t <- t(dtm.m)
  #performs the cosine test
  dcos <- cosine(dtm.t)
  #gets measure of dissimilarity
  dis<- as.dist(1-dcos)
  #reurns the dissimilrity measure and [1] indicates just the correct measure
  return(dis[1])
}

#have to reorgaize data to get correct measure by moving ID to last column
alltext$ID <- NULL

#performs the function
alltext$dissimilarity <-by(alltext, seq_len(nrow(alltext)), func)
#converts dissimilarity to a percentage from decimal
alltext$dissimilarity <- alltext$dissimilarity*100

#reassign ID to last column
alltext$ID <- seq.int(nrow(alltext))

#remove all commas from the text to export to csv
alltext$intro.text <- gsub(",", "", alltext$intro.text)
alltext$last.text <- gsub(",", "", alltext$last.text)
#export to csv to save data
write.table(alltext, file = "alltextDISSIM.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\r", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"))


###Now export at different levels of dissimilarity to look through and determine whther or not GandA took place

#export csv for those between 90 and 100
h90 <- as.data.frame(subset(alltext, dissimilarity >=90, select=c(intro.text:ID)))
write.table(h90, file = "h90.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\r", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"))

#export csv for those between 80 and 90
h80 <- as.data.frame(subset(alltext, dissimilarity >=80 & dissimilarity < 90, select=c(intro.text:ID)))
write.table(h80, file = "h80.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\r", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"))

#export csv for those between 70 and 80
h70 <- as.data.frame(subset(alltext, dissimilarity >=70 & dissimilarity < 80, select=c(intro.text:ID)))
write.table(h70, file = "h70.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\r", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"))

#export csv for those between 60 and 70
h60 <- as.data.frame(subset(alltext, dissimilarity >=60 & dissimilarity < 70, select=c(intro.text:ID)))
write.table(h60, file = "h60.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\r", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"))

#export csv for those between 50 and 60
h50 <- as.data.frame(subset(alltext, dissimilarity >=50 & dissimilarity < 60, select=c(intro.text:ID)))
write.table(h50, file = "h50.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\r", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"))

#export csv for those between 40 and 50
h40 <- as.data.frame(subset(alltext, dissimilarity >=40 & dissimilarity < 50, select=c(intro.text:ID)))
write.table(h40, file = "h40.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\r", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"))

#export csv for those between 0 and 40
hlow <- as.data.frame(subset(alltext, dissimilarity<40, select=c(intro.text:ID)))
write.table(hlow, file = "hlow.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\r", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"))


##NOW WORK OUTSIDE OF R
#Go through bills in these files to determine which ones were subject to G and A
#IGNORE BUDGET BILLS



