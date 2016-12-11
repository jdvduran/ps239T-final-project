#######################################
#This script takes the gut and amend data 
#found from manually sorting thorugh the 
#exported dissimilarity files and visualizes
#the topics using word clouds.
#######################################




#install packages
packages2 = c("mallet", "wordcloud", "rjava", "RColorBrewer")
install.packages(packages)

#load packages
library(wordcloud)
library(RColorBrewer)

#rJava is finnicky and has to be installed this way (and subsequently mallet)
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)
library(mallet)

#exported files from last script are gone through to get G and A bills 
#make new dataframe to save which ones were G and A bills
GAid <- alltext
GAid$GandA <- 0

#Store ID's of identified G and A bills in list
galist = c(278,	1051,	1210,	1341,	1683,	20,	51,	91,	179,	203,	243,	275,	331,	496,	512,	530,	545,	547,	571,	572,	604,	675,	696,	725,	747,	770,	
           825,	832,	841,	843,	870,	912,	936,	943,	966,	1006,	1156,	1205,	1228,	1234,	1276,	1284,	1322,	1323,	1335,	1349,	1357,	1363,	1388,	1408,	1432,	
           1478,	1495,	1499,	1510,	1537,	1546,	1576,	1595,	1599,	1743,	1781,	1821,	2023,	2025,	2150,	2174,	2195,	2213,	2230,	2246,	2321,	2404,	2487,	2536,	2607,	
           2640,	2653,	2669,	2684,	2701,	2777,	2787,	2792,	2796,	2809,	2831,	2836,	2856,	2861,	2912,	2914,	2934,	3153,	3317,	3336,	3355,	3368,	3369,	3444,	3461,	
           3474,	3478,	3481,	3521,	3553,	3576,	3586,	3612,	3644,	3694,	3697,	3699,	3716,	3721,	3742,	3781,	3849,	3881,	3910,	3931,	3974,	3998,	4000,	4010,	4027,	
           4033,	4063,	4103,	4191,	4222,	4390,	4399,	4431,	4531,	4577,	4621,	4647,	4653,	4702,	4704,	4715,	4789,	27,	31,	56,	337,	361,	379,	393,	396,	405,	
           410,	412,	431,	465,	516,	534,	577,	621,	623,	626,	672,	692,	764,	798,	881,	903,	910,	930,	944,	995,	1036,	1079,	1085,	1096,	1097,	
           1106,	1146,	1154,	1181,	1197,	1215,	1220,	1222,	1238,	1251,	1267,	1279,	1290,	1318,	1328,	1334,	1344,	1441,	1442,	1465,	1477,	1487,	1508,	1509,	1521,	
           1531,	1538,	1551,	1554,	1557,	1566,	1785,	1810,	1833,	1886,	1919,	2019,	2067,	2096,	2179,	2180,	2184,	2185,	2241,	2288,	2359,	2395,	2402,	2441,	2463,	
           2474,	2483,	2520,	2561,	2568,	2592,	2603,	2651,	2666,	2737,	2757,	2758,	2759,	2794,	2843,	2847,	2866,	2890,	2893,	2908,	2955,	3298,	3324,	3335,	3360,	
           3363,	3564,	3571,	3582,	3691,	3729,	3755,	3758,	3831,	3839,	3856,	3904,	3932,	3969,	3971,	3987,	3989,	3990,	3997,	4007,	4022,	4043,	4094,	4095,	4116,	
           4192,	4213,	4268,	4299,	4314,	4337,	4391,	4435,	4507,	4552,	4558,	4615,	4662,	4671,	4676,	4696,	4701,	4742,	4760,	4772,	46,	191,	193,	204,	232,	
           349,	363,	491,	495,	612,	613,	676,	750,	795,	858,	866,	906,	920,	933,	1032,	1060,	1148,	1160,	1247,	1305,	1314,	1336,	1468,	1472,	1540,	
           1553,	1803,	1961,	1978,	1997,	2143,	2170,	2276,	2426,	2454,	2501,	2616,	2634,	2635,	2667,	2839,	2840,	2867,	2873,	2885,	2913,	2922,	3037,	3061,	3070,	
           3351,	3459,	3542,	3550,	3611,	3648,	3688,	3689,	3731,	3765,	3936,	3939,	3946,	3967,	3978,	3995,	4023,	4051,	4082,	4311,	4463,	4517,	4644,	4710,	4725,	4739,
           213,	268,	622,	641,	730,	852,	873,	1568,	2209,	2601,	2728,	2819,	2942,	3537,	3580,	3734,	3840,	3891,	4034,	4207,	4352,	4419,	4586,	4673,	4744)

#sort the list in alphabetical order
galist <- sort(galist, decreasing=F)

#lop through the list of IDs to assign 1s to bills subject to G and A
for (i in galist){
  GAid$GandA[i] =1
}

#export CSV of G and A bills for future reference
write.table(GAid, file = "ALLGAcoded.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\r", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"))



#Subset data into only bills that were G and A
#(change order of column selection depending on your particular data frame after merging)
GAtext <- subset(GAid, GandA == 1, select=c(ID:intro.text))

#Export CSV of subset for future reference
write.table(GAtext, file = "GAsubset.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\r", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"))



###wordclouds###
#make a wordcloud for both the first and last drafts of gut and amend bills to see how their topics differ

#Make new data frame for the intro text word cloud
GAwci <- GAtext
GAwci$ID <- GAtext$ID
GAwci$text <- GAtext$intro.text

#remove punctuation
GAwci$text <- gsub(pattern="[[:punct:]]", replacement=" ", GAwci$text)

#load data into mallet
mallet.instances <- mallet.import(as.character(GAwci$ID),
                                  as.character(GAwci$text), 
                                  "stoplist.csv", 
                                  FALSE, 
                                  token.regexp="[\\p{L}']+")
#number of topics to model
n.topics = 10
#topic trainer object
topic.model <- MalletLDA(n.topics)
#load the intro text
topic.model$loadDocuments(mallet.instances)
#get vocab and word frequency stats
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
#examine word freqs and add anything to stoplist necessary then rerun mallet
word.freqs[1:50,]

#optimize hyperparameters every 20 iterations
topic.model$setAlphaOptimization(20,50)
#train the model over 100 iterations
topic.model$train(100)
#get probablity f topics and probability of words in topics
#smooth to get probabilities
doc.topics <- mallet.doc.topics(topic.model, smoothed = T, normalized = T)
topic.words <- mallet.topic.words(topic.model, smoothed = T, normalized = T)


#make the word cloud
num.topics <- 10
num.top.words <- 50
#set the color palette
pal2 <- brewer.pal(9, "Paired")
#export wordcloud to png fle
png("wordcloud.png", width=1280,height=800)
for (i in 1:num.topics){
  topic.top.words <- mallet.top.words(topic.model, topic.words[i,], num.top.words)
  wordcloud(topic.top.words$words, topic.top.words$weights, c(4, .8), rot.per = 0, random.order = F, colors = pal2)
}
dev.off()



#Make new data frame for the last text word cloud
GAwcl <- GAtext
GAwcl$ID <- GAtext$ID
GAwcl$text <- GAtext$last.text

#remove punctuation
GAwcl$text <- gsub(pattern="[[:punct:]]", replacement=" ", GAwcl$text)

#load data into mallet
mallet.instances <- mallet.import(as.character(GAwcl$ID),
                                  as.character(GAwcl$text), 
                                  "stoplist.csv", 
                                  FALSE, 
                                  token.regexp="[\\p{L}']+")

#number of topics to model
n.topics = 10
#topic trainer object
topic.model <- MalletLDA(n.topics)
#load the last text
topic.model$loadDocuments(mallet.instances)
#get vocab and word frequency stats
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
#optimize hyperparameters every 20 iterations
topic.model$setAlphaOptimization(20,50)
#train the model over 100 iterations
topic.model$train(100)
#get probablity f topics and probability of words in topics
#smooth to get probabilities
doc.topics <- mallet.doc.topics(topic.model, smoothed = T, normalized = T)
topic.words <- mallet.topic.words(topic.model, smoothed = T, normalized = T)


#make next wordcloud
num.topics <- 10
num.top.words <- 50
png("wordcloud2.png", width=1280,height=800)
#export wordcloud to png fle
for (i in 1:num.topics){
  topic.top.words <- mallet.top.words(topic.model, topic.words[i,], num.top.words)
  wordcloud(topic.top.words$words, topic.top.words$weights, c(4, .8), rot.per = 0, random.order = F, colors = pal2)
}
dev.off()
