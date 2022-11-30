### Mass Shooter Threat Analytics   ###
### Case Study                      ###
###                                 ###

#### REQUIRED PACKAGES/DIRECTORIES =============================================
pkgs <- c("here","stringr","tm","ggplot2","dplyr","plotly","hrbrthemes","wordcloud")
#install.packages(pkgs) #installs packages listed in pkgs
lapply(pkgs, library, character.only = TRUE) #loads required packages in pkgs

#### PART I: DATA PRE-PROCESSING ===============================================
## STEP 1 - Read in Threats ----
data <- read.csv(here("data", "Threat Texts.csv"),header = F)
dat <- data[,1]
Encoding(dat) <- "UTF-8"
dat <- iconv(dat, "UTF-8", "UTF-8",sub='')


## STEP 2 - Data Cleaning ----
head(dat,n=40) #view semi-structured data

# Construct a corpus, and specify the source to be characters by row (aka character vectors)
corpus <- Corpus(VectorSource(dat))
inspect(corpus[1:10])

# Clean Text
cleanset <- tm_map(corpus, tolower) # Convert all text to lower case
cleanset <- tm_map(cleanset, removePunctuation) # Remove all puncutation
cleanset <- tm_map(cleanset, removeNumbers) # Remove all numbers
cleanset <- tm_map(cleanset, removeWords, stopwords('english'))

inspect(cleanset[1:10])

# Remove Extra Text
cleanset <- tm_map(cleanset, removeWords, c("include", "make", "also", "from",
                                            "can", "cant", "etc", "use", "like",
                                            "one", "must", "will", "doesnt",
                                            "thats", "didnt", "dont", "youve",
                                            "ive", "youre", "theyre", "theyve",
                                            "amp", "theres", "sic"))

# Remove Extra White Space
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:10])

# Term Document Matrix
tdm <- TermDocumentMatrix(cleanset, control = list(WordLength=c(1,Inf)))

# Review the summary of tdm
tdm

# View tdm via a matix
tdm <- as.matrix(tdm)
tdm[1:10, 1:20] 

# Number Of Times A Term Appears
termFreqency <- rowSums(tdm)
termFreqency
head(sort(termFreqency,decreasing = TRUE),n=20)

# Subset fequent words
termFreqency <- subset(termFreqency, termFreqency>20)
termFreqency <- sort(termFreqency, decreasing = TRUE)
termFreqency


#### PART II: DATA ANALYSIS ====================================================
## STEP 3 - Term Frequencies ----
# Figure 2 - Bar Plot
#png("F02-term-freq-barplot.png",width=12,height=6,units="in",res=600)
barplot(termFreqency, las=2)
#Plot labels need to be shifted
sas <- barplot(termFreqency, axes = TRUE, axisnames = FALSE, las=2, 
               main = "Word Usage Frequency", cex.main=1.5, cex.axis=1.5)
text(sas[,1], -1.0, srt = 60, adj = c(1.1,1.1), xpd = TRUE, 
     labels = names(termFreqency),cex=1.5)
dev.off()


## STEP 4 - Wordclouds ----
# - max.words will plot the specified number of words and discard least frequent terms
# - min.freq will discard all terms whose frequency is below the specified value
wordFreq <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(123)

#png("F03-wordcloud.png",width=12,height=6,units="in",res=600)
wordcloud(words = names(wordFreq), freq = wordFreq, random.order = F, max.words = 30, 
          colors = brewer.pal(6, 'Dark2'), scale = c(5,0.5), rot.per = 0.0,)
dev.off()


# STEP 5 - Sentiment Analysis ----
# Load Sentiment Packages
sent <- c("syuzhet","lubridate","ggplot2","scales","reshape2","dplyr")
lapply(sent, library, character.only = TRUE) #loads required packages in pkgs

# Get sentiment scores
sent <- get_nrc_sentiment(dat)
head(sent)
dat[2]
dat[100]

# Exploratory Bar Plot
barplot(colSums(sent), las = 2,  ylab = "Count", 
        main = "NRC Sentiment Scores Tweets")
# tw <- barplot(colSums(sent), axes = TRUE, axisnames = FALSE, las=2,  
#               ylab = "Count", main = "NRC Sentiment Scores for Tweets")
# text(tw[,1], -1.0, srt = 60, adj = c(1.1,1.1), xpd = TRUE, labels = names(sent), cex=1)
dev.off()

# Figure 5: Sentiment Radar Chart
library(radarchart)
library(webshot)
library(htmlwidgets)
radarsent <- data.frame(Sentiment = str_to_title(colnames(sent)),
                        Frequency = colSums(sent))
plt <- chartJSRadar(radarsent, labelSize = 24)
saveWidget(plt, "plt.html")
#webshot::install_phantomjs()
#webshot("plt.html")


# STEP 6 - Term Association ----
#install.packages('BiocManager')
#BiocManager::install("graph")
#BiocManager::install("Rgraphviz")
library(graph)
library(Rgraphviz)

# Find frequent terms
tdm <- TermDocumentMatrix(cleanset, control = list(WordLength=c(1,Inf)))
(freq.terms <- findFreqTerms(tdm, lowfreq = 20))

# Find network of terms
dev.off()
#png("F04.1-tdm-linkage-network-20x.png",width=12,height=6,units="in",res=600)
plot(tdm, term = freq.terms, corThreshold = 0.03, weighting = T,
     main = "Associated Terms - Used Greater than 20 Times",cex.main=1.5,
     attrs=list(node=list(width=20,fontsize=15)))
dev.off()

tdm <- TermDocumentMatrix(cleanset, control = list(WordLength=c(1,Inf)))
(freq.terms <- findFreqTerms(tdm, lowfreq = 40))

#png("F04.2-tdm-linkage-network-40x.png",width=12,height=6,units="in",res=600)
plot(tdm, term = freq.terms, corThreshold = 0.03, weighting = T,
     main = "Associated Terms - Used Greater than 40 Times",cex.main=1.5)
dev.off()


## STEP 7: SENTIMENT VALUES - NUMERICAL ----
sent_new <- get_sentiment(dat)
head(sent_new)

#png("F06-sentiment-histogram.png",width=12,height=6,units="in",res=600)
hist(sent_new, breaks = c(seq(-6,6,length.out=13)), main=NULL, xaxt = "n",
     cex.lab = 1.5, xlim = c(-6,6), ylim = c(0,275), xlab = "Threat Sentiment Score")
axis(side=1, at=seq(-6,6, length.out=13), labels=seq(-6,6,length.out=13),
     cex.axis = 1.5)
dev.off()

sent_tab <- data.frame(threat=dat,
                       sentiment_score=sent_new)

## Valence Shifters - Polarized Words
library(sentimentr)
sentiment_attributes(dat)


#### PART III: DATA ANALYSIS - DENDROGRAM ======================================
library(stringdist)
library(dplyr)
dat2 <- dat
#stringdist(dat2[1], dat2, method = "cosine")

dat2 <- gsub("[^A-Za-z0-9 ]", "", dat2)

n_occur <- data.frame(table(dat2))
names(n_occur) <- c("threat","Freq")
n_occur[n_occur$Freq > 1,]
which(dat2 %in% n_occur$Var1[n_occur$Freq > 1]) #duplicate threats

uniquemodels <- unique(as.character(dat2))
distancemodels <- stringdistmatrix(dat2,dat2,method = "jw")
#rownames(distancemodels) <- uniquemodels
hc <- hclust(as.dist(distancemodels))

# Dendrogram with labels - GitHub Repo
#png("F07-dendrogram-labs.png",width=24,height=8,units="in",res=1200)
plot(hc,cex=0.45, cex.lab=2, cex.axis=2)
rect.hclust(hc,k=20)
dev.off()

hcd <- as.dendrogram(hc)
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.7, col = "blue")

png("F06-dendogram.png",width=24,height=8,units="in",res=600)
plot(hcd,  ylab = "Height", nodePar = nodePar, leaflab = "none",
     edgePar = list(col = 2:3, lwd = 2:1),cex.lab=1.5,cex.axis=1.5)
dev.off()

library("ape")
png("unrooted-plot.png",width=20,height=20,units="in",res=600)
plot(as.phylo(hc), type = "unrooted", cex = 0.4,
     no.margin = TRUE)
dev.off()

library(pvclust)
data(lung) # 916 genes for 73 subjects
set.seed(1234)
result <- pvclust(distancemodels, method.dist="cor", 
                  method.hclust="average", nboot=10)
seplot(result)
png("F06-dendogram.png",width=24,height=6,units="in",res=600)
plot(result,cex=0.45)

#remove duplicate threats and append N col here
cossim_out <- data.frame(string1 = character(),
                         index1 = numeric(),
                         string2 = character(),
                         index2 = numeric(),
                         cossim = numeric(),
                         n = numeric())

jj <- 1
for (i in 1:nrow(n_occur)) {
  string1 <- as.character(n_occur$threat[i]) #string in
  Freq <- as.numeric(n_occur$Freq[i]) #string in
  cs <- stringdist(string1, n_occur$threat, method = "cosine") #cossim string and others
  group <- sort(cs)[2:6] #skip #1 (self)
  #find index of strings in cs
  g_index <- c()
  for (j in 1:length(group)) {
    g_index[j] <- which(cs == group[j])
  }
  for (k in 1:length(g_index)) {
    string2 <- dat2[g_index[k]]
    cossim_s1_s2 <- group[k]
    cossim_out[jj,] <- cbind(string1, i, string2, g_index[k], cossim_s1_s2, Freq)
    jj <- jj + 1
  }
  
}

cleaned <- cossim_out %>% filter(index1 != index2)
which(cleaned$cossim ==0)
cleaned <- cleaned[-1275,]


nodes <- data.frame(id = cleaned$index1,
                    size = (as.numeric(cleaned$n)* 3))
nodes <- nodes %>% distinct(id, size)
length(unique(nodes$id))

links <- data.frame(from = cleaned$index1,
                    to = cleaned$index2,
                    lweight = cleaned$cossim)

library(igraph)
g <- graph_from_data_frame(links, directed=FALSE, vertices=nodes)
#plot of each threat with 5 closest threats
plot(g, vertex.label=NA,layout=layout.fruchterman.reingold,vertex.color = "black")

get.vertex.attribute(g)

sessionInfo() # see GitHub Repo

### END ###
###     ###