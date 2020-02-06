Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust",  "cluster", "igraph", "fpc")
install.packages(Needed, dependencies = TRUE)
install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")
cname <- file.path("C:", "texts")   
cname   
dir(cname)
library(tm)
#Create Corpus after you create the Source
docs <- VCorpus(DirSource(cname))   
summary(docs)   
#Load details of any documents in the corpus
#for example, load the first document in corpus
inspect(docs[1])
docs <- tm_map(docs,removePunctuation) 
for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("\u2028", " ", docs[[j]])}
docs <- tm_map(docs, removeNumbers)  
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, PlainTextDocument)
DocsCopy <- docs
docs <- tm_map(docs, removeWords, stopwords("english"))   
docs <- tm_map(docs, PlainTextDocument)
#Removing particular words
docs <- tm_map(docs, removeWords, c("syllogism", "tautology"))   
#Combining words that should stay together.for example, combine "inner", "city" as "inner-city" so you can analyze them together
for (j in seq(docs))
{docs[[j]] <- gsub("fake news", "fake_news", docs[[j]])
docs[[j]] <- gsub("inner city", "inner-city", docs[[j]])
docs[[j]] <- gsub("politically correct", "politically_correct", docs[[j]])}
docs <- tm_map(docs, PlainTextDocument)
docs_st <- tm_map(docs, stemDocument)   
docs_st <- tm_map(docs_st, PlainTextDocument)
writeLines(as.character(docs_st[1]))
docs_stc <- tm_map(docs_st, stemCompletion, dictionary = DocsCopy, lazy=TRUE)
docs_stc <- tm_map(docs_stc, PlainTextDocument)
writeLines(as.character(docs_stc[1]))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)
dtm <- DocumentTermMatrix(docs)   
dtm 
#Transpose the matrix
tdm <- TermDocumentMatrix(docs)   
tdm  
#Organize terms by Frequency
freq <- colSums(as.matrix(dtm))   
length(freq)
ord <- order(freq) 
m <- as.matrix(dtm)   
dim(m) 
#Explore as csv
write.csv(m, file="DocumentTermMatrix.csv")   
#  Start by removing sparse terms:
dtms <- removeSparseTerms(dtm, 0.2) # This makes a matrix that is 20% empty space, maximum.   
dtms
# most and least frequently occurring words.
freq <- colSums(as.matrix(dtm)) 
#Check out the frequency of frequencies
head(table(freq), 20) 
tail(table(freq), 20) # The ", 20" indicates that we only want the last 20 frequencies
freq <- colSums(as.matrix(dtms))   
freq 
#sort the most frequent words as decreasing order
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14) #select top 14
#create a data frame for next steps
wf <- data.frame(word=names(freq), freq=freq)   
head(wf)
library(ggplot2)#load package ggplot2
#Plot a histogram for words that appear at least 50 times
p <- ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) +geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=45, hjust=1))
p  
findAssocs(dtm, c("country" , "american","jobs","women"), corlimit=0.85) 
# First load the package that makes word clouds in R.    
library(wordcloud)   
dtms <- removeSparseTerms(dtm, 0.15) # Prepare the data (max 15% empty space)   
freq <- colSums(as.matrix(dtm)) # Find word frequencies   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)     
dtms <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space.
library(cluster)   
d <- dist(t(dtms), method="euclidian")   # First calculate distance between words
fit <- hclust(d=d, method="complete")    # Also try: method="ward.D"   
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=6)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=6, border="red") 
dtms <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space.
library(cluster)   
d <- dist(t(dtms), method="euclidian")   
fit2 <- hclust(d=d, method="ward.D")    #  method="ward.D"   
plot.new()
plot(fit2, hang=-1)
groups2 <- cutree(fit2, k=6)   # "k=" defines the number of clusters you are using   
rect.hclust(fit2, k=6, border="red") # draw dendogram with red borders around the       
library(fpc)   
library(cluster)  
dtms <- removeSparseTerms(dtm, 0.15) # Prepare the data (max 15% empty space)   
d <- dist(t(dtms), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)  
