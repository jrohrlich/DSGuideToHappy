# HappyAnalysis
# src = https://rit-public.github.io/HappyDB/

library(mallet)
library(syuzhet)

setwd("~~/HappyDB")

# Text manipulation helper functions library by R. C. Alvarado
source('./textman.R')



## Import and clean
happy.df <- read.csv("cleaned_hm.csv", header = TRUE)
happy.df$wid <- as.character(happy.df$wid)
happy.df <- happy.df[order(happy.df$wid), ]

demog.df <- read.csv("demographic.csv")
demog.df$age <- as.integer(demog.df$age)
demog.df$wid <- as.character(demog.df$wid)


# summary
summary(demog.df$age)
typeof(demog.df$age)


# Demographic groups DataFrames
male <- happy.df[which(happy.df$wid %in% unique(demog.df[which(demog.df$gender=="m"), 1])),]
female <- happy.df[which(happy.df$wid %in% unique(demog.df[which(demog.df$gender=="f"), 1])),]
single <- happy.df[which(happy.df$wid %in% unique(demog.df[which(demog.df$marital=="single"), 1])),]
married_parent <- happy.df[which(happy.df$wid %in% unique(demog.df[which(demog.df$marital=="married" & demog.df$parenthood=="y"), 1])),]
married_not_parent <- happy.df[which(happy.df$wid %in% unique(demog.df[which(demog.df$marital=="married" & demog.df$parenthood=="n"), 1])),]
twenties <- happy.df[which(happy.df$wid %in% unique(demog.df[which(demog.df$age>=20 & demog.df$age<30), 1])),]
thirties <- happy.df[which(happy.df$wid %in% unique(demog.df[which(demog.df$age>=30 & demog.df$age<40), 1])),]
forties <- happy.df[which(happy.df$wid %in% unique(demog.df[which(demog.df$age>=40 & demog.df$age<50), 1])),]
fifties <- happy.df[which(happy.df$wid %in% unique(demog.df[which(demog.df$age>=50 & demog.df$age<60), 1])),]
sixties <- happy.df[which(happy.df$wid %in% unique(demog.df[which(demog.df$age>=60 & demog.df$age<70), 1])),]
seventy_plus <- happy.df[which(happy.df$wid %in% unique(demog.df[which(demog.df$age>=70), 1])),]



## Sentiment Boxplots
par(mfrow=c(1,5))

# (repeat for each DF)
text.v <- gsub("[\\S]*http\\S+", " ", male$cleaned_hm) #change DF here
text.v <- gsub("[^A-z'\"]", " ", text.v)
sentiment.values.v <- get_sentiment(text.v, method="syuzhet")
summary(sentiment.values.v)
boxplot(sentiment.values.v, ylim=c(-5, 20), main="Male", ylab="Sentiment of Experiences")



## Topic Modeling

docs.l <- list()
memberlist <- unique(happy.df$wid)

# create list of documents for each contributer to HappyDB
for (id in memberlist) {
  docs.l[[id]] <- as.vector(happy.df[which(happy.df$wid==id), 5])
}

# Estimate appropriate chunk size for model
mems <- c()
for (i in unique(happy.df$wid)) {
  memnum <- nrow(happy.df[which(happy.df$wid==i),])
  print(memnum)
  mems <- c(mems, memnum)
}
length(mems)
hist(mems)
mean(mems)


# Create a corpus of documents
corpus.df = textman_get_doc_corpus(docs.l)

# Generate the model
topic_model = textman_get_topic_model(corpus.df)

# Grab some info from the model
topic_info = textman_get_topic_model_info(topic_model)

# Better list of labels
better_topic_labels = textman_get_topic_labels(topic_info)
print(better_topic_labels)
