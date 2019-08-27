### ---------------------------------------------------------------------------
### Dependencies
### ---------------------------------------------------------------------------

dependencies <- c(
  "twitteR",
  "ROAuth",
  "SnowballC",
  "data.table",
  "tm",
  "ggplot2",
  "graph",
  "topicmodels",
  "wordcloud",
  "Rgraphviz"
)

dependency_loader <- function(dependencies) {
  for (index in 1:length(dependencies)) {
    dependency_package_name <- dependencies[index]
    if (!require(dependency_package_name, character.only = TRUE)) {
      install.packages(dependency_package_name, dependencies = TRUE)
      library(dependency_package_name)
    }
  }
}

dependency_loader(dependencies)



### ---------------------------------------------------------------------------
### Constants
### ---------------------------------------------------------------------------

TWITTER_USER_NAME <- "nodejs"
TWITTER_MAX_NUMBER_OF_TWEETS <- 3200

TWITTER_CONSUMER_KEY <- "__USE_U_TWITTER_CONSUMER_KEY__"
TWITTER_CONSUMER_SECRET <- "__USE_U_TWITTER_CONSUMER_SECRET__"
TWITTER_ACCESS_TOKEN <- "__USE_U_TWITTER_ACCESS_TOKEN__"
TWITTER_ACCESS_SECRER <- "__USE_U_TWITTER_ACCESS_SECRER__"


TWEETS_FILE_PATH <- paste("./", TWITTER_USER_NAME, ".rds", sep = "")
WORKING_DIRECTORY <- getwd()


# Codes: http://www.loc.gov/standards/iso639-2/php/code_list.php
LANGUAGE_CODE <- "en"

WORDS_TO_ANALISE <- c("app", "team")

CUSTOM_STOP_WORDS <- c("can", "us")
STOP_WORDS <- c(stopwords("english"), stopwords("russian"), CUSTOM_STOP_WORDS)

HEAD_FIRST_TWEETS_COUNT <- 3
NUMBER_OF_TOP_WORDS <- 60

ASSIC_LOWER_CORRELATION_LIMITS = 0.2

FREQ_LOWER_BOUND <- 16
FREQ_MIN_VALUE <- 5

CLUSTERANALYSE_SPARSE_WEIGHT <- 0.95
CLUSTERANALYSE_CLUSTERS_NUMBER <- 5

TOPICMODEL_TOPIC_COUNT <- 10
POPICMODEL_TOPIC_TERMS_COUNT <- 5



### ---------------------------------------------------------------------------
### Scraping
###
### @see https://developer.twitter.com/en/docs/tweets/timelines/api-reference/get-statuses-user_timeline
### @see http://geoffjentry.hexdump.org/twitteR.pdf
### ---------------------------------------------------------------------------

# Make an API Calls to the Twitter, only one time. After tweets saves on file,
# and used from the local store.
if (!file.exists(TWEETS_FILE_PATH)) {
  # Authorize a Twitter resource by OAuth protocol.
  setup_twitter_oauth(
    TWITTER_CONSUMER_KEY,
    TWITTER_CONSUMER_SECRET,
    TWITTER_ACCESS_TOKEN,
    TWITTER_ACCESS_SECRER)
  
  # Scraping tweets.
  tweets <- userTimeline(TWITTER_USER_NAME,
                         n = TWITTER_MAX_NUMBER_OF_TWEETS)
  
  # Save tweets to *.rds file.
  saveRDS(tweets, file = TWEETS_FILE_PATH)
}


### ---------------------------------------------------------------------------
### Loading from local store
### ---------------------------------------------------------------------------

# Check for existing file with tweets.
if (!file.exists(TWEETS_FILE_PATH)) {
  stop(paste("File doesn`t exist in work directory!\n",
             "File path[", TWEETS_FILE_PATH, "],\n",
             "Working directory path [", WORKING_DIRECTORY, "]."))
}


# Load tweets from local file store.
tweets <- readRDS(TWEETS_FILE_PATH)


# Getting real numbers of tweets loaded to memory.
tweets.length <- length(tweets)
print(paste("Number of downloaded tweets: ", tweets.length))


# Show example of tweets to standard out.
print(paste("Fist ", HEAD_FIRST_TWEETS_COUNT, "tweets:"))
for (index in 1:HEAD_FIRST_TWEETS_COUNT) {
  cat(paste("[Tweet#", index, "] ", tweets[[index]]$text, "\n", sep = ""))
}


# Convert TwitteR Lists To Data.Frames
tweets.data_frame <- twListToDF(tweets)



### ---------------------------------------------------------------------------
### Text Cleaning
### ---------------------------------------------------------------------------

# Build a corpus, and specify the source.
# A vector source interprets each element of the vector as a document.
# https://cran.r-project.org/web/packages/tm/tm.pdf#52
documents <- Corpus(VectorSource(tweets.data_frame$text))


# Remove unsupported encode chars(broken UTF-pairs, etc).
documents <- tm_map(documents, function (x) {
  x <- gsub("\n", "", x)
  return(iconv(x, 'UTF-8', 'UTF-8', sub = ""))
})

# Transform characters to lowercase.
documents <- tm_map(documents, content_transformer(tolower))

# Remove punctuation symbols.
documents <- tm_map(documents, removePunctuation)

# Remove numeric values.
documents <- tm_map(documents, removeNumbers)

# Remove URLs links.
# @see: http://www.rdatamining.com/books/rdm/faq/removeurlsfromtext
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
documents <- tm_map(documents, content_transformer(removeURL))

# Remove stopwords.
documents <- tm_map(documents, removeWords, STOP_WORDS)

# Stem words in a text document using Porter's stemming algorithm.
documents <- tm_map(documents, function(x) {
  return(stemDocument(x, language = LANGUAGE_CODE))
})


# Show the first N documents (tweets)
print(paste("Example of tweets after text cleaning: ", 
            HEAD_FIRST_TWEETS_COUNT))
for (index in 1:HEAD_FIRST_TWEETS_COUNT) {
  cat(paste("[Tweet#", index, "] ", documents[[index]], "\n", sep = ""))
}



### ---------------------------------------------------------------------------
### Freq&Assoc Analise.
### ---------------------------------------------------------------------------

# Constructs coerces to a term-document matrix.
termDocMatrix <- TermDocumentMatrix(documents,
                                    control = list(wordLengths = c(1, Inf),
                                                   tolower = FALSE))

# Show term-document matrix.
print(termDocMatrix)

# Association with selected words(terms).
associatedWeights <- findAssocs(termDocMatrix,
                                terms = WORDS_TO_ANALISE,
                                corlimit = ASSIC_LOWER_CORRELATION_LIMITS)

# Show association weights.
print(associatedWeights)


# Creating Term/Freq data frame.
freq.terms <- findFreqTerms(termDocMatrix, lowfreq = FREQ_LOWER_BOUND)
term.freq <- rowSums(as.matrix(termDocMatrix))
term.freq <- subset(term.freq, term.freq >= FREQ_MIN_VALUE)
termFreqDataFrame <- data.frame(term = names(term.freq), freq = term.freq)


# Sort Term/Freq data frame by freq.
termFreqDataFrame <- termFreqDataFrame[order(-termFreqDataFrame$freq),]
topTerm <- head(termFreqDataFrame, NUMBER_OF_TOP_WORDS)


# Build plot with top freq words.
ggplot(topTerm, aes(x = reorder(term, -freq), y = freq)) +
  geom_bar(stat = "identity", fill = "orange") +
  xlab("Terms") + ylab("Count") +
  ggtitle("Histogram: Most freq words in tweets") +
  coord_flip()



### ---------------------------------------------------------------------------
### Word cloud.
### ---------------------------------------------------------------------------

# Calculate the frequency of words and sort it by frequency.
word.freq <- sort(rowSums(as.matrix(termDocMatrix)), decreasing = TRUE)
# Build WordCloud plot.
wordcloud(
  words = names(word.freq),
  freq = word.freq,
  min.freq = FREQ_MIN_VALUE,
  random.order = FALSE)



### ---------------------------------------------------------------------------
### Clustering.
### ---------------------------------------------------------------------------

# Remove sparse terms.
clearedTermDocMatrix <- removeSparseTerms(
  termDocMatrix, 
  sparse = CLUSTERANALYSE_SPARSE_WEIGHT)

# Transform to matrix.
termMatrix <- as.matrix(clearedTermDocMatrix)

# Cluster terms.
distMatrix <- dist(scale(termMatrix))


# Hierarchical cluster analysis on a set of dissimilarities and methods
# for analyzing it.
# @see: http://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html
hCluster <- hclust(distMatrix, method = "ward.D")

# Build cluster dendrogram.
plot(hCluster)
rect.hclust(
  hCluster,
  k = CLUSTERANALYSE_CLUSTERS_NUMBER,
  border = 1:CLUSTERANALYSE_CLUSTERS_NUMBER)

# Perform k-means clustering on a data matrix.
kmeansResult <- kmeans(t(termMatrix), CLUSTERANALYSE_CLUSTERS_NUMBER)
# Round cluster matrix centers, and show it to standart out.
round(kmeansResult$centers, digits = 4)



### ---------------------------------------------------------------------------
### Topic model
### ---------------------------------------------------------------------------

# Constructs document-term matrix.
docTermMatrix <- as.DocumentTermMatrix(termDocMatrix)

# Latent Dirichlet allocation by N-topics.
lda <- LDA(docTermMatrix, k = TOPICMODEL_TOPIC_COUNT)

# Buid terms by every topic.
term <- terms(lda, POPICMODEL_TOPIC_TERMS_COUNT)
term <- apply(term, 2, paste, collapse = ", ")

# Show terms by evry topic.
print(term)

# Named topic list.
topic <- topics(lda, 1)

# Create data frame: Created date / Topic.
topics <- data.frame(date = as.IDate(tweets.data_frame$created), topic)

# Build topic model plot. Usage term on timeline.
qplot(
  date,
  ..count..,
  data = topics,
  geom = "density",
  main = "Timeline usage a terms(words) on the tweets",
  fill = term[topic])
