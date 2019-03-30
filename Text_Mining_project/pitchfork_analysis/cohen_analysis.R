suppressWarnings(library(stringr))
suppressWarnings(library(httr))
suppressWarnings(library(plyr))
suppressWarnings(library(tm))
suppressWarnings(library(topicmodels))

### Sentiment Analysis
suppressWarnings(library(syuzhet))
suppressWarnings(library(ggplot2))
suppressWarnings(library(scales))
suppressWarnings(library(reshape2))
suppressWarnings(library(dplyr))

str(cohen)
head(cohen)

#data cleansing and exploration
sum(is.na(cohen))
sum(is.na(cohen$review))
na_count <- colSums(is.na(cohen))
na_count <- data.frame(na_count)
na_count

total_en <- data.frame(na.omit(cohen$review))
colnames(total_en) <- c('text')

#Text Preprocessing
sk = total_en$text
TextPreprocessing = lapply(sk, function(x) {
  
  x = gsub('http\\S+\\s*', '', x) ## Remove URLs
  
  x = gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  
  x = gsub("\\d", '', x) ## Remove Controls and special characters
  
  x = gsub('[[:punct:]]', '', x) ## Remove Punctuations
  
  x = gsub("^[[:space:]]*","",x) ## Remove leading whitespaces
  
  x = gsub("[[:space:]]*$","",x) ## Remove trailing whitespaces
  
})

# or as a vector
bd_list = as.vector(TextPreprocessing)
mycorpus <- Corpus(VectorSource(bd_list))
mycorpus = tm_map(mycorpus, content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))

### Transformer all characters to lower case
mycorpus = tm_map(mycorpus, content_transformer(tolower))

### Remove all Punctuation
mycorpus = tm_map(mycorpus, removePunctuation)

### Remove all Numbers
mycorpus = tm_map(mycorpus, removeNumbers)

### Remove Stopwords
mycorpus = tm_map(mycorpus, removeWords, stopwords('english'))

#### transform to Document Term Matrix
skip.dtm = DocumentTermMatrix(mycorpus)

### Topic Model Analysis
rowTotals = apply(skip.dtm, 1, sum)
smtpmodel = skip.dtm[rowTotals>0, ]
smmodel_review = LDA(smtpmodel, 5)
terms <- data.frame(terms(smmodel_review, 40))

### Preparation for Tableau 
## Create dataframe of discovered topics
topic_col=topics(smmodel_review)
topic_col=as.data.frame(topic_col)
clean = total_en
colnames(clean) <- c('text')

### Adding Topic Models to the Total dataframe
new_df = cbind(clean, topic_col)
str(new_tw_df)

## Prepare for sentiment analysis
clean$text <- iconv(clean$text, 'UTF-8', 'ASCII')

# Get Sentiment Analysis
comb_sent = get_nrc_sentiment(clean$text)

#combining Dataframes
total_en_final = cbind(new_tw_df, comb_sent)
write.csv(total_en_final, "cohen_sentiment.csv", row.names = FALSE)