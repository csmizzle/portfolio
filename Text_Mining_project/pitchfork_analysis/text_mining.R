library(tidytext)
library(stringr)
library(tidyverse)
library(tidyr)
library(dplyr)
#setiment analysis 
#reviews_scan<-scan('cleaned_trimmed_review.txt', what=character(),sep='\n')
#reviews <-data_frame(line=1:10183,text=reviews_scan)
#unpacked_reviews <- unnest_tokens(reviews,word,text)

#diff approach 
df <- read.csv("clean_comma.csv")
colnames(df) <- c("genre","review")
df$review <- as.character(df$review)

#clean up 
tidy_reviews <- df %>%
  group_by(genre) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, review)
  
# sentiment for each review 
reviews_sentiment <- tidy_reviews %>%
  inner_join(get_sentiments("bing")) %>%
  count(genre, index = linenumber %/% 2, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

library(ggplot2)

ggplot(reviews_sentiment, aes(index, sentiment, fill = genre)) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~genre, ncol = 2, scales = "free_x")+
  xlab("Review")+
  ylab("Sentiment")+
  ggtitle("Sentiment for All Genres by Review")+
  theme(text = element_text(size=20))

# taking out the unneeded words with %in% (dplyr), ! flips the code
#unpacked_reviews <- df %>%
  #filter(!word %in% stop_words$word)

bing_word_counts <- tidy_reviews %>%
  inner_join(get_sentiments("bing")) %>%
  filter(!word %in% stop_words$word)%>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# sentiment graph
plot <- bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to Sentiment",
       x = "Word") +
  ggtitle("Top Ten Words that drive Sentiment 2008-2018")+
  theme(text = element_text(size=20))+
  coord_flip()
plot

#with new df 
r_words <- df %>%
  unnest_tokens(word, review) %>%
  count(genre, word, sort = TRUE) %>%
  ungroup()

total_words <- r_words %>% 
  group_by(genre) %>% 
  summarize(total = sum(n))

r_words <- left_join(r_words, total_words)

#graph
ggplot(r_words, aes(n/total, fill = genre)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~genre, ncol = 2, scales = "free_y")

#freq
freq_by_rank <- r_words %>% 
  group_by(genre) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

#power law 
rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

#ggplot - Zpif's Law
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = genre)) + 
  geom_abline(intercept = -0.92, slope = -1.04, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) + 
  scale_x_log10() +
  scale_y_log10() +
  ggmain("Frequency Ranks by Genre")+
  ylab("Term Frequency")+
  xlab("Rank")

#tfidf
r_words <- r_words %>%
  bind_tf_idf(word, genre, n)

#high tfidf
r_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

r_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(genre) %>% 
  top_n(10) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = genre)) +
  geom_col(show.legend = TRUE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~genre, ncol = 2, scales = "free") +
  coord_flip()+
  xlab("Tfidf Score")+
  ylab("Genre")+
  theme(text = element_text(size=20))+
  ggtitle("What words are most important using Tfidf transforamtion?")

#bi-gram analysis
review_bigram <- df %>%
  unnest_tokens(bigram, review, token = "ngrams", n = 2)

review_bigram %>% 
  count(bigram, sort = TRUE)

#filter out stop words
bigrams_separated <- review_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

# combine into new df
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

#filtering by word in bigram 
#bigrams_filtered %>%
  #filter(word2 == "street") %>%
  #count(book, word1, sort = TRUE)

#tf_idf bigram
bigram_tf_idf <- bigrams_united %>%
  count(genre, bigram) %>%
  bind_tf_idf(bigram, genre, n) %>%
  arrange(desc(tf_idf))

#plot
bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(genre) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill = genre)) +
  geom_col(show.legend = TRUE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~genre, ncol = 2, scales = "free") +
  coord_flip()+
  xlab("Tfidf Score")+
  ylab("Genre")+
  ggtitle("What words are most important using Tfidf transforamtion?")

#bigram sentiment - not
bigrams_separated %>% 
  filter(word1 == "hard") %>%
  count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")

hard_words <- bigrams_separated %>%
  filter(word1 == "hard") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

#visualize negation words
hard_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = TRUE) +
  xlab("Words proceded by \"hard\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()+
  theme(text = element_text(size=20))+
  ggtitle("How do words following 'hard' differ in sentiment?")

#bigram sentiment - love 
bigrams_separated %>% 
  filter(word1 == "love") %>%
  count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")

very_words <- bigrams_separated %>%
  filter(word1 == "love") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

#visualize negation words
very_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words proceded by \"love\"") +
  ylab("Sentiment score * number of occurrences") +
  theme(text = element_text(size=20))+
  coord_flip()+
  ggtitle("How do words following 'love' differ in sentiment?")

#looking at multiple negation words
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()

negated_words %>%
  group_by(word1)
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~word1, scales = "free_y") +
  xlab("Words preceded by \"very\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()+
  ggtitle("How do words following 'very' differ in sentiment?")