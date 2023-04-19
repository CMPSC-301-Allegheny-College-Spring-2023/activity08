# Activity_08

## Name
## Add your name here

# Instructions: Please run and study the below modified code which was inspired from the tutorial at reference: https://content-analysis-with-r.com/1-basics.html. There are some questions at the end of the code to which you are to respond.

#################################

rm(list = ls()) # clear out the variables from memory to make a clean execution of the code.

# If you want to remove all previous plots and clear the console, run the following two lines.
graphics.off() # clear out all plots from previous work.

cat("\014") # clear the console


## Libraries

#quanteda.bundle <- c( "quanteda", "quanteda.textmodels", "quanteda.textstats", "quanteda.textplots" )
#install.packages( quanteda.bundle )
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)

# install the library if it has not been installed locally
if(!require("readtext")) {install.packages("readtext"); library("readtext")}
if(!require("tidyverse")) {install.packages("tidyverse"); library("tidyverse")}
if(!require("RColorBrewer")) {install.packages("RColorBrewer"); library("RColorBrewer")}
theme_set(theme_bw())


# Note: You are choosing the path to the data, not a specific file in this path! To do this, use file.choose() to determine a string to any file in the data directory. Edit the string to remove the filename, leaving only the data directory. Then, use this modified string to point to the data directory for the readtext().


#dataPath <-file.choose()

dataPath  <- "data/*"
#sherlock <- readtext("data/sherlock/novels/[0-9]*.txt") 

sherlock <- readtext(dataPath) 
sherlock$doc_id <- str_sub(sherlock$doc_id, start = 4, end = -5)
my.corpus <- corpus(sherlock)
docvars(my.corpus, "Textno") <- sprintf("%02d", 1:ndoc(my.corpus))
my.corpus

my.corpus.stats <- summary(my.corpus)
my.corpus.stats$Text <- reorder(my.corpus.stats$Text, 1:ndoc(my.corpus), order = T)
my.corpus.stats

ggplot(my.corpus.stats, aes(Text, Tokens, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Tokens per novel") + xlab("") + ylab("")

ggplot(my.corpus.stats, aes(Text, Types, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Types per novel") + xlab("") + ylab("")

ggplot(my.corpus.stats, aes(Text, Sentences, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + ggtitle("Sentences per novel") + xlab("") + ylab("")

ggplot(my.corpus.stats, aes(Tokens, Types, group = 1, label = Textno)) + geom_smooth(method = "lm", se = FALSE) + geom_text(check_overlap = T) + ggtitle("Type-Token-Ratio (TTR) per novel")

#str_sub(my.corpus[1], start = 1, end = 500)
str_sub(my.corpus[2], start = 1, end = 500) # Scandal in Bohemia

my.corpus.sentences <- corpus_reshape(my.corpus, to = "sentences")
my.corpus.sentences[200]

example.sentence <- corpus_sample(my.corpus.sentences, size = 1)
example.sentence[1]

docvars(my.corpus.sentences, "CharacterCount") <- ntoken(my.corpus.sentences)
docvars(my.corpus.sentences, "LongSentence") <- ntoken(my.corpus.sentences) >= 25
my.corpus.sentences_long <- corpus_subset(my.corpus.sentences, LongSentence == TRUE)
my.corpus.sentences_long[1:3]


# Tokenization
my.tokens <- tokens(my.corpus) %>% as.list()
#head(my.tokens$`A Scandal in Bohemia`, 12)
head(my.tokens[2], 12)

my.tokens <- tokens(my.corpus, ngrams = 3) %>% as.list()
#head(my.tokens$`A Scandal in Bohemia`)
head(my.tokens[2])


my.tokens <- tokens(my.corpus)
tokens.retained <- tokens_select(my.tokens, c("holmes", "watson")) %>% as.list()
#head(tokens.retained$`A Scandal in Bohemia`)
head(tokens.retained[2])

tokens.removed <- tokens_remove(my.tokens, c("Sherlock", "in", "is", "the")) %>% as.list()
#head(tokens.removed$`A Scandal in Bohemia`)
head(tokens.removed[2])

my.tokens <- tokens(my.corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>% 
  tokens_remove(c(stopwords("english"), "sherlock", "holmes")) %>% 
  as.list()
#head(my.tokens$`A Scandal in Bohemia`)
head(my.tokens[2])


###

my.dfm <- dfm(my.corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, remove = stopwords("english"))
my.dfm


# fixed code?
#my.dfm <- dfm(tokens( remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE, dfm_remove(stopwords("english"))))
#my.dfm

ndoc(my.dfm)

nfeat(my.dfm)

#head(docnames(my.dfm)) # errors!

head(featnames(my.dfm), 50)

# error? nf is not used
head(my.dfm, n = 12, nf = 10) # Features/texts as a matrix

head(dfm_sort(my.dfm, decreasing = TRUE, margin = "both"), n = 12, nf = 10) 

topfeatures(my.dfm) # basic word frequencies

word.frequencies <- textstat_frequency(my.dfm) # more elaborate frequencies
head(word.frequencies)

head(dfm_sort(my.dfm, decreasing = TRUE, margin = "both"), n = 12, nf = 10) 

dfm_select(my.dfm, pattern = "lov*")

my.dfm.stemmed <- dfm_wordstem(my.dfm)
topfeatures(my.dfm.stemmed)

my.dfm.proportional <- dfm_weight(my.dfm, scheme = "propmax")
convert(my.dfm.proportional, "data.frame")


my.dfm.propmax <- dfm_weight(my.dfm, scheme = "propmax")
topfeatures(my.dfm.propmax[1,])


my.dfm.tfidf <- dfm_tfidf(my.dfm)
topfeatures(my.dfm.tfidf)

my.dfm.trim <- dfm_trim(my.dfm, min_docfreq = 11)
head(my.dfm.trim, n = 12, nf = 10) 

my.dfm.trim <- dfm_trim(my.dfm, min_termfreq = 0.95, termfreq_type = "quantile")
head(my.dfm.trim, n = 12, nf = 10) 

textplot_wordcloud(my.dfm, min_size = 1, max_size = 5, max_words = 100)

textplot_wordcloud(my.dfm[1:4,], color = brewer.pal(4, "Set1"), min_size = 0.2, max_size = 4, max_words = 50, comparison = TRUE)

########################################

#### Questions!!!

#####
# Q 1: In a few lines, describe what you have learned from the tutorial.

# TODO

#####
# Q 2: What does the below code do?
if(!require("tidyverse")) {install.packages("tidyverse"); library("tidyverse")}

# TODO


#####
# Q 3: What are tokens?

# TODO


#####
# Q 4: What are stop words?

# TODO


#####
# Q 5: What do the three plots (titles listed below) tell you?

  # Title: "Tokens per novel"
  # TODO


  # Title: "Types per novel"
  # TODO


  # Title: "Sentences per novel"
  # TODO


  # Title: "Type-Token-Ratio (TTR) per novel"
  # TODO


#####
# Q 6: How many times was "Holmes" name mentioned in each of the adventures?
dfm_select(my.dfm, pattern = "holmes")

#TODO 


#####
# Q 7: How many times was the word, "elementary" used in each of the adventures?
dfm_select(my.dfm, pattern = "elementary")

# TODO


#####
# Q 8: What decides the size of the words in a word-cloud?

# TODO


# (Did you remember to write your name above?)
