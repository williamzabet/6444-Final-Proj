library(tm) # tm library
library(wordcloud) 
library(quanteda) 
library(syuzhet) 
library(textreuse)
library(wordnet)
library(zipfR)
library(corpustools)

#set directory (folder containing 28 documents that is each chapter within the book)
setwd("/Users/williamzabet/proj3/chapters")
#getting the directory
getwd()

#a.) trying out the functions from lecture 8

#initializing a VCorpus of the diretory
Tarzan<-VCorpus(DirSource(".", ignore.case = TRUE, mode = "text"))

#inspect function gets the metadata and # of chars w/in each document
inspect(Tarzan)

#str function outputs the 7 pieces of metadata
str(Tarzan)

#Document term matrix
TarzanDTM <- DocumentTermMatrix(Tarzan)
TarzanDTM
inspect(TarzanDTM)
str(TarzanDTM)
a#Term document matrix
TarzanTDM <- TermDocumentMatrix(Tarzan)
#TarzanTDM
inspect(TarzanTDM)
str(TarzanTDM)

#Term Frequency
test1 <- Tarzan[[1]]
test1TF <- termFreq(test1)
test1TF
str(test1TF)

#dataframe of test1
test1DF <- as.data.frame(test1TF)
test1DF

wa#corpus cleansing
TarzanLow <- tm_map(Tarzan, content_transformer(tolower))
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
TarzanCL <- tm_map(TarzanLow, content_transformer(removeNumPunct))
TarzanCL
TarzanCLDTM <- DocumentTermMatrix(TarzanCL)
TarzanCLDTM
inspect(TarzanCLDTM)
myStopWords <- c(tm::stopwords('english'))
myStopWords
TarzanStop <- tm::tm_map(TarzanCL, tm::removeWords, myStopWords)
TarzanStopDTM <- tm::DocumentTermMatrix(TarzanStop)
inspect(TarzanStopDTM)

# Finding Freq words
TarzanStopTDM <- TermDocumentMatrix(TarzanStop)
freqTerms <- tm::findFreqTerms(TarzanStopTDM, lowfreq = 4)
freqTermsgirlsAssoc <- tm::findAssocs(TarzanStopTDM, "girls", 0.3)
girlsAssoc

#Term Frequency
TarzanStopTF <- rowSums(as.matrix(TarzanStopTDM))
TarzanStopTF
TarzanStopFsub <- subset(TarzanStopTF, TarzanStopTF >= 6)
length(TarzanStopFsub)

#Removing Sparse Terms
tm::inspect(TarzanStopTDM)
TarzanSparse <- tm::removeSparseTerms(TarzanStopTDM, sparse = 0.6)
tm::inspect(TarzanSparse)

#Finding Informative Words
test1Stop <- TarzanStop[[1]]
test1Stop$content

#Word Cloud
TarzanStopWF <- sort(TarzanStopTF, decreasing = T)
TarzanStopWF

#Frequency Analysis
TarzanStopDTM
freq <- colSums(as.matrix(TarzanStopDTM))
freq
TarzanOrder <- order(freq, decreasing = TRUE)
freq[head(TarzanOrder)]
freq[tail(TarzanOrder)]

#Text Analysis: Tokenization
test1$content
test1Tokens <- tokens(test1$content)
test1Tokens
test1DFM <- quanteda::dfm(test1Tokens, tolower = TRUE)
str(test1DFM)

T#Sentiment Analysis
test1SentenceDF <- as.data.frame(test1$content)
test1SentenceDF
test1Sentiment <- get_nrc_sentiment(test1$content)
test1Sentiment
test1SentimentSummary <- rowSums(test1Sentiment)
test1SentimentSummary
test1SentimentColumns <- colSums(test1Sentiment)
test1SentimentColumns


#Text Weighting
test1DFMFreq <- docfreq(test1DFM)
test1DFMFreq
test1Weights <- dfm_weight(test1DFM)
test1Weights
test1TfIdf <- dfm_tfidf(test1DFM, scheme_tf = "count", scheme_df = "inverse")
test1TfIdf
test1TfIdf@i

#b.) finding the 10 longest words and 10 longest sentences in the document, and preparing a table of this data as well as showing these items.

# 10 longest words
TarzanWords <- TarzanDTM$dimnames$Terms
TarzanWords <- as.array(TarzanWords)
TarzanLongestWords <- TarzanWords[order(nchar(TarzanWords), decreasing = TRUE)][1:10]
TarzanLongestWords

# 10 longest sentences/phrases (by number of characters)
TarzanSentences <- c()
#looping through the content of each text document
for (i in 1:28) {
  #finding the 10 longest sentences/phrases of each text document
  intermediate <- Tarzan[[i]]$content[order(nchar(Tarzan[[i]]$content), decreasing = TRUE)][1:10]
  #appending it into an array
  TarzanSentences <- append(TarzanSentences, intermediate)
}
#finding the 10 longest sentences/phrases in the entire directory (by number of characters)
TarzanSentences <- TarzanSentences[order(nchar(TarzanSentences), decreasing = TRUE)][1:10]
TarzanSentences

#displaying data on a data frame.
bDF <- data.frame("Longest_Words" = TarzanLongestWords, "Word_Length" = nchar(TarzanLongestWords), "Longest_Sentences" = TarzanSentences,
                  "Sentence_Length" = nchar(TarzanSentences))


#c.) filtering out stop words, removing numbers and punctuation, removing sparse words, and displaying the dendrogram.

TarzanFiltered <- TarzanSparse #TarzanSparse filtered out the stop words, numbers/punctuation, and sparse words!

TarzanFilteredDF <- as.data.frame(TarzanFiltered[[1]])
TarzanDist <- dist(TarzanFilteredDF)
TarzanDG <- hclust(TarzanDist, method = "ward.D2")
str(TarzanDG)
plot(TarzanDG)

#d.) using  WordNet to mark the parts of speech for the 10 longest sentences found in part b for nouns and verbs having a length of 5 or greater

setDict("/Users/williamzabet/proj3/WordNet-3.0/dict")
Sys.setenv(WNHOME = "/Users/williamzabet/proj3/WordNet-3.0/dict")

# written function to return the nouns and verbs of each sentence
nounsANDverbs = function() {
  
  #tokenizes each word from each sentence
  tenTokens <- quanteda::tokens(TarzanSentences[1:10])
  #selects the tokens that have at least 5 characters
  tenTokens <- tokens_select(tenTokens, min_nchar = 5)
  
  #empty list for verbs and nouns
  nouns <- c()
  verbs <- c()
  
  # loops through each of the sentences
  for (i in 1:10) {
    
    # intermediate noun and verb lists
    nounEmpty <- c()
    verbEmpty <- c()
    
    # loops through each word within the sentence
    for (word in as.array(tenTokens[[i]])) {
      
      # uses the wordnet package to extract noun synonyms for the word
      nounFilter <- getTermFilter("ExactMatchFilter", word, TRUE)
      nounTerms <- getIndexTerms("NOUN", 1, nounFilter)
      nounRes <- try(getSynonyms(nounTerms[[1]]), silent = TRUE)
      # if there is no error (there is a match for the word as a noun within the dictionary), then the word gets added to the intermediate noun list
      if (!isTRUE(grepl("Error", nounRes, fixed = TRUE))) {
        nounEmpty <- append(nounEmpty, word)
      }
      
      # uses the wordnet package to extract verb synonyms for the word
      verbFilter <- getTermFilter("ExactMatchFilter", word, TRUE)
      verbTerms <- getIndexTerms("VERB", 1, verbFilter)
      verbRes <- try(getSynonyms(verbTerms[[1]]), silent = TRUE)
      # if there is no error (there is a match for the word as a verb within the dictionary), then the word gets added to the intermediate verb list
      if (!isTRUE(grepl("Error", verbRes, fixed = TRUE))) {
        verbEmpty <- append(verbEmpty, word)
      }
    }
    # words matching in each sentence get addded to each index of the noun and verbs list
    nouns[[i]] <- nounEmpty
    verbs[[i]] <- verbEmpty
  }
  
  # prints out the nouns in each sentence
  print("Nouns in Each Sentence:")
  for (i in nouns) {
    print(i)
  }
  
  # spaces out the output
  print("")
  print("")
  print("")
  
  # prints out the verbs in each sentence
  print("Verbs in Each Sentence:")
  for (i in verbs[]) {
    print(i)
  }
  
}

nounsANDverbs()

#e. analyzing the word frequency using functions from zipfR

#Further filtering down  for analytical purposes
TarzanFilteredTF <- rowSums(as.matrix(TarzanFiltered))
TarzanFilteredFsub <- subset(TarzanFilteredTF, TarzanFilteredTF >= 6)
TarzanFilteredFsub

#using zipFr tools:
#frequency spectrum
TarzanFilteredSPC <- zipfR::spc(TarzanFilteredFsub)
#Vocabulary Growth curves
TarzanFilteredVGC <- zipfR::ItaRi.emp.vgc(TarzanFilteredFsub)
#computing an sr
TarzanFilteredFsub.ItaRi.bin.vgc <- vgc.interp(TarzanFilteredSPC, N(TarzanFilteredVGC), m.max = 1)
#fZM model by fitting the expected frequency spectrum of the model to the observed spectrum for ri-.
TarzanFilteredFZM <- lnre("fzm", TarzanFilteredSPC, exact = FALSE)
#the spectrum of expected frequencies predicted by the fZM model at the sample size we used to compute the model
TarzanFiltered.fzm.spc <- lnre.spc(TarzanFilteredFZM, N(TarzanFilteredFZM))
#The  model used to obtain estimates of V and Vm (the spectrum elements) at arbitrary sample sizes
TarzanFiltered.fzm.vgc <- lnre.vgc(TarzanFilteredFZM, (1:100) * 28e3)

#function that returns the heads of the frequency spectrum, vocabulary growth curves, and interpolated growth curve
heads <- function() {
  head(TarzanFilteredSPC)
  head(TarzanFilteredVGC)
  head(TarzanFilteredFsub.ItaRi.bin.vgc)
}
heads()

# function that gets the summaries of the SPC, VGC, and FZM
summaries <- function() {
  summary(TarzanFilteredSPC)
  summary(TarzanFilteredVGC)
  summary(TarzanFilteredFZM)
}
summaries()

# function returns various plots
plots <- function() {
  plot(TarzanFilteredSPC)
  plot(TarzanFilteredSPC, log = "x")
  with(TarzanFilteredSPC, plot(m, Vm, main = "Frequency Spectrum"))
  plot(TarzanFilteredVGC, add.m = 1)
  plot(TarzanFilteredVGC, TarzanFilteredFsub.ItaRi.bin.vgc,
       legend = c("observed", "interpolated"))
  plot(TarzanFilteredSPC, TarzanFiltered.fzm.spc, legend = c("observed", "fZM"))
  plot(TarzanFilteredVGC, TarzanFiltered.fzm.vgc, N0=N(TarzanFilteredFZM),
       legend = c("observed", "fZM"))
}
plots()


#f.) generating bigrams and trigrams for all words whose length is greater than 6 characters in the 10 longest sentences

#function that takes in x as the number of ngrams you want to generate for each sentence
bigramORtrigram <- function(x) {
  
  #intermediary tokenizer that tokenizes each of the 10 sentences and selects the ones that have at least length of 6.
  interTokens <- quanteda::tokens(TarzanSentences[1:10])
  interTokens <- tokens_select(interTokens, min_nchar = 6)

  #loops through each of the sentences
  for (i in 1:10) {
    #for each sentence, prints out the ngram desired for the words w/in the sentence
    print(ngrams(interTokens[[i]], x))
  }
}

bigramORtrigram(2)
bigramORtrigram(3)

#g.) 

# W
findWordinPhrase <- function(x) {
  #counter for num of occurences
  counter <- 0
  # loops through each chapter
  for (txt in 1:28) {
    # loops through each phrase w/in the chapter
    for (i in TarzanStop[[txt]]$content) {
      #if detected:
      if (isTRUE(stringi::stri_detect_regex(i, x))) {
        #counter gets incremented once
        counter <- counter + 1
        #prints out the sentence
        print(i)}}}
  
  #prints out the number of sentences that contains the word
  print(paste0("Number of sentences with the word '",x,"': ", counter))
}

findWordinPhrase("tarzan")
findWordinPhrase("tarzans")

# function that returns a wordcloud of the inputted word along with words that are contained in the phrase
TarzanWordCloud <- function(x) {
  # empty intermediate list to get the list of sentences that contain the inputted word
  interTarzan <- c()
  
  # loops through each document, if the word is found w/ stringi, then the entire phrase gets appended to a list
  for (txt in 1:28) {
    # loops through each phrase w/in the chapter
    for (i in TarzanStop[[txt]]$content) {
      #if detected:
      if (isTRUE(stringi::stri_detect_regex(i, x))) {
        interTarzan <- append(interTarzan, i)}}}
  
  # tokenizes each word within the list
  interTarzan <- tokenize_word(interTarzan)
  
  # places each word into a new list, and removes white spaces
  wcWords <- c()
  for (i in interTarzan) {
    for (z in i) {
      if (!grepl(z, " ", fixed = TRUE)) {
        wcWords <- append(wcWords, z)}}}
  
  # term frequency of each word
  wcTF <- termFreq(wcWords)
  #names of each word
  wcNames <- names(wcTF)
  # color pallete for word cloud
  pal <- brewer.pal(9, "BuGn")
  # initializing word cloud on the words, the frequency, and the color pallete
  wordcloud(wcNames, wcTF, colors = pal[-(1:4)])
}

TarzanWordCloud("tarzan")
TarzanWordCloud("jane")

# function to plot the # of tokens for each chapter
plotTokens <- function(x) {
  
  TarzanTK <- c()
  chapterNum <- c()
  chap <- 0
  TKcounter <- 0
  # loop through each chapter
  for (chapters in 1:28) {
    TKcounter <- 0
    chap <- chap + 1
    # using quanteda: tokenizes each word and removes white space, counts each token
    for (s in tokenize_word(x[[chapters]]$content)) {
      for (i in s) {
        if (!grepl(i, " ", fixed = TRUE)) {
          TKcounter <- TKcounter + 1
        }
      }
    }
    # appends chapter number and tokens per chapter to the lists
    chapterNum <- append(chapterNum, chap)
    TarzanTK <- append(TarzanTK, TKcounter)
  }
  
  # displays barplot
  barplot(TarzanTK, main="Tokens in Each Chapter", names.arg= chapterNum, las=3, xlab = "Chapter #", ylab = "tokens")
  
  # prints out the number of tokens in each sentence
  loopCounter <- 0
  for (i in TarzanTK) {
    loopCounter <- loopCounter + 1
    print(paste("Tokens in sentence",loopCounter,":", i))
  }
}

plotTokens(Tarzan)
plotTokens(TarzanStop)


# wasnt working when put inside a function so no function used here
# plotting a semantic network based on co-occurence for each chapter
tc <- create_tcorpus(TarzanStop[[1]]$content) 
tc$preprocess(min_freq = 1, remove_stopwords = F, remove_numbers = F)
g <- semnet_window(tc, 'feature')
gb <- backbone_filter(g, alpha = 0.03, max_vertices = 100)
plot_semnet(gb)



