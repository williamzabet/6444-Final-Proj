# 6444-Final-Proj
Code written by: William B. Zabet

Results/Assessment written by: William B. Zabet, and Mariamawit Lisanu

#### Listing of Functions:
1. nounsANDverbs(): Prints out the nouns and verbs within the 10 longest sentences. The
function starts by looping through each of the 10 sentences. By using wordnet, the
function then gets “noun” and “verb” synonyms for each word. If a list of words is
returned, then the word is considered a noun or verb depending on which POS was
selected. Afterwards, the words are printed out in two separate lists (nouns and verbs).
2. heads(): returns the head() function for the frequency spectrum, vocabulary growth
curves, and the interpolated growth curve from the ri- spectrum.
3. summaries(): returns the summary() function for the frequency spectrum, vocabulary
growth curves, and the fZM model.
4. plots(): returns 3 different plots for the frequency spectrum, the vocabulary growth
curves, the vocabulary growth curves against the sr, the frequency spectrum against the
spectrum of expected frequencies predicted by the fZM model at the sample size we used
to compute the model, and the vocabulary growth curves against the model used to obtain
estimates of V and Vm (the spectrum elements) at arbitrary sample sizes.
5. bigramORtrigram(): returns the n grams for words within each sentence. For example,
bigramORtrigram(2), returns bigrams and bigramORtrigram(3) returns trigrams.
6. findWordinPhrase(): This function uses the stringi package to detect inputted word w/in
the phrases of the document. It loops through each chapter within the book (without stop
words), and it prints out phrases that contain each sentence along with the total number of
phrases detected.
7. TarzanWordCloud(): takes a string and generates a word cloud of all the words that are
within phrases that contain the inputted string. It returns all phrases within the book
(without stop words), tokenizes each phrase, and then plots their frequencies on a word
cloud.
8. plotTokens(): plots the number of tokens within each chapter. Examples:
plotTokens(entire book), plotTokens(book without stop words).
