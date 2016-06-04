# Web Scraping and NLP

# http://www.sumsar.net/blog/2014/12/peter-norvigs-spell-checker-in-two-lines-of-r/
# read a corpus of words, sort by most common
sorted_words <- names(sort(table(strsplit(tolower(paste(readLines("week5/big.txt"), collapse = " ")), "[^a-z]+")), decreasing = TRUE))

# error correction function
correct <- function(word) {
  c(sorted_words[adist(word, sorted_words) <= min(adist(word, sorted_words), 2)], word)[1]
  }

# make a naive bayes classifier
filelist = list.files("datasets/CSDMC2010_SPAM/TRAINING")

spam_df = as.data.frame(matrix(NA, nrow=length(filelist), ncol = 2))
colnames(spam_df) = c("filename", "contents")
spam_df[[1]] = filelist

setwd("datasets/CSDM2010_SPAM/TRAINING")

for (i in seq_len(nrow(filelist))) {
  spam_df[i,2] = paste(readLines(filelist[i]), collapse="\n")
}


# implement http://norvig.com/spell-correct.html myself
# sometime later


# import re, collections

words = function(text) {
  thewords = str.split("[a-z]+")
  return()
}
# def words(text): return re.findall('[a-z]+', text.lower())
# 
# def train(features):
#   # set default value of any key to 1
#   model = collections.defaultdict(lambda: 1)
# for f in features:
#   model[f] += 1
# return model
# 
# NWORDS = train(words(file('big.txt').read()))
# 
# alphabet = 'abcdefghijklmnopqrstuvwxyz'
# 
# def edits1(word):
#   splits     = [(word[:i], word[i:]) for i in range(len(word) + 1)]
# deletes    = [a + b[1:] for a, b in splits if b]
# transposes = [a + b[1] + b[0] + b[2:] for a, b in splits if len(b)>1]
# replaces   = [a + c + b[1:] for a, b in splits for c in alphabet if b]
# inserts    = [a + c + b     for a, b in splits for c in alphabet]
# return set(deletes + transposes + replaces + inserts)
# 
# def known_edits2(word):
#   return set(e2 for e1 in edits1(word) for e2 in edits1(e1) if e2 in NWORDS)
# 
# def known(words): return set(w for w in words if w in NWORDS)
# 
# def correct(word):
#   candidates = known([word]) or known(edits1(word)) or known_edits2(word) or [word]
# return max(candidates, key=NWORDS.get)