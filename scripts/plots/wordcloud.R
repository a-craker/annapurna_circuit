library(tm)
library(wordcloud)
library(SnowballC)
library(ggplot2)

mooncloud <- Corpus(DirSource("data/journal/"))

mooncloud <- tm_map(mooncloud, stripWhitespace)
# Convert to lowercase
mooncloud <- tm_map(mooncloud, tolower)
# Remove conjunctions etc.
mooncloud <- tm_map(mooncloud, removeWords, stopwords("english"))
# Remove suffixes to the common 'stem'
mooncloud <- tm_map(mooncloud, stemDocument)
# Remove commas etc.
mooncloud <- tm_map(mooncloud, removePunctuation)

#(optional) arguments of 'tm' are converting the document to something other than text, to avoid, run this line
mooncloud <- tm_map(mooncloud, PlainTextDocument)


# -------------------------------------------------------------------------

wordcloud(mooncloud
          , scale=c(5,0.5)     # Set min and max scale
          , max.words=220  # Set top n words
          , random.order=FALSE # Words in decreasing freq
          , rot.per=0.25       # % of vertical words
          , use.r.layout=FALSE # Use C++ collision detection
          , colors=brewer.pal(8, "Dark2"))


par(mar = c(0, 0, 0, 0))  # remove plot margins

wordcloud(
  mooncloud,
  scale        = c(5, 0.5),
  max.words    = 220,
  random.order = FALSE,
  rot.per      = 0.25,
  use.r.layout = FALSE,
  colors       = brewer.pal(8, "Dark2")
)
