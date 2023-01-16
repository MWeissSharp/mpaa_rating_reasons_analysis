library(shiny)
library(shinythemes)

library(tidyverse)
library(ggplot2)
library(tidytext)
library(tm)
library(widyr)
library(wordcloud)
library(wordcloud2)
library(glue)

# read in scraped and cleaned data
full_mpaa <- read_rds("data/mpaa.rds")

# Content page 1 Top words

# Top Words dataframe
# define stop words
mpaa_stop_words2 <- tribble(
  ~word, ~lexicon,
  "rated", "CUSTOM",
  "pg", "CUSTOM",
  "pg13", "CUSTOM",
  "r",  "CUSTOM",
  "nc17", "CUSTOM",
  "for", "CUSTOM",
  "and", "CUSTOM",
  "a", "CUSTOM",
  "an", "CUSTOM",
  "of", "CUSTOM",
  "the", "CUSTOM",
  "including", "CUSTOM",
  "involving", "CUSTOM",
  "some", "CUSTOM",
  "content", "CUSTOM",
  "brief", "CUSTOM",
  "strong", "CUSTOM",
  "thematic", "CUSTOM",
  "images", "CUSTOM",
  "material", "CUSTOM",
  "elements", "CUSTOM",
  "mild", "CUSTOM",
  "throughout", "CUSTOM",
  "references", "CUSTOM",
  "scene", "CUSTOM",
  "scenes", "CUSTOM",
  "sequences", "CUSTOM",
  "related", "CUSTOM",
  "dialogue", "CUSTOM",
  "partial", "CUSTOM",
  "pervasive", "CUSTOM",
  "throughout", "CUSTOM"
)

# unnest with tidytext
top_unigrams <- full_mpaa %>% 
  unnest_tokens(word, reason, drop = FALSE) %>% 
  anti_join(mpaa_stop_words2)

# putting the space or dash back between the compound terms, combine words w/ similar root
top_unigrams <- top_unigrams %>% 
  mutate(word = str_replace(word, "martialarts", "martial arts"),
         word = str_replace(word, "druguse", "drug use"),
         word = str_replace(word, "scifi", "sci-fi"),
         word = str_replace(word, "sex$", "sex*"),
         word = str_replace(word, "sexual$", "sex*"),
         word = str_replace(word, "sexually$", "sex*"),
         word = str_replace(word, "^sexuality", "sex*"),
         word = str_replace(word, "sexy$", "sex*"),
         word = str_replace(word, "drug$", "drug(s)/drug use"),
         word = str_replace(word, "drugs$", "drug(s)/drug use"),
         word = str_replace(word, "^drug use", "drug(s)/drug use"),
         word = str_replace(word, "violence$", "violence/violent"),
         word = str_replace(word, "^violent", "violence/violent")
  )
 

# count words by year and rating
top_yr_rating_counts <- top_unigrams %>% 
  group_by(year, rating) %>% 
  count(word) %>% 
  ungroup()



# Content page 2 Wordclouds

# Wordcloud dataframe
# define stop words
mpaa_stop_words1 <- tribble(
  ~word, ~lexicon,
  "rated", "CUSTOM",
  "pg", "CUSTOM",
  "pg13", "CUSTOM",
  "r",  "CUSTOM",
  "nc17", "CUSTOM",
  "for", "CUSTOM",
  "and", "CUSTOM",
  "a", "CUSTOM",
  "an", "CUSTOM",
  "of", "CUSTOM",
  "the", "CUSTOM",
  "including", "CUSTOM",
  "involving", "CUSTOM"
)

# unnest with tidytext
wc_unigrams <- full_mpaa %>% 
  unnest_tokens(word, reason) %>% 
  anti_join(mpaa_stop_words1)


# for use in wordclouds, workaround needed for drug use and martial arts to keep them together
wc_unigrams <- wc_unigrams %>% 
  mutate(word = str_replace(word, "martialarts", "martial-arts"),
         word = str_replace(word, "druguse", "drug-use"),
         word = str_replace(word, "scifi", "sci-fi")
  )

# Define function to create wordcloud matrix
create_matrix <- function(rat1, rat2) {
  # filter for the ratings of interest
  rating1_df<- wc_unigrams %>% 
    filter(rating == rat1)
  
  rating2_df<- wc_unigrams %>% 
    filter(rating == rat2)
  
  # pull out the words for those ratings
  rating1_words <- paste(rating1_df$word,
                         collapse = " ")
  
  rating2_words <- paste(rating2_df$word,
                         collapse = " ")
  
  #create a TDM
  combined_tdm <- TermDocumentMatrix(VCorpus(VectorSource(c(rating1_words, rating2_words))))
  
  #name columns based on selected ratings
  #colnames(combined_tdm) <- c(rat1, rat2)
  
  #convert to matrix
  combined_m <- as.matrix(combined_tdm)
  
}
