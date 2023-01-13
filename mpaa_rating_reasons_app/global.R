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
full_mpaa <- read_rds("../data/mpaa.rds")

rating_list <- 

# define stop words
mpaa_stop_words <- tribble(
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
full_unigrams <- full_mpaa %>% 
  unnest_tokens(word, reason, drop = FALSE) %>% 
  anti_join(mpaa_stop_words)
# replace the dash in sci-fi
full_unigrams$word <- str_replace(full_unigrams$word, "scifi", "sci-fi")

# create frame for use in wordclouds, workaround needed for the remaining compound terms
wc_unigrams <- full_unigrams %>% 
  mutate(word = str_replace(word, "martialarts", "martial-arts"),
         word = str_replace(word, "druguse", "drug use") 
  )

# for other uses, putting the space back between the compound terms
full_unigrams <- full_unigrams %>% 
  mutate(word = str_replace(word, "martialarts", "martial arts"),
         word = str_replace(word, "druguse", "drug use")
  )

# count words by year and rating
full_yr_rating_counts <- full_unigrams %>% 
  group_by(year, rating) %>% 
  count(word) %>% 
  ungroup()
