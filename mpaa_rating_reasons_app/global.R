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

# rating list
rating_list <- list("PG", "PG-13", "R", "NC-17")

# Overview page
# define color palette for ratings
col_pal <- c("G" = "#1A9850", 
             "PG" = "#D9EF8B", 
             "PG-13" = "#FEE08B", 
             "R" = "#F46D43", 
             "NC-17" = "#A50026")

# mean reason length across all reasons
overall_mean_len <- full_mpaa %>% 
  filter(rating != "G") %>%
  mutate(overall = mean(str_count(reason,"\\W+")))

# dataframe to account for years there were no NC-17 movies
all_yr_rating <- expand_grid(rating = c("G", "PG", "PG-13", "R", "NC-17"), year = 1992:2022)

# dataframe of mean reason lengths
mean_rea_len <- full_mpaa %>% 
  group_by(year, rating) %>% 
  summarize(mean_len = mean(str_count(reason,"\\W+"))) %>% 
  ungroup() %>% 
  full_join(all_yr_rating) %>% 
  filter(rating != "G")
  
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
  "reference", "CUSTOM",
  "references", "CUSTOM",
  "scene", "CUSTOM",
  "scenes", "CUSTOM",
  "sequences", "CUSTOM",
  "situations", "CUSTOM",
  "related", "CUSTOM",
  "dialogue", "CUSTOM",
  "partial", "CUSTOM",
  "pervasive", "CUSTOM",
  "throughout", "CUSTOM",
  "moments", "CUSTOM",
  "theme", "CUSTOM"
)

# unnest with tidytext
top_unigrams <- full_mpaa %>% 
  unnest_tokens(word, reason, drop = FALSE) %>% 
  anti_join(mpaa_stop_words2)

# putting the space or dash back between the compound terms
top_unigrams <- top_unigrams %>% 
  mutate(word = str_replace(word, "martialarts", "martial arts"),
         word = str_replace(word, "druguse", "drug use"),
         word = str_replace(word, "drugabuse", "drugabuse"),
         word = str_replace(word, "substanceabuse", "substance abuse"),
         word = str_replace(word, "substanceuse", "substance use"),
         word = str_replace(word, "scifi", "sci-fi")
         ) %>% 
          # combine words w/ similar root
  mutate(word = str_replace(word, "sex$", "sex*"),
               word = str_replace(word, "sexual$", "sex*"),
               word = str_replace(word, "sexually$", "sex*"),
               word = str_replace(word, "^sexuality", "sex*"),
               word = str_replace(word, "sexy$", "sex*"),
               word = str_replace(word, "drug$", "drugs/substances**"),
               word = str_replace(word, "drugs$", "drugs/substances**"),
               word = str_replace(word, "^substance", "drugs/substances**"),
               word = str_replace(word, "^drug use", "drugs/substances**"),
               word = str_replace(word, "^drug abuse", "drugs/substances**"),
               word = str_replace(word, "^substance use", "drugs/substances**"),
               word = str_replace(word, "^substance abuse", "drugs/substances**"),
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
create_matrix <- function(rat1, rat2, year1, year2) {
  # filter for the ratings of interest
  rating1_df<- wc_unigrams %>% 
    filter(rating == rat1,
           year >= year1,
           year <= year2)
  
  rating2_df<- wc_unigrams %>% 
    filter(rating == rat2,
           year >= year1,
           year <= year2)
  
  # pull out the words for those ratings
  rating1_words <- paste(rating1_df$word,
                         collapse = " ")
  
  rating2_words <- paste(rating2_df$word,
                         collapse = " ")
  
  #create a TDM
  combined_tdm <- TermDocumentMatrix(VCorpus(VectorSource(c(rating1_words, rating2_words))))
  
  #name columns based on selected ratings
  colnames(combined_tdm) <- c(rat1, rat2)
  
  #convert to matrix
  combined_m <- as.matrix(combined_tdm)
  
}
