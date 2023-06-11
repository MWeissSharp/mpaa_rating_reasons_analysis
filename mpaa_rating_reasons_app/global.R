library(shiny)
library(shinythemes)
library(DT)

library(tidyverse)
library(ggplot2)
library(tidytext)
library(tm)
library(widyr)
library(wordcloud)
library(glue)
library("scales")

# read in scraped and cleaned data
full_mpaa <- read_rds("data/mpaa.rds")

# rating list
rating_list <- list("PG", "PG-13", "R", "NC-17")

# Overview page
# Tokenize all words
all_tokens <- full_mpaa %>% 
  unnest_tokens(word, reason)

# Create mini stop words list for determining top word and count
mini_stop <- tribble(
  ~word, ~lexicon,
  "rated", "CUSTOM",
  "and", "CUSTOM",
  "for", "CUSTOM")

# define color palette for ratings
col_pal <- c("G" = "#1A9850", 
             "PG" = "darkolivegreen3", 
             "PG-13" = "darkgoldenrod1", 
             "R" = "#F46D43", 
             "NC-17" = "#A50026")

# Get number of movies for each rating/year
movies_yr_rating <- full_mpaa %>% 
  group_by(year, rating) %>% 
  count(rating) %>% 
  rename(total_movies = n) %>% 
  ungroup()

# mean reason length across all reasons
overall_mean_len <- full_mpaa %>% 
  filter(rating != "G") %>%
  summarize(overall = mean(reason_len))

# dataframe to account for years there were no NC-17 movies
all_yr_rating <- expand_grid(rating = c("G", "PG", "PG-13", "R", "NC-17"), year = 1992:2022)

# dataframe of mean reason lengths
mean_rea_len <- full_mpaa %>% 
  group_by(year, rating) %>% 
  summarize(mean_len = mean(reason_len)) %>% 
  ungroup() %>% 
  full_join(all_yr_rating) %>% 
  filter(rating != "G")

# short reasons
short_reasons <- full_mpaa %>% 
  filter(reason_len > 2,
         reason_len <= 4) %>% 
  group_by(year, rating) %>% 
  count(rating) %>% 
  ungroup()

# Pull out longest rating reason movie info
long_reason <- full_mpaa %>% 
  slice_max(reason_len, n=3) %>% 
  mutate(reason = str_replace(reason, "martialarts", "martial arts"),
         reason = str_replace(reason, "scifi", "sci-fi"),
         reason = str_replace(reason, "druguse", "drug use"),
         reason = str_replace(reason, "oldfashioned", "old fashioned"),
         reason = str_replace(reason, "risktaking", "risk taking"),
         reason = str_replace(reason, "sexed", "sex-ed"),
         reason = str_replace(reason, "PG13", "PG-13"),
         reason = str_replace(reason, "NC17", "NC-17")) %>% 
  select(Title = title, 
         Rating = rating, 
         `Year Rated` = year, 
         `Rating Reason` = reason,
         `Total Words` = reason_len)

# Get dataframe of the short reasons
shorties <- full_mpaa%>% 
  filter(rating != "G",
         reason_len > 0,
         reason_len <= 4)

# Content page 1 Top words
# Top Words dataframe

# define stop words
mpaa_stop_words2 <- tribble(
  ~word, ~lexicon,
  "rated", "CUSTOM",
  "g", "CUSTOM",
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
  mutate(reason = str_replace(reason, "martialarts", "martial arts"),
         reason = str_replace(reason, "druguse", "drug use")) %>% 
  unnest_tokens(word, reason, drop = FALSE) %>% 
  anti_join(mpaa_stop_words2)

# putting the space or dash back between the compound terms
top_unigrams <- top_unigrams %>% 
  mutate(word = str_replace(word, "scifi", "sci-fi"),
         word = str_replace(word, "sexed", "sex-ed"),
         word = str_replace(word, "oldfashioned", "old-fashioned"),
         word = str_replace(word, "risktaking", "risk-taking")) %>% 
  # combine words w/ similar root
  mutate(word = str_replace(word, "sex$", "sex*"),
         word = str_replace(word, "sexual$", "sex*"),
         word = str_replace(word, "sexually$", "sex*"),
         word = str_replace(word, "sexuality", "sex*"),
         word = str_replace(word, "drug$", "drug(s)/substance"),
         word = str_replace(word, "drugs$", "drug(s)/substance"),
         word = str_replace(word, "^substance", "drug(s)/substance"),
         word = str_replace(word, "violence$", "violence/violent"),
         word = str_replace(word, "^violent", "violence/violent")) %>% 
  # fix reasons for instances where reasons will be displayed
  mutate(reason = str_replace(reason, "martialarts", "martial arts"),
         reason = str_replace(reason, "scifi", "sci-fi"),
         reason = str_replace(reason, "druguse", "drug use"),
         reason = str_replace(reason, "oldfashioned", "old fashioned"),
         reason = str_replace(reason, "risktaking", "risk taking"),
         reason = str_replace(reason, "sexed", "sex-ed"),
         reason = str_replace(reason, "PG13", "PG-13"),
         reason = str_replace(reason, "NC17", "NC-17")
  )

# count words by year and rating
word_yr_rating_counts <- top_unigrams %>% 
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


# for use in wordclouds, workaround needed for a few terms to keep them together
wc_unigrams <- wc_unigrams %>% 
  mutate(word = str_replace(word, "martialarts", "martial-arts"),
         word = str_replace(word, "druguse", "drug-use"),
         word = str_replace(word, "oldfashioned", "old-fashioned"),
         word = str_replace(word, "risktaking", "risk-taking"),
         word = str_replace(word, "scifi", "sci-fi"),
         word = str_replace(word, "sexed", "sex-ed")
         
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

# Content page 3, modifiers

# Create a version of full_mpaa that has corrected the reason words
full_mpaa_reasons <- full_mpaa %>% 
  mutate(reason = str_replace(reason, "martialarts", "martial arts"),
         reason = str_replace(reason, "scifi", "sci-fi"),
         reason = str_replace(reason, "druguse", "drug use"),
         reason = str_replace(reason, "oldfashioned", "old fashioned"),
         reason = str_replace(reason, "risktaking", "risk taking"),
         reason = str_replace(reason, "sexed", "sex-ed"),
         reason = str_replace(reason, "PG13", "PG-13"),
         reason = str_replace(reason, "NC17", "NC-17")
  )

word_list1 <- list("language", "violence", "sexual", "sexuality", "nudity", "drug", "action", "sci-fi", "fantasy",
                  "humor", "graphic", "gore", "sensuality", "suggestive", "horror", "smoking", "teens",
                  "content", "images", "material", "elements", "references", "scene", "sequences")

# Create extended color palettes
nb.cols <- 18
mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(nb.cols)

# create second extended color palette
num.cols <- 12
mycolors_2 <- colorRampPalette(brewer.pal(8, "Dark2"))(num.cols)

# Content page 4, singletons
# Word options
word_list2 <- list(" ", "language", "violence", "sexual", "nudity", "drug", "action", "sci-fi", "fantasy",
                  "humor", "graphic", "gore", "sensuality", "suggestive", "horror", "smoking", "teens",
                  "content", "images", "material", "elements", "reference", "scene", "sequence")

# Get words that only occur once in the data set
singleton_words <- top_unigrams %>%
  count(word) %>% 
  filter(n == 1) %>% 
  select(word)

# Get details about all movies containing the unique words
full_singletons <- top_unigrams %>% 
  filter(word %in% singleton_words$word) %>% 
  select(title, reason_len, reason, rating, year, word)  

# Content page 5, Money

# rating list with G
rating_list5 <- list("G", "PG", "PG-13", "R", "NC-17")

# List of content topics for revenue facet grid
money_content_list <- list("language", "violence", "sexual", "nudity", "drug", "action", "sci-fi", "fantasy", "smoking", 
                           "teens", "humor", "graphic", "gore", "sensuality", "suggestive", "horror")

# unnest with tidytext
pop_unigrams <- full_mpaa %>% 
  unnest_tokens(word, reason, drop = FALSE) %>% 
  anti_join(mpaa_stop_words1)%>% 
  mutate(word = str_replace(word, "martialarts", "martial arts"),
         word = str_replace(word, "druguse", "drug use"),
         word = str_replace(word, "scifi", "sci-fi"),
         word = str_replace(word, "sexed", "sex-ed"),
         word = str_replace(word, "oldfashioned", "old fashioned"),
         word = str_replace(word, "risktaking", "risk taking")
  ) %>% 
  mutate(reason = str_replace(reason, "martialarts", "martial arts"),
         reason = str_replace(reason, "druguse", "drug use"),
         reason = str_replace(reason, "scifi", "sci-fi"),
         reason = str_replace(reason, "sexed", "sex-ed"),
         reason = str_replace(reason, "oldfashioned", "old fashioned"),
         reason = str_replace(reason, "risktaking", "risk taking"),
         reason = str_replace(reason, "PG13", "PG-13"),
         reason = str_replace(reason, "NC17", "NC-17")
  )

