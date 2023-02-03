function(input, output, session) {
  
  #Overview page
  
  # Total # movies in dataset
  output$total_movies <- renderText({
    as.character(movies_yr_rating %>% 
                   summarize(grand_total = sum(total_movies))
    )
  })
  
  # Total distinct words in rating reasons
  output$distinct_words <- renderText({
    as.character(n_distinct(all_tokens$word)
    )
  })
  
  # Determine top used word (other than "rated" "for" or "and")
  output$topword <- renderText({
    as.character(all_tokens %>% 
                   anti_join(mini_stop) %>% 
                   count(word) %>% 
                   slice_max(n, n=1) %>% 
                   select(word))
  })
  
  # Total times topword appears in reasons
  output$total_topword <- renderText({
    as.character(all_tokens %>% 
                   anti_join(mini_stop) %>% 
                   count(word) %>% 
                   slice_max(n, n=1) %>% 
                   select(n))
  })
  
  # Determine number of words used only once
  output$singletons <- renderText({
    as.character(all_tokens %>% 
                   anti_join(mini_stop) %>% 
                   count(word) %>%
                   filter(n == 1) %>% 
                   count(n) %>% 
                   select(nn)
    )
  })
  
  # Line plot of movie ratings by rating across the years
  output$rating_trends <- renderPlot({
    movies_yr_rating %>% 
      ggplot() +
      geom_line(aes(x = year, y = total_movies, color = rating),
                size = .75,
      ) +
      geom_point(aes(x = year, y = total_movies, color = rating),
                 size = 1.5,
      ) +
      scale_color_manual(values = col_pal,
                         breaks=c('G', 'PG', 'PG-13', 'R', 'NC-17')) +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            axis.text=element_text(size=12),
            legend.text=element_text(size=11)) +
      scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
      labs(y = "",
           color = "Rating",
           x = "") 
  })
  
  #Line plote of reason lengths by rating and year
  output$reason_lengths <- renderPlot({
    mean_rea_len %>% 
      ggplot() +
      geom_line(aes(x = year, y = mean_len, color = rating),
                size = .75,
      ) +
      geom_point(aes(x = year, y = mean_len, color = rating),
                 size = 1.5,
      ) +
      scale_color_manual(values = col_pal,
                         breaks=c('G', 'PG', 'PG-13', 'R', 'NC-17')) +
      geom_hline(data = overall_mean_len, aes(yintercept = overall))+
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            axis.text=element_text(size=12),
            legend.text=element_text(size=11)) +
      scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
      scale_y_continuous(breaks = c(2,4,6,8,10,12),
                         limit = c(0, 12)) +
      labs(y = "",
           color = "Rating",
           x = "")
  })
  
  # Total movies with short reasons
  output$total_shorties <- renderText({
    as.character(short_reasons %>% 
                   summarize(total_short = sum(n))
    )
  })
  
  # Plot of short reasons over time by rating
  output$shorties <- renderPlot({
    short_reasons %>% 
      inner_join(movies_yr_rating) %>% 
      ggplot() +
      geom_line(aes(x = year, y = n/total_movies, color = rating),
                size = .75,
      ) +
      geom_point(aes(x = year, y = n/total_movies, color = rating),
                 size = 1.5,
      ) +
      scale_color_manual(values = col_pal,
                         breaks=c('G', 'PG', 'PG-13', 'R', 'NC-17')) +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank()) +
      scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
      scale_y_continuous(labels = scales::percent) +
      labs(y = "",
           color = "Rating",
           x = "")
  })
  
  # Table of longest rating reasons
  output$longestReason <- DT::renderDataTable({
    DT::datatable(
      long_reason,
      rownames = FALSE,
      options = list(
        dom = 't'
      )
    )
  })
  
  # Content page 1 Top Content Concerns
  
  # Get count of number of movies that fit within chosen criteria
  movie_count <- reactive({
    full_mpaa %>% 
      filter(rating %in% input$checkRating1,
             year >= min(input$yearSlider),
             year <= max(input$yearSlider)) %>% 
      n_distinct()
  })
  
  # Create output of that number
  output$m_count <- renderText({
    as.character(movie_count())
  })
  
  # Identify top 5 content words based on the filters
  top_words <- reactive({
    word_yr_rating_counts %>%
      filter(rating %in% input$checkRating1,
             year >= min(input$yearSlider),
             year<= max(input$yearSlider)) %>% 
      group_by(word) %>%
      summarize(total = sum(n)) %>%
      arrange(desc(total)) %>%
      ungroup() %>%
      slice_max(total, n = 5)
  })
  
  # create lollipop graph of the number of times those words appeared in the filtered dataset
  output$top_col <- renderPlot({
    top_words() %>%  
      ggplot(aes(x = total, y = reorder(word, total), fill = word)) +
      geom_col(width = .05) +
      geom_point(aes(color = word),
                 size = 6,
                 show.legend = FALSE) +
      scale_fill_brewer(palette = "Dark2") + 
      scale_colour_brewer(palette = "Dark2")+
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            axis.text=element_text(size=12),
            legend.text = element_text(size=11)) +
      labs(y = "",
           fill = "Content Words",
           x = "Total Movies")
  })
  
  #create dataset of number of movies per year based on relevant filters
  filtered_movies_yr <- reactive({
    movies_yr_rating %>%
      filter(rating %in% input$checkRating1,
             year >= min(input$yearSlider),
             year<= max(input$yearSlider)) %>% 
      group_by(year) %>% 
      summarize(total_mov = sum(total_movies))
    
  })
  
  # create dataset of word counts each year based on relevant filters
  filtered_words_yr <- reactive({
    word_yr_rating_counts %>%
      filter(rating %in% input$checkRating1,
             year >= min(input$yearSlider),
             year<= max(input$yearSlider)) %>%
      group_by(year, word) %>% 
      summarize(totals = sum(n)) %>% 
      ungroup()
  })
  
  # Create line plot of top 5 content words over time (proportion)
  output$top_line <- renderPlot({
    filtered_words_yr() %>% 
      filter(word %in% top_words()$word, ) %>%  
      inner_join(filtered_movies_yr()) %>% 
      ggplot(aes(x = year, y = totals/total_mov, color = word)) +
      geom_line(size = .5) +
      geom_point()+
      scale_colour_brewer(palette="Dark2") +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            axis.text=element_text(size=12),
            legend.text = element_text(size=11)) +
      scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
      scale_y_continuous(labels = scales::percent) +
      labs(y = "",
           color = "Content Words",
           x = "")
  })
  
  # Content page 2 Wordclouds
  #pull out ratings selected
  output$rating_1 <- renderText({
    paste(input$ratingRad1)
  })
  
  output$rating_2 <- renderText({
    paste(input$ratingRad2)
  })
  
  # create matrix
  wc_matrix <- reactive({
    create_matrix(input$ratingRad1, input$ratingRad2, min(input$yearSlider2), max(input$yearSlider2))
  })
  
  output$commonality_wc <- renderPlot({
    commonality.cloud(wc_matrix(), 
                      scale = c(6, .75), 
                      random.order = FALSE,
                      color = "darkorchid4", 
                      max.words = 60)
  }) 
  
  output$comparison_wc <- renderPlot({
    comparison.cloud(wc_matrix(), 
                     scale = c(6, .75),
                     random.order = FALSE,
                     colors = c("firebrick2", "blue"), 
                     max.words = 60,
                     title.size = 0.1,
                     title.colors = "white",
                     title.bg.colors = "white")
  })
  
  # Content page 3 Modifiers
  
  # pull out selected word
  output$word_selected <- renderText({
    paste(input$select_word)
  })
  
  output$word_selected2 <- renderText({
    paste(input$select_word)
  })
  
  # setup to search for modifying words and phrases
  noun <- reactive({as.character(input$select_word)})
  
  filtered_mod_mpaa <- reactive({
    full_mpaa %>%
      filter(rating %in% input$checkRating3,
             year >= min(input$yearSlider3),
             year<= max(input$yearSlider3)) %>%
      mutate(reason = str_replace(reason, "martialarts", "martial arts"),
             reason = str_replace(reason, "druguse", "drug use"),
             reason = str_replace(reason, "scifi", "sci-fi")) 
  })
  
  output$mod_plot <- renderPlot({
    filtered_mod_mpaa() %>% 
      filter(grepl(noun(), reason)) %>% 
      mutate(reason = str_trim(reason),
             modifiers = str_match(reason, 
                                   glue("(?<=, |for )(.+ and)?( for)?([^,]*?{noun()}[^,]*?)(.|,.+?| and.+?)?$"))[ ,4]) %>% 
      mutate(modifiers = str_trim(modifiers)) %>% 
      filter(grepl(glue("{noun()}( |$)"), modifiers)) %>% 
      count(modifiers) %>% 
      mutate(mod_count = n) %>% 
      slice_max(mod_count, n=10) %>% 
      filter(n > 1) %>% 
      arrange(desc(n)) %>% 
      ggplot(aes(x = mod_count, reorder(modifiers, mod_count), fill= modifiers)) +
      geom_col(show.legend = FALSE) + 
      scale_fill_manual(values = mycolors) +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            axis.text=element_text(size=12)) +
      labs(y = "",
           x = "Total Movies")
  })
  
  modifier_movie_count <- reactive({
    filtered_mod_mpaa() %>% 
      filter(grepl(noun(), reason)) %>% 
      n_distinct()
  })
  
  output$mod_m_count <- renderText({
    as.character(modifier_movie_count())
  })
  
  modifier_count <- reactive({
    filtered_mod_mpaa() %>% 
      filter(grepl(noun(), reason)) %>% 
      mutate(reason = str_trim(reason),
             modifiers = str_match(reason, 
                                   glue("(?<=, |for )(.+ and)?( for)?([^,]*?{noun()}[^,]*?)(.|,.+?| and.+?)?$"))[ ,4]) %>% 
      mutate(modifiers = str_trim(modifiers)) %>% 
      select(modifiers) %>% 
      n_distinct()
  })
  
  output$mod_count <- renderText({
    as.character(modifier_count())
  })
  
  avg_mod_reason_len <- reactive ({
    filtered_mod_mpaa() %>% 
      filter(grepl(noun(), reason)) %>%
      summarize(avg_reason_len = mean(reason_len))
  })
  
  output$avg_mod_reason <-renderText({
    as.character(round(avg_mod_reason_len()$avg_reason_len, 2))
  })
  
  # Word correlations
  filtered_unigrams <- reactive({
    wc_unigrams %>% 
      mutate(rating %in% input$checkRating3,
             year >= min(input$yearSlider3),
             year<= max(input$yearSlider3),
             word = str_replace(word, "martial-arts", "martial arts"),
             word = str_replace(word, "drug-use", "drug use"))
  })
  
  output$assoc_plot <- renderPlot({
    filtered_unigrams() %>%
    group_by(word) %>%
    filter(n() >= 20) %>%
    pairwise_cor(word, X, sort = TRUE)%>% 
    filter(item2 == noun() ) %>% 
    filter(correlation > .1) %>% 
    slice_max(correlation, n=10)%>% 
    ggplot() +
    geom_col(aes(y = correlation, x = reorder(item1, correlation), fill = item1),
             width = .02,
             show.legend = FALSE) +
    geom_point(aes(y = correlation, x = reorder(item1, correlation), color = item1),
               size = 8,
               show.legend = FALSE) + 
    scale_color_manual(values = mycolors_2) +
    scale_fill_manual(values = mycolors_2) +
    theme_bw() +
    theme(panel.grid.minor.x = element_blank(),
          axis.text=element_text(size=12)) +
    labs(y = "Correlation Value",
         x = "Associated Word")
  })
  
}

