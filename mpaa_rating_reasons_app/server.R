function(input, output, session) {

  #Overview page
  output$rating_trends <- renderPlot({
    full_mpaa %>% 
      group_by(year, rating) %>% 
      count(rating) %>% 
      ungroup() %>% 
      ggplot(aes(x = year, y = n, color = rating)) +
      geom_line(size = 1) +
      scale_color_manual(values = col_pal,
                         breaks=c('G', 'PG', 'PG-13', 'R', 'NC-17')) +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank()) +
      scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
      labs(y = "Number of Movies Rated",
           color = "Rating",
           x = "") 
  })
  
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
      theme(panel.grid.minor.x = element_blank()) +
      scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
      scale_y_continuous(breaks = c(2,4,6,8,10,12),
                         limit = c(0, 12)) +
      labs(y = "Average Number of Words in Rating Reason",
           color = "Rating",
           x = "")
  })
  
  # Content page 1 Top Words
  
  # Count number of movies that fit within chosen criteria
  movie_count <- reactive({
    full_mpaa %>% 
      filter(rating %in% input$checkRating,
              year >= min(input$yearSlider),
              year <= max(input$yearSlider)) %>% 
      n_distinct()
  })
  
  output$m_count <- renderText({
    as.character(movie_count())
  })
  
  # Identify top 10 words based on the filters
  top_words <- reactive({
    top_yr_rating_counts %>%
      filter(rating %in% input$checkRating,
             year >= min(input$yearSlider),
             year<= max(input$yearSlider)) %>% 
    group_by(word) %>%
      summarize(total = sum(n)) %>%
      arrange(desc(total)) %>%
      ungroup() %>%
      slice_max(total, n = 5)
  })
  
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
      theme(panel.grid.minor.x = element_blank()) +
      labs(y = "Word(s)",
           color = "Words",
           x = "Total Rating Reasons")
  })
  
  output$top_line <- renderPlot({
    top_yr_rating_counts %>% 
      filter(word %in% top_words()$word) %>% 
      group_by(year, word) %>% 
      summarize(totals = sum(n)) %>% 
      ungroup() %>% 
      ggplot(aes(x = year, y = totals, color = word)) +
      geom_line(size = .5) +
      geom_point()+
      scale_colour_brewer(palette="Dark2") +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank()) +
      scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
      # scale_y_continuous(breaks = c(2,4,6,8,10,12),
      #                    limit = c(0, 12)) +
      labs(y = "Total Rating Reasons",
           color = "Words",
           x = "")
  })
  
  # Content page 2 Wordclouds
  
  # create matrix
  wc_matrix <- reactive({
    create_matrix(input$ratingRad1, input$ratingRad2, min(input$yearSlider2), max(input$yearSlider2))
  })

  output$commonality_wc <- renderPlot({
    commonality.cloud(wc_matrix(), 
                      scale = c(6, .75), 
                      color = "darkorchid4", 
                      max.words = 60)
  }) 
  
  output$comparison_wc <- renderPlot({
    comparison.cloud(wc_matrix(), 
                     scale = c(6, .75), 
                     colors = c("firebrick2", "blue"), 
                     max.words = 60)
  })
 
  # Content page 3 Modifiers
  
  # setup to search for modifying words and phrases
  noun <- reactive({as.character(input$select_word)})
  
  pattern <- 
  output$mod_plot <- renderPlot({
    full_mpaa %>% 
      filter(grepl(noun(), reason)) %>% 
      mutate(reason = str_trim(reason),
             modifiers = str_match(reason, 
                                   glue("(?<=, |for )(.+ and)?([^,]*?{noun()}[^,]*?)(.|,.+?| and.+?)$"))[ ,3]) %>% 
      mutate(modifiers = str_trim(modifiers)) %>% 
      count(modifiers) %>% 
      mutate(mod_count = n) %>% 
      slice_max(mod_count, n=10) %>% 
      arrange(desc(n)) %>% 
      ggplot(aes(x = mod_count, y= modifiers)) +
      geom_col()
  })
  
   
}

