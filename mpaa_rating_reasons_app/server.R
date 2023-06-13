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
            axis.text=element_text(size=13),
            legend.title = element_text(size=13),
            legend.text=element_text(size=12)) +
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
            axis.text=element_text(size=13),
            legend.title = element_text(size=13),
            legend.text=element_text(size=12)) +
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
                size = .75
      ) +
      geom_point(aes(x = year, y = n/total_movies, color = rating),
                 size = 1.5
      ) +
      scale_color_manual(values = col_pal,
                         breaks=c('G', 'PG', 'PG-13', 'R', 'NC-17')) +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            axis.text=element_text(size=13),
            legend.title = element_text(size=13),
            legend.text=element_text(size=12)) +
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
            axis.title=element_text(size=14),
            axis.text=element_text(size=13),
            legend.title = element_text(size=13),
            legend.text = element_text(size=12)) +
      labs(y = "",
           fill = "Content Words",
           x = "Total Movies"
           )
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
      filter(word %in% top_words()$word) %>%  
      inner_join(filtered_movies_yr()) %>% 
      ggplot(aes(x = year, y = totals/total_mov, color = word)) +
      geom_line(size = .5) +
      geom_point()+
      scale_colour_brewer(palette="Dark2") +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            axis.text=element_text(size=13),
            legend.title = element_text(size=13),
            legend.text = element_text(size=12)) +
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
    paste(input$select_word3)
  })
  
  output$word_selected2 <- renderText({
    paste(input$select_word3)
  })
  
  # setup to search for modifying words and phrases
  noun3 <- reactive({as.character(input$select_word3)})
  
  filtered_mod_mpaa <- reactive({
    full_mpaa_reasons %>%
      filter(rating %in% input$checkRating3,
             year >= min(input$yearSlider3),
             year<= max(input$yearSlider3)) 
  })
  
  output$mod_plot <- renderPlot({
    filtered_mod_mpaa() %>% 
      filter(grepl(noun3(), reason)) %>% 
      mutate(reason = str_trim(reason),
             modifiers = str_match(reason, 
                                   glue("(?<=, |for )(.+ and)?( for)?([^,]*?{noun3()}[^,]*?)(,.+?| and.+?)?$"))[ ,4]) %>% 
      mutate(modifiers = str_trim(modifiers)) %>% 
      filter(grepl(glue("{noun3()}( |$)"), modifiers)) %>% 
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
            axis.text=element_text(size=13),
            axis.title=element_text(size=14)) +
      labs(y = "",
           x = "Total Movies")
  })
  
  modifier_movie_count <- reactive({
    filtered_mod_mpaa() %>% 
      filter(grepl(noun3(), reason)) %>% 
      n_distinct()
  })
  
  output$mod_m_count <- renderText({
    as.character(modifier_movie_count())
  })
  
  modifier_count <- reactive({
    filtered_mod_mpaa() %>% 
      filter(grepl(noun3(), reason)) %>% 
      mutate(reason = str_trim(reason),
             modifiers = str_match(reason, 
                                   glue("(?<=, |for )(.+ and)?( for)?([^,]*?{noun3()}[^,]*?)(,.+?| and.+?)?$"))[ ,4]) %>% 
      mutate(modifiers = str_trim(modifiers)) %>% 
      select(modifiers) %>% 
      n_distinct()
  })
  
  output$mod_count <- renderText({
    as.character(modifier_count())
  })
  
  avg_mod_reason_len <- reactive ({
    filtered_mod_mpaa() %>% 
      filter(grepl(noun3(), reason)) %>%
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
             word = str_replace(word, "drug-use", "drug use"),
             word = str_replace(word, "old-fashioned", "old fashioned"),
             word = str_replace(word, "risk-taking", "risk taking"))
  })
  
  output$assoc_plot <- renderPlot({
    filtered_unigrams() %>%
    group_by(word) %>%
    filter(n() >= 20) %>%
    pairwise_cor(word, rowname, sort = TRUE)%>% 
    filter(item2 == noun3() ) %>% 
    filter(correlation > .08) %>% 
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
          axis.text=element_text(size=13),
          axis.title=element_text(size=14)) +
    labs(y = "Correlation Value",
         x = "Associated Word")
  })
  
  # Content page 4 One-offs
  
  # pull out selected word
  output$word_selecteda <- renderText({
    paste(input$select_word4)
  })
  
  # setup to search for content word
  noun4 <- reactive({as.character(input$select_word4)})
  
  filtered_singleton_mpaa <- reactive({
    full_singletons %>%
      filter(rating %in% input$checkRating4,
             year >= min(input$yearSlider4),
             year<= max(input$yearSlider4)) 
  })
  
  # Number of movies with rating reasons containing word selected & one-off word(s)
  singleton_movie_count <- reactive({
    filtered_singleton_mpaa() %>% 
      filter(grepl(noun4(), reason)) %>% 
      select(title) %>% 
      n_distinct()
  })
  
  output$singleton_m_count <- renderText({
    as.character(singleton_movie_count())
  })
  
  # Number of words that only appear once in all rating reasons 
  singleton_count <- reactive({
    filtered_singleton_mpaa() %>% 
      filter(grepl(noun4(), reason)) %>% 
      select(word) %>% 
      n_distinct()
  })
  
  output$sing_count <- renderText({
    as.character(singleton_count())
  })
  
  # Plot of one-off words and movies with rating reasons containing one-offs over time
  output$singleton_time_plot <- renderPlot({
    filtered_singleton_mpaa() %>% 
      filter(grepl(noun4(), reason)) %>% 
      group_by(year) %>% 
      summarize(`Total One-Offs` = n_distinct(word),
                `Rating Reasons with One-Offs` = n_distinct(title))%>%
      arrange(year) %>% 
      pivot_longer(!year, names_to = "variable",
                   values_to = "count")%>% 
      ggplot() +
      geom_line(aes(x = year, y = count, color = variable),
                size = .75,
      ) +
      geom_point(aes(x = year, y = count, color = variable),
                 size = 1.5,
      ) +
      scale_color_manual(values = c('#9EDCDA', '#8856A7')) +
      scale_y_continuous(limits=c(0,45))+
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            axis.text=element_text(size=13),
            legend.title = element_text(size=13),
            legend.text = element_text(size=12)) +
      scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
      labs(
        y = "",
        color = "Legend",
        x = "")
  })
  
  # Plot of one-off words and movies with rating reasons containing one-offs by MPAA rating
  output$singleton_rating_plot <- renderPlot({
    filtered_singleton_mpaa() %>% 
      filter(grepl(noun4(), reason)) %>% 
      group_by(rating) %>% 
      summarize(`Total One-Off words in Rating Reasons` = n_distinct(word),
                `Total Movies with One-off word(s) in Rating Reason` = n_distinct(title))%>% 
      arrange(rating) %>% 
      pivot_longer(!rating, names_to = "variable",
                   values_to = "count") %>%  
      ggplot() +
      geom_col(aes(x = rating, y = count, fill = rating)
      ) +
      facet_grid(. ~ variable) +
      scale_fill_manual(values = col_pal
      ) +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(size=13),
            legend.title = element_text(size=13),
            legend.text = element_text(size=12),
            strip.text.x = element_text(size=14)) +
      labs(title = "", 
           y = "",
           fill = "Rating",
           x = "")
  })
  
  # Filter the singletons table and rename columns
  weirdo_reasons <- reactive({
    full_singletons %>%
      filter(rating %in% input$checkRating4,
             year >= min(input$yearSlider4),
             year<= max(input$yearSlider4),
             grepl(noun4(), reason)) %>%
      arrange(word) %>% 
      select(Title = title,  
             Weirdo = word,
             `Rating Reason` = reason,
             Rating = rating, 
             `Year Rated` = year)
  })
  
  # Table of rating reasons with weirdo words
  output$weirdo_table <- DT::renderDataTable({
    DT::datatable(
      weirdo_reasons(),
      rownames = FALSE,
      options = list(
        #dom = 't'
      )
    )
  })
  
  # Content page 5 $$$
  
  # setup to search for content word
  noun5 <- reactive({as.character(input$select_word5)})
  
  gross_rating_year <- reactive({
    full_mpaa_reasons %>%
      filter(!is.na(gross_num),
             rating %in% input$checkRating5,
             year >= min(input$yearSlider5),
             year<= max(input$yearSlider5),
             grepl(noun5(), reason)) %>% 
      group_by(year, rating) %>% 
      summarize(mean_gross = mean(gross_num),
                med_gross = median(gross_num),
                total_gross = sum(gross_num),
                count_gross = n()) %>% 
      ungroup()
      
  })
  
  output$money_m_count <- renderText({
    as.character(gross_rating_year() %>% 
                   select(count_gross) %>% 
                   sum())
  })
  
  output$all_money <- renderText({
    as.character(gross_rating_year() %>% 
                   select(total_gross) %>% 
                   sum()/1000000)
  })
  
  max_money_movies <- reactive({
    full_mpaa_reasons %>%
      filter(!is.na(gross_num),
             rating %in% input$checkRating5,
             year >= min(input$yearSlider5),
             year<= max(input$yearSlider5),
             grepl(noun5(), reason)) 
    
  })
  
  output$big_money_title <- renderText({
    as.character(max_money_movies() %>% 
                   slice_max(gross_num, n=1, with_ties = FALSE) %>% 
                   select(title))
    })
  
  output$big_money_gross <- renderText({
    as.character(max_money_movies() %>% 
                   slice_max(gross_num, n=1, with_ties = FALSE) %>% 
                   select(gross_num)/1000000)
  })
  
  
  output$big_money_year <- renderText({
    as.character(max_money_movies() %>% 
                   slice_max(gross_num, n=1, with_ties = FALSE) %>% 
                   select(year))
  })
  
  output$gross_rating_year_plot <- renderPlot({
    gross_rating_year() %>% 
      ggplot() +
      geom_line(aes(x = year, y = mean_gross, color = rating),
                size = .75,
      ) +
      geom_point(aes(x = year, y = mean_gross, color = rating),
                 size = 1.5,
      ) +
      scale_color_manual(values = col_pal,
                         breaks=c('G', 'PG', 'PG-13', 'R', 'NC-17')) +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            axis.text=element_text(size=13),
            legend.title = element_text(size=13),
            legend.text = element_text(size=12)) +
      scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
      scale_y_continuous(labels =label_number(prefix = "$", suffix = " M", scale = 1e-6)) +
      labs(y = "",
           color = "Rating",
           x = "") 
  })
  
  
  # Table of top performing movies
  output$big_money_table <- DT::renderDataTable({
    DT::datatable(
      max_money_movies()  %>% 
        slice_max(`gross_num`, n=10) %>% 
        transmute(Title = title,
                  Rating = rating,
                  `Year Rated` = year,
                  `Box Office Revenue` = paste("$", gross_num/1000000, "M"),
                  Reason = reason
        ),
      rownames = FALSE,
      options = list(
        dom = 't'
      )
    )
  })
  
  # Use money unigrams to identify movie fitting criteria + group by key content concerns
  money_unigrams <- reactive({
    pop_unigrams %>%
      filter(!is.na(gross_num),
             word %in% money_content_list,
             rating %in% input$checkRating5,
             year >= min(input$yearSlider5),
             year<= max(input$yearSlider5),
             grepl(noun5(), reason)) %>% 
      group_by( word) %>% 
      summarize(Average = mean(gross_num),
                `Top Movie` = max(gross_num)
      ) %>%  
      ungroup() %>% 
      pivot_longer(!word, names_to = 'variable', values_to= 'amount')
  })
  
  # show box office revenue for movies meeting criteria and key content terms, order by valu
  output$gross_content_plot <- renderPlot({
    money_unigrams() %>% 
      ggplot()+ 
      geom_col(aes(x = amount, y = reorder_within(word, amount, variable), fill = word)
      ) +
      facet_grid(variable ~ ., scales = "free_y") +
      scale_y_reordered() + 
      scale_fill_manual(values = mycolors)+
      scale_x_continuous(labels =label_number(prefix = "$", suffix = " M", scale = 1e-6))+
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            axis.title=element_text(size=14),
            axis.text=element_text(size=13),
            strip.text.y = element_text(size=14),
            legend.position="none",
            panel.grid.major.y = element_blank()
      ) +
      labs(y = "",
           fill = "",
           x = "Box Office Revenue") 
      
  }, height = 500)
  
  # Content page 6 popularity
  
  # setup to search for content word
  noun6 <- reactive({as.character(input$select_word6)})
  
  # Pull numbers for cards
  pop_overall <- reactive({
    full_mpaa_reasons %>%
      filter(rating %in% input$checkRating6,
             year >= min(input$yearSlider6),
             year<= max(input$yearSlider6),
             grepl(noun6(), reason))  %>%
      summarize(mean_votes = round(mean(votes, na.rm=TRUE), 0),
                count_m_votes = sum(!is.na(votes)),
                wt_avg_score = round((sum(vote_x_rating, na.rm=TRUE) / sum(votes, na.rm=TRUE)), 1),
                count_m_scores = sum(!is.na(imdb_ratings)),
                mean_meta = round(mean(metascores, na.rm=TRUE), 1), 
                count_m_metas = sum(!is.na(metascores))) %>% 
      ungroup()
  })
  
  output$vote_m_count <- renderText({
    as.character(pop_overall() %>% 
                   select(count_m_votes) )
  })
  
  output$score_m_count <- renderText({
    as.character(pop_overall() %>% 
                   select(count_m_scores) )
  })
  
  output$meta_m_count <- renderText({
    as.character(pop_overall() %>% 
                   select(count_m_metas) )
  })
  
  output$vote_avg <- renderText({
    as.character(pop_overall() %>% 
                   select(mean_votes) )
  })
  
  output$score_avg <- renderText({
    as.character(pop_overall() %>% 
                   select(wt_avg_score) )
  })
  
  output$meta_avg <- renderText({
    as.character(pop_overall() %>% 
                   select(mean_meta) )
  })
  
  # Apply filters
  mpaa_pop <- reactive({
    full_mpaa_reasons %>%
      filter(rating %in% input$checkRating6,
             year >= min(input$yearSlider6),
             year<= max(input$yearSlider6),
             grepl(noun6(), reason)) 
    
  })
  
  
  
  # Box plote of votes
  output$vote_box <- renderPlot({
    mpaa_pop() %>% 
      ggplot(aes(y= fct_relevel(rating, rev), x= (votes), colour=rating, na.rm=TRUE)) +
      geom_boxplot()+
      scale_color_manual(values = col_pal,
                         breaks=c('G', 'PG', 'PG-13', 'R', 'NC-17')) +
      scale_x_continuous(#limits=c(0, 3000000),
                         labels =label_number(suffix = " M", scale = 1e-6))+
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size=13),
            legend.title = element_text(size=13),
            legend.text = element_text(size=12)
      ) +
      labs(colour = "Rating")
  })
  
  # Box plot of IMDB viewer scores
  output$score_box <- renderPlot({
    mpaa_pop() %>% 
      ggplot(aes(y= fct_relevel(rating, rev), x= imdb_ratings, colour=rating, na.rm=TRUE)) +
      geom_boxplot()+
      scale_color_manual(values = col_pal,
                         breaks=c('G', 'PG', 'PG-13', 'R', 'NC-17')) +
      scale_x_continuous(limits=c(0,10))+
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size=13),
            legend.position = "none"
      ) +
      labs(colour = "Rating")
  })
  
  # Box plot of Critic Metascores
  output$meta_box <- renderPlot({
    mpaa_pop() %>% 
      ggplot(aes(y= fct_relevel(rating, rev), x= metascores, colour=rating, na.rm=TRUE)) +
      geom_boxplot()+
      scale_color_manual(values = col_pal,
                         breaks=c('G', 'PG', 'PG-13', 'R', 'NC-17')) +
      scale_x_continuous(limits=c(0,100))+
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size=13),
            legend.position = "none"
      ) +
      labs(colour = "Rating")
  })
  
  # Page 7 deep dive into popularity and reason words
  
  # setup to search for content word
  noun7 <- reactive({as.character(input$select_word7)})
  
  # Filter unigrams data by votes
  votes_filtered <- reactive({
    pop_unigrams %>% 
      filter(!is.na(votes),
             word %in% money_content_list,
             rating %in% input$checkRating7,
             year >= min(input$yearSlider7),
             year<= max(input$yearSlider7),
             grepl(noun7(), reason),
             votes >= min(input$voteSlider),
             votes <= max(input$voteSlider),
             imdb_ratings >= min(input$scoreSlider),
             imdb_ratings <= max(input$scoreSlider),
             metascores >= min(input$metaSlider),
             metascores <= max(input$metaSlider)
      ) %>% 
      group_by( word) %>% 
      summarize(Average = mean(votes),
                `Top Movie` = max(votes)
      ) %>%  
      ungroup()
    })
  
  #Words by votes
  output$vote_words <- renderPlot({
     votes_filtered() %>% 
      pivot_longer(!word, names_to = 'variable', values_to= 'amount')%>% 
      ggplot()+ 
      geom_col(aes(x = amount, y = reorder_within(word, amount, variable), fill = word)
      ) +
      facet_grid(variable ~ ., scales = "free_y") +
      scale_y_reordered() + 
      scale_fill_manual(values = mycolors)+
      scale_x_continuous(labels =label_number(suffix = " M", scale = 1e-6))+
      theme_bw(base_size = 18) +
      theme(panel.grid.major.x = element_blank(),
            legend.position="none",
            panel.grid.major.y = element_blank()
      ) +
      labs(y = "",
           fill = "",
           x = "Votes")
  }, height = 500)
  
  # Filter unigrams data by votes
  scores_filtered <- reactive({
    pop_unigrams %>% 
      filter(!is.na(imdb_ratings),
             word %in% money_content_list,
             rating %in% input$checkRating7,
             year >= min(input$yearSlider7),
             year<= max(input$yearSlider7),
             grepl(noun7(), reason),
             votes >= min(input$voteSlider),
             votes <= max(input$voteSlider),
             imdb_ratings >= min(input$scoreSlider),
             imdb_ratings <= max(input$scoreSlider),
             metascores >= min(input$metaSlider),
             metascores <= max(input$metaSlider)
      ) %>% 
      group_by( word) %>% 
      summarize(`Average (Weighted)` = sum(vote_x_rating, na.rm=TRUE) / sum(votes),
                `Top Movie` = max(imdb_ratings)
      ) %>%  
      ungroup()
  })
  
  #Words by IMDB viewer ratings
  output$score_words <- renderPlot({
    scores_filtered() %>% 
      pivot_longer(!word, names_to = 'variable', values_to= 'amount')%>% 
      ggplot()+ 
      geom_col(aes(x = amount, y = reorder_within(word, amount, variable), fill = word)
      ) +
      facet_grid(variable ~ ., scales = "free_y") +
      scale_y_reordered() + 
      scale_fill_manual(values = mycolors)+
      scale_x_continuous(limits = c(0, 10))+
      theme_bw(base_size = 18) +
      theme(panel.grid.major.x = element_blank(),
            legend.position="none",
            panel.grid.major.y = element_blank()
      ) +
      labs(y = "",
           fill = "",
           x = "Score")
  }, height = 500)
  
  # Filter unigrams data by votes
  meta_filtered <- reactive({
    pop_unigrams %>% 
      filter(!is.na(metascores),
             word %in% money_content_list,
             rating %in% input$checkRating7,
             year >= min(input$yearSlider7),
             year<= max(input$yearSlider7),
             grepl(noun7(), reason),
             votes >= min(input$voteSlider),
             votes <= max(input$voteSlider),
             imdb_ratings >= min(input$scoreSlider),
             imdb_ratings <= max(input$scoreSlider),
             metascores >= min(input$metaSlider),
             metascores <= max(input$metaSlider)
      ) %>% 
      group_by( word) %>% 
      summarize(`Average (Weighted)` = mean(metascores),
                `Top Movie` = max(metascores)
      ) %>%  
      ungroup()
  })
  
  #Words by Critic Metascores
  output$meta_words <- renderPlot({
    meta_filtered() %>% 
      pivot_longer(!word, names_to = 'variable', values_to= 'amount')%>% 
      ggplot()+ 
      geom_col(aes(x = amount, y = reorder_within(word, amount, variable), fill = word)
      ) +
      facet_grid(variable ~ ., scales = "free_y") +
      scale_y_reordered() + 
      scale_fill_manual(values = mycolors)+
      scale_x_continuous(limits = c(0, 100))+
      theme_bw(base_size = 18) +
      theme(panel.grid.major.x = element_blank(),
            legend.position="none",
            panel.grid.major.y = element_blank()
      ) +
      labs(y = "",
           fill = "",
           x = "Score")
  }, height = 500)
  
  max_votes <- reactive({
    pop_unigrams %>% 
      filter(word %in% money_content_list,
             rating %in% input$checkRating7,
             year >= min(input$yearSlider7),
             year<= max(input$yearSlider7),
             votes >= min(input$voteSlider),
             votes <= max(input$voteSlider),
             grepl(noun7(), reason),
             imdb_ratings >= min(input$scoreSlider),
             imdb_ratings <= max(input$scoreSlider),
             metascores >= min(input$metaSlider),
             metascores <= max(input$metaSlider)
      )  %>% 
    transmute(Title = title,
              Rating = rating,
              `Year Rated` = year,
              `Viewer Votes` = votes,
              `Viewer Score` = imdb_ratings,
              `Critic Metascore` = metascores,
              Reason = reason,
              Metric = "Top Viewer Votes")%>% 
      distinct() %>% 
      slice_max(`Viewer Votes`, n = 3)
    })
  
  max_scores <- reactive({
    pop_unigrams %>% 
      filter(word %in% money_content_list,
             rating %in% input$checkRating7,
             year >= min(input$yearSlider7),
             year<= max(input$yearSlider7),
             grepl(noun7(), reason),
             votes >= min(input$voteSlider),
             votes <= max(input$voteSlider),
             imdb_ratings >= min(input$scoreSlider),
             imdb_ratings <= max(input$scoreSlider),
             metascores >= min(input$metaSlider),
             metascores <= max(input$metaSlider)
      ) %>%  
    transmute(Title = title,
              Rating = rating,
              `Year Rated` = year,
              `Viewer Votes` = votes,
              `Viewer Score` = imdb_ratings,
              `Critic Metascore` = metascores,
              Reason = reason,
              Metric = "Top Viewer Scores")%>% 
      distinct() %>% 
      slice_max(`Viewer Score`, n = 3)
    })
  
  
  max_meta <- reactive({
    pop_unigrams %>% 
    filter(!is.na(metascores),
           word %in% money_content_list,
           rating %in% input$checkRating7,
           year >= min(input$yearSlider7),
           year<= max(input$yearSlider7),
           grepl(noun7(), reason),
           votes >= min(input$voteSlider),
           votes <= max(input$voteSlider),
           imdb_ratings >= min(input$scoreSlider),
           imdb_ratings <= max(input$scoreSlider),
           metascores >= min(input$metaSlider),
           metascores <= max(input$metaSlider)
    ) %>% 
    transmute(Title = title,
              Rating = rating,
              `Year Rated` = year,
              `Viewer Votes` = votes,
              `Viewer Score` = imdb_ratings,
              `Critic Metascore` = metascores,
              Reason = reason,
              Metric = "Top Metascores") %>% 
      distinct() %>% 
      slice_max(`Critic Metascore`, n = 3)
  })
  
  # Table of top performing movies
  output$top_pops_table <- DT::renderDataTable({
    DT::datatable(
      bind_rows(max_votes(), max_scores(), max_meta()),
      rownames = FALSE,
      options = list(
        #dom = 't'
      )
    )
  })
  
  
  
  
}


