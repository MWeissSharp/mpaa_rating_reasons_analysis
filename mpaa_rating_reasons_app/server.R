function(input, output, session) {

  #Landing page
  output$rating_trends <- renderPlot({
    full_mpaa %>% 
      group_by(year, rating) %>% 
      count(rating) %>% 
      ungroup() %>% 
      ggplot(aes(x = year, y = n, color = rating)) +
      geom_line(size = 1.25) +
      scale_color_manual(values = col_pal,
                         breaks=c('G', 'PG', 'PG-13', 'R', 'NC-17')) +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank()) +
      scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
      labs(y = "Number of Movies Rated",
           color = "Rating",
           x = "") 
  })
  
  # Content page 1 Top Words
  
  # Content page 2 Wordclouds
  # create matrix
  wc_matrix <- reactive({
    create_matrix(input$rating_rad1, input$rating_rad2)
  })

  output$commonality_wc <- renderPlot({
    commonality.cloud(wc_matrix(), scale = c(6, .75), color = "darkorchid4", max.words = 75)
  }) 
  
  output$comparison_wc <- renderPlot({
    comparison.cloud(wc_matrix(), scale = c(6, .75), colors = c("firebrick2", "blue"), max.words = 75)
  })
  
}

