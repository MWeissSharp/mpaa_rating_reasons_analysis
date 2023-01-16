
ui <- tagList(
  
  navbarPage(
          theme = shinytheme("flatly"),
          "MPAA Rating Reasons",
          
           # Landing Page - Project intro
           tabPanel("Intro",
              fluidPage(h2("Swashbuckling Violence and Nonstop Ninja Action"),
                        br(),
                        plotOutput("rating_trends")
                        )
                    ),
          
          # Content Page 1 - Top Words With Filters
          tabPanel("Top Words",
                   fluidPage(
                     
                     h3("Choose Your Filters, See Top Words Over Time")
                   )
          ),
          
           # Content Page 2 - Rating Compare & Contrast
          tabPanel("By Rating",
                   fluidPage(
                     sidebarLayout(position = "right",
                                   sidebarPanel(
                                        wellPanel(
                                          radioButtons("rating_rad1", 
                                                       label = "Choose Your 1st Rating",
                                                       choices = list("PG", "PG-13", "R", "NC-17"), 
                                                       selected = "PG"
                                                       )
                                                  ),
                                        wellPanel(
                                          radioButtons("rating_rad2", 
                                                       label = "Choose Your 2nd Rating",
                                                       choices = list("PG", "PG-13", "R", "NC-17"), 
                                                       selected = "PG-13"
                                          )
                                        ),
                                        wellPanel(
                                          sliderInput("year_slider", 
                                                      label = "Rating Year Range", 
                                                      min = 1992, 
                                                      max = 2022, 
                                                      sep = "",
                                                      value = c(1992, 2022)
                                                      )
                                        )  
                                    ),
                     mainPanel(h3("We are more alike than we are different- or are we?"),
                               plotOutput("commonality_wc", width = "100%", height = "510px"),
                               plotOutput("comparison_wc", width = "100%", height = "510px")
                               )
                            )
                          )
                ),
           
          # Content Page 3 - Modifying Words/Phrases
          tabPanel("Modifiers",
                   fluidPage(h3("The Devil's in the Details: Modifiers")
                   )
          ),
          
          # Content Page 4 - Let's Talk about S.E.X.
          tabPanel("S.E.X",
                   fluidPage(h3("Let's talk about sex, baby...")
                   )
          ),
          
          # Content Page 5 - One-Offs
          tabPanel("One-offs",
                   fluidPage(h3("One-offs and Weirdos")
                   )
          )
          
  )
)
