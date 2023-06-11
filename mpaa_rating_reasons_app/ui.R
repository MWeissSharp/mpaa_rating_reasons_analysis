ui <- tagList(
  
  navbarPage(
    theme = shinytheme("flatly"),
    "MPAA Rating Reasons",
    
    # Landing Page - Project intro
    tabPanel("Intro",
             fluidPage(h2("Swashbuckling Violence and Nonstop Ninja Action"),
                       h4("A Text Analysis of Movie Rating Reasons"),
                       br(),
                       tags$div(img(src = "ratings-anatomy.png", height = 450, width = 600), 
                                style="text-align: center;"),
                       p(h6("Image from filmratings.com", style="text-align: right;")),
                       br(),
                       p(strong("A Brief History of Movie Ratings")),
                       p("Film ratings have their origins in the \"Hays Code\", moral 
                          censorship guidelines established by William Hays in the 1930's. 
                          The modern voluntary movie rating system, designed with a focus on
                          providing parents with guidance for decisions about showing content
                          to their children,was established in 1968 
                          with the ratings of G, M, R, and X. In the years that followed, the 
                          M rating was changed to PG, the PG-13 rating was added, and the 
                          X rating was updated to the current NC-17. In 1990 came the addition 
                          of reasons attached to ratings with the goal of offering parents even 
                          more detailed information as they made decisions about which movies 
                          to show their children. "),
                       p("By 1992, these reasons were in full swing, coming along with every
                          rating of PG or higher. Since these reasons serve to point out content
                          that may not be appropriate for audiences under a certain age, 
                          G rated movies, those deemed suitable for all audiences, do not have
                          reasons assigned along with their ratings."),
                       br(),
                       p(strong("Resources to Learn More")),
                       p("MPAA's History of Ratings: ",
                         a("filmratings.com", href = "https://www.filmratings.com/History")),
                       p("MPAA Film Rating Rules: ",
                         a("PDF of Classification and Rating Rules", href = "https://www.filmratings.com/Content/Downloads/rating_rules.pdf")),
                       p("How the MPAA Changed the Film Industry Forever: ",
                        a("observer.com", href="https://observer.com/2018/11/mpaa-50-years-movie-ratings-system-changed-film-forever/")),
                       p("Motion Picture Association film rating system: ",
                         a("wikipedia.org", href = "https://en.wikipedia.org/wiki/Motion_Picture_Association_film_rating_system#Releases")),
                       br(),
                       br()
                       )
             ),
    # Data overview
    tabPanel("Data Overview",
             fluidPage(
               fluidRow(
                 column(12,
                        h2("Reasoning through the Reasons")
                        )
                 ),
               br(),
               fluidRow(
                 column(3,
                        h4("Total Movies in Dataset:"),
                        h4(strong(textOutput("total_movies")))
                        ),
                 column(3,
                        h4("Total Distinct Words in Rating Reasons:"),
                        h4(strong(textOutput("distinct_words")))
                        ),
                 column(3,
                        h4("Most Frequently Occurring Word*:"),
                        h4(strong("\"", textOutput("topword", 
                                                   inline = TRUE), "\" appears",textOutput("total_topword",
                                                                                           inline = TRUE),"times"))
                 ),
                 column(3,
                        h4("Total Words Used Only Once:"),
                        h4(strong(textOutput("singletons")))
                 )
               ),
               br(),
               br(),
               fluidRow(
                 column(12,
                        h4(strong("Number of Movies Rated Each Year, R Movies Always On Top"), 
                           style="text-align: center;"),
                        plotOutput("rating_trends")
                 )
               ),
               br(),
               fluidRow(
                 column(12,
                        h4(strong("Average Words per Rating Reason Similar Across Ratings, Except NC-17"), 
                           style="text-align: center;"),
                        plotOutput("reason_lengths")
                 )
               ),
               br(),
               fluidRow(
                 column(12,
                        h3("Short and Sweet, Four-word Rating Reasons"),
                        h4("Total Movies with Four-word Rating Reasons: ", strong(textOutput("total_shorties", 
                                                                                             inline = TRUE))),
                        h4(strong("Proportion of Movies with Four-word Rating Reasons Generally Declining"),
                           style="text-align: center;"),
                        plotOutput("shorties")
                 )
               ),
               br(),
               fluidRow(
                 column(12,
                        h3("So Much to Say, Longest Rating Reasons"),
                        dataTableOutput("longestReason")
                 )
               ),
               br(),
               br(),
               fluidRow(
                 column(12,
                        h6("*the words \"rated\", \"for\", and \"and\" were excluded from consideration
                            for most frequently occurring word in the dataset")
                 )
               ),
               br(),
               br()
             )
    ),
    
    # Content Page 1 - Top Content Concerns With Filters
    tabPanel("Top Content Concerns",
             fluidPage(
               fluidRow(
                 column(12,
                        h3("Language is a consistent top content concern, adjust filters to explore others!")
                 )
               ),
               br(),
               fluidRow(
                 column(4,
                        wellPanel(
                          div(style = "height: 120px;",
                              checkboxGroupInput("checkRating1", label = "Select Rating(s)",
                                                 choices = rating_list,
                                                 selected = c("PG", "PG-13", "R", "NC-17"))
                          )
                        )
                 ),
                 column(8,
                        wellPanel(
                          div(style = "height: 120px;",
                              sliderInput("yearSlider", 
                                          label = "Rating Year Range",
                                          min = 1992,
                                          max = 2022,
                                          sep = "",
                                          value = c(1992, 2022)
                              )
                          )
                        )
                 )
               ),
               br(),
               fluidRow(
                 column(4,
                        h4("Total Movies:", style="text-align: center;"),
                        h3(textOutput("m_count"), style="text-align: center;")
                 ),
                 column(8,
                        h4("Top 5 Content Concerns", style="text-align: center;"),
                        plotOutput("top_col", 
                                   width = "100%", 
                                   height = "250px"),
                        div("*\"sex\" includes occurrences of any of the following: sex, sexual, sexually, sexuality",
                            style = "font-size:60%;text-align: right"
                        )
                 )
               ),
               br(),
               fluidRow(
                 column(12,
                        h4("Proportion of Movie Rating Reasons Noting These Content Concerns Over Time",
                           style="text-align: center;"),
                        plotOutput("top_line"))
               ),
               br(),
               br()
             )
    ),
    
    # Content Page 2 - Rating Compare & Contrast
    tabPanel("Compare & Contrast",
             fluidPage(
               fluidRow(
                 column(12,
                        h3("We are more alike than we are different- or are we?"),
                        h4("Select Two Different Ratings and a Range of Years to Explore Similarities and Differences")
                 )
               ),
               br(),
               fluidRow(
                 column(3,
                        wellPanel(
                          div(style = "height: 120px;",
                              radioButtons("ratingRad1", 
                                           label = "Choose Your 1st Rating",
                                           choices = list("PG", "PG-13", "R", "NC-17"),
                                           selected = "PG"
                              )
                          )
                        )
                 ),
                 column(3,
                        wellPanel(
                          div(style = "height: 120px;",
                              radioButtons("ratingRad2", 
                                           label = "Choose Your 2nd Rating",
                                           choices = list("PG", "PG-13", "R", "NC-17"),
                                           selected = "PG-13"
                              )
                          )
                        )
                 ), 
                 column(6,
                        wellPanel(
                          div(style = "height: 120px;",
                              sliderInput("yearSlider2",
                                          label = "Rating Year Range", 
                                          min = 1992, 
                                          max = 2022, 
                                          sep = "",
                                          value = c(1992, 2022)
                              )
                          )
                        )
                 )
               ),
               fluidRow(
                 column(6,
                        h4("Common Words Between Ratings"),
                        br(),
                        plotOutput("commonality_wc",
                                   width = "100%",
                                   height = "565px"
                        ),
                        br()
                 ),
                 column(6,
                        h4("Comparison of Words Between Ratings"),
                        h5("Words more apt to appear in", 
                                  strong(textOutput("rating_1", 
                                             inline = TRUE)), 
                                  "movie rating reasons",
                           style="text-align:center"
                        ),
                        plotOutput("comparison_wc",
                                   width = "100%",
                                   height = "565px"
                        ),
                        h5("Words more apt to appear in", 
                                  strong(textOutput("rating_2", 
                                             inline = TRUE)), 
                                  "movie rating reasons",
                           style="text-align: center;"
                        )
                 )
               ),
               br(),
               br()
               )
             ),
    
    # Content Page 3 - Modifying Words/Phrases & Associations
    tabPanel("Modifiers & Associates",
             fluidPage(
               fluidRow(
                 column(12,
                        h3("The Devil's in the Details: Modifying Words/Phrases & Associated Words")
                 )
               ),
               br(),
               fluidRow(
                 column(3,
                        wellPanel(
                          div(style = "height: 120px;",
                              selectInput("select_word3",
                                          label = "Choose a content word",
                                          choices = word_list1,
                                          selected = "language"
                              )
                          )
                        )
                 ),
                 column(3,
                        wellPanel(
                          div(style = "height: 120px;",
                              checkboxGroupInput("checkRating3", label = "Select Rating(s)",  
                                                 choices = rating_list,
                                                 selected = c("PG", "PG-13", "R", "NC-17")
                              )
                          )
                        )
                 ),
                 column(6,
                        wellPanel(
                          div(style = "height: 120px;",
                              sliderInput("yearSlider3",
                                          label = "Rating Year Range",
                                          min = 1992,
                                          max = 2022,
                                          sep = "",
                                          value = c(1992, 2022)
                              )
                          )
                        )
                 )
               ),
               fluidRow(
                 column(4,
                        h4("Total movies:"),
                        h3(textOutput("mod_m_count"))
                 ),
                 column(4,
                        h4("Total unique modifying phrases:"),
                        h3(textOutput("mod_count"))
                 ),
                 column(4,
                        h4("Average length of rating reasons for these movies:"),
                        h3(textOutput("avg_mod_reason", inline = TRUE), " words")
                 )
               ),
               br(),
               fluidRow(
                 column(12,
                        h4("Most Common Phrases Including \"",
                           textOutput("word_selected", inline = TRUE), "\" Found in Rating Reasons",
                           style="text-align: center;"
                        ),
                        plotOutput("mod_plot")
                 )
               ),
               br(),
               fluidRow(
                 column(12,
                        h4("Other Words Most Likely To Appear in Rating Reasons with \"",
                           textOutput("word_selected2", inline = TRUE),"\"",
                           style="text-align: center;"
                        ),
                        plotOutput("assoc_plot")
                        )
                 ),
               br(),
               br() 
               )
             ),
    
           #Content Page 4 - One-Offs
           tabPanel("One-offs",
                    fluidPage(
                      fluidRow(
                        column(12,
                               h3("One-offs and Weirdos- Words Used Only Once in Rating Reasons")
                        )
                      ),
                      br(),
                      fluidRow(
                        column(3,
                               wellPanel(
                                 div(style = "height: 120px;",
                                     selectInput("select_word4",
                                                 label = "Choose a content word",
                                                 choices = word_list2,
                                                 selected = " "
                                     )
                                 )
                               )
                        ),
                        column(3,
                               wellPanel(
                                 div(style = "height: 120px;",
                                     checkboxGroupInput("checkRating4", label = "Select Rating(s)",  
                                                        choices = rating_list,
                                                        selected = c("PG", "PG-13", "R", "NC-17")
                                     )
                                 )
                               )
                        ),
                        column(6,
                               wellPanel(
                                 div(style = "height: 120px;",
                                     sliderInput("yearSlider4",
                                                 label = "Rating Year Range",
                                                 min = 1992,
                                                 max = 2022,
                                                 sep = "",
                                                 value = c(1992, 2022)
                                     )
                                 )
                               )
                              )
                      ),
                      br(),
                      br(),
                      fluidRow(
                        column(4,
                               h4("Total movies with one-off words in rating reason:"),
                               h3(textOutput("singleton_m_count"))
                        ),
                        column(4,
                               h4("Total unique one-off words in these rating reasons:"),
                               h3(textOutput("sing_count"))
                        ),
                      column(4,
                             h4("Average length of rating reasons for these movies:"),
                             h3(textOutput("avg_sing_reason", inline = TRUE), " words")
                      )
                  ),
                  fluidRow(
                    column(12,
                           h4("Mid-90's were the Peak of Unique Word Use in MPAA Rating Reasons",
                              style="text-align: center;"
                              ),
                           plotOutput("singleton_time_plot")
                           )
                  ),
                  br(),
                  fluidRow(
                    column(12,
                           h4("One-Off word by Rating Trends Look Similar to Overall Total Movies by Rating",
                              style="text-align: center;"
                           ),
                           plotOutput("singleton_rating_plot")
                    )
                  ),
                  br(),
                  fluidRow(
                    column(12,
                           h3("What are these weirdo words? Check them out below!"),
                           dataTableOutput("weirdo_table")
                    )
                  ),
                  br(),
                  br()
                )
           ),
          
          # Content Page 5 - Show me the $$$
          tabPanel("Show Me the $$$",
                    fluidPage(
                      fluidRow(
                        column(12,
                            h3("Ratings, Reasons, and Dollar Bills - Financial Performance")
                            )
                        ),
                   br(),
                   fluidRow(
                     column(3,
                            wellPanel(
                              div(style = "height: 150px;",
                                  selectInput("select_word5",
                                              label = "Choose a content word",
                                              choices = word_list2,
                                              selected = " "
                                              )
                                  )
                              )
                            ),
                     column(3,
                            wellPanel(
                              div(style = "height: 150px;",
                                  checkboxGroupInput("checkRating5", label = "Select Rating(s)",  
                                                     choices = rating_list5,
                                                     selected = c("G", "PG", "PG-13", "R", "NC-17")
                                                     )
                                  )
                              )
                            ),
                     column(6,
                            wellPanel(
                              div(style = "height: 150px;",
                                  sliderInput("yearSlider5",
                                              label = "Rating Year Range",
                                              min = 1992,
                                              max = 2022,
                                              sep = "",
                                              value = c(1992, 2022)
                                              )
                                  )
                              )
                            )
                     ),
                   br(),
                   fluidRow(
                     column(4,
                           h4("Total movies fitting criteria that have revenue data:"),
                           h3(textOutput("money_m_count"))
                     ),
                     column(4,
                            h4("Total gross for all movies fitting criteria:"),
                            h3("$", textOutput("all_money", inline = TRUE), "M")
                     ),
                     column(4,
                            h4("Highest grossing movie fitting these criteria:"),
                            h3(p(em(textOutput("big_money_title", inline = TRUE)), ","),
                               p("$", textOutput("big_money_gross", inline = TRUE), "M", " in ", textOutput("big_money_year", inline = TRUE)))
                            )
                     ),
                   br(),
                   br(), 
                   fluidRow(
                     column(12,
                            h4("Broader Audience Age Likely Related to Higher Average Revenue",
                               style="text-align: center;"
                            ),
                            plotOutput("gross_rating_year_plot")
                            )
                     ),
                   br(),
                   br(),
                   fluidRow(
                     column(12,
                            div(style = "height:600px",
                              h4("General Trends in Revenue & Rating Reason Language Often Violated by Top Movies",
                                   style="text-align: center;"
                                   ),
                                plotOutput("gross_content_plot")
                            )
                     )
                     ),
                   br(),
                   br(),
                   fluidRow(
                     column(12,
                            h4("Explore the Details of Top Performers in this Group of Movies ",
                               style="text-align: center;"
                               ),
                            dataTableOutput("big_money_table")
                            )
                          )
                    )
                   ),
    
    # Content Page 5 - Show me the $$$
    tabPanel("Not All That Glitters...",
             fluidPage(
               fluidRow(
                 column(12,
                        h3("What do the people want? A look at movie popularity")
                 )
               )
             )
      )
    )
)
