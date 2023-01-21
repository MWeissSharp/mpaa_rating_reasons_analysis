
ui <- tagList(
  
  navbarPage(
          theme = shinytheme("flatly"),
          "MPAA Rating Reasons",
          
           # Landing Page - Project intro
           tabPanel("Intro",
              fluidPage(h2("Swashbuckling Violence and Nonstop Ninja Action"),
                        h4("A Text Analysis of Movie Rating Reasons"),
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
                        tags$div(img(src = "ratings-anatomy.png", height = 450, width = 600), 
                            style="text-align: center;"),
                        p(h6("Image from filmratings.com", style="text-align: right;"))
                        # p(strong("The App")),
                        # p("The goal of this app is to offer a fun way to dive in and explore the 
                        #   content of these rating reasons. Pulled directly from", 
                        #   a("filmratings.com", href = "https://www.filmratings.com/"), "the d" )
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
                              h4(strong("Number of Movies Rated Each Year, R Movies Always On Top")),
                              plotOutput("rating_trends")
                         )
                       ),
                     br(),
                     fluidRow(
                       column(12,
                              h4(strong("Average Words per Rating Reason Similar Across Ratings, Except NC-17")),
                              plotOutput("reason_lengths")
                              )
                       ),
                     br(),
                     fluidRow(
                       column(12,
                              h4(strong("To the Point, Four-word Rating Reasons")),
                              h4("Total Movies with Four-word Rating Reasons: ", strong(textOutput("total_shorties", 
                                                                                                   inline = TRUE))),
                              h4("Proportion of Movies with Four-word Reasons Generally Declining"),
                              plotOutput("shorties")
                              )
                       ),
                     br(),
                     fluidRow(
                       column(12,
                              h4(strong("So Much to Say, Longest Rating Reasons")),
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
                     )
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
                            h4("Proportion of Rating Reasons Noting Top Content Concerns Over Time",
                               style="text-align: center;"),
                            plotOutput("top_line"))
                   )
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
                                                   label = "Choose Your 1st Rating",
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
                                h5(strong("Words more apt to be in", 
                                   textOutput("rating_1", 
                                              inline = TRUE), 
                                   "movie rating reasons"),
                                   style="text-align:center"
                                   ),
                                plotOutput("comparison_wc",
                                           width = "100%",
                                           height = "565px"
                                           ),
                                h5(strong("Words more apt to be in", 
                                   textOutput("rating_2", 
                                              inline = TRUE), 
                                   "movie rating reasons"),
                                   style="text-align: center;"
                                   )
                                )
                         )
                     )
                   ),
           
          # Content Page 3 - Modifying Words/Phrases
          tabPanel("Modifiers",
                   fluidPage(
                     fluidRow(
                       column(12,
                              h3("The Devil's in the Details: Modifying Words and Phrases")
                       )
                     ),
                     br(),
                     fluidRow(
                       column(3,
                              wellPanel(
                                div(style = "height: 120px;",
                                    selectInput("select_word",
                                                label = "Choose a word",
                                                choices = word_list,
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
                              h4("Most Commonly Used Phrases Including \"",
                                 textOutput("word_selected", inline = TRUE), "\" Found in Rating Reasons",
                                 style="text-align: center;"
                                 ),
                              plotOutput("mod_plot")
                              )
                     )
                   )
          ),
          
          # Content Page 4 - Let's Talk about S.E.X.
          # tabPanel("S.E.X",
          #          fluidPage(h3("Let's talk about sex, baby...")
          #          )
          # ),
          
          # Content Page 5 - One-Offs
          # tabPanel("One-offs",
          #          fluidPage(h3("One-offs and Weirdos")
          #          )
          # )
          
  )
)
