
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
                        p("Image from filmratings.com", style="text-align: right;")
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
                     fluidRow(
                       column(3,
                              h4("Total Rating Reasons Analyzed:", 
                                 style="text-align: center;"),
                              h3(textOutput("total_movies"), 
                                 style="text-align: center;")
                              ),
                       column(3,
                              h4("Total Distinct Words:", 
                                 style="text-align: center;"),
                              h3()
                              ),
                       column(3,
                              h4("Most Frequently Occurring Word:", 
                                 style="text-align: center;"),
                              h3("\"Language\" appears",textOutput("total_language"),"times", 
                                 style="text-align: center;")
                              ),
                       column(3,
                              h4("Total Words Used Only Once:", 
                                 style="text-align: center;"),
                              h3()
                              )
                     ),
                     br(),
                     fluidRow(
                       column(12,
                              h4("Rated R Movies Always On Top"),
                              plotOutput("rating_trends")
                         )
                       ),
                     fluidRow(
                       column(12,
                              h4("Average Reason Lengths Similar Across Ratings - Except NC-17"),
                              plotOutput("reason_lengths")
                              )
                       ),
                     fluidRow(h4("Longest Rating Reason")),
                     fluidRow(
                       column(12,
                         dataTableOutput("longestReason")
                         )
                       )
                     )
                   ),
          
          # Content Page 1 - Top Content Concerns With Filters
          tabPanel("Top Content Concerns",
                   fluidPage(
                     fluidRow(
                       column(12,
                              h3("What types of content have been noted most often? 
                                 Choose your filters and see!")
                       )
                   ),
                   fluidRow(
                     column(4,
                            wellPanel(
                              checkboxGroupInput("checkRating1", label = "Select Rating(s)", 
                                                 choices = rating_list,
                                                 selected = c("PG", "PG-13", "R", "NC-17"))
                              )
                            ),
                     column(8,
                            wellPanel(
                              sliderInput("yearSlider", 
                                          label = "Rating Year Range", 
                                          min = 1992, 
                                          max = 2022, 
                                          sep = "",
                                          value = c(1992, 2022)
                                          )
                              )
                            )
                   ),
                   fluidRow(
                     column(4,
                            h4("Number of movies fitting these criteria:"),
                            h3(textOutput("m_count"))
                     ),
                     column(8,
                            plotOutput("top_col", 
                                       width = "100%", 
                                       height = "250px"))
                   ),
                   fluidRow(
                     column(12,
                            plotOutput("top_line"))
                   )
                  )
          ),
          
           # Content Page 2 - Rating Compare & Contrast
          tabPanel("Compare & Contrast",
                   fluidPage(
                     fluidRow(
                       column(12,
                              h3("We are more alike than we are different- or are we?")
                       )
                     ),
                     fluidRow(
                         column(3,
                                wellPanel(
                                  radioButtons("ratingRad1", 
                                               label = "Choose Your 1st Rating",
                                               choices = list("PG", "PG-13", "R", "NC-17"), 
                                               selected = "PG"
                                               )
                                  )
                                ),
                         column(3,
                                wellPanel(
                                  radioButtons("ratingRad2",
                                               label = "Choose Your 2nd Rating",
                                               choices = list("PG", "PG-13", "R", "NC-17"),
                                               selected = "PG-13"
                                               )
                                  )
                                ), 
                         column(6,
                                wellPanel(
                                  sliderInput("yearSlider2", 
                                              label = "Rating Year Range", 
                                              min = 1992, 
                                              max = 2022, 
                                              sep = "",
                                              value = c(1992, 2022)
                                              )
                                  )
                                )
                         ),
                     fluidRow(
                         column(6,
                                plotOutput("commonality_wc", 
                                           width = "100%", 
                                           height = "510px"
                                           )
                                ),
                         column(6,
                                plotOutput("comparison_wc", 
                                           width = "100%", 
                                           height = "510px"
                                           )
                                )
                         ),
                     
                              
                               
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
                     fluidRow(
                       column(3,
                              wellPanel(
                                selectInput("select_word",
                                            label = "Choose a word",
                                            choices = word_list,
                                            selected = "language"
                                )
                              )
                       ),
                       column(3,
                              wellPanel(
                                checkboxGroupInput("checkRating3", label = "Select Rating(s)", 
                                                   choices = rating_list,
                                                   selected = c("PG", "PG-13", "R", "NC-17")
                                                   )
                                )
                              ),
                              column(6,
                                     wellPanel(
                                       sliderInput("yearSlider3", 
                                                   label = "Rating Year Range", 
                                                   min = 1992, 
                                                   max = 2022, 
                                                   sep = "",
                                                   value = c(1992, 2022)
                                       )
                                     )
                              )
                     ),
                     fluidRow(
                       column(4,
                              h4("Number of movies fitting these criteria:"),
                              h3(textOutput("mod_m_count"))
                              ),
                       column(4,
                              h4("Number of unique modifying phrases"),
                              h3(textOutput("mod_count"))
                              ),
                       column(4,
                              h4("Average length rating reasons including this word:"),
                              h3(textOutput("avg_mod_reason"), " words")
                              )
                     ),
                     fluidRow(
                       column(12,
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
