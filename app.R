source("imdb5000Script.R")
source("imdb5000Plots.R")

# Define UI ----
ui <- fluidPage(
    titlePanel("Movies highest grossing genres and scores based on IMDB5000 dataset from Kaggle"),
    
    sidebarLayout( 
                sidebarPanel(checkboxGroupInput("genrepicks",
                                                h3("Pick your genres to estimate the gross and score"),
                                                choices = list("Action","Adventure","Animation" ,"Biography","Comedy", 
                                                               "Crime","Documentary" , "Drama" ,
                                                               "Family" , "Fantasy" , "History" ,
                                                               "Horror" , "Music" , "Musical" , "Mystery" , 
                                                               "Romance" , "Sci-Fi" = "`Sci-Fi`" ,
                                                               "Sport" , "Thriller" , "War" , "Western"
                                                               )
                                                
                                                )
                             ),
                  mainPanel(
                      tabsetPanel(
                        tabPanel("Information",
                                 h3("Information about the application/research questions"),
                                 p("With this application i've tried to answer the following questions:"),
                                 tags$ul(
                                   tags$li("What genres are making the most money? (Estimated gross per genre tab)"),
                                   tags$li("What genres get the best ratings? (Estimated score per genre tab)"),
                                   tags$li("How does this change over the years? (Estimated score/gross per genre over the decades tabs)")
                                 ),
                                 p("Other than these questions i was also curious about the effect of a certain genre on the gross/scoring and you can
                                   see this in the remaining 2 tabs."),
                                 p("Because most movies contained multiple genres i used a linear model to identify the impact of a genre on the gross/score 
                                   and used this model to answer the above research questions."),
                                 p("Using the inputs on the left you can see an calculated estimation of the gross/scoring of a genre or multiple of your choosing
                                   on the bottom of this/other pages (as can been seen in the plots about effect adding some genres actually result in a lower gross/score)."),
                                 h3("Information about the data"),
                                 p("The data is gathered from kaggle and gave information about 5000 movies sourced from the IMDB website. In this dataset you
                                   could find information like the gross, genres, title, any well known actors, how many actors were on the movies' poster etcetera.
                                   I've selected only the genres, gross, title_year and imdb_score because these were all i needed to answer the research questions.
                                  After removing any rows which had NA's in any of the above features i kept a total of around 4200 movies. After further pruning i've
                                  also removed the data for any movies before 1980 and changed movies from a year to a decade. This is because there were only 100 movies 
                                  total in the period of 1930-1980 and the amount of different years resulted in very unclear and busy plots.")
                                 ),
                          tabPanel("Estimated gross per genre", plotOutput("estimatedGrossPlot")),
                          tabPanel("Estimated score per genre", plotOutput("estimatedScorePlot")),
                          tabPanel("Effect of genre on the estimated gross", plotOutput("effectGrossPlot")),
                          tabPanel("Effect of genre on the estimated score", plotOutput("effectScorePlot")),
                          tabPanel("Estimated gross per genre over the decades", plotOutput("grossOverDecadesPlot")),
                          tabPanel("Estimated score per genre over the decades", plotOutput("scoreOverDecadesPlot"))
                      ),
                      br(),
                      hr(),
                      h3("Estimations based on your chosen genres"),
                      textOutput("estimations"),
                      hr(),
                      p("Chris Santema 0927765 Minor Data Science 2019-2020")
                  ))
)

# Define server logic ----
server <- function(input, output) {
  
    output$estimatedGrossPlot <- renderPlot({
      estimatedGrossPlot
    })
      output$estimatedScorePlot<- renderPlot({
        estimatedScorePlot
    })
    output$effectGrossPlot <- renderPlot({
      effectGrossPlot
    })
    output$effectScorePlot <- renderPlot({
      effectScorePlot
    })
    output$grossOverDecadesPlot <- renderPlot({
      grossOverDecadesPlot
    })
    output$scoreOverDecadesPlot <- renderPlot({
      scoreOverDecadesPlot
    })
    
    output$estimations <- renderText({
      paste("Estimated score: ", calcScore(input$genrepicks), "and gross in millions: ", calcGross(input$genrepicks))
    })

}

# Run the app ----
shinyApp(ui = ui, server = server)
