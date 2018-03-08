library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Brazil Traveling Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons('unit',
                  'Trourists count time unit',
                  list('Month','Year'), selected = 'Month'),
      sliderInput('time',
                  'Year range',
                  sep = "",
                  min=1989,
                  max=2015,
                  value=c(1989,2015)
      ),
      sliderInput("bins",
                  "Number of bins in histogram:",
                  min = 1,
                  max = 30,
                  value = 15)
      #radioButtons('model',
      #             'Select models',
      #             list('Auto selected Model','Manually selected Model'), 
      #             selected = 'Manually selected Model')
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type='tab',
                  tabPanel('Dataset Sample',tableOutput('data')),
                  tabPanel('Data Summary',verbatimTextOutput('summary')),
                  tabPanel('Distribution',plotOutput("distPlot",
                                                     height='1000px',
                                                     width = '800px')),
                  tabPanel('Prediction',plotOutput('predPlot'))
    )
  )
)))
