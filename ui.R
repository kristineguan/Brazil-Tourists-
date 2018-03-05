library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Brazil Traveling Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput('unit',
                  'Trourists count by month/year',
                  choices = c('Month','Year'),selected = 'Month'),
      sliderInput("bins",
                  "Number of bins in histogram:",
                  min = 1,
                  max = 30,
                  value = 15),
      sliderInput('time',
                  'Year range',
                  sep = "",
                  min=1989,
                  max=2015,
                  value=c(1989,2015)
                  )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type='tab',
                  tabPanel('Dataset Sample',tableOutput('data')),
                  tabPanel('Data Summary',verbatimTextOutput('summary')),
                  tabPanel('Distribution',plotOutput("distPlot"))
    )
  )
)))
