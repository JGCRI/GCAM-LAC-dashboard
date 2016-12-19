
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("GCAM Dashboard"),

  # Sidebar with user controls
  sidebarLayout(
    sidebarPanel(
        fileInput('projectFile', 'GCAM Project Data File')
    ),

    # main display area
    mainPanel(
        h2('Project Status'),
        h3('Project file: '),
        textOutput('projFilename'),
        h3('Scenarios in project file:'),
        verbatimTextOutput('scenarios'),
        h3('Queries in first scenario:'),
        verbatimTextOutput('queries')
    )
  )
))
