
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("GCAM Dashboard"),

  # Sidebar with user controls
  sidebarLayout(
    sidebarPanel(
        fileInput('projectFile', 'GCAM Project Data File'),
        selectInput('plotScenario', 'Select Scenario to Plot', choices=list()),
        selectInput('plotQuery', 'Select Query to Plot', choices=list()),
        checkboxInput('diffCheck', 'Plot Difference vs Another Scenario'),
        conditionalPanel(
            condition = "input.diffCheck == true",
            selectInput('diffScenario', 'Select Difference Scenario', choices=list())
        )
    ),

    # main display area
    mainPanel(
        h2('Project Status'),
        h3('Project file: '),
        verbatimTextOutput('projFilename'),
        h3('Scenarios in project file:'),
        verbatimTextOutput('scenarios'),
        selectInput('scenarioInput','Select scenarios',choices=list(), multiple=TRUE),
        h3('Queries present in ALL selected scenarios:'),
        verbatimTextOutput('queries')
    )
  )
))
