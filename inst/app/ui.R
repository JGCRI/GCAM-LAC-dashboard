
library(shiny)
library(shinydashboard)
library(shinythemes)
library(GCAMdashboard)


shinyUI(fluidPage(theme="style.css",
  dashboardPage(
    dashboardHeader(title = 'Latin America GCAM Dashboard', titleWidth = 450),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("File Upload", tabName = "file", icon = icon("tint", lib = "font-awesome")),
        menuItem("Water", tabName = "water", icon = icon("tint", lib = "font-awesome")),
        menuItem("Energy", tabName = "energy", icon = icon("bolt", lib = "font-awesome")),
        fileInput('projectFile', 'GCAM Project Data File')
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
          fluidRow(
            box(title = 'Hydrogen production by technology',
                solidHeader = TRUE, status = "primary",
                plotOutput('landingPlot', height='250px')),
            box(title = 'Population by Region',
                solidHeader = TRUE, status = "primary",
                plotOutput('landingPlot2', height='250px'))
          ),
          fluidRow(
              tabBox(title = 'Water Scarcity',
                  id = "waterTabset",
                  tabPanel("Supply",
                    plotOutput('landingPlot3', height='250px')),
                  tabPanel("Demand",
                    plotOutput('landingPlot3', height='250px')),
                  tabPanel("Scarcity",
                    plotOutput('landingPlot3', height='250px'))
              ),
              box(title = 'Crop Production by AEZ',align='center',
                  solidHeader = TRUE, status = "success",
                  plotOutput('landingPlot4', height='250px'))
          )
        ),
        # upload
        tabItem(tabName = "file",
          fluidRow(
            box(
              h2('Select a file to upload')

            ),
              fileInput('projectFile', 'GCAM Project Data File')
            # h3('Project file: '),
            # verbatimTextOutput('projFilename'),
            # h3('Project data size: '),
            # verbatimTextOutput('projectSize'),
            # h3('Scenarios in project file:'),
            # verbatimTextOutput('scenarios'),
            # selectInput('scenarioInput','Select scenarios to see available queries:',
            #             choices=list(), multiple=TRUE),
            # h3('Queries present in ALL selected scenarios:'),
            # verbatimTextOutput('queries'),
            # fluidRow(
            #   column(8,
            #          selectInput('plotScenario', 'Select Scenario to Plot', choices=list()))),
            # fluidRow(
            #   column(8,
            #          selectInput('plotQuery', 'Select Query to Plot',
            #                      choices=list())),
            #   column(4,
            #          checkboxInput('inclSpatial', 'Include Spatial Queries', value=TRUE))
            # ),
            # checkboxInput('diffCheck', 'Plot Difference vs Another Scenario'),
            # conditionalPanel(
            #   condition = "input.diffCheck == true",
            #   fluidRow(column(8,
            #                   selectInput('diffScenario', 'Select Difference Scenario', choices=list())))
            # )
          )
        ),
        tabItem(tabName = "water",
          fluidRow(
            h3(textOutput('mapName', inline=TRUE),align='center'),
            plotOutput('mapPlot',height='600px'),
            sliderInput('mapYear', 'Year', width='80%', min=2005, max=2100, step=5, value=2050,
                        sep='', animate = TRUE),
            h3('Options'),
            selectInput('mapProjection', 'Map Type',
                        choices=c(Global='global',
                                  USA='usa',
                                  China='china',
                                  Africa='africa',
                                  `Latin America and Caribbean`='lac'),
                        selected = 'global')
          )
        ),
        tabItem(tabName = "energy",
          fluidRow(
            plotOutput('timePlot', height='600px'),
            h3('Options'),
            selectInput('tvSubcatVar', 'Break totals into subcategories by:',
                        choices=c('none','region')),
            h4('Regions'),
            fluidRow(
              checkboxInput('tvFilterCheck',
                            'Limit plot to selected regions')),
            fluidRow(
              column(1,checkboxGroupInput('tvRgns5', 'Asia-Pacific',
                                          choices=asiapac.rgns)),
              column(1,checkboxGroupInput('tvRgns2',
                                          'Latin America and Caribbean',
                                          choices=lac.rgns), offset=2),
              column(1,checkboxGroupInput('tvRgns1', 'Africa',
                                          choices=africa.rgns), offset=2),
              column(1,checkboxGroupInput('tvRgns4', 'Europe',
                                          choices=europe.rgns), offset=2),
              column(1,checkboxGroupInput('tvRgns3', 'North America',
                                          choices=north.america.rgns), offset=2)
            )
          )
        ) #tabItem
      ) # tabItems
    ) # dashboardBody
  ) #dashboardPage
))

# # Define UI for application that draws a histogram
# shinyUI(fluidPage(theme = shinytheme("flatly"),
#   # Sidebar
#   fluidRow(
#     column(2,
#       # Sidebar with user controls
#       sidebarLayout(
#         sidebarPanel(
#           fileInput('projectFile', 'GCAM Project Data File'),
#           fluidRow(
#             column(8,
#                    selectInput('plotScenario', 'Select Scenario to Plot', choices=list()))),
#           fluidRow(
#             column(8,
#                    selectInput('plotQuery', 'Select Query to Plot',
#                                choices=list())),
#             column(4,
#                    checkboxInput('inclSpatial', 'Include Spatial Queries', value=TRUE))
#           ),
#           checkboxInput('diffCheck', 'Plot Difference vs Another Scenario'),
#           conditionalPanel(
#             condition = "input.diffCheck == true",
#             fluidRow(column(8,
#                            selectInput('diffScenario', 'Select Difference Scenario', choices=list())))
#           )
#         )
#       )
#     ),
#     column(10,
#       # main display area
#       mainPanel(
#         tabsetPanel(
#           tabPanel('Project Info',
#             h2('Project Information'),
#             h3('Project file: '),
#             verbatimTextOutput('projFilename'),
#             h3('Project data size: '),
#             verbatimTextOutput('projectSize'),
#             h3('Scenarios in project file:'),
#             verbatimTextOutput('scenarios'),
#             selectInput('scenarioInput','Select scenarios to see available queries:',
#                         choices=list(), multiple=TRUE),
#             h3('Queries present in ALL selected scenarios:'),
#             verbatimTextOutput('queries')
#           ),
#
#           tabPanel('Map View',
#             h3(textOutput('mapName', inline=TRUE),align='center'),
#             plotOutput('mapPlot',height='600px'),
#             sliderInput('mapYear', 'Year', width='80%', min=2005, max=2100, step=5, value=2050,
#                         sep='', animate = TRUE),
#             h3('Options'),
#             selectInput('mapProjection', 'Map Type',
#                         choices=c(Global='global',
#                         USA='usa',
#                         China='china',
#                         Africa='africa',
#                         `Latin America and Caribbean`='lac'),
#                         selected = 'global'),
#             # placeholders downward to make room for the map projection selector
#             br(),
#             br(),
#             br(),
#             br()
#           ),
#
#           tabPanel('Time View',
#             plotOutput('timePlot', height='600px'),
#             h3('Options'),
#             selectInput('tvSubcatVar', 'Break totals into subcategories by:',
#                         choices=c('none','region')),
#             h4('Regions'),
#             fluidRow(
#             checkboxInput('tvFilterCheck',
#                           'Limit plot to selected regions')),
#             fluidRow(
#                 column(1,checkboxGroupInput('tvRgns5', 'Asia-Pacific',
#                                             choices=asiapac.rgns)),
#                 column(1,checkboxGroupInput('tvRgns2',
#                                             'Latin America and Caribbean',
#                                             choices=lac.rgns), offset=2),
#                 column(1,checkboxGroupInput('tvRgns1', 'Africa',
#                                             choices=africa.rgns), offset=2),
#                 column(1,checkboxGroupInput('tvRgns4', 'Europe',
#                                             choices=europe.rgns), offset=2),
#                 column(1,checkboxGroupInput('tvRgns3', 'North America',
#                                    choices=north.america.rgns), offset=2)
#             )
#           ),
#
#           tabPanel('Landing Page',
#             h3('GCAM Latin America'),
#             fluidRow(
#                column(6, h5('Hydrogen production by technology',align='center'),
#                       plotOutput('landingPlot', height='250px')),
#                column(6, h5('Population by Region',align='center'),
#                       plotOutput('landingPlot2', height='250px'))
#             ),
#             fluidRow(
#                column(6, h5('Water Scarcity',align='center'),
#                       plotOutput('landingPlot3', height='250px')),
#                column(6, h5('Crop Production by AEZ',align='center'),
#                       plotOutput('landingPlot4', height='250px'))
#             )
#           )
#         )  # tabset panel
#       ) #  main Panel
#     ) # sidebar layout
#   ) # fluid row
# ))
