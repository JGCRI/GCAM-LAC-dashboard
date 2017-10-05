
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinythemes)
library(GCAMdashboard)


shinyUI(fluidPage(theme="style.css",
  dashboardPage(
    dashboardHeader(title = 'Latin America GCAM Dashboard', titleWidth = 450),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Water", tabName = "water", icon = icon("tint", lib = "font-awesome")),
        menuItem("Energy", tabName = "energy", icon = icon("bolt", lib = "font-awesome")),
        menuItem("File Manager", tabName = "file", icon = icon("file", lib = "font-awesome"))
      ),
      fileInput('projectFile', 'GCAM Project Data File'),
      sidebarMenu(
        menuItem("File Info", icon = icon("dashboard"),
           p(" File size: ", textOutput('projectSize', inline = TRUE)),
           p(" Scenarios: ", textOutput('scenarios', inline = TRUE)),
            selectInput('scenarioInput','Select scenarios to see available queries:',
                        choices=list(), multiple=TRUE),
           p(" Queries present in ALL selected scenarios: ", textOutput('queries'))
        )
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
              tabBox(title = 'Water', side = 'right',
                  id = "waterTabset",
                  tabPanel("Supply",
                    plotOutput('landingPlot3', height='250px')),
                  tabPanel("Demand",
                    plotOutput('landingPlot5', height='250px')),
                  tabPanel("Scarcity",
                    plotOutput('landingPlot6', height='250px'))
              ),
              box(title = 'Crop Production by AEZ',align='center',
                  solidHeader = TRUE, status = "success",
                  plotOutput('landingPlot4', height='250px'))
          )
        ),
        # upload
        tabItem(tabName = "file",
          fluidRow(
            column(1, selectInput('plotScenario', 'Select Scenario to Plot', choices=list())),
            column(1, selectInput('plotQuery', 'Select Query to Plot', choices=list())),
            column(3, checkboxInput('inclSpatial', 'Include Spatial Queries', value=TRUE))
          ),
          checkboxInput('diffCheck', 'Plot Difference vs Another Scenario'),
          conditionalPanel(
            condition = "input.diffCheck == true",
            fluidRow(column(8,
                            selectInput('diffScenario', 'Select Difference Scenario', choices=list())))
          )
        ),
        tabItem(tabName = "water",
          fluidRow(
            column(6,
              box(title = "Map Type", width = NULL, status = "warning",
                  selectInput('mapProjection', label = NULL,
                              choices=c(Global='global', USA='usa', China='china',
                                        Africa='africa', `Latin America and Caribbean`='lac'),
                              selected = 'lac')
              ),
              box(width = NULL, height = '450px',
                 status = "primary",
                 h3(textOutput('mapName', inline=TRUE),align='center'),
                 plotOutput('mapPlot')
              ),
              box(width = NULL,
                 sliderInput('mapYear', NULL, min=2005, max=2100, step=5, value=2050,
                             sep='', animate = TRUE)
              )
            ),

            column(6,
              box(title = "Water withdrawals for irrigation", width = NULL,
                 solidHeader = TRUE, status = "success", height = '350px'),
              box(title = "Water withdrawals for electricity", width = NULL,
                 solidHeader = TRUE, status = "success", height = '350px')
            )
          )
        ),
        tabItem(tabName = "energy",
          fluidRow(
            column(width = 8,
              box(status = "primary", width = NULL,
                plotOutput('timePlot', height='400px')
              ),
              box(title="Options", status = "primary", width = NULL,
                  selectInput('tvSubcatVar', 'Break totals into subcategories by:',
                              choices=c('none','region'))
              )
            ),

            column(width = 4,
              box(title = "Filter by Region", status = "primary", solidHeader = TRUE,
                width = NULL, height = '420px',
                tableOutput("regionFilter"),
                actionButton('rgnSelectAll', 'Select all regions'),

                bsCollapse(open="Latin America and Caribbean",
                  bsCollapsePanel(title='Latin America and Caribbean', style="success",
                    actionButton('rgns2All', 'Deselect All'),
                    checkboxGroupInput('tvRgns2', NULL, choices=lac.rgns, selected = lac.rgns)
                  ),
                  bsCollapsePanel(title="Africa",
                    actionButton('rgns1All', 'Select All'),
                    checkboxGroupInput('tvRgns1', NULL, choices=africa.rgns)
                  ),
                  bsCollapsePanel(title="Asia-Europe",
                    actionButton('rgns4All', 'Select All'),
                    checkboxGroupInput('tvRgns4', NULL, choices=europe.rgns)
                  ),
                  bsCollapsePanel(title="Asia-Pacific",
                    actionButton('rgns5All', 'Select All'),
                    checkboxGroupInput('tvRgns5', NULL, choices=asiapac.rgns)
                  ),
                  bsCollapsePanel(title="North America",
                    actionButton('rgns3All', 'Select All'),
                    checkboxGroupInput('tvRgns3', NULL, choices=north.america.rgns)
                  )
                )
              )
            )
          )
        ) #tabItem
      ) # tabItems
    ) # dashboardBody
  ) #dashboardPage
))

