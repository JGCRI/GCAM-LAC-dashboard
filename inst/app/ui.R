
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinythemes)
library(GCAMdashboard)


shinyUI(fluidPage(theme="style.css",
  dashboardPage(
    dashboardHeader(title = 'GCAM Latin America',
                    # by making an li element with class dropdown we can trick
                    # it into accepting our link
                    tags$li(span("Project File"), class = "dropdown fileSelectLabel"),
                    tags$li(span(selectInput('fileList', label = NULL, choices=list(), width = '200px')),
                            id = "fileSelect",
                            class = "dropdown"),
                    tags$li(actionButton('triggerUploadModal', NULL, icon("upload", lib = "font-awesome")),
                            class = "dropdown")
                    ),

    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Maps", tabName = "maps", icon = icon("map", lib = "font-awesome")),
        menuItem("Explore", tabName = "energy", icon = icon("search", lib = "font-awesome")),
        menuItem("Scenarios", tabName = "scenarios", icon = icon("line-chart", lib = "font-awesome")),
        menuItem("File Explorer", tabName = "file", icon = icon("file", lib = "font-awesome"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
          fluidRow(
            column(3,
              tabBox(title = NULL, side = 'left', width = NULL,
                     id = "waterTabset",
                     tabPanel("Supply",
                              plotOutput('waterSupplyPlot', height='580px')),
                     tabPanel("Demand",
                              plotOutput('waterDemandPlot', height='580px')),
                     tabPanel("Scarcity",
                              plotOutput('waterScarcityPlot', height='580px'))
              )
            ),
            column(3,
              box(title = 'Agriculture Production',align='center', width = NULL,
                  solidHeader = TRUE, status = "primary",
                  plotOutput('landingPlot3', height='583px'))
            ),
            column(6,
              box(title = 'Primary Energy Consumption by Fuel', width = NULL,
                  solidHeader = TRUE, status = "primary", collapsible = T,
                  footer = radioButtons('lp1toggle', NULL,
                                        c('Reference Scenario', 'Not Reference Scenario'),
                                        inline = T),
                  plotOutput('landingPlot1', height='250px')),
              box(title = 'CO2 Emissions', width = NULL,
                  solidHeader = TRUE, status = "primary",
                  footer = radioButtons('lp2toggle', NULL,
                                        c('Reference Scenario', 'Not Reference Scenario'),
                                        inline = T),
                  plotOutput('landingPlot2', height='250px'))
            )
          )
        ),
        tabItem(tabName = "maps",
          fluidRow(
            column(6,
              box(width = NULL, status = "primary",
                  fluidRow(column(6,
                    selectInput('mapExtent', label = "Map Extent", width = NULL,
                                choices=c(Global='global', USA='usa', China='china',
                                        Africa='africa', `Latin America`='lac'),
                                selected = 'lac')
                    ),
                    column(6,
                      selectInput('mapType', label = "Map Type", width = NULL,
                                  choices=c(`GCAM Regions`='regions',
                                            China='china',
                                            Countries='countries',
                                            Basins='basins',
                                            `No Borders`='none'),
                                  selected = 'lac')
                  ))
              ),
              box(width = NULL, height = '450px',
                 status = "primary",
                 h3(textOutput('mapName', inline=TRUE),align='center'),
                 plotOutput('mapPlot')
              ),
              box(width = NULL,
                 sliderInput('mapYear', NULL, min=2005, max=2100, step=5,
                             value=2050, sep='', animate = TRUE)
              )
            ),

            column(6,
              box(title = "Water withdrawals for irrigation", width = NULL,
                 solidHeader = TRUE, status = "primary", height = '350px'),
              box(title = "Water withdrawals for electricity", width = NULL,
                 solidHeader = TRUE, status = "primary", height = '350px')
            )
          )
        ),
        tabItem(tabName = "energy",
          fluidRow(
            column(width = 8,
              box(title=NULL, status="primary", width=NULL,
              fluidRow(
                column(width = 5, selectInput('scenarioInput', 'Scenario:',
                            choices=list())
                ),
                column(width = 5, conditionalPanel(condition = "input.diffCheck == true",
                      selectInput('diffScenario', 'Difference Scenario:', choices=list()))
                ),
                column(width = 2, checkboxInput('diffCheck', 'Add Difference Scenario')
                )
              )),
              box(status = "warning", width = NULL,
                plotOutput('timePlot', height='400px',
                           hover = hoverOpts("energyHover", delay = 50, delayType = 'throttle')),
                uiOutput('hoverInfo')
              ),
              box(title=NULL, status = "primary", width = NULL,
                selectInput('tvSubcatVar', 'Break totals into subcategories by:',
                            choices=c('none','region'))
              )
            ),

            column(width = 4,
              box(title = "Filter by Region", status = "primary", solidHeader = TRUE,
                width = NULL, height = '550px',
                tableOutput("regionFilter"),
                actionButton('rgnSelectAll', 'Select all regions'),
                br(),

                bsCollapse(open="Latin America and Caribbean",
                  bsCollapsePanel(title='Latin America and Caribbean', style="primary",
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
              ),
              box(status = "primary", width = NULL,
                  selectInput('plotQuery', label="Plot Variable", choices=list())
              )
            )
          )
        ),
        tabItem(tabName = "scenarios",
                h3("Compare Scenarios"),
                fluidRow(
                  column(3, selectInput('sspCategory', label = NULL,
                                        choices = c("Population"), selected = "Population")),
                  column(2, h5("Selected Scenarios:", align = "right")),
                  column(3, selectInput('sspChoices', label = NULL,
                                        c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5"),
                                        selected = c("SSP1", "SSP2"), multiple = TRUE))
                ),

                box(solidHeader = F, width = 12,
                    htmlOutput('sspTitle'),
                    plotOutput('sspComparison', height = "520px", width = "100%"))
        ),
        # upload
        tabItem(tabName = "file", "Nothing to see here.")
      ), # tabItems
      bsModal('uploadModal', 'Upload a File', trigger = 'triggerUploadModal', size = 'large',
              fileInput('projectFile', 'Upload Project Data File'),
              p(" File size: ", textOutput('projectSize', inline = TRUE)),

              p(" Queries present in ALL selected scenarios: ", textOutput('queries')),
              fluidRow(
                column(3, checkboxInput('inclSpatial', 'Include Spatial Queries', value=TRUE))
              )
      ) #bsModal
    ) # dashboardBody
  ) #dashboardPage
))

