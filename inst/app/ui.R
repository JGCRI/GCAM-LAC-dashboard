
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
      sidebarMenu(id = "sidebar",
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Maps", tabName = "maps", icon = icon("map", lib = "font-awesome")),
        menuItem("Explore", tabName = "explore", icon = icon("search", lib = "font-awesome")),
        menuItem("Compare Scenarios", tabName = "scenarios", icon = icon("line-chart", lib = "font-awesome"))
      )
    ),
    dashboardBody(
      # Wrap everything in conditional panel to only show when loaded
      conditionalPanel("output.setupComplete", {
      tabItems(
        tabItem(tabName = "dashboard",
          fluidRow(
            column(3, align = "left",
              h4("Water"),
              tabBox(title = NULL, side = 'left', width = NULL,
                     id = "waterTabset",
                     tabPanel("Supply", value = "Water Supply",
                              plotOutput('waterSupplyPlot', height='500px')),
                     tabPanel("Demand", value = "Water Demand",
                              plotOutput('waterDemandPlot', height='500px')),
                     tabPanel("Scarcity", value = "Water Scarcity",
                              plotOutput('waterScarcityPlot', height='500px'))
              ),
              div(align="center", radioButtons('waterYearToggle', NULL,
                                               choices = 2050, inline = T)
              )
            ),
            column(3, align = "left",
              h4("Population and GDP"),
              tabBox(title = NULL, side = 'left', width = NULL,
                    id = "popTabset",
                    tabPanel("Population", value = "Population by region",
                             plotOutput('popPlot', height='500px')),
                    tabPanel("GDP", value = "GDP by region",
                             plotOutput('gdpPlot', height='500px'))
              ),
              sliderInput('popYear', NULL, min=2005, max=2100, step=5,
                          value=2050, sep='', animate = F)
            ),
            column(6,
              h4("Energy and Emissions"),
              box(title = 'Primary Energy Consumption by Fuel', width = NULL,
                  solidHeader = TRUE, status = "primary",
                  plotOutput('landingPlot1', height='211px')
              ),
              box(title = 'CO2 Emissions', width = NULL,
                  solidHeader = TRUE, status = "primary",
                  plotOutput('landingPlot2', height='211px')
              ),
              div(align="center", radioButtons('lptoggle', NULL,
                                               c('Reference Scenario', 'Policy Scenario'),
                                               inline = T)
              )
            )
          )
        ),
        tabItem(tabName = "maps",
          fluidRow(
            column(7,
              box(width = NULL, status = "primary", solidHeader = T,
                  title = textOutput('mapName', inline=TRUE), align = 'center',
                  plotOutput('mapPlot', height = '600px')
              ),
              sliderInput('mapYear', NULL, min=2005, max=2100, step=5,
                          value=2050, sep='', animate = F, width = '100%',
                          ticks = TRUE)
            ),

            column(5,
              box(width = NULL, status = "primary", title = "Map Options",
                  solidHeader = T,
                  fluidRow(
                    column(6,
                           selectInput('mapQuery', 'Query',
                                       choices=list()),
                           selectInput('mapExtent', label = "Extent", width = NULL,
                                       choices=c(Global='global', USA='usa', China='china',
                                                 Africa='africa', `Latin America`='lac'),
                                       selected = 'lac')),
                    column(6,
                           selectInput('mapScenario', 'Scenario',
                                       choices=list()),
                           conditionalPanel(condition = "output.mapIsGrid",
                             selectInput('mapType', label = "Map Type", width = NULL,
                                         choices=c(`GCAM Regions`='regions',
                                                   China='china',
                                                   Countries='countries',
                                                   Basins='basins',
                                                   `No Borders`='none'),
                                         selected = 'lac')
                           )
                    )
                  )
              ),
              box(title = NULL, width = NULL, status = "primary",
                  plotOutput('mapAltPlot', height = '430px')
              )
            )
          )
        ),
        tabItem(tabName = "explore",
          bsModal('tableModal', 'View Table', trigger = 'triggerTableModal', size = 'large',
                  dataTableOutput('timeTable')
          ),
          fluidRow(
            column(width = 8,
              box(title=NULL, status="primary", width=NULL,
                fluidRow(
                  column(width = 7,
                         selectInput('tvSubcatVar', 'Break totals into subcategories by:',
                                     choices=c('none','region'))
                  ),
                  column(width = 5,
                         div(class="table-title", align="right", "View Table",
                            br(),
                            actionButton('triggerTableModal', NULL,
                                         icon("table", lib = "font-awesome"))
                         )
                  )
                )
              ),
              box(status = "warning", width = NULL,
                plotOutput('timePlot', height='450px',
                           hover = hoverOpts("exploreHover", delay = 50, delayType = 'throttle')),
                uiOutput('hoverInfo')
              ),
              box(title=NULL, status = "primary", width = NULL,
                fluidRow(
                  column(width = 5, selectInput('scenarioInput', 'Scenario:',
                                                choices=list())
                  ),
                  column(width = 5, conditionalPanel(condition = "input.diffCheck == true",
                                                     selectInput('diffScenario', 'Difference Scenario:', choices=list()))
                  ),
                  column(width = 2, checkboxInput('diffCheck', 'Add Difference Scenario')
                  )
                )
              )
            ),

            column(width = 4,
              box(status = "primary", width = NULL,
                  selectInput('plotQuery', label="Plot Variable", choices=list()),
                  checkboxInput('inclSpatial', 'Include Spatial Queries', value=TRUE)
              ),
              div(class = "box-overflow",
              box(title = "Filter by Region", status = "primary", solidHeader = TRUE,
                width = NULL, height = '580px', class = "box-overflow-y",
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
              )
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
        )
      ) # tabItems
      }), # loading panel
      conditionalPanel("!output.setupComplete", {
        div(class="loading-panel", div(class="loading-panel-container",
          h3("Loading"),
          div(class="spinner",
              div(class="rect1"),
              div(class="rect2"),
              div(class="rect3"),
              div(class="rect4"),
              div(class="rect5")
          )
        ))
      }),
      bsModal('uploadModal', 'Upload a File', trigger = 'triggerUploadModal', size = 'small',
              fileInput('projectFile', 'Upload Project Data File'),
              p("File size: ", textOutput('projectSize', inline = TRUE)),
              p("Project contains scenarios: ", verbatimTextOutput('scenarios'))
      ) #bsModal
    ) # dashboardBody
  ) #dashboardPage
))

