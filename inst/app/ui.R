library(shiny)
library(shinyBS)
library(shinydashboard, warn.conflicts = F) # Masks 'box' from graphics package
library(gcamlacdash)


shinyUI(fluidPage(theme="style.css",
  dashboardPage(
    dashboardHeader(title = 'GCAM Dashboard',
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
        tabItem(tabName = "dashboard", landingPageUI("dashboard")),
        tabItem(tabName = "maps",
          fluidRow(
            column(5,
              box(width = NULL, status = "primary", title = "Map Options",
                  solidHeader = T,
                  fluidRow(
                    column(6,
                           selectInput('mapScenario', 'Scenario',
                                       choices=list()),
                           selectInput('mapQuery', 'Variable',
                                       choices=list())
                    ),
                    column(6,
                           selectInput('mapExtent', label = "Extent", width = NULL,
                                       choices=c(Global='global', USA='usa', China='china',
                                                 Africa='africa', `Latin America`='lac'),
                                       selected = 'lac'),
                           conditionalPanel(condition = "!output.mapIsGrid",
                             selectizeInput('mapCats', label = "Filter by",
                                            choices=list(), multiple=T,
                                            options=list(placeholder='Select filters...'))),
                           conditionalPanel(condition = "output.mapIsGrid",
                             selectInput('mapType', label = "Map Type", width = NULL,
                                         choices=c(`GCAM Regions`='regions',
                                                   Countries='countries',
                                                   Basins='basins'),
                                         selected = 'lac')#,
                                         #size='400px')
                           )
                    )
                  )
              ),
              # conditionalPanel(condition = "input.mapCats",
              #   box(title = 'Filters', width = NULL, status = "primary", height='215px',
              #       checkboxGroupInput('mapFilters', NULL, choices = list())
              #   )
              # ),
              box(title = NULL, width = NULL, status = "primary",
                  plotOutput('mapAltPlot', height = '430px')
              )
            ),
            column(7,
              box(width = NULL, status = "primary", solidHeader = T,
                  title = textOutput('mapName', inline=TRUE), align = 'center',
                  plotOutput('mapPlot', height = '550px'),
                  absolutePanel(class = 'zoom-buttons', bottom = 10, right = 10,
                    column(12,
                      fluidRow(
                        actionButton('zoomIn', icon("plus", lib = "font-awesome"))
                      ),
                      fluidRow(
                        actionButton('zoomOut', icon("minus", lib = "font-awesome"))
                      )
                    )
                  )
              ),
              sliderInput('mapYear', NULL, min=2005, max=2100, step=5,
                          value=2050, sep='', animate = F, width = '100%',
                          ticks = TRUE)
            )
          )
        ),
        tabItem(tabName = "explore",
          bsModal('tableModal', textOutput('tableTitle', inline = T),
                  trigger = 'triggerTableModal', size = 'large',
            dataTableOutput('timeTable'),
            downloadButton('downloadButton')
          ),
          fluidRow(
            column(width = 8,
              box(title="Plot Options", status = "primary", width = NULL,
                  solidHeader = T,
                  fluidRow(
                    column(width = 5, selectInput('scenarioInput', 'Scenario',
                                                  choices=list()),
                           selectizeInput('plotQuery', label="Variable", choices=list(),
                                          options=list(placeholder='Choose a variable...'))
                    ),
                    column(width = 5,
                           conditionalPanel(condition = "input.diffCheck == true",
                                            selectInput('diffScenario', 'Difference Scenario:', choices=list())),
                           conditionalPanel(condition = "input.diffCheck != true",
                                            div(style = "height: 67px; padding-top: 21px;",
                                              checkboxInput('diffCheck', 'Add Difference Scenario')
                                            )),
                           selectInput('tvSubcatVar', 'Break totals into subcategories by:',
                                       choices=list())
                    ),
                    column(width = 2,
                           conditionalPanel(condition = "input.diffCheck == true",
                             actionButton('triggerDiffCheck', NULL, icon("times"),
                                          lib = "font-awesome")
                           ),
                           div(class="table-title", align="right", "View as Table",
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
                barChartHoverUI("timePlot")
              )
            ),

            column(width = 4,
              regionFilterInput("tpFilters")
            )
          )
        ),
        tabItem(tabName = "scenarios", scenarioComparisonUI("ssp"))
      ) # tabItems
      }), # loading panel
      conditionalPanel("!output.setupComplete", {
        div(class="loading-panel", div(class="loading-panel-container",
          h3("Loading"),
          div(class="spinner",
            lapply(1:5, function(i) { div(class = paste0("rect", i)) })
          )
        ))
      }),
      bsModal('uploadModal', 'Upload a File', trigger = 'triggerUploadModal', size = 'small',
              fileInput('projectFile', 'Upload Project Data File'),
              p("File size: ", textOutput('projectSize', inline = TRUE)),
              p("Project contains scenarios: ", verbatimTextOutput('scenarios'))
      ) # bsModal
    ) # dashboardBody
  ), # dashboardPage
  singleton(tags$head(tags$script(src = "custom-scripts.js")))
) # fluidPage
) # shinyUI

