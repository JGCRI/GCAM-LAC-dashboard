library(shiny)



# Dashboard Tab -----------------------------------------------------------

landingPageUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    # Tabbed box with water plots on far left
    column(3, align = "left",

      h4("Water"),
      tabBox(id = ns("waterTabset"), title = NULL, width = NULL, side = 'left',
        tabPanel("Supply", value = "Water Supply", plotOutput(ns("waterSupplyPlot"), height='500px')),
        tabPanel("Demand", value = "Water Demand", plotOutput(ns("waterDemandPlot"), height='500px')),
        tabPanel("Scarcity", value = "Water Scarcity", plotOutput(ns("waterScarcityPlot"), height='500px'))
      ),

      div(align="center", radioButtons(ns('waterYearToggle'), NULL,
                                      choices = 2050, inline = T)
      )
    ),

    # Tabbed box with population and GDP plots in middle
    column(3, align = "left",

      h4("Population and GDP"),
      tabBox(id = ns("popTabset"), title = NULL, side = 'left', width = NULL,
        tabPanel("Population", value = "Population by region", plotOutput(ns("popPlot"), height='500px')),
        tabPanel("GDP", value = "GDP by region", plotOutput(ns("gdpPlot"), height='500px'))
      ),

      sliderInput(ns('popYear'), NULL, min=2005, max=2100, step=5, value=2050,
                  sep='', animate = F)
    ),

    # Bar charts on far right
    column(6,

      h4("Energy and Emissions"),
      box(title = 'Primary Energy Consumption by Fuel', width = NULL,
         solidHeader = TRUE, status = "primary",
         plotOutput(ns("landingPlot1"), height='211px')
      ),
      box(title = 'CO2 Emissions', width = NULL,
         solidHeader = TRUE, status = "primary",
         plotOutput(ns("landingPlot2"), height='211px')
      ),

      div(align="center", radioButtons(ns('lptoggle'), NULL,
                                      c('Reference Scenario', 'Policy Scenario'),
                                      inline = T)
      ) # div
    ) # column
  ) # fluidRow
} # landingPageUI


landingPage <- function(input, output, session, data) {

  # When the map plots on the landing page are loaded, generate year toggle
  observe({
    query <- input$waterTabset
    years <- getQuery(waterData, query, "Reference")$year %>% unique()
    middleYear <- years[(length(years) + 1) / 2]
    updateRadioButtons(session, 'waterYearToggle', NULL, choices = years,
                       selected = middleYear, inline = TRUE)
  })

  # Water plots
  lapply(c("Scarcity", "Supply", "Demand"), function(query) {
    output[[paste0("water", query, "Plot")]] <- renderPlot({
      query <- paste("Water", query)
      pscen <- "Reference"
      year <- as.integer(input$waterYearToggle)
      plotMap(waterData, query, pscen, NULL, "lac", year)
    })
  })

  # Pop and GDP plots
  lapply(c("popPlot", "gdpPlot"), function(outputID) {
    output[[outputID]] <- renderPlot({
      query <- input$popTabset
      pscen <- "REFlu_e6_mex"
      year <- input$popYear
      plotMap(data, query, pscen, NULL, "lac", year)
    })
  })

  # Bar charts
  output$landingPlot1 <- renderPlot({
    if(input$lptoggle == "Reference Scenario")
      scen <- "REFlu_e6_mex"
    else
      scen <- "PIAlu_e6_mex"
    plotTime(data, "Primary Energy Consumption (Direct Equivalent)", scen, NULL, "fuel", lac.rgns)
  })

  output$landingPlot2 <- renderPlot({
    if(input$lptoggle == "Reference Scenario")
      scen <- "REFlu_e6_mex"
    else
      scen <- "PIAlu_e6_mex"
    plotTime(data, "Top 12 CO2 emissions by sector", scen, NULL, "sector", lac.rgns)
  })
}


# SSP Tab -----------------------------------------------------------------

scenarioComparisonUI <- function(id) {
  ns <- NS(id)

  tagList(

    h3("Compare Scenarios"),

    fluidRow(
      column(3, selectInput(ns("sspCategory"), label = NULL,
                            choices = c("Population"), selected = "Population")),
      column(2, h5("Selected Scenarios:", align = "right")),
      column(3, selectInput(ns("sspChoices"), label = NULL,
                            c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5"),
                            selected = c("SSP1", "SSP2"), multiple = TRUE))
    ),

    box(solidHeader = F, width = 12,
        htmlOutput(ns("sspTitle")),
        plotOutput(ns("sspComparison"), height = "520px", width = "100%"))
  )
}

scenarioComparison <- function(input, output, session) {

  # Initialize data for the scenario comparison tab
  updateSelectInput(session, 'sspCategory', choices=listQueries(sspData),
                    selected = "Population")

  output$sspTitle <- renderUI({
    h3(input$sspCategory, align = "center")
  })

  output$sspComparison <- renderPlot({
    query <- input$sspCategory
    scens <- input$sspChoices
    subcatvar <- tail(getQuerySubcategories(sspData, scens[1], query), n=1)
    plotScenComparison(sspData, query, scens, NULL, subcatvar, lac.rgns)
  })
}


# Region Filters ----------------------------------------------------------

regionFilterInput <- function(id) {
  ns <- NS(id)

  div(class = "box-overflow",
    box(title = "Filter by Region", status = "primary", solidHeader = TRUE,
      width = NULL, height = '580px', class = "box-overflow-y",
      actionButton(ns('rgnSelectAll'), 'Select all regions'),
      br(),

      bsCollapse(open="Latin America and Caribbean",
        bsCollapsePanel(title='Latin America and Caribbean', style="primary",
          actionButton(ns('rgns2All'), 'Deselect All'),
          checkboxGroupInput(ns('tvRgns2'), NULL, choices=lac.rgns, selected = lac.rgns)
        ),
        bsCollapsePanel(title="Africa",
          actionButton(ns('rgns1All'), 'Select All'),
          checkboxGroupInput(ns('tvRgns1'), NULL, choices=africa.rgns)
        ),
        bsCollapsePanel(title="Asia-Europe",
          actionButton(ns('rgns4All'), 'Select All'),
          checkboxGroupInput(ns('tvRgns4'), NULL, choices=europe.rgns)
        ),
        bsCollapsePanel(title="Asia-Pacific",
          actionButton(ns('rgns5All'), 'Select All'),
          checkboxGroupInput(ns('tvRgns5'), NULL, choices=asiapac.rgns)
        ),
        bsCollapsePanel(title="North America",
          actionButton(ns('rgns3All'), 'Select All'),
          checkboxGroupInput(ns('tvRgns3'), NULL, choices=north.america.rgns)
        )
      ) #bsCollapse
    ) # box
  ) # div
} # regionFilterInput


regionFilter <- function(input, output, session) {

  # When a 'select all' or 'deselect all' button is pressed, update region
  # filtering checkboxes
  observeEvent(input$rgns1All, {
    updateRegionFilter(session, 'rgns1All', 'tvRgns1', input$rgns1All%%2 == 0, africa.rgns)
  })
  observeEvent(input$rgns2All, {
    updateRegionFilter(session, 'rgns2All', 'tvRgns2', input$rgns2All%%2 == 1, lac.rgns) # starts with all checked
  })
  observeEvent(input$rgns3All, {
    updateRegionFilter(session, 'rgns3All', 'tvRgns3', input$rgns3All%%2 == 0, north.america.rgns)
  })
  observeEvent(input$rgns4All, {
    updateRegionFilter(session, 'rgns4All', 'tvRgns4', input$rgns4All%%2 == 0, europe.rgns)
  })
  observeEvent(input$rgns5All, {
    updateRegionFilter(session, 'rgns5All', 'tvRgns5', input$rgns5All%%2 == 0, asiapac.rgns)
  })
  observeEvent(input$rgnSelectAll, {
    # Select all
    sAll <- input$rgnSelectAll%%2 == 0
    updateRegionFilter(session, 'rgnSelectAll', 'tvRgns1', sAll, africa.rgns)
    updateRegionFilter(session, 'rgnSelectAll', 'tvRgns2', sAll, lac.rgns)
    updateRegionFilter(session, 'rgnSelectAll', 'tvRgns3', sAll, north.america.rgns)
    updateRegionFilter(session, 'rgnSelectAll', 'tvRgns4', sAll, europe.rgns)
    updateRegionFilter(session, 'rgnSelectAll', 'tvRgns5', sAll, asiapac.rgns)
  })
  return(reactive({c(input$tvRgns1, input$tvRgns2, input$tvRgns3, input$tvRgns4, input$tvRgns5)}))
}

#' Update the checkbox filters when select/deselect all button is pressed
#'
#' @param session The main session.
#' @param btnId The id of the actionButton that was pressed.
#' @param groupId The id of the checkboxGroupInput to act on.
#' @param selectAll If TRUE, select all checkboxes in the group defined by
#'   groupId. If not TRUE then deselect.
#' @param choices The labels of the checkboxes.
#' @export
updateRegionFilter <- function(session, btnId, groupId, selectAll, choices) {
  if(selectAll) {
    updateCheckboxGroupInput(session, groupId, choices = choices)
    newText <- "Select all"
  }
  else {
    updateCheckboxGroupInput(session, groupId, choices = choices, selected = choices)
    newText <- "Deselect all"
  }

  updateCheckboxInput(session, btnId, label = newText)
}
