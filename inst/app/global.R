library(shiny)


## =============== Dashboard Tab ============= ##
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

  # Plot outputs
  output$waterScarcityPlot <- renderPlot({
    query <- "Water Scarcity"
    pscen <- "Reference"
    year <- as.integer(input$waterYearToggle)
    plotMap(waterData, query, pscen, NULL, "lac", year)
  })

  output$waterSupplyPlot <- renderPlot({
    query <- "Water Supply"
    pscen <- "Reference"
    year <- as.integer(input$waterYearToggle)
    plotMap(waterData, query, pscen, NULL, "lac", year)
  })

  output$waterDemandPlot <- renderPlot({
    query <- "Water Demand"
    pscen <- "Reference"
    year <- as.integer(input$waterYearToggle)
    plotMap(waterData, query, pscen, NULL, "lac", year)
  })

  output$popPlot <- renderPlot({
    query <- input$popTabset
    pscen <- "REFlu_e6_mex"
    year <- input$popYear
    plotMap(data, query, pscen, NULL, "lac", year)
  })

  output$gdpPlot <- renderPlot({
    query <- input$popTabset
    pscen <- "REFlu_e6_mex"
    year <- input$popYear
    plotMap(data, query, pscen, NULL, "lac", year)
  })

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


## ================= SSP Tab ================= ##
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
