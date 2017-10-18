library(shiny)
library(rgcam)
library(magrittr)
library(GCAMdashboard)

options(shiny.maxRequestSize=512*1024^2) # 512 MB max file upload size.

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    ## Set up some UI state
    scenarios <- ""
    queries <- ""

    ## Get the new data file on upload
    rFileinfo <- reactive({
      fileinfo <- input$projectFile
      # if(is.null(fileinfo)) {
      #   project.filename <- NULL
      #   project.data <- NULL
      # }
      # else {
      #   project.data <- loadProject(fileinfo$datapath)   # should be only one file
      #   project.filename <- fileinfo$name
      #   updateSelectInput(session, 'scenarioInput', choices=listScenarios(project.data))
      # }
      # list(project.filename=project.filename, project.data=project.data)
    })

    ## ----- INITIALIZATION -----
    ## Data that should show on load
    defaultProj <- 'idb.dat'
    files <- list()
    files[[defaultProj]] <- loadProject(system.file(defaultProj, package = "GCAMdashboard"))
    updateSelectInput(session, 'fileList', choices=names(files))
    # files[[input$fileList]] <- loadProject(system.file('idb_ndc_long.dat', package = "GCAMdashboard"))


    ## ----- PLOT OPTION UPDATES -----
    ## When a new file is uploaded, update available projects
    observe({
      fileinfo <- input$projectFile
      if(!is.null(input$projectFile)) {
        project.data <- loadProject(fileinfo$datapath)  # should be only one file
        project.filename <- fileinfo$name
        files[[project.filename]] <<- project.data
        updateSelectInput(session, 'fileList', choices=names(files), selected=project.filename)

      }
    })

    ## When a new project is selected, update available scenarios
    observe({
      if(input$fileList != "") {
        project.data <- files[[input$fileList]]
        updateSelectInput(session, 'scenarioInput', choices=listScenarios(project.data))
        updateSelectInput(session, 'diffScenario', choices=listScenarios(project.data))
      }
    })

    ## When a new scenario is selected, update available queries
    observe({
      prj <- isolate(files[[input$fileList]])

      # Get the current scenario (or scenarios for a difference plot)
      if(input$scenarioInput == "") {
        return() # There should always be a dataset loaded
      }
      else if(input$diffCheck) {
        qscenarios <- c(input$scenarioInput, input$diffScenario)
      }
      else {
        qscenarios <- input$scenarioInput
      }

      queries <- getScenarioQueries(prj, qscenarios)

      ## Filter out grid queries, if requested.
      if(!input$inclSpatial) {
        nonGrid <- sapply(queries,
                          function(q) {!isGrid(prj, input$scenarioInput, q)})
        queries <- queries[nonGrid]
      }

      ## Preserve selected value if possible, else allow update to reset selection
      sel <- input$plotQuery
      if(!(sel %in% queries)) sel <- NULL

      updateSelectInput(session, 'plotQuery', choices = queries, selected = sel)
      updateSelectInput(session, 'lp1Query', choices = queries, selected = sel)
    })

    ## When a new query is selected, update available subcategories
    observe({
        prj <- files[[defaultProj]]
        scen <- input$scenarioInput
        query <- input$plotQuery

        ## Assumes that a particular query has the same columns in all scenarios
        catvars <- getQuerySubcategories(prj, scen, query)
        prevSubcat <- if(input$tvSubcatVar %in% catvars) input$tvSubcatVar else 'none'
        updateSelectInput(session, 'tvSubcatVar', choices=c('none', catvars),
                          selected=prevSubcat)
    })

    ## TODO: revisit this
    observe({
        ## update the limits on the time slider on the map plot.  Only do this
        ## when the selected plot query changes.
        scen <- isolate(input$plotScenario)
        prj <- rFileinfo()$project.data
        query <- input$plotQuery
        if(uiStateValid(prj, scen, query)) {
            ## now do the map slider
            yrlimits <- getQueryYears(prj, scen, query)
            yrsel <- isolate(input$mapYear)
            if(yrsel < yrlimits[1])
                yrsel <- yrlimits[1]
            else if(yrsel > yrlimits[2])
                yrsel <- yrlimits[2]
            else
                yrsel <- NULL           # NULL means leave it alone
            updateSliderInput(session, 'mapYear', min=yrlimits[1],
                              max=yrlimits[2], value=yrsel)
        }
    })

    ## When a 'select all' or 'deselect all' button is pressed, update region
    ## filtering checkboxes
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


    ## -----
    output$projectSize <- renderText({
        if(is.null(input$projectFile))
          0
        else
          utils:::format.object_size(input$projectFile$size, "auto")
    })

    output$scenarios <- renderText({
        getProjectScenarios(files[[input$fileList]], concat='\n')
    })

    output$queries <- renderText({
        getScenarioQueries(files[[input$fileList]], input$scenarioInput, concat='\n')
    })

    output$mapPlot <- renderPlot({
        if(uiStateValid( rFileinfo()$project.data, input$plotScenario,
                        input$plotQuery )) {
            diffscen <- if(input$diffCheck) {
                input$diffScenario
            } else {
                  NULL
              }
            year <- input$mapYear
            map <- switch(input$mapType,
                          regions =   gcammaptools::map.rgn32.simple,
                          china =     gcammaptools::map.chn.simple,
                          countries = gcammaptools::map.rgn32,
                          basins =    gcammaptools::map.basin235,
                          none =      gcammaptools::map.basin235.simple)
            plotMap(rFileinfo()$project.data, input$plotQuery,
                    input$plotScenario, diffscen, input$mapExtent, year, map)
        }
        else {
            default.plot('No Data')
        }
    })

    output$mapName <- renderText({input$plotQuery})

    output$timePlot <- renderPlot({
        prj <- isolate(files[[input$fileList]])
        scen <- input$scenarioInput
        query <- input$plotQuery

        diffscen <- if(input$diffCheck) input$diffScenario else NULL
        # if(diffscen == scen) return(default.plot("Scenarios are the same"))

        tvSubcatVar <- input$tvSubcatVar

        region.filter <- c(input$tvRgns1, input$tvRgns2, input$tvRgns3,
                           input$tvRgns4, input$tvRgns5)

        # If the scenario has changed, the value of the query selector may not
        # be valid anymore.
        availableQueries <- getScenarioQueries(prj, c(scen, diffscen))
        if(!query %in% availableQueries) {
          query <- availableQueries[1]
        }

        # If the query has changed, the value of the subcategory selector may
        # not be valid anymore. Change it to none.
        if(!tvSubcatVar %in% names(getQuery(prj, query, scen))) {
           tvSubcatVar <- 'none'
        }

        plotTime(prj, query, scen, diffscen, tvSubcatVar, region.filter)
    })

    output$hoverInfo <- renderUI({
      hover <- input$exploreHover
      df <- getTimePlotData()
      subcat <- input$tvSubcatVar

      # Don't attempt hover if tab isn't selected. TODO: Fix bug where changing
      # other time plot (like the one on the landing page) also updates the data
      # the hover refers to.
      if(input$sidebar != 'explore') return(NULL)
      if(is.null(hover)) return(NULL)
      if(is.null(df)) return(NULL)

      # Detect the year of the bar that is being hovered over
      hoverYear <- df[which.min(abs(df$year - hover$x)), 'year'][[1]]
      df <- dplyr::filter(df, year==hoverYear)

      # If there's a subcategory, we also need to find which category is being
      # hovered over
      if(subcat != 'none') {

        # Find which segment of the stacked bar the hover is closest to
        stackedSum <- sum(df$value) - cumsum(df$value)
        index <- which(stackedSum - hover$y < 0)[1]

        if(hover$y > sum(df$value)) return(NULL) # Hover is above the bar
        if(is.na(index)) return(NULL) # Hover is below the bar

        # Get the region name and value for display
        val <- paste(df[[subcat]][index], round(df$value[index], digits=1), sep=': ')

      } else {
        val <- df[which.min(abs(df$year - hover$x)), 'value'] %>%
               round(digits=1)
        if(hover$y > val) return(NULL)
        val <- as.character(val)
      }

      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

      left <- round(left_px) - 30
      top <- round(top_px) - 30
      if(left < 0 || top < 0) return(NULL)

      # actual tooltip created as absolutePanel
      absolutePanel(
        class = 'hoverPanel',
        left = paste0(left, "px"),
        top = paste0(top, "px"),
        # style = style,
        p(HTML(val)))

    })

    output$landingPlot1 <- renderPlot({
      if(input$lp1toggle == "Reference Scenario")
        scen <- "REFlu_e6_mex"
      else
        scen <- "PIAlu_e6_mex"
      plotTime(files[[input$fileList]], "Primary Energy Consumption by Fuel",
               scen, NULL, "fuel", lac.rgns)
    })

    output$landingPlot2 <- renderPlot({
      if(input$lp2toggle == "Reference Scenario")
        scen <- "REFlu_e6_mex"
      else
        scen <- "PIAlu_e6_mex"
      plotTime(files[[input$fileList]], "CO2 Emissions",
               scen, NULL, "sector", lac.rgns)
    })

    output$landingPlot3 <- renderPlot({
        query <- "Agriculture production"
        plotMap(files[[input$fileList]], query, "REFlu_e6_mex", NULL,
                 "lac", 2050)
    })


    waterData <- loadProject(system.file('hydro.dat', package = "GCAMdashboard"))
    output$waterScarcityPlot <- renderPlot({
      query <- "Water Scarcity"
      pscen <- "Reference"
      plotMap(waterData, query, pscen, NULL, "lac", 2050)
    })

    output$waterSupplyPlot <- renderPlot({
      query <- "Water Supply"
      pscen <- "Reference"
      plotMap(waterData, query, pscen, NULL, "lac", 2050)
    })

    output$waterDemandPlot <- renderPlot({
      query <- "Water Demand"
      pscen <- "Reference"
      plotMap(waterData, query, pscen, NULL, "lac", 2050)
    })


    output$sspTitle <- renderUI({
      h3(input$sspCategory, align = "center")
    })

    sspData <- loadProject(system.file('ssp.dat', package = "GCAMdashboard"))
    updateSelectInput(session, 'sspCategory', choices=listQueries(sspData), selected = "Population")

    output$sspComparison <- renderPlot({
      query <- input$sspCategory
      scens <- input$sspChoices
      subcatvar <- tail(getQuerySubcategories(sspData, scens[1], query), n=1)
      plotScenComparison(sspData, query, scens, NULL, subcatvar, lac.rgns)
    })

    ## update region controls on time view panel
    ## None of this is necessary anymore, since we hardwired the region lists,
    ## but I'm keeping it around for now in case we want to allow for the
    ## possibility that a data set has custom regions.
    ## observe({
    ##     prj <- rFileinfo()$project.data
    ##     scen <- input$plotScenario
    ##     query <- input$plotQuery
    ##     if(uiStateValid(prj, scen, query)) {
    ##         tbl <- getQuery(prj,query,scen)
    ##         rgns <- unique(tbl$region) %>% sort
    ##         updateCheckboxGroupInput(session, 'tvRgns', choices = rgns,
    ##                                  selected = last.region.filter)
    ##     }
    ##})
### Debugging
    ## observe({
    ##             print('****************Change of Input****************')
    ##             cat('plotScenario: ', input$plotScenario, '\n')
    ##             cat('diffScenario: ', input$diffScenario, '\n')
    ##             cat('plotQuery: ', input$plotQuery, '\n')
    ##         })
})
