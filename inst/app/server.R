library(shiny)
library(rgcam)
library(magrittr)
library(gcamlacdash)

options(shiny.maxRequestSize=512*1024^2) # 512 MB max file upload size.

shinyServer(function(input, output, session) {

    ## ----- INITIALIZATION -----

    # Show loading screen on start up
    initComplete <- reactiveVal(FALSE)
    output$setupComplete <- reactive({ initComplete() })
    outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)

    # Data that should show on load. The default data come from the objects
    # defaultData, waterData, and sspData (in the data directory).
    defaultProj <- 'Default Project'
    files <- list() # files is the master list of project files
    files[[defaultProj]] <- defaultData

    # To access the current project file, refer to the select in the upper right
    updateSelectInput(session, 'fileList', choices=names(files))

    # Initialize reactive values to hold the data frame being displayed in both
    # the time plot view and the map plot view. These data frames are used for
    # getting hover values and for viewing the raw table data.
    timePlot.df <- reactiveVal()
    mapZoom <- reactiveVal(0)


    ## ----- PLOT OPTION UPDATES -----

    ## When a new file is uploaded, update available projects
    observe({
      fileinfo <- input$projectFile
      if(!is.null(input$projectFile)) {
        project.data <- loadProject(fileinfo$datapath)  # should be only one file
        project.data <- convertProjectToLongform(project.data)

        project.filename <- fileinfo$name
        files[[project.filename]] <<- project.data
        updateSelectInput(session, 'fileList', choices=names(files), selected=project.filename)
      }
    })

    ## When a new project is selected, update available scenarios
    observe({
      if(input$fileList != "") {
        project.data <- files[[input$fileList]]
        scens <- listScenarios(project.data)
        updateSelectInput(session, 'scenarioInput', choices=scens)
        updateSelectInput(session, 'diffScenario', choices=scens)
        updateSelectInput(session, 'mapScenario', choices=scens)
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
      if(!input$inclSpatial && input$sidebar != "maps") {
        nonGrid <- sapply(queries,
                          function(q) {!isGrid(prj, input$scenarioInput, q)})
        queries <- queries[nonGrid]
      }

      ## Preserve selected value if possible, else allow update to reset selection
      sel <- input$plotQuery
      if(!(sel %in% queries)) sel <- NULL

      updateSelectInput(session, 'plotQuery', choices = queries, selected = sel)
      updateSelectInput(session, 'mapQuery', choices = queries, selected = sel)
    })

    ## When a new query is selected, update available subcategories
    observe({
        prj <- files[[input$fileList]]
        scen <- input$scenarioInput
        query <- input$plotQuery

        ## Assumes that a particular query has the same columns in all scenarios
        catvars <- getQuerySubcategories(prj, scen, query)
        prevSubcat <- if(input$tvSubcatVar %in% catvars) input$tvSubcatVar else 'none'
        updateSelectInput(session, 'tvSubcatVar', choices=c('none', catvars),
                          selected=prevSubcat)
    })

    ## When a new map query is selected, update limits on the time slider
    observe({
        prj <- files[[input$fileList]]
        scen <- input$mapScenario
        query <- input$mapQuery

        yrlimits <- getQueryYears(prj, scen, query)

        # Usually there isn't consistent data before 2005, so filter out early
        # years.
        if(yrlimits[1] < 2005) yrlimits[1] <- 2005

        yrsel <- isolate(input$mapYear)
        if(yrsel < yrlimits[1])
            yrsel <- yrlimits[1]
        else if(yrsel > yrlimits[2])
            yrsel <- yrlimits[2]
        else
            yrsel <- NULL           # NULL means leave it alone
        updateSliderInput(session, 'mapYear', min=yrlimits[1],
                          max=yrlimits[2], value=yrsel)
        output$mapName <- renderText({query})
        mapZoom(0)
    })

    observeEvent(input$zoomIn, { if(mapZoom() > -30) mapZoom(mapZoom() - 3) })
    observeEvent(input$zoomOut, { if(mapZoom() < 30) mapZoom(mapZoom() + 3) })

    ## ----- FILE UPLOAD UPDATES -----
    output$projectSize <- renderText({
        if(is.null(input$projectFile))
          0
        else
          utils:::format.object_size(input$projectFile$size, "auto")
    })

    output$scenarios <- renderText({
        if(input$fileList != defaultProj)
          getProjectScenarios(files[[input$fileList]], concat='\n')
        else
          NULL
    })

    output$mapIsGrid <- reactive({
      prj <- files[[input$fileList]]
      scen <- input$mapScenario
      query <- input$mapQuery
      if(uiStateValid(prj, scen, query)) isGrid(prj, scen, query) else FALSE
    })
    outputOptions(output, "mapIsGrid", suspendWhenHidden = FALSE)

    ## ----- PLOT UPDATES -----
    output$mapPlot <- renderPlot({
      prj <- files[[input$fileList]]
      scen <- input$mapScenario
      query <- input$mapQuery

      if(!uiStateValid(prj, scen, query)) return(default.plot("Updating..."))

      diffscen <- if(input$diffCheck) input$diffScenario

      # If the scenario has changed, the value of the query selector may not
      # be valid anymore.
      availableQueries <- getScenarioQueries(prj, c(scen, diffscen))
      if(!query %in% availableQueries) {
        query <- availableQueries[1]
      }

      year <- input$mapYear
      map <- switch(input$mapType,
                    regions =   gcammaptools::map.rgn32,
                    countries = gcammaptools::map.countries,
                    basins =    gcammaptools::map.basin235,
                    usa =       gcammaptools::map.usa)

      plotMap(prj, query, scen, NULL, input$mapExtent, year, map, mapZoom())
    })

    output$mapAltPlot <- renderPlot({
      prj <- files[[input$fileList]]
      scen <- input$mapScenario
      query <- input$mapQuery
      if(!uiStateValid(prj, scen, query)) return(default.plot("Updating..."))

      diffscen <- if(input$diffCheck) input$diffScenario
      regions <- lac.rgns

      # If the scenario has changed, the value of the query selector may not
      # be valid anymore.
      availableQueries <- getScenarioQueries(prj, c(scen, diffscen))
      if(!query %in% availableQueries) {
        query <- availableQueries[1]
      }

      plotTime(prj, query, scen, NULL, "region", regions)
    })

    region.filter <- callModule(regionFilter, "tpFilters")

    output$timePlot <- renderPlot({
        prj <- isolate(files[[input$fileList]])
        scen <- input$scenarioInput
        query <- input$plotQuery
        if(!uiStateValid(prj, scen, query)) return(default.plot())

        diffscen <- if(input$diffCheck) input$diffScenario else NULL
        if(!is.null(diffscen) && diffscen == scen) {
          timePlot.df(NULL)
          return(default.plot("Scenarios are the same"))
        }

        tvSubcatVar <- input$tvSubcatVar

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

        output$timeTable <- renderDataTable({ timePlot.df() })

        # Set the data table title
        tableTitle <- query
        if(tvSubcatVar != 'none') {
          tableTitle <- paste(query, tvSubcatVar, sep = ' by ')
        }
        output$tableTitle <- renderText(tableTitle)

        # Update the plot and the time plot data frame (used for the hover)
        plt <- plotTime(prj, query, scen, diffscen, tvSubcatVar, region.filter())
        t <- if(is.null(plt$plotdata)) 'dis' else 'en'
        session$sendCustomMessage(type = paste0(t, 'able-element'),
                                  message = 'triggerTableModal')
        timePlot.df(plt$plotdata)
        plt$plot
    })

    # Downloadable csv of current plot table dataset
    output$downloadButton <- downloadHandler(
      filename = function() {
        paste(input$plotQuery, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(timePlot.df(), file, row.names = FALSE)
      }
    )

    # Add a hover over the time plot bar chart
    callModule(barChartHover, "timePlot", reactive(input$exploreHover),
               reactive(timePlot.df()), reactive(input$tvSubcatVar))

    callModule(landingPage, "dashboard", files[[defaultProj]])

    callModule(scenarioComparison, "ssp")

    initComplete(TRUE)

})
