library(shiny)
library(rgcam)
library(magrittr)
library(GCAMdashboard)

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

    output$timePlot <- renderPlot({
        prj <- isolate(files[[input$fileList]])
        scen <- input$scenarioInput
        query <- input$plotQuery
        if(!uiStateValid(prj, scen, query)) return(default.plot())

        diffscen <- if(input$diffCheck) input$diffScenario else NULL
        if(!is.null(diffscen) && diffscen == scen)
          return(default.plot("Scenarios are the same"))

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

        output$timeTable <- renderDataTable({ timePlot.df() })

        # Set the data table title
        tableTitle <- query
        if(tvSubcatVar != 'none') {
          tableTitle <- paste(query, tvSubcatVar, sep = ' by ')
        }
        output$tableTitle <- renderText(tableTitle)

        # Update the plot and the time plot data frame (used for the hover)
        plt <- plotTime(prj, query, scen, diffscen, tvSubcatVar, region.filter)
        if(!is.null(plt$plotdata)) {
          timePlot.df(plt$plotdata)
          session$sendCustomMessage(type = 'enable-element', message = 'triggerTableModal')
        } else {
          session$sendCustomMessage(type = 'disable-element', message = 'triggerTableModal')
        }
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

    output$hoverInfo <- renderUI({
      hover <- input$exploreHover
      df <- timePlot.df()
      subcat <- input$tvSubcatVar

      # Don't attempt hover if tab isn't selected.
      if(input$sidebar != 'explore') return(NULL)
      if(is.null(hover)) return(NULL)
      if(is.null(df)) return(NULL)

      # Detect the year of the bar that is being hovered over
      hoverYear <- df[which.min(abs(df$year - hover$x)), 'year'][[1]]
      df <- dplyr::filter(df, year==hoverYear)

      # If there's a subcategory, we also need to find which category is being
      # hovered over
      if(subcat != 'none') {

        y <- hover$y
        if(y < 0) {
          df <- df[which(df$value < 0), ]
          df$value <- abs(df$value)
          y <- abs(y)
        } else {
          df <- df[which(df$value > 0), ]
        }

        # Find which segment of the stacked bar the hover is closest to
        stackedSum <- sum(df$value) - cumsum(df$value)
        index <- which(stackedSum - y < 0)[1]

        # Don't show hover if position is above the sum of positive values or
        # below the sum of negative values.
        if(y > sum(df$value)) return(NULL) # Hover is above the bar
        if(is.na(index)) return(NULL) # Hover is below the bar

        # Get the region name and value for display
        val <- paste(df[[subcat]][index],
                     round(df$value[index], digits=1) * sign(hover$y),
                     sep=': ')

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
        p(HTML(val)))

    })

    callModule(landingPage, "dashboard", files[[defaultProj]])

    callModule(scenarioComparison, "ssp")

    initComplete(TRUE)

})
