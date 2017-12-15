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
    # defaultData and sspData (in the data directory).
    defaultProj <- 'GCAM LAC'
    sspProj <- 'SSP'

    # files is the master list of project files
    files <- setNames(list(defaultData, sspData),
                      c(defaultProj, sspProj))

    # To access the current project file, refer to the select in the upper right
    updateSelectInput(session, 'fileList', choices=names(files))

    # Initialize reactive values to hold the data frame being displayed in both
    # the time plot view and the map plot view. These data frames are used for
    # getting hover values and for viewing the raw table data.
    timePlot.df <- reactiveVal()
    mapZoom <- reactiveVal(0)

    # Initialize state variables to facilitate the smooth updating of the plot
    # on the Explore panel. The scenario, query, and subcategory selectInputs
    # are part of a chain that starts with the projectFile input, and ends with
    # the plot. The updating of the choices in one of the three selectInputs is
    # triggered by the user making a new selection XOR a change in an input
    # higher up on the chain. In the latter case, the reactiveValues in the
    # 'updates' variable let the inputs further down the chain know they need to
    # update.
    #
    # This whole structure arises because updateSelectInput only triggers
    # observers of that input if the selected parameter changes. It may be
    # possible in the future to avoid this semi-roundabout way of signaling, if
    # this feature gets implemented: https://github.com/rstudio/shiny/issues/928
    updates <- reactiveValues(scenarios = 0, queries = 0, subcats = 0)
    previous <- list(scenario = list(choices = list(), selected = ""),
                     query    = list(choices = list(), selected = ""),
                     subcat   = list(choices = list(), selected = ""))


    ## ----- PLOT OPTION UPDATES -----

    ## FILE UPDATES - When a new file is uploaded, update available projects
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

    ## SCENARIO UPDATES - When a new project is selected, update available scenarios
    observe({
      if(input$fileList != "") {
        project.data <- files[[input$fileList]]
        scens <- listScenarios(project.data)

        if(isTRUE(all.equal(scens, previous$scenario$choices))) {
          updates$scenarios <- isolate(updates$scenarios) + 1
        }
        else {
          updateSelectInput(session, 'scenarioInput', choices=scens)
          previous$scenario$choices <<- scens
        }

        updateSelectInput(session, 'diffScenario', choices=scens)
        updateSelectInput(session, 'mapScenario', choices=scens)
      }
    })

    ## When a new scenario is selected, update available queries
    observe({
      prj <- isolate(files[[input$fileList]])
      scen <- input$scenarioInput
      query <- isolate(input$plotQuery)
      trigger <- updates$scenarios

      # Get the current scenario (or scenarios for a difference plot)
      if(scen == "") {
        return() # There should always be a dataset loaded
      }
      else if(input$diffCheck) {
        qscenarios <- c(scen, input$diffScenario)
      }
      else {
        qscenarios <- scen
      }

      queries <- getScenarioQueries(prj, qscenarios)

      ## Preserve selected value if possible, else take first from list
      sel <- query
      if(!(sel %in% queries)) sel <- queries[1]

      newQuerySelected <- sel != previous$query$selected
      newQueryChoices <- !isTRUE(all.equal(queries, previous$query$choices))

      if(newQuerySelected || newQueryChoices) {
        updateSelectInput(session, 'plotQuery', choices = queries, selected = sel)
        previous$scenario$selected <- scen
      }

      # If the selection hasn't changed, still trigger queries to refresh
      if(!newQuerySelected) updates$queries <- isolate(updates$queries) + 1

      previous$query$choices <<- queries
      previous$query$selected <<- sel

      updateSelectInput(session, 'mapQuery', choices = queries, selected = sel)
    })

    observe({
        prj <- isolate(files[[input$fileList]])
        scen <- input$mapScenario
        query <- input$mapQuery
        catvars <- input$mapCats

        if(uiStateValid(prj, scen, query)) {
          df <- getQuery(prj, query, scen)

          # The first one is region and the next is the most aggregated
          subcat <- getQuerySubcategories(prj, scen, query)[2]
          if(!is.null(catvars)) {
            okunits <- df[df[[subcat]] %in% catvars, ][[1, 'Units']]
            choices <- dplyr::filter(df, Units == okunits) %>%
                       dplyr::pull(UQ(as.name(subcat))) %>%
                       unique()
          } else {
            choices <- df[[subcat]] %>% unique()
          }

          updateSelectInput(session, 'mapCats', choices=choices)
          # updateCheckboxGroupInput(session, 'mapFilters', choices=catvars)
        }
    })

    ## When a new query is selected, update available subcategories
    observe({
        prj <- isolate(files[[input$fileList]])
        scen <- isolate(input$scenarioInput)
        query <- input$plotQuery
        subcat <- isolate(input$tvSubcatVar)
        trigger <- updates$queries

        ## Assumes that a particular query has the same columns in all scenarios
        if(uiStateValid(prj, scen, query)) {
          catvars <- getQuerySubcategories(prj, scen, query)
          catvars <- c('none', catvars)
          selected <- getNewSubcategory(prj, scen, query, subcat)

          newSubcatSelected <- selected != previous$subcat$selected
          newSubcatChoices <- !isTRUE(all.equal(catvars, previous$subcat$choices))

          if(newSubcatSelected || newSubcatChoices) {
            updateSelectInput(session, 'tvSubcatVar', choices=catvars,
                             selected=selected)
            previous$query$selected <<- query
          }

          # If the selection hasn't changed, still trigger plot to redraw
          if(!newSubcatSelected) updates$subcats <- isolate(updates$subcats) + 1

          previous$subcat$choices <<- catvars
          previous$subcat$selected <<- selected
        }
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
    observeEvent(input$triggerDiffCheck, { updateCheckboxInput(session, 'diffCheck', value = F) })

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
      subcat <- input$mapCats

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

      plotMap(prj, query, scen, NULL, input$mapExtent, subcat, year, map, mapZoom())
    })

    output$mapAltPlot <- renderPlot({
      prj <- files[[input$fileList]]
      scen <- input$mapScenario
      query <- input$mapQuery
      subcat <- input$mapCats
      if(!uiStateValid(prj, scen, query)) return(default.plot("Updating..."))

      diffscen <- if(input$diffCheck) input$diffScenario
      filters <- list(region = lac.rgns)

      # TODO: don't get subcat this way
      sc <- getQuerySubcategories(prj, scen, query)
      if (length(sc) > 1) filters[[sc[2]]] <- subcat

      # If the scenario has changed, the value of the query selector may not
      # be valid anymore.
      availableQueries <- getScenarioQueries(prj, c(scen, diffscen))
      if(!query %in% availableQueries) {
        query <- availableQueries[1]
      }

      plotTime(prj, query, scen, NULL, "region", filters)
    })

    region.filter <- callModule(regionFilter, "tpFilters")

    output$timePlot <- renderPlot({
        prj <- isolate(files[[input$fileList]])
        scen <- isolate(input$scenarioInput)
        query <- isolate(input$plotQuery)
        subcat <- input$tvSubcatVar
        trigger <- updates$subcats

        if(!uiStateValid(prj, scen, query)) return(default.plot())

        diffscen <- if(input$diffCheck) input$diffScenario else NULL
        if(!is.null(diffscen) && diffscen == scen) {
          timePlot.df(NULL)
          return(default.plot("Scenarios are the same"))
        }

        # If the scenario has changed, the value of the query selector may not
        # be valid anymore.
        availableQueries <- getScenarioQueries(prj, c(scen, diffscen))
        if(!query %in% availableQueries) {
          query <- availableQueries[1]
        }

        # If the query has changed, the value of the subcategory selector may
        # not be valid anymore. Change it to the new one.
        tvSubcatVar <- getNewSubcategory(prj, scen, query, subcat)

        output$timeTable <- renderDataTable(options = list(scrollX = TRUE), {
          if(tvSubcatVar == 'none')
            timePlot.df()
          else
            convertToWideform(timePlot.df())
        })

        # Set the data table title
        tableTitle <- query
        if(tvSubcatVar != 'none') {
          tableTitle <- paste(query, tvSubcatVar, sep = ' by ')
        }
        output$tableTitle <- renderText(tableTitle)

        rgnfilter <- list(region = region.filter())

        # Update the plot and the time plot data frame (used for the hover)
        plt <- plotTime(prj, query, scen, diffscen, tvSubcatVar, rgnfilter)
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
        downloadData <- getQuery(files[[input$fileList]], input$plotQuery) %>%
                        convertToWideform()
        write.csv(downloadData, file, row.names = FALSE)
      }
    )

    dashboardData <- reactive({
      proj <- files[[input$fileList]]
      s <- listScenarios(proj)
      q <- listQueries(proj, s[1])
      co2Present <- grep("co2.*emissions", q, ignore.case = T) %>% length > 0
      nrgPresent <- grep("primary.*energy", q, ignore.case = T) %>% length > 0
      agrPresent <- grep("biomass.*production", q, ignore.case = T) %>% length > 0
      bioPresent <- grep("agriculture.*production", q, ignore.case = T) %>% length > 0
      if(is.null(proj))
        list("", "")
      else if(co2Present && nrgPresent && agrPresent && bioPresent)
        list(err = "", proj = proj)
      else
        list(err = paste0(length(which(!c(co2Present, nrgPresent, agrPresent, bioPresent))),
                   " queries not found for the dataset ", input$fileList,
                   ". Using 'GCAM LAC' dataset instead."),
             proj = defaultData)
    })

    callModule(landingPage, "dashboard", dashboardData)

    # Add a hover over the time plot bar chart
    callModule(barChartHover, "timePlot", reactive(input$exploreHover),
               reactive(timePlot.df()), reactive(input$tvSubcatVar))

    callModule(scenarioComparison, "ssp", reactive(files[[input$fileList]]))

    initComplete(TRUE)

})
