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
        if(is.null(fileinfo)) {
            project.data <- NULL
        }
        else {
            project.data <- loadProject(fileinfo$datapath)   # should be only one file
            updateSelectInput(session, 'scenarioInput', choices=listScenarios(project.data))
        }
        list(project.data=project.data)
    })

    ## Update controls on sidebar in response to user selections
    observe({
        if(is.null(rFileinfo()$project.data)) {
            new.scenarios <- list()
        }
        else {
            new.scenarios <- getProjectScenarios(rFileinfo)
        }

        if(!all(scenarios == new.scenarios)) {
            scenarios <<- new.scenarios # Update UI state
            updateSelectInput(session, 'plotScenario', choices=scenarios)
            updateSelectInput(session, 'diffScenario', choices=scenarios)
        }

        if(!is.null(rFileinfo()$project.data)) {
            if(input$plotScenario == "") {
                # When first loading a dataset, no scenario is selected
                qscenarios <- scenarios
            }
            else if(input$diffCheck) {
                qscenarios <- c(input$plotScenario, input$diffScenario)
            }
            else {
                qscenarios <- input$plotScenario
            }
            new.queries <- getScenarioQueries(rFileinfo, qscenarios)
            if(!input$inclSpatial) {
                ## Filter out grid queries, if requested.
                nonGrid <- sapply(new.queries,
                                  function(q) {!isGrid(rFileinfo()$project.data,
                                                      input$plotScenario, q)})
                new.queries <- new.queries[nonGrid]
            }
            if(!identical(queries,new.queries)) {
                ## capture new query list
                queries <<- new.queries
                ## preserve selected value if possible
                sel <- input$plotQuery
                if(!(sel %in% queries))
                   sel <- NULL          # allow update to reset selection
                updateSelectInput(session, 'plotQuery', choices=queries,
                                  selected=sel)
            }
        }

    })

    observe({
        ## update the subcategory selector on the time value plot and the limits
        ## on the time slider on the map plot.  Only do this when the selected
        ## plot query changes.
        scen <- isolate(input$plotScenario)
        prj <- isolate(rFileinfo()$project.data)
        query <- input$plotQuery
        if(uiStateValid(prj, scen, query)) {
            ## Assumes that a particular query has the same columns in all scenarios
            querycols <- getQuery(prj, query, scen) %>% names
            catvars <- querycols[!querycols %in% c('scenario', 'Units', 'year', 'value')]
            prevSubcat <- if(input$tvSubcatVar %in% catvars) input$tvSubcatVar else 'none'
            updateSelectInput(session, 'tvSubcatVar', choices=c('none', catvars),
                              selected=prevSubcat)

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

    # Update checkboxes when select or deselect checkbox is pressed
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
    observeEvent(input$rgn , {
      # Select all
      sAll <- input$rgnSelectAll%%2 == 0
      updateRegionFilter(session, 'rgnSelectAll', 'tvRgns1', sAll, africa.rgns)
      updateRegionFilter(session, 'rgnSelectAll', 'tvRgns2', sAll, lac.rgns)
      updateRegionFilter(session, 'rgnSelectAll', 'tvRgns3', sAll, north.america.rgns)
      updateRegionFilter(session, 'rgnSelectAll', 'tvRgns4', sAll, europe.rgns)
      updateRegionFilter(session, 'rgnSelectAll', 'tvRgns5', sAll, asiapac.rgns)
      # Update button text
      # updateCheckboxInput(session, 'rgnSelectAll', label = paste0(ifelse(sAll, "Des", "S"), "elect all"))
    })

    output$projectSize <- renderText({
        if(is.null(rFileinfo()$project.data))
            0
        else
            format(object.size(rFileinfo()$project.data), units='auto')
    })

    output$scenarios <- renderText({
        getProjectScenarios(rFileinfo, concat='\n')
    })

    output$queries <- renderText({
        getScenarioQueries(rFileinfo, input$scenarioInput, concat='\n')
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
        prj <- rFileinfo()$project.data
        scen <- input$plotScenario
        query <- input$plotQuery
        if(uiStateValid(prj, scen, query)) {
               diffscen <- if(input$diffCheck) {
                   input$diffScenario
               } else {
                   NULL
               }
               tvSubcatVar <- input$tvSubcatVar

               region.filter <- c(input$tvRgns1, input$tvRgns2, input$tvRgns3,
                                  input$tvRgns4, input$tvRgns5)
               last.region.filter <<- region.filter

               # If the query has changed, the value of the subcategory selector
               # may not be valid anymore. Change it to none.
               if(!tvSubcatVar %in% names(getQuery(prj, query, scen))) {
                  tvSubcatVar <- 'none'
               }

               plotTime(prj, query, scen, diffscen, tvSubcatVar, region.filter)
           }
        else {                          # UI state is invalid
            default.plot('No Data')
        }
    })

    output$hoverInfo <- renderUI({
      hover <- input$energyHover
      if (is.null(hover)) return(NULL)
      prj <- isolate(rFileinfo()$project.data)
      scen <- isolate(input$plotScenario)
      query <- isolate(input$plotQuery)
      tvSubcatVar <- isolate(input$tvSubcatVar)
      region.filter <- c(input$tvRgns1, input$tvRgns2, input$tvRgns3,
                         input$tvRgns4, input$tvRgns5)
      last.region.filter <<- region.filter

      df <- getPlotData(prj, query, scen, NULL, tvSubcatVar, 'region', region.filter)

      # Detect the year of the bar that is being hovered over
      hoverYear <- df[which.min(abs(df$year - hover$x)), 'year'][[1]]
      df <- dplyr::filter(df, year==hoverYear)

      # If there's a subcategory, we also need to find which category is being
      # hovered over
      if (tvSubcatVar == 'region') {
        # Return NULL if the hover is above the top of the bar
        if (hover$y > sum(df$value)) return(NULL)

        # Find which segment of the stacked bar the hover is closest to
        stackedSum <- rev(cumsum(df$value))
        index <- which.min(abs(stackedSum - hover$y))

        # Make sure we are choosing the segment the hover is actually over
        if (df[index,]$value < hover$y)
          index <- index - 1
        val <- paste(df[index,]$region, round(df[index,]$value, digits=1), sep=': ')

      } else {
        val <- df[which.min(abs(df$year - hover$x)), 'value'] %>%
               round(digits=1)
        if (hover$y > val) return(NULL)
        val <- as.character(val)
      }

      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top

      # actual tooltip created as wellPanel
      absolutePanel(
        class = 'hoverPanel',
        top = paste0(round(top_px) - 30, "px"),
        left = paste0(round(left_px) - 30, "px"),
        # style = style,
        p(HTML(val)))

    })

    output$landingPlot <- renderPlot({
      if(uiStateValid( rFileinfo()$project.data, input$plotScenario,
                       isolate(input$plotQuery) )) {
        query <- "Hydrogen production by technology"
        plotTime(rFileinfo()$project.data, query, input$plotScenario, NULL,
                 "technology", lac.rgns)
      }
      else {
        default.plot('No Data')
      }
    })

    output$landingPlot2 <- renderPlot({
      if(uiStateValid( rFileinfo()$project.data, input$plotScenario,
                       input$plotQuery )) {
        plotTime(rFileinfo()$project.data, input$plotQuery, input$plotScenario,
                 NULL, "region", lac.rgns)
      }
      else {
        default.plot('No Data')
      }
    })

    output$landingPlot3 <- renderPlot({
      if(uiStateValid( rFileinfo()$project.data, input$plotScenario,
                       isolate(input$plotQuery) )) {
        query <- "CO2 emissions by region"
        plotMap(rFileinfo()$project.data, query, input$plotScenario, NULL,
                 "lac", 2050)
      }
      else {
        default.plot('No Data')
      }
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

    sspData <- loadProject(system.file('ssp.dat', package = "GCAMdashboard"))

    output$sspComparison <- renderPlot({
      query <- "population"
      scens <- input$sspChoices
      plotScenComparison(sspData, query, scens, NULL, "region", lac.rgns)
    })

    output$ssp1 <- renderPlot({
      query <- "population"
      pscen <- "SSP1"
      plotTime(sspData, query, pscen, NULL, "region", lac.rgns)
    })

    output$ssp2 <- renderPlot({
      query <- "population"
      pscen <- "SSP2"
      plotTime(sspData, query, pscen, NULL, "region", lac.rgns)
    })

    output$ssp3 <- renderPlot({
      query <- "population"
      pscen <- "SSP3"
      plotTime(sspData, query, pscen, NULL, "region", lac.rgns)
    })

    output$ssp4 <- renderPlot({
      query <- "population"
      pscen <- "SSP4"
      plotTime(sspData, query, pscen, NULL, "region", lac.rgns)
    })

    output$ssp5 <- renderPlot({
      query <- "population"
      pscen <- "SSP5"
      plotTime(sspData, query, pscen, NULL, "region", lac.rgns)
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
