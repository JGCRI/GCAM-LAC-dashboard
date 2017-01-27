library(shiny)
library(rgcam)
library(magrittr)

server.R <- function() {}              # marker function
srcdir <- getSrcDirectory(server.R)
source(file.path(srcdir,'server-helpers.R'))
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
            project.filename <- NULL
            project.data <- NULL
        }
        else {
            project.data <- loadProject(fileinfo$datapath)   # should be only one file
            project.filename <- fileinfo$name
            updateSelectInput(session, 'scenarioInput', choices=listScenarios(project.data))
        }
        list(project.filename=project.filename, project.data=project.data)
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
            if(input$diffCheck) {
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
            ## Asumes that a particular query has the same columns in all scenarios
            querycols <- getQuery(prj, query, scen) %>% names
            catvars <- grep(year.regex, querycols, invert=TRUE, value=TRUE) %>%
                grep('scenario|Units', . , invert=TRUE, value=TRUE)
            updateSelectInput(session, 'tvSubcatVar', choices=c('none',
                                                      catvars))
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



    output$projFilename <- renderText({
        getProjectName(rFileinfo)
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
            plotMap(rFileinfo()$project.data, input$plotQuery,
                    input$plotScenario, diffscen, input$mapProjection, year)
        }
        else {
            default.plot('Updating')
        }
    })

    output$mapName <- renderText({input$plotQuery})

    output$timePlot <- renderPlot({
        if(uiStateValid( rFileinfo()$project.data, input$plotScenario,
                        input$plotQuery )) {
               diffscen <- if(input$diffCheck) {
                   input$diffScenario
               } else {
                     NULL
                 }
               region.filter <- c(input$tvRgns1, input$tvRgns2, input$tvRgns3,
                                  input$tvRgns4, input$tvRgns5)
               last.region.filter <<- region.filter
               plotTime(rFileinfo()$project.data, input$plotQuery,
                        input$plotScenario, diffscen, input$tvSubcatVar,
                        input$tvFilterCheck, region.filter)
           }
        else {                          # UI state is invalid
            default.plot('Updating')
        }
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
