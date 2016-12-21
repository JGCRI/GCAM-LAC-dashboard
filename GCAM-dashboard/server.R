library(shiny)
library(rgcam)
library(magrittr)

server.R <- function() {}              # marker function
srcdir <- getSrcDirectory(server.R)
source(file.path(srcdir,'server-helpers.R'))

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
            if(!all(queries == new.queries)) {
                queries <<- new.queries
                updateSelectInput(session, 'plotQuery', choices=queries)
            }
        }

    })


    output$projFilename <- renderText({
        getProjectName(rFileinfo)
    })

    output$scenarios <- renderText({
        getProjectScenarios(rFileinfo, concat='\n')
    })

    output$queries <- renderText({
        getScenarioQueries(rFileinfo, input$scenarioInput, concat='\n')
    })

    output$mapPlot <- renderPlot({
        diffscen <- if(input$diffCheck) {
            input$diffScenario
        } else {
            NULL
        }
        year <- 2050             # placeholder until we put a year control in.
        plotMap(rFileinfo()$project.data, input$plotQuery,
                input$plotScenario, diffscen, input$mapProjection, year)
    })

### Debugging
    ## observe({
    ##             print('****************Change of Input****************')
    ##             cat('plotScenario: ', input$plotScenario, '\n')
    ##             cat('diffScenario: ', input$diffScenario, '\n')
    ##             cat('plotQuery: ', input$plotQuery, '\n')
    ##         })
})
