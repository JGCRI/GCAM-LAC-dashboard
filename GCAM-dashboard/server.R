library(shiny)
library(rgcam)
library(magrittr)

server.R <- function() {}              # marker function
srcdir <- getSrcDirectory(server.R)
source(file.path(srcdir,'server-helpers.R'))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    rFileinfo <- reactive({
        fileinfo <- input$projectFile
        print(fileinfo)
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

    output$projFilename <- renderText({
        getProjectName(rFileinfo)
    })

    output$scenarios <- renderText({
        getProjectScenarios(rFileinfo)
    })

    output$queries <- renderText({
        getScenarioQueries(rFileinfo, input$scenarioInput)
    })
})
