library(shiny)
library(rgcam)
library(magrittr)

server.R <- function() {}              # marker function
srcdir <- getSrcDirectory(server.R)
source(file.path(srcdir,'server-helpers.R'))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

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
        }
        list(project.filename=project.filename, project.data=project.data)
    })

    output$projFilename <- renderText({
        getProjectName(rFileinfo)
    })

    output$scenarios <- renderText({
        pd <- rFileinfo()$project.data
        if(is.null(pd)) {
            '->none<-'
        } else {
            listScenarios(rFileinfo()$project.data)} %>% paste(collapse='\n')
        })

    output$queries <- renderText({
        pd <- rFileinfo()$project.data
        if(is.null(pd)) {
            '->none<-'
        } else {
            scen1 <- listScenarios(pd)[1]
            listQueries(pd, scen1) %>% paste(collapse='\n')
        }
    })
})
