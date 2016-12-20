### Helper functions for the server side of the app.

### Conventions:
###    rFileinfo:  The reactive fileinfo structure returned by the file browser

getProjectName <- function(rFileinfo)
{
    fn <- rFileinfo()$project.filename
    if(is.null(fn)) {
        '->none<-'
    } else {
        rFileinfo()$project.filename
    }
}

getProjectScenarios <- function(rFileinfo, concat=NULL)
{
    pd <- rFileinfo()$project.data
    if(is.null(pd)) {
        '->none<-'
    } else {
        listScenarios(rFileinfo()$project.data) %>% paste(collapse=concat)
    }
}

getScenarioQueries <- function(rFileinfo, scenarios, concat=NULL)
{
    prj <- rFileinfo()$project.data
    if(is.null(prj)) {
        '->none<-'
    }
    else if(length(scenarios) == 0 || all(scenarios=='')) {
        '->No scenarios selected<-'
    }
    else {
        lapply(scenarios, . %>% listQueries(prj, .)) %>%
            Reduce(intersect,.) %>%
            paste(collapse=concat)
    }
}


### Helpers for making plots
library('ggplot2')

default.plot <- function(label.text='No data selected')
{
    ggplot(mapping=aes(x=0,y=0)) + geom_label(aes_(label=label.text), size=10) +
        theme_minimal()
}

plotMap <- function(prjdata, query, pltscen, diffscen, projection)
{
    if(is.null(prjdata)) {
        default.plot()
    }
    else if( is.null(pltscen) ||
             !pltscen %in% listScenarios(prjdata) ||
            (!is.null(diffscen) && pltscen==diffscen) ) {
        ## These condition(s) all indicate a transitional state
        last_plot()
    }
    else {
        scens <- paste(c(pltscen, diffscen), collapse=', ')
        cat('scenarios: ', scens, '\n')
        qr <- paste('query: ', query)
        cat('queries: ', query, '\n')
        cat('projection: ', projection, '\n')
        label.txt <- paste(c('scenarios: ', 'query: ', 'projection: '),
                           c(scens, query, projection), collapse='\n')
        default.plot(label.txt)
    }
}
