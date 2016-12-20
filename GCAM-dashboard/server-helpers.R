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
