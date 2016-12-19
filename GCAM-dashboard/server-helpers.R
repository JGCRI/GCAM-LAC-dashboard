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

getProjectScenarios <- function(rFileinfo)
{
    pd <- rFileinfo()$project.data
    if(is.null(pd)) {
        '->none<-'
    } else {
        listScenarios(rFileinfo()$project.data) %>% paste(collapse='\n')
    }
}

getScenarioQueries <- function(rFileinfo, scenarios)
{
    prj <- rFileinfo()$project.data
    if(is.null(prj)) {
        '->none<-'
    }
    else if(length(scenarios) == 0) {
        '->No scenarios selected<-'
    }
    else {
        lapply(scenarios, . %>% listQueries(prj, .)) %>%
            Reduce(intersect,.) %>%
            paste(collapse='\n')
    }
}
