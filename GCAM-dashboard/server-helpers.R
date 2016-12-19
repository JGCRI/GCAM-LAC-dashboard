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
