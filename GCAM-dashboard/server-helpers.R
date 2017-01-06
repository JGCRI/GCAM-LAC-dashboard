### Helper functions for the server side of the app.

### Conventions:
###    rFileinfo:  The reactive fileinfo structure returned by the file browser

tag.noscen <- '->No scenarios selected<-'     # placeholder when no scenario selected
year.regex <- 'X[0-9]{4}'
year.cols <- function(df) {grep(year.regex, names(df), value=TRUE)}
strip.xyear <- function(xyear) {as.integer(substr(xyear, 2, 5))}

#### State variables
last.region.filter <- NULL


## UI helpers

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
        tag.noscen
    }
    else {
        lapply(scenarios, . %>% listQueries(prj, .)) %>%
            Reduce(intersect,.) %>%
            paste(collapse=concat)
    }
}


### Helpers for making plots
library('ggplot2')
library('dplyr')
library('tidyr')
library('lazyeval')
library('RColorBrewer')
library('gcammaptools')

data(map.rgn32)
data(map.basin235)

default.plot <- function(label.text='No data selected')
{
    ggplot(mapping=aes(x=0,y=0)) + geom_label(aes_(label=label.text), size=10) +
        theme_minimal()
}

plotMap <- function(prjdata, query, pltscen, diffscen, projselect, year)
{
    if(is.null(prjdata)) {
        default.plot()
    }
    else if(is.null(pltscen) ||
            !pltscen %in% listScenarios(prjdata) ||
            (!is.null(diffscen) && pltscen==diffscen) ||
            query==tag.noscen) {
        ## These condition(s) all indicate a transitional state, so don't do anything.
        last_plot()
    }
    else {
        scens <- paste(c(pltscen, diffscen), collapse=', ')

        is.diff <- !is.null(diffscen)      # We'll do a couple of things differently for a diff plot

        mapset <- determineMapset(prjdata, pltscen, query)
        key <- if(mapset==basin235) 'basin' else 'region'
        pltdata <- getPlotData(prjdata, query, pltscen, diffscen, key)

        ## map plot is expecting the column coresponding to the map locations to
        ## be called "region", so if we're working with water basins, we have to
        ## rename it.
        if(mapset==basin235)
            pltdata$region <- pltdata$basin

        mapLimits <- getMapLimits(pltdata, is.diff)
        unitstr <- summarize.unit(pltdata$Units)
        pltdata <- addRegionID(pltdata, lookupfile=mapset, drops=mapset)
        if(mapset==rgn32)
            map.dat <- map.rgn32
        else if(mapset==basin235)
            map.dat <- map.basin235
        plt.map <- merge(map.dat, pltdata)

        ## get the projection and extent for the map
        map.params <- getMapParams(projselect)

        pal <- getMapPalette(is.diff)

        ## name of the year column
        xyear <- paste('X',year, sep='')

        plot_GCAM(plt.map, col=xyear,
                  proj=map.params$proj, extent=map.params$ext, orientation=map.params$orientation,
                  colors=pal, legend=TRUE, limits=mapLimits, qtitle=unitstr) +
            guides(fill=guide_colorbar(title.position='bottom', title.hjust=0.5,
                                       barwidth=unit(4,'in')))
    }
}

## Figure out which map to plot the query on.  Right now we assume that if the
## query table contains a 'basin' column, then we want to plot on the basin map;
## otherwise we plot on the region map.
determineMapset <- function(prjdata, pltscen, query)
{
    tp <- getQuery(prjdata, query, pltscen)
    if('basin' %in% names(tp)) {
        ## mapping the 235 basins
        mapset <- basin235
    }
    else {
        ## mapping the 32 regions
        mapset <- rgn32
    }
}

### Data wrangling

getPlotData <- function(prjdata, query, pltscen, diffscen, key, filtervar=NULL,
                        filterset=NULL)
{
    ## prjdata:  Project data structure
    ## query:  Query to plot
    ## pltscen:  Scenario to plot
    ## diffscen:  Difference scenario, if any
    ## key: aggregation variable.  (e.g., 'region' or 'sector')
    ## filtervar:  if not NULL, filter on this variable before aggregating
    ## filterset:  set of values to include in the filter operation.  Ignored if
    ##             filtervar is NULL.
    tp <- getQuery(prjdata, query, pltscen)
    if(!is.null(diffscen)) {
        dp <- getQuery(prjdata, query, diffscen)
    }
    else {
        dp <- NULL
    }

    yearcols <- year.cols(tp)
    if(!is.null(dp)) {
        ## we're doing a difference plot, so subtract the difference scenario.
        tp[yearcols] <- as.matrix(tp[yearcols]) - as.matrix(dp[yearcols])
    }

    ## If filtering is in effect, do it now
    if(!is.null(filtervar) &&
       !is.null(filterset) &&
       length(filterset) > 0
       ) {
        ## This is horrible.  There has got to be a better way to do this.
        tp <- filter_(tp, paste(fcol,
                                '%in% c(',
                                paste(shQuote(filterset), collapse=', '), ')'
                                ))
    }

    ## select the key and year columns, then sum all values with the same key.  Force the sum
    ## to have the same name as the original column
    outcol <- c(
        lapply(yearcols, function(xyear){interp(~sum(col), col=as.name(xyear))}),
        ~summarize.unit(Units)
    )
    tp <- select_(tp, .dots=c(key, yearcols, 'Units')) %>% group_by_(.dots=key) %>%
        summarise_(.dots=setNames(outcol, c(yearcols, 'Units')))

    ## Occasionally you get a region with "0.0" for the unit string because most of its entries were zero.
    ## Fix these so that the column all has the same unit.
    tp$Units <- summarize.unit(tp$Units)
    tp
}

getMapParams <- function(projselect)
{
    ## currently valid values are 'global' and 'lac'
    if(projselect == 'global') {
        list(proj=eck3, ext=EXTENT_WORLD, orientation=NULL)
    }
    else if(projselect == 'lac') {
        list(proj=ortho, ext=EXTENT_LA, orientation=ORIENTATION_LA)
    }
}

getMapPalette <- function(is.diff)
{
    if(is.diff) {
        brewer.pal(9, 'RdBu')
    } else {
        brewer.pal(9,'Blues')
    }
}

getMapLimits <- function(pltdata, is.diff)
{
    pltdata <- select(pltdata, matches(year.regex))
    limits <- c(min(pltdata, na.rm=TRUE), max(pltdata, na.rm=TRUE))
    if(is.diff) {
        ## For a difference plot, force the limits to be balanced on either side of zero
        mag <- max(abs(limits))
        c(-mag, mag)
    }
    else {
        limits
    }
}

summarize.unit <- function(unitcol)
{
    ## Summarize the unit column of a GCAM data frame by taking the most common entry.  (GCAM isn't always great about
    ## setting its unit tag)
    unitcol[which.max(table(unitcol))]
}

plotTime <- function(prjdata, query, scen, diffscen, subcatvar, filter, rgns)
{
    ## prjdata: a project data structure
    ## query:  Name of the query to plot
    ## scen:  Plot scenario
    ## diffscen:  Difference scenario, or NULL if none
    ## subcatvar:  Variable to use for subcategories in the plot
    ## filter:  If TRUE, then filter to regions in the rgns argument
    ## rgns:  Regions to filter to, if filter is set.
##    label <- paste(query,scen,diffscen,subcatvar,filter,rgns, sep='\n')
##    default.plot(label.text=label)

    if(is.null(prjdata)) {
        default.plot()
    }
    else {
        if(filter)
            filtervar <- 'region'
        else
            filtervar <- NULL

        if(subcatvar=='none')
            subcatvar <- NULL

        pltdata <- getPlotData(prjdata, query, scen, diffscen, subcatvar,
                               filtervar, rgns) %>%
            gather(year, value, matches(year.regex)) %>%
            mutate(year=strip.xyear(year))

        ggplot(pltdata, aes_string('year','value', fill=subcatvar)) +
            geom_bar(stat='identity') + theme_minimal() + ylab(pltdata$Units)
    }
}
