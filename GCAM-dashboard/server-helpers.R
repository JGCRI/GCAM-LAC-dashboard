### Helper functions for the server side of the app.

### Conventions:
###    rFileinfo:  The reactive fileinfo structure returned by the file browser

tag.noscen <- '->No scenarios selected<-'     # placeholder when no scenario selected
year.regex <- 'X[0-9]{4}'

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
        mapLimits <- getMapLimits(prjdata, pltscen, diffscen, query)

        ## name of the year column
        xyear <- paste('X',year, sep='')

        pltdata <- getPlotData(prjdata, query, pltscen, diffscen, year)
        unitstr <- summarize.unit(pltdata$Units)
        mapset <- attr(pltdata,'mapset')
        pltdata <- addRegionID(pltdata, lookupfile=mapset, drops=mapset)
        if(mapset==rgn32)
            map.dat <- map.rgn32
        else if(mapset==basin235)
            map.dat <- map.basin235
        plt.map <- merge(map.dat, pltdata)

        ## get the projection and extent for the map
        map.params <- getMapParams(projselect)

        pal <- getMapPalette(is.diff)

        plot_GCAM(plt.map, col=xyear,
                  proj=map.params$proj, extent=map.params$ext, orientation=map.params$orientation,
                  colors=pal, legend=TRUE, limits=mapLimits, qtitle=unitstr) +
            guides(fill=guide_colorbar(title.position='bottom', title.hjust=0.5,
                                       barwidth=unit(4,'in')))
    }
}


### Data wrangling

getPlotData <- function(prjdata, query, pltscen, diffscen, year)
{
    tp <- getQuery(prjdata, query, pltscen)
    if(!is.null(diffscen)) {
        dp <- getQuery(prjdata, query, diffscen)
    }
    else {
        dp <- NULL
    }

    ## Try to figure out what type of map we are supposed to be plotting here.
    if('basin' %in% names(tp)) {
        ## mapping the 235 basins
        key <- 'basin'
        mapset <- basin235
    }
    else {
        ## mapping the 32 regions
        key <- 'region'
        mapset <- rgn32
    }

    xyear <- paste('X',year,sep='')
    if(!is.null(dp)) {
        ## we're doing a difference plot, so aggregate the difference scenario and subtract
        tp[[xyear]] <- tp[[xyear]] - dp[[xyear]]
    }
    ## select the key and year columns, then sum all values with the same key.  Force the sum
    ## to have the same name as the original column
    outcol <- list(interp(~sum(col), col=as.name(xyear)), ~summarize.unit(Units))
    tp <- select_(tp, .dots=c(key, xyear, 'Units')) %>% group_by_(.dots=key) %>%
        summarise_(.dots=setNames(outcol, c(xyear, 'Units'))) %>% rename_('region'=key)

    ## Occasionally you get a region with "0.0" for the unit string because most of its entries were zero.
    ## Fix these so that the column all has the same unit.
    tp$Units <- summarize.unit(tp$Units)

    attr(tp, 'mapset') <- mapset       # kind of ugly.  Is there some other way we could communicate this?
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

getMapLimits <- function(prjdata, pltscen, diffscen, query)
{
    pltdata <-
        getQuery(prjdata, query, pltscen) %>%
        select(matches(year.regex)) %>%
        as.matrix

    if(!is.null(diffscen)) {
        diffdata <-
            getQuery(prjdata, query, diffscen) %>%
            select(matches(year.regex)) %>%
            as.matrix
        pltdata <- pltdata - diffdata
    }

    limits <- c(min(pltdata, na.rm=TRUE), max(pltdata, na.rm=TRUE))
    if(is.null(diffscen)) {
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
