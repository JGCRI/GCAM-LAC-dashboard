### Helper functions for the server side of the app.

### Conventions:
###    rFileinfo:  The reactive fileinfo structure returned by the file browser

tag.noscen <- '->No scenarios selected<-'     # placeholder when no scenario selected

#### State variables
last.region.filter <- NULL

## Color schemes
rgb255 <- function(r, g, b) {rgb(r,g,b, maxColorValue=255)}

region32 <- c(
    'Africa_Northern' = rgb255(139,69,19),
    'Africa_Eastern' = rgb255(139,115,88),
    'Africa_Southern' = rgb255(255,211,155),
    'Africa_Western' = rgb255(255,185,15),
    'South Africa' = rgb255(255,215,0),

    'Canada' = rgb255(224,238,224),
    'USA' = rgb255(77,77,77),

    'Argentina' = rgb255(0,100,0),
    'Brazil' = rgb255(154,205,50),
    'Central America and Caribbean' = rgb255(46,139,87),
    'Colombia' = rgb255(102,205,170),
    'Mexico' = rgb255(50,205,50),
    'South America_Southern' = rgb255(72,209,204),
    'South America_Northern' = rgb255(0,255,0),

    'EU-12' = rgb255(25,25,112),
    'EU-15' = rgb255(131,111,255),
    'Europe_Eastern' = rgb255(173,216,230),
    'Europe_Non_EU' = rgb255(0,104,139),
    'European Free Trade Association' = rgb255(58,95,205),

    'Russia' = rgb255(104,34,139),
    'China' = rgb255(255,0,0),
    'Middle East' = rgb255(188,143,143),
    'Australia_NZ' = rgb255(255,193,193),
    'Central Asia' = rgb255(139,0,0),
    'India' = rgb255(208,32,144),
    'Indonesia' = rgb255(139, 28, 98),
    'Japan' = hsv(0.01, 0.75, 0.65),
    'Pakistan' = rgb255(205, 181, 205),
    'South Asia' = rgb255(139, 123, 139),
    'South Korea' = rgb255(205, 92, 92),
    'Southeast Asia' = rgb255(240, 128, 128),
    'Taiwan' = rgb255(150, 150, 150)
    )




#' Get the name of the project for display
#'
#' Returns a place holder string if no project has been loaded yet.
#' @param rFileinfo Reactive fileinfo object returned by the file browser in the UI
#' @export
getProjectName <- function(rFileinfo)
{
    fn <- rFileinfo()$project.filename
    if(is.null(fn)) {
        '->none<-'
    } else {
        rFileinfo()$project.filename
    }
}

#' Get the scenarios in the project for display
#'
#' Returns a place holder string if no project has been loaded yet.
#' @param rFileinfo Reactive fileinfo object returned by file browser in the UI.
#' @param concat Separator string to use when concatenating scenario names.
#' @importFrom magrittr "%>%"
#' @export
getProjectScenarios <- function(rFileinfo, concat=NULL)
{
    pd <- rFileinfo()$project.data
    if(is.null(pd)) {
        '->none<-'
    } else {
        rgcam::listScenarios(rFileinfo()$project.data) %>% paste(collapse=concat)
    }
}

#' Get the queries for a project and scenario(s) for display
#'
#' @param rFileinfo Reactive fileinfo object returned by file browser in the UI.
#' @param scenarios List of scenarios.
#' @param concat Separator string for concatenating query names.
#' @importFrom magrittr "%>%"
#' @export
getScenarioQueries <- function(rFileinfo, scenarios, concat=NULL)
{
    prj <- rFileinfo()$project.data
    if(is.null(prj)) {
        if(is.null(concat))
            ''                          # probably not intended for display
        else
            '->none<-'                  # probably intended for display
    }
    else if(length(scenarios) == 0 || all(scenarios=='')) {
        if(is.null(concat))
            ''                          # probably not intended for display
        else
            tag.noscen                  # probably intended for display
    }
    else {
        tryCatch(
            lapply(scenarios, . %>% rgcam::listQueries(prj, .)) %>%
                Reduce(intersect,.) %>% sort %>%
                    paste(collapse=concat),
            ## errors in the pipeline above are caused by selecting a new data
            ## set that doesn't contain the current scenario.  The problem will
            ## clear up once the scenario selector is repopulated.
            error = function(e) {
                if(is.null(concat)) '' else tag.noscen
            })
    }
}

#' Indicate whether the UI is in an obviously invalid state.
#'
#' Invalid states frequently occur as transients when a new project is being
#' loaded and the UI elements are being updated.
#'
#' @param prj Project data structure
#' @param scenario Scenario name
#' @param query Query name
#' @return Boolean indicating whether the UI state appears to be valid.
#' @export
uiStateValid <- function(prj, scenario, query)
{
    valid.values <- !(is.null(prj) || scenario == '' || query == '' ||
                          query==tag.noscen)
    if(valid.values) {
        prjscens <- listScenarios(prj)
        valid.scen <- all(scenario %in% prjscens)
    }
    else {
        valid.scen <- FALSE
    }

    ## This if block is the return value
    if(valid.scen) {
        scenqueries <- listQueries(prj, scenario)
        all(query %in% scenqueries)
    }
    else {
        FALSE
    }
}

#' Indicate whether a query is a gridded data set
#'
#' @param prj Project data structure
#' @param scenario Name of the scenario
#' @param query Name of the query
#' @export
isGrid <- function(prj, scenario, query)
{
    colnames <- names(getQuery(prj, query, scenario))
    'lat' %in% colnames && 'lon' %in% colnames
}


#' Get the years for which a query is defined
#'
#' @param prj Project data structure
#' @param scenario Name of the scenario
#' @param query Name of the query
#' @export
getQueryYears <- function(prj, scenario, query)
{
    if(!uiStateValid(prj, scenario, query)) {
        c(2005, 2100)
    }
    else {
        range(getQuery(prj, query, scenario)["year"])
    }
}


### Helpers for making plots

#' Plot a default panel
#'
#' Mainly intended for use when no data has been loaded.
#'
#' @param label.text Text to display in the middle of the panel
#' @importFrom ggplot2 ggplot geom_label theme_minimal aes aes_
#' @export
default.plot <- function(label.text='No data selected')
{
    ggplot(mapping=aes(x=0,y=0)) + geom_label(aes_(label=label.text), size=10) +
        theme_minimal()
}

#' Plot GCAM data on a global or regional map
#'
#' @param prjdata Project data file
#' @param query Name of the query to plot
#' @param pltscen Name of the scenario to plot
#' @param diffscen Name of the scenario to difference against pltscen, or NULL if none
#' @param projselect Projection to use for the map
#' @param year Year to plot data for
#' @importFrom ggplot2 scale_fill_gradientn guides
#' @importFrom gcammaptools plot_GCAM add_region_ID plot_GCAM_grid
#' @export
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
        ggplot2::last_plot()
    }
    else {
        scens <- paste(c(pltscen, diffscen), collapse=', ')

        is.diff <- !is.null(diffscen)      # We'll do a couple of things differently for a diff plot

        mapset <- determineMapset(prjdata, pltscen, query)
        if(isGrid(prjdata, pltscen, query)) {
            key <- c('lat', 'lon')
        }
        else {
            key <- if(mapset==gcammaptools::basin235) 'basin' else 'region'
        }
        pltdata <- getPlotData(prjdata, query, pltscen, diffscen, key)

        ## map plot is expecting the column coresponding to the map locations to
        ## be called "region", so if we're working with water basins, we have to
        ## rename it.
        if(mapset==gcammaptools::basin235 && 'basin' %in% names(pltdata))
            pltdata$region <- pltdata$basin

        mapLimits <- getMapLimits(pltdata, is.diff)
        unitstr <- summarize.unit(pltdata$Units)

        # Filter the data to only the selected year
        pltdata <- dplyr::filter_(pltdata, paste("year ==", year))

        map.params <- getMapParams(projselect) # map projection and extent
        pal <- getMapPalette(is.diff)   # color palette
        datacol <- 'value' # name of the column with the data.

        if('region' %in% names(pltdata)) {
            ## This is a table of data by region
            # pltdata <- add_region_ID(pltdata, lookupfile=mapset, drops=mapset)
            # if(mapset==gcammaptools::rgn32) {
            #     # map.dat <- gcammaptools::map.rgn32
            #     map.dat <- "../../../gcammaptools/inst/extdata/rgn32/reg32_spart.shp"
            #     map.dat <- gcammaptools::import_mapdata(map.dat)
            #     map.dat <- sf::st_simplify(map.dat, preserveTopology=TRUE, dTolerance=2.0)
            # }
            # else if(mapset==gcammaptools::basin235)
            #     map.dat <- gcammaptools::map.basin235

            plt <- plot_GCAM(map.dat, col=datacol, proj=map.params$proj,
                             extent=map.params$ext, legend=TRUE, colors=pal,
                             qtitle=unitstr, limits=mapLimits, gcam_df=pltdata, gcam_key='id',
                             mapdata_key='region_id')


        }
        else {

            plt <- plot_GCAM_grid(pltdata, col=datacol, proj=map.params$proj,
                                  extent=map.params$ext, legend=TRUE) +
                scale_fill_gradientn(colors=pal, limits=mapLimits, name=unitstr)
        }
        ## set up elements that are common to both kinds of plots here
        plt + guides(fill=ggplot2::guide_colorbar(title.position='bottom', title.hjust=0.5,
                     barwidth=ggplot2::unit(4,'in')))
    }
}

#' Figure out which map to plot a query on.
#'
#' Right now we assume that if the query table contains a 'basin' column, then
#' we want to plot on the basin map; otherwise we plot on the region map.
#' @param prjdata Project data structure
#' @param pltscen Name of the scenario to plot
#' @param query Name of the GCAM query to plot
#' @return Base map suitable for plotting this data
determineMapset <- function(prjdata, pltscen, query)
{
    tp <- rgcam::getQuery(prjdata, query, pltscen)
    if('basin' %in% names(tp)) {
        ## mapping the 235 basins
        mapset <- gcammaptools::basin235
    }
    else {
        ## mapping the 32 regions
        mapset <- gcammaptools::rgn32
    }
}

### Data wrangling

#' Extract and format data for a plot
#'
#' @param prjdata Project data structure
#' @param query Name of the query to plot
#' @param pltscen Name of the scenario to plot
#' @param diffscenDifference scenario, if any
#' @param key Aggregation variable.  (e.g., 'region' or 'sector')
#' @param filtervar If not NULL, filter on this variable before aggregating
#' @param filterset:  Set of values to include in the filter operation.  Ignored
#'   if filtervar is NULL.
#' @keywords internal
getPlotData <- function(prjdata, query, pltscen, diffscen, key, filtervar=NULL,
                        filterset=NULL)
{
    tp <- getQuery(prjdata, query, pltscen) # 'table plot'

    if('region' %in% names(tp)) {
        ## If the data has a region column, put it in the canoncial order given above.
        tp$region <- factor(tp$region, levels=c(names(region32), '0'), ordered=TRUE) # convert to ordered factor
    }
    if(!is.null(diffscen)) {
        dp <- getQuery(prjdata, query, diffscen) # 'difference plot'
        if('region' %in% names(dp)) {
            dp$region <- factor(dp$region, levels=c(names(region32), '0'), ordered=TRUE)
        }
    }
    else {
        dp <- NULL
    }

    if(!is.null(dp)) {
        ## We're doing a difference plot, so subtract the difference scenario.
        ## Join the data sets first so that we can be sure that we have matched
        ## the rows and columns correctly
        varnames <- names(tp)
        mergenames <- varnames[!varnames %in% c('scenario', 'Units', 'value')]

        joint.data <- merge(tp, dp, by=mergenames, all=TRUE)
        if(anyNA(joint.data))
            joint.data[is.na(joint.data)] <- 0 # zero out missing values

        value <- joint.data$value.x - joint.data$value.y

        mergenames <- sapply(mergenames, as.name) # Don't eval hyphenated col names

        # Construct the new data frame.  We use the scenario name from the left
        # (dp) data frame.
        tp <- dplyr::rename(joint.data, scenario=scenario.x, Units=Units.x) %>%
           dplyr::select_(.dots=c('scenario', mergenames, 'Units')) %>% cbind(value)
    }

    ## If filtering is in effect, do it now
    if(!is.null(filtervar) &&
       !is.null(filterset) &&
       length(filterset) > 0 &&
       filtervar %in% names(tp)
       ) {

        tp <- dplyr::filter_(tp, lazyeval::interp(~y %in% x, y = as.name(filtervar), x = filterset))

        #if(nrow(tp) == 0) {
        #  return(tp)
        #}
    }

    if(!isGrid(prjdata, pltscen, query)) {
        ## select the key and year columns, then sum all values with the same key.  Force the sum
        ## to have the name 'value'.  Skip this step for grid data.
        if(!is.null(key) &&
           toString(key) %in% (tp %>% names %>% setdiff(c('year', 'Units')))
           ) {
          tp <- dplyr::group_by_(tp, key, 'year', 'Units') %>%
                dplyr::summarise(value = sum(value))
        }
        else {
          tp <- dplyr::group_by_(tp, 'year', 'Units') %>%
                dplyr::summarise(value = sum(value))
        }
    }
    else {
        ## for gridded data, just get the lat, lon, year, data, and units
        tp <- dplyr::select_(tp, .dots=c('lat', 'lon', 'value', 'year', 'Units'))
    }

    ## Occasionally you get a region with "0.0" for the unit string because most of its entries were zero.
    ## Fix these so that the column all has the same unit.
    tp$Units <- summarize.unit(tp$Units)
    tp
}

#' Get projection parameters for the pre-defined projections
#'
#' Valid inputs are "global", "lac" (Latin America and Caribbean), "usa",
#' "china", and "africa".
#'
#' @param projselect Name of the predefined projection
#' @keywords internal
getMapParams <- function(projselect)
{
    ## currently valid values are 'global' and 'lac'
    if(projselect == 'global') {
        list(proj=gcammaptools::eck3, ext=gcammaptools::EXTENT_WORLD, orientation=NULL)
    }
    else if(projselect == 'usa') {
        list(proj=gcammaptools::na_aea, ext=gcammaptools::EXTENT_USA, orientation=NULL)
    }
    else if(projselect == 'china') {
        list(proj=gcammaptools::ch_aea, ext=gcammaptools::EXTENT_CHINA, orientation=NULL)
    }
    else if(projselect == 'africa') {
        list(proj=gcammaptools::ortho, ext=gcammaptools::EXTENT_AFRICA, orientation=gcammaptools::ORIENTATION_AFRICA)
    }
    else if(projselect == 'lac') {
        list(proj=gcammaptools::ortho, ext=gcammaptools::EXTENT_LA, orientation=gcammaptools::ORIENTATION_LA)
    }
}

#' Select a suitable color palette
#' @param is.diff Boolean indicating whether the plot is a difference plot
#' @keywords internal
getMapPalette <- function(is.diff)
{
    if(is.diff) {
        RColorBrewer::brewer.pal(9, 'RdBu')
    } else {
        RColorBrewer::brewer.pal(9,'Blues')
    }
}

#' Select suitable scale limits for a plot
#'
#' @param pltdata The data being plotted
#' @param is.diff Boolean indicating whether the plot is a difference plot
#' @keywords internal
getMapLimits <- function(pltdata, is.diff)
{
    limits <- range(pltdata['value'], na.rm=TRUE)
    if(is.diff) {
        ## For a difference plot, force the limits to be balanced on either side of zero
        mag <- max(abs(limits))
        c(-mag, mag)
    }
    else {
        limits
    }
}

#' Summarize the unit column of a GCAM data frame by taking the most common
#' entry.
#'
#' In theory the unit should have a single, common value, but in practice GCAM
#' isn't always great about getting its unit strings consistent.
#' @param unitcol Character vector of unit names.
#' @keywords internal
summarize.unit <- function(unitcol)
{
    unitcol[which.max(table(unitcol))]
}

#' Plot values over time as a bar chart
#' @param prjdata A project data structure
#' @param query  Name of the query to plot
#' @param scen  Name of the scenario to plot
#' @param diffscen  Name of the difference scenario, or NULL if none
#' @param subcatvar  Variable to use for subcategories in the plot
#' @param filter  If TRUE, then filter to regions in the rgns argument
#' @param rgns  Regions to filter to, if filter is TRUE.
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot aes_string geom_bar theme_minimal ylab scale_fill_manual
#' @export
plotTime <- function(prjdata, query, scen, diffscen, subcatvar, filter, rgns)
{
    if(is.null(prjdata)) {
        default.plot()
    }
    else if(isGrid(prjdata, scen, query)) {
        default.plot("Can't plot time series of spatial grid data.")
    }
    else {
        if(filter)
            filtervar <- 'region'
        else
            filtervar <- NULL

        if(subcatvar=='none')
            subcatvar <- NULL
        else
            subcatvar <- as.name(subcatvar)

        pltdata <- getPlotData(prjdata, query, scen, diffscen, subcatvar,
                               filtervar, rgns)

        plt <- ggplot(pltdata, aes_string('year','value', fill=subcatvar)) +
          geom_bar(stat='identity') + theme_minimal() + ylab(pltdata$Units)

        if(is.null(subcatvar)) {
            plt
        }
        else {
            subcatvar <- toString(subcatvar)
            if(subcatvar=='region')
                fillpal <- region32
            else {
                n <- length(unique(pltdata[[subcatvar]]))
                if(n<3) {
                    fillpal <- RColorBrewer::brewer.pal(3,'Set3')
                }
                else if(n<=12) {
                    fillpal <- RColorBrewer::brewer.pal(n,'Set3')
                }
                else {
                    fillpal <- grDevices::rainbow(n, 0.8, 0.9)
                }
            }

            plt + scale_fill_manual(values=fillpal)
        }
    }
}
