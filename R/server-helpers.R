### Helper functions for the server side of the app.

tag.noscen <- '->No scenarios selected<-'     # placeholder when no scenario selected
mapCache <- new.env() # Use this environment for efficiently caching maps


# Project helper functions ------------------------------------------------

#' Convert all queries in the project from wideform to long form
#'
#' Assumes that a wideform table has no 'year' column and several Xyear columns.
#' @param projData Project data to convert
#' @export
convertProjectToLongform <- function(projData) {
  for(q in listQueries(projData)) {
    qtable <- getQuery(projData, q)

    # Assume that if a query has a year column, the data is already longform
    if('year' %in% names(qtable)) {
      break
    } else {
      ycols <- grep('^X?[12]\\d{3}$', names(qtable))
      names(qtable)[ycols] <- sub('X', '', names(qtable)[ycols]) # remove the X
      qtable <- tidyr::gather(qtable, 'year', 'value', ycols, na.rm = T, convert = T)
    }
    projData <- addQueryTable(projData, qtable, q, clobber = T, saveProj = F)
  }
  projData
}

#' Get the scenarios in the project for display
#'
#' Returns a place holder string if no project has been loaded yet.
#' @param projData Project data to query.
#' @param concat Separator string to use when concatenating scenario names.
#' @importFrom magrittr "%>%"
#' @export
getProjectScenarios <- function(projData, concat=NULL)
{
    pd <- projData
    if(is.null(pd)) {
        '->none<-'
    } else {
        rgcam::listScenarios(pd) %>% paste(collapse=concat)
    }
}

#' Get the queries for a project and scenario(s) for display
#'
#' @param projData Project data to query.
#' @param scenarios List of scenarios.
#' @param concat Separator string for concatenating query names.
#' @importFrom magrittr "%>%"
#' @export
getScenarioQueries <- function(projData, scenarios, concat=NULL)
{
    if(is.null(scenarios) || !all(scenarios %in% listScenarios(projData))) {
        tag.noscen
    }
    else {
        lapply(scenarios, . %>% rgcam::listQueries(projData, .)) %>%
            Reduce(intersect,.) %>% sort %>%
            paste(collapse=concat)
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
    valid.values <- !(is.null(prj) || is.null(scenario) || scenario == '' ||
                                      query==tag.noscen ||  query == '')
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


# Query helper functions --------------------------------------------------

#' Convert a data frame to wideform
#'
#' Takes a data frame with a 'year' column and creates a new column for each
#' distinct year.
#' @param data Data frame to convert, likely the output of a query.
#' @export
convertToWideform <- function(data) {

  if(!'year' %in% names(data)) return(data)
  tidyr::spread(data, year, value)
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
        range(as.integer(getQuery(prj, query, scenario)$year))
    }
}

#' Get the subcategories for a query
#'
#' A subcategory is defined as column that doesn't define the scenario, year,
#' units, or value. If the subcategory contains only one unique element, it is
#' not included.
#'
#' @param prj Project data structure.
#' @param scenario Name of the scenario.
#' @param query Name of the query.
#' @export
getQuerySubcategories <- function(prj, scenario, query)
{
    if(!uiStateValid(prj, scenario, query)) {
      NULL
    }
    else {
      numcats <- countUniqueSubcatValues(prj, scenario, query)
      names(which(numcats != 1))
    }
}

#' Get the subcategory to show for a new query
#'
#' A subcategory is defined as column that doesn't define the scenario, year,
#' units, or value. When a new query is selected, the new subcategory should be
#' the same as one in the previous query if it exists. If the new query does not
#' have that subcategory, a new one is chosen based on number of unique values.
#' If the subcategory contains only one unique element, it is not included.
#'
#' @param prj Project data structure.
#' @param scenario Name of the scenario.
#' @param query Name of the query.
#' @param oldSubcategory The subcategory for the previous query.
#' @export
getNewSubcategory <- function(prj, scenario, query, oldSubcategory = NULL)
{
  if(!uiStateValid(prj, scenario, query)) {
    NULL
  }
  else {
    numcats <- countUniqueSubcatValues(prj, scenario, query)
    numcats <- numcats[which(numcats != 1)] # Filter out useless categories

    # A good choice will have more than 2 elements, but not too many
    choices <- numcats[which(numcats > 2)]
    if(!is.null(oldSubcategory) & (oldSubcategory %in% names(numcats) ||
                                   oldSubcategory == 'none')) {
      oldSubcategory
    }
    else if(length(choices) == 0) {
      'none'
    }
    else {
      names(which(choices == min(choices)))[1]
    }
  }
}

#' Counts the number of unique values for a query subcategory
#'
#' @param prj Project data structure.
#' @param scenario Name of the scenario.
#' @param query Name of the query.
countUniqueSubcatValues <- function(prj, scenario, query)
{
  querydata <- getQuery(prj, query, scenario)
  querycols <- names(querydata)
  ignorecols <- c('scenario', 'Units', 'year', 'value')

  sapply(querydata[, !querycols %in% ignorecols], function(x) {
    length(unique(x))
  })
}


# Helpers for building plots ----------------------------------------------

#' Get projection parameters for the pre-defined projections
#'
#' Valid inputs are "global", "lac" (Latin America and Caribbean), "usa",
#' "china", and "africa".
#'
#' @param projselect Name of the predefined projection
#' @keywords internal
getMapParams <- function(projselect, zoom)
{
  if(projselect == 'global') {
    list(proj=gcammaptools::eck3, ext=gcammaptools::EXTENT_WORLD, zoom=zoom)
  }
  else if(projselect == 'usa') {
    list(proj=gcammaptools::na_aea, ext=gcammaptools::EXTENT_USA, zoom=zoom)
  }
  else if(projselect == 'china') {
    list(proj=gcammaptools::ch_aea, ext=gcammaptools::EXTENT_CHINA, zoom=zoom)
  }
  else if(projselect == 'africa') {
    list(proj=gcammaptools::af_ortho, ext=gcammaptools::EXTENT_AFRICA, zoom=10 + zoom)
  }
  else if(projselect == 'lac') {
    list(proj=7567, proj_type='SR-ORG', ext=gcammaptools::EXTENT_LA, zoom=8 + zoom)
  }
}

#' Select a suitable color palette for map plots
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

#' Select a suitable color palette for time series bar plots
#' @param pltdata The data getting plotted
#' @param subcatvar The subcategory selected
#' @keywords internal
barPlotScale <- function(pltdata, subcatvar)
{
  if(is.null(subcatvar)) {
    fillpal <- "#808080"
  }
  else {
    subcatvar <- toString(subcatvar)

    if(subcatvar == 'region')
      fillpal <- gcammaptools::gcam32_colors
    else {
      n <- length(unique(pltdata[[subcatvar]]))
      if(n < 3) {
        fillpal <- RColorBrewer::brewer.pal(3,'Set3')
      }
      else if(n <= 12) {
        fillpal <- RColorBrewer::brewer.pal(n,'Set3')
      }
      else {
        # https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/
        fillpal <- rep(c("#e6194b", "#3cb44b", "#ffe119", "#0082c8",
                         "#f58231", "#911eb4", "#46f0f0", "#f032e6",
                         "#d2f53c", "#fabebe", "#008080", "#e6beff",
                         "#aa6e28", "#fffac8", "#800000", "#aaffc3",
                         "#808000", "#ffd8b1", "#000080", "#808080",
                         "#FFFFFF", "#000000"), n / 20 + 1)
      }
    }
  }
  ggplot2::scale_fill_manual(values = fillpal)
}

#' The theme for bar plots on the dashboard
#' @param legendPos The position of the legend
#' @importFrom ggplot2 theme element_text element_blank margin
#' @keywords internal
barPlotTheme <- function(pltdata, subcatvar, legendPos = "right")
{
  uq <- length(unique(pltdata[[subcatvar]]))
  vshift <- if (uq > 10) 10 * (uq - 10) else 0

  theme(axis.text=element_text(size=12),
        axis.text.x=element_text(angle=90, vjust=0.5),
        axis.title=element_text(size=13,face="bold"),
        legend.title=element_blank(),
        legend.position=legendPos,
        legend.margin=margin(vshift, 0, 0, 0))
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


# Plot data processing ----------------------------------------------------

#' Filter out data that cannot be plotted
#'
#' @param plotData Data frame containing the data for a plot.
#' @return Cleaned data frame.
cleanPlotData <- function(plotData)
{
  # If the data has a region column, put it in the canoncial order for GCAM.
  if('region' %in% names(plotData)) {
    plotData$region <- factor(plotData$region,
                              levels=c(names(gcammaptools::gcam32_colors), '0'),
                              ordered=TRUE) %>% # convert to ordered factor
      as.character()
  }

  # Convert the data to long form if it isn't already
  if(!'year' %in% names(plotData)) {
    ycols <- grep('^X?[12]\\d{3}$', names(plotData))
    names(plotData)[ycols] <- substring(names(plotData)[ycols], 2) # remove the X
    plotData <- tidyr::gather(plotData, 'year', 'value', ycols, convert = T)
  }

  # Ensure spatial data column names are correct
  if ('longitude' %in% names(plotData))
    plotData <- dplyr::rename(plotData, lon=longitude)
  if ('latitude' %in% names(plotData))
    plotData <- dplyr::rename(plotData, lat=latitude)

  # Make sure the data's year column is numeric
  if(class(plotData$year) != "integer")
    plotData$year <- as.integer(plotData$year)

  plotData
}

#' Filter out data that should not be plotted
#'
#' @param plotData Data frame containing the data for a plot.
#' @param filters: Named list of variables and values to filter on before
#'   aggregating. Values can be character or a list.
#' @param startYear Minimum year to display. If NULL, searches year column for
#'   minimum in data.
#' @param endYear Maximum year to display. If NULL, searches year column for
#'   maximum in data.
#' @return Filtered data frame.
filterPlotData <- function(plotData, filters, startYear = NULL, endYear = NULL)
{
  # Only select relevant years
  if (is.null(startYear)) {
    startYear = min(plotData$year)
  }
  if (is.null(endYear)) {
    endYear = max(plotData$year)
  }
  plotData <- dplyr::filter(plotData, year >= startYear & year <= endYear)

  # Filter out all items in filterset that are found in column filtervar
  filterPlotVar <- function(filtervar, filters) {
    filterset <- filters[[filtervar]]
    plotData <<- dplyr::filter(plotData, UQ(as.name(filtervar)) %in% filterset)
  }
  sapply(names(filters), filterPlotVar, filters)

  # To plot cleanly, we have to filter out data with different units
  units <- unique(plotData$Units)
  if (length(units) > 1) {
    mostCommonUnit <- sort(table(plotData$Units), decreasing = T)[1] %>% names()
    plotData <- dplyr::filter(plotData, Units == mostCommonUnit)
  }

  plotData
}

#' Extract and format data for a plot
#'
#' @param prjdata Project data structure
#' @param query Name of the query to plot
#' @param pltscen Name of the scenario to plot
#' @param diffscen Difference scenario, if any
#' @param key Aggregation variable.  (e.g., 'region' or 'sector')
#' @param filters: Named list of variables and values to filter on before
#'   aggregating. Values can be character or a list.
#' @param yearRange A vector of two integers of form \code{c(start year, end
#'   year)} to filter the data to.
#' @keywords internal
#' @export
getPlotData <- function(prjdata, query, pltscen, diffscen, key, filters,
                        yearRange = c(2005, 2050))
{
    # table plot
    tp <- getQuery(prjdata, query, pltscen) %>%
          cleanPlotData() %>%
          filterPlotData(filters, yearRange[1], yearRange[2])

    if (nrow(tp) == 0) return(NULL)

    if(!is.null(diffscen)) {
        # 'difference plot'
        dp <- getQuery(prjdata, query, diffscen) %>%
              cleanPlotData() %>%
              filterPlotData(filters, yearRange[1], yearRange[2])
    }
    else {
        dp <- NULL
    }

    if(!is.null(dp)) {
        ## We're doing a difference plot, so subtract the difference scenario.
        ## Join the data sets first so that we can be sure that we have matched
        ## the rows and columns correctly
        varnames <- names(tp)
        mergenames <- varnames[!varnames %in% c('scenario', 'value')]

        joint.data <- merge(tp, dp, by=mergenames, all=TRUE)

        # zero out missing values
        joint.data$value.x[is.na(joint.data$value.x)] <- 0
        joint.data$value.y[is.na(joint.data$value.y)] <- 0

        value <- joint.data$value.x - joint.data$value.y

        mergenames <- sapply(mergenames, as.name) # Don't eval hyphenated col names

        # Construct the new data frame.  We use the scenario name from the left
        # (dp) data frame.
        tp <- dplyr::rename(joint.data, scenario=scenario.x) %>%
           dplyr::select_(.dots=c('scenario', mergenames, 'Units')) %>% cbind(value)
    }

    if(!isGrid(prjdata, pltscen, query)) {
        ## Select the key and year columns, then sum all values with the same
        ## key.  Force the sum to have the name 'value'. Skip this step for
        ## grid data.
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

    ## Occasionally you get a region with "0.0" for the unit string because
    ## most of its entries were zero. Fix these so that the column all has the
    ## same unit.
    tp$Units <- summarize.unit(tp$Units)
    tp
}


# Plot style and construction ---------------------------------------------

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
#' @param filters Named list of variables and values to filter to
#' @param map Base map to plot on (for gridded data only)
#' @importFrom ggplot2 scale_fill_gradientn guides
#' @importFrom gcammaptools add_region_ID plot_GCAM plot_GCAM_grid
#' @export
plotMap <- function(prjdata, query, scen, diffscen, projselect, subcat, year,
                    filters = NULL, map = NULL, zoom = 0)
{

  if(is.null(prjdata)) {
    default.plot()
  }
  else if(!uiStateValid(prjdata, scen, query)) {
    ggplot2::last_plot()
  }
  else {
    # Check the cache to see if we have created this plot before
    cacheKey <- paste0(attr(prjdata, "file"), query, scen, diffscen, projselect,
                       paste(subcat, collapse = ""), year, nrow(map), zoom)
    if(!is.null(mapCache[[cacheKey]])) return(mapCache[[cacheKey]])

    mapset <- determineMapset(prjdata, scen, query)
    filters <- list()

    if(isGrid(prjdata, scen, query)) {
      key <- c('lat', 'lon')
    }
    else {
      key <- if(mapset==gcammaptools::basin235) 'basin' else 'region'
      if(projselect == "lac") filters$region <- lac.rgns

      if (!is.null(subcat)) {
        sc <- getNewSubcategory(prjdata, scen, query)
        if (sc != 'none') filters[[sc]] <- subcat
      }
    }

    # Get the data and make sure it is valid
    pltdata <- getPlotData(prjdata, query, scen, diffscen, key, filters,
                           yearRange = c(year, year))
    if(is.null(pltdata)) return(default.plot())

    ## map plot is expecting the column coresponding to the map locations to
    ## be called "region", so if we're working with water basins, we have to
    ## rename it.
    if(mapset==gcammaptools::basin235 && 'basin' %in% names(pltdata))
      pltdata$region <- pltdata$basin

    is.diff <- !is.null(diffscen)
    map.pal <- getMapPalette(is.diff)   # color palette
    map.limits <- getMapLimits(pltdata, is.diff)
    map.params <- getMapParams(projselect, zoom) # map projection, extent, and zoom
    unitstr <- summarize.unit(pltdata$Units)
    datacol <- 'value' # name of the column with the data.

    # Determine whether to use basin or region map, and which level of detail
    simplify_map <- isTRUE(all.equal(map.params$ext, gcammaptools::EXTENT_WORLD))
    if(mapset==gcammaptools::rgn32 && simplify_map)
      map.dat <- gcammaptools::map.rgn32.simple    # rgn32 and world extent
    else if(mapset==gcammaptools::rgn32)
      map.dat <- gcammaptools::map.rgn32           # rgn32 and smaller extent
    else if(simplify_map)
      map.dat <- gcammaptools::map.basin235.simple # basin235 and world extent
    else
      map.dat <- gcammaptools::map.basin235        # basin235 and smaller extent


    if('region' %in% names(pltdata)) {
      # This is a table of data by region
      pltdata <- add_region_ID(pltdata, lookupfile = mapset, drops = mapset)

      plt <- plot_GCAM(map.dat, col = datacol, proj = map.params$proj,
                       proj_type = map.params$proj_type, extent = map.params$ext,
                       legend = TRUE, gcam_df = pltdata, gcam_key = 'id',
                       mapdata_key = 'region_id', zoom = map.params$zoom) +
        scale_fill_gradientn(colors = map.pal, na.value = gray(0.75),
                             name = query, limits = map.limits)

    }
    else if(isGrid(prjdata, scen, query)) {
      if (!is.null(map)) map.dat <- map
      plt <- plot_GCAM_grid(pltdata, datacol, map = map.dat,
                            proj_type = map.params$proj_type,
                            proj = map.params$proj, extent = map.params$ext,
                            zoom = map.params$zoom, legend = TRUE) +
        # scale_fill_gradientn(colors = map.pal, name = unitstr)
        ggplot2::scale_fill_distiller(palette = "Spectral")
    } else {
      plt <- default.plot(label.text = "No geographic data available for this query")
    }
    ## set up elements that are common to both kinds of plots here
    plt <- plt + guides(fill=ggplot2::guide_colorbar(title=unitstr,
                                                     barwidth=ggplot2::unit(3.1,'in'),
                                                     title.position="bottom")) +
      ggplot2::theme(legend.position="bottom", legend.title.align = 0.5)
    mapCache[[cacheKey]] <- plt
    plt
  }
}

#' Plot values over time as a bar chart
#'
#' If it is possible to build a plot with the data, this function will return
#' a list containing the data frame being plotted as the second element.
#'
#' @param prjdata A project data structure
#' @param query  Name of the query to plot
#' @param scen  Name of the scenario to plot
#' @param diffscen  Name of the difference scenario, or NULL if none
#' @param subcatvar  Variable to use for subcategories in the plot
#' @param filters Named list of variables and values to filter to
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot aes_string geom_bar ylab
#' @export
plotTime <- function(prjdata, query, scen, diffscen, subcatvar, filters)
{
    if(is.null(prjdata)) {
      list(plot = default.plot())
    }
    else if(isGrid(prjdata, scen, query)) {
      list(plot = default.plot("Can't plot time series of\nspatial grid data."))
    }
    else {
        if(subcatvar=='none')
            subcatvar <- NULL
        else
            subcatvar <- as.name(subcatvar)

        pltdata <- getPlotData(prjdata, query, scen, diffscen, subcatvar, filters)

        if(is.null(pltdata)) return(list(plot = default.plot()))

        plt <- ggplot(pltdata, aes_string('year','value', fill=subcatvar)) +
               geom_bar(stat='identity') + barPlotTheme(pltdata, subcatvar) +
               ylab(pltdata$Units)

        # Get a color scheme for the subcategories
        plt <- plt + barPlotScale(pltdata, subcatvar)

        list(plot = plt, plotdata = pltdata)
    }
}

#' Plot values over time as a bar chart
#' @param scens List of scenario names to plot
#' @inheritParams plotTime
#' @importFrom ggplot2 ggplot aes_string geom_bar theme ylab facet_grid
#' @export
plotScenComparison <- function(prjdata, query, scens, diffscen, subcatvar, rgns)
{
  filters <- list(region = rgns)

  if(subcatvar=='none')
    subcatvar <- NULL
  else
    subcatvar <- as.name(subcatvar)

  plt <- ggplot(data = NULL, aes_string('year','value', fill=subcatvar)) +
         facet_grid(.~panel, scales="free")

  d <- NULL
  for (scen in scens) {
    pltdata <- getPlotData(prjdata, query, scen, diffscen, subcatvar, filters)
    d <- rbind(d, pltdata)
    pltdata$panel <- scen
    units <- pltdata$Units
    plt <- plt + geom_bar(data = pltdata, stat = "identity")
  }

  plt + ylab(units) + barPlotTheme(pltdata, subcatvar, "bottom") +
        barPlotScale(d, subcatvar)
}
