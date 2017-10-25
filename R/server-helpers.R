### Helper functions for the server side of the app.

tag.noscen <- '->No scenarios selected<-'     # placeholder when no scenario selected

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
    prj <- projData
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
      querydata <- getQuery(prj, query, scenario)
      querycols <- names(querydata)
      ignorecols <- c('scenario', 'Units', 'year', 'value')

      # Remove subcategories that only have one unique element
      numcats <- sapply(querydata[, !querycols %in% ignorecols], function(x) {
        length(unique(x))
      })
      ignorecols <- c(ignorecols, names(which(numcats == 1)))

      catvars <- querycols[!querycols %in% ignorecols]
    }
}

#' Update the checkbox filters when select/deselect all button is pressed
#'
#' @param session The main session.
#' @param btnId The id of the actionButton that was pressed.
#' @param groupId The id of the checkboxGroupInput to act on.
#' @param selectAll If TRUE, select all checkboxes in the group defined by
#'   groupId. If not TRUE then deselect.
#' @param choices The labels of the checkboxes.
#' @export
updateRegionFilter <- function(session, btnId, groupId, selectAll, choices) {
  if(selectAll) {
    updateCheckboxGroupInput(session, groupId, choices = choices)
    newText <- "Select all"
  }
  else {
    updateCheckboxGroupInput(session, groupId, choices = choices, selected = choices)
    newText <- "Deselect all"
  }

  updateCheckboxInput(session, btnId, label = newText)
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
#' @param map Base map to plot on (for gridded data only)
#' @importFrom ggplot2 scale_fill_gradientn guides
#' @importFrom gcammaptools add_region_ID plot_GCAM plot_GCAM_grid
#' @export
plotMap <- function(prjdata, query, pltscen, diffscen, projselect, year, map = NULL, zoom = 0)
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

        pltdata <- getPlotData(prjdata, query, pltscen, diffscen, key,
                               yearRange = c(year, year))
        if (is.null(pltdata)) return(default.plot())

        ## map plot is expecting the column coresponding to the map locations to
        ## be called "region", so if we're working with water basins, we have to
        ## rename it.
        if(mapset==gcammaptools::basin235 && 'basin' %in% names(pltdata))
            pltdata$region <- pltdata$basin

        mapLimits <- getMapLimits(pltdata, is.diff)
        unitstr <- summarize.unit(pltdata$Units)

        map.params <- getMapParams(projselect, zoom) # map projection, extent, and zoom
        pal <- getMapPalette(is.diff)   # color palette
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
              scale_fill_gradientn(colors = pal, na.value = gray(0.75),
                                   name = query)

        }
        else if(isGrid(prjdata, pltscen, query)) {
            if (!is.null(map)) map.dat <- map
            plt <- plot_GCAM_grid(pltdata, datacol, map = map.dat,
                                  proj_type = map.params$proj_type,
                                  proj = map.params$proj, extent = map.params$ext,
                                  zoom = map.params$zoom, legend = TRUE) +
                # scale_fill_gradientn(colors = pal, name = unitstr)
                ggplot2::scale_fill_distiller(palette = "Spectral")
        } else {
            plt <- default.plot(label.text = "No geographic data available for this query")
        }
        ## set up elements that are common to both kinds of plots here
        plt + guides(fill=ggplot2::guide_colorbar(title=unitstr,
                                                  barwidth=ggplot2::unit(3.1,'in'),
                                                  title.position="bottom")) +
          ggplot2::theme(legend.position="bottom", legend.title.align = 0.5)
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

#' Filter out data that cannot be plotted
#'
#' @param plotData Data frame containing the data for a plot.
#' @return Cleaned data frame.
cleanPlotData <- function(plotData)
{
  # To plot cleanly, we can only take data with the same units
  units <- unique(plotData$Units)
  if (length(units) > 1) {
    mostCommonUnit <- sort(table(plotData$Units), decreasing = T)[1] %>% names()
    plotData <- dplyr::filter(plotData, Units == mostCommonUnit)
  }

  # If the data has a region column, put it in the canoncial order for GCAM.
  if('region' %in% names(plotData)) {
    plotData$region <- factor(plotData$region,
                              levels=c(names(gcammaptools::gcam32_colors), '0'),
                              ordered=TRUE) # convert to ordered factor
  }

  plotData
}

#' Filter out data that should not be plotted
#'
#' @param plotData Data frame containing the data for a plot.
#' @param filtervar Filter on this variable before aggregating.
#' @param filterset Set of values to include in the filter operation.
#' @param startYear Minimum year to display. If NULL, searches year column for
#'   minimum in data.
#' @param endYear Maximum year to display. If NULL, searches year column for
#'   maximum in data.
#' @return Filtered data frame.
filterPlotData <- function(plotData, filtervar, filterset, startYear, endYear)
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
  if(!is.null(filtervar) &&
     !is.null(filterset) &&
     length(filterset) > 0 &&
     filtervar %in% names(plotData)
  ) {
    plotData <- dplyr::filter_(plotData, lazyeval::interp(~y %in% x,
                                                          y = as.name(filtervar),
                                                          x = filterset))
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
#' @param filtervar If not NULL, filter on this variable before aggregating
#' @param filterset:  Set of values to include in the filter operation.  Ignored
#'   if filtervar is NULL.
#' @param yearRange A vector of two integers of form c(start year, end year)
#'   to filter the data to.
#' @keywords internal
#' @export
getPlotData <- function(prjdata, query, pltscen, diffscen, key, filtervar=NULL,
                        filterset=NULL, yearRange = c(2005, 2050))
{
    # table plot
    tp <- getQuery(prjdata, query, pltscen) %>%
          cleanPlotData() %>%
          filterPlotData(filtervar, filterset, yearRange[1], yearRange[2])

    if (nrow(tp) == 0) return(NULL)

    if(!is.null(diffscen)) {
        # 'difference plot'
        dp <- getQuery(prjdata, query, diffscen) %>%
              cleanPlotData() %>%
              filterPlotData(filtervar, filterset, yearRange[1], yearRange[2])
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
#'
#' If it is possible to build a plot with the data, this function will return
#' a list containing the data frame being plotted as the second element.
#'
#' @param prjdata A project data structure
#' @param query  Name of the query to plot
#' @param scen  Name of the scenario to plot
#' @param diffscen  Name of the difference scenario, or NULL if none
#' @param subcatvar  Variable to use for subcategories in the plot
#' @param rgns  Regions to filter to
#' @importFrom magrittr "%>%"
#' @importFrom ggplot2 ggplot aes_string geom_bar theme_minimal ylab scale_fill_manual
#' @export
plotTime <- function(prjdata, query, scen, diffscen, subcatvar, rgns)
{
    if(is.null(prjdata)) {
      list(plot = default.plot())
    }
    else if(isGrid(prjdata, scen, query)) {
      list(plot = default.plot("Can't plot time series of\nspatial grid data."))
    }
    else if(length(rgns) == 0) {
      list(plot = default.plot("No regions selected."))
    }
    else {
        filtervar <- 'region'

        if(subcatvar=='none')
            subcatvar <- NULL
        else
            subcatvar <- as.name(subcatvar)

        pltdata <- getPlotData(prjdata, query, scen, diffscen, subcatvar,
                               filtervar, rgns)

        plt <- ggplot(pltdata, aes_string('year','value', fill=subcatvar)) +
          geom_bar(stat='identity') + ggplot2::theme(axis.text=ggplot2::element_text(size=12),
                                                     axis.title=ggplot2::element_text(size=13,face="bold")) +
          ylab(pltdata$Units)

        # Get a color scheme for the subcategories
        if(!is.null(subcatvar)) {
            subcatvar <- toString(subcatvar)

            if(subcatvar == 'region')
                fillpal <- gcammaptools::gcam32_colors
            else {
                n <- length(unique(pltdata[[subcatvar]]))
                if(n<3) {
                    fillpal <- RColorBrewer::brewer.pal(3,'Set3')
                }
                else if(n<=12) {
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
            plt <- plt + scale_fill_manual(values=fillpal)
        }
        list(plot = plt, plotdata = pltdata)
    }
}

#' Plot values over time as a bar chart
#' @param scens List of scenario names to plot
#' @inheritParams plotTime
#' @importFrom ggplot2 ggplot aes_string geom_bar theme ylab facet_grid element_text
#' @export
plotScenComparison <- function(prjdata, query, scens, diffscen, subcatvar, rgns)
{
  filtervar <- "region"
  plt <- ggplot(data = NULL, aes_string('year','value', fill=subcatvar)) +
         facet_grid(.~panel, scales="free")

  for (scen in scens) {
    pltdata <- getPlotData(prjdata, query, scen, diffscen, subcatvar, filtervar, rgns)
    pltdata$panel <- scen
    units <- pltdata$Units
    plt <- plt + geom_bar(data = pltdata, stat = "identity")
  }

  plt + ylab(units) + theme(axis.text=element_text(size=12),
                            axis.title=element_text(size=13, face="bold"),
                            legend.position="bottom")
}
