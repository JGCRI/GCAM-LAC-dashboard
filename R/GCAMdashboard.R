#' An Interactive Dashboard for Exploring GCAM Scenario Data
#'
#' The GCAM Dashboard is a scenario explorer for GCAM.  Its purpose is to
#' provide a way to give users a quick view of the data in a collection
#' of scenarios.  You can get a listing of the scenarios in a data set
#' and the queries available for each scenario, or available jointly for
#' a collection of scenarios.  You can plot the queries for a single
#' scenario in a map view or over time, or you can plot the difference in
#' output values between two scenarios in either of the same two views.
#'
#' @section Usage:
#' To run the GCAM Dashboard, from the R command console enter
#' \code{GCAMdashboard::run()}.
#'
#' \subsection{Uploading Data}{
#' The GCAM Dashboard gets its data from the project data files created
#' by the \code{\link[rgcam]{rgcam}} package.  Add one or
#' more GCAM scenarios to a project data file using that package.  In the
#' panel at the left of the app's display you will see a widget for
#' uploading your project file.  Select your project file using the file
#' explorer, and it will be uploaded into the app.  The Project Info tab
#' will show the name of the data file and the scenarios contained in
#' it.  You can also select one or more scenarios to see a list of all of
#' the queries common to those scenarios.
#' }
#' \subsection{Plotting Maps}{
#' Switch to the Map View tab.  The control panel at the left of the
#' display has widgets for selecting a scenario to plot and a query to
#' plot.  Select the scenario and query you wish to plot, and the map
#' will be updated.  You can also choose to show a difference plot using
#' the check box.  If you do, then a selector for the difference scenario
#' will appear.
#'
#' At the bottom of the map there are controls for the display.  The
#' slider bar selects the year to plot.  There is also a selector box for
#' choosing what part of the map to display (global, or several regions
#' of interest).
#' }
#' \subsection{Plotting Data Over Time}{
#' The Time View tab provides a view of the query variable over time.  It
#' uses the same controls on the left panel to select the scenario and
#' query to display.  As before, you can display either a single scenario
#' or a difference between two scenarios.
#'
#' Below the plot you will find controls for additional plot options.
#' The selector box labeled "Break totals into subcategories by:" allows
#' you to change the bar plot into a stacked bar plot, with the elements
#' making up the stack coming from the variable you select.  There is
#' also a check box for filtering the data to certain regions.  Selecting
#' it will cause a list of regions to appear, and you can select the ones
#' you want to include in the plot.  Note, however, that if \emph{no} regions
#' are selected, the plot will instead display \emph{all} regions, the same as
#' if the filter regions check box were not selected.
#' }

"_PACKAGE"

#' Run the GCAM dashboard
#' @export
run <- function() {
  shiny::runApp(system.file('app', package='GCAMdashboard'))
}
