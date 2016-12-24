# GCAM Dashboard: A scenario explorer for GCAM

The GCAM Dashboard is a scenario explorer for GCAM.  Its purpose is to
provide a way to give users a quick view of the data in a collection
of scenarios.  You can get a listing of the scenarios in a data set
and the queries available for each scenario, or available jointly for
a collection of scenarios.  You can plot the queries for a single
scenario in a map view or over time, or you can plot the difference in
output values between two scenarios in either of the same two views.  

## Installation

The easiest way to run the GCAM Dashboard is using the
[R Studio](https://www.rstudio.com/) IDE.  Download and install R
Studio.  Start a new session in R Studio, and install the required
packages if you don't have them already.  You can do that by running
the following sequence of commands:
```R
install.packages(c('ggplot2','dplyr','shiny','tidyr','lazyeval',
	               'RColorBrewer','devtools'))
devtools::install_github('hrbrmstr/ggalt')
devtools::install_github('JGCRI/rgcam')
devtools::install_github('JGCRI/gcammaptools')
```
Then, download this repository.  Open the file `ui.R` in the
`GCAM-dashboard` directory.  You should see a button marked "Run App"
at the top-right of the window showing the file you just opened.
Press that button, and the GCAM-dashboard app should open up and start
running.

## Usage

### Uploading data

The GCAM Dashboard gets its data from the project data files created
by the [rgcam](https://github.com/JGCRI/rgcam) package.  Add one or
more GCAM scenarios to a project data file using that package.  In the
panel at the left of the app's display you will see a widget for
uploading your project file.  Select your project file using the file
explorer, and it will be uploaded into the app.  The Project Info tab
will show the name of the data file and the scenarios contained in
it.  You can also select one or more scenarios to see a list of all of
the queries common to those scenarios.

### Plotting maps

Switch to the Map View tab.  The control panel at the left of the
display has widgets for selecting a scenario to plot and a query to
plot.  Select the scenario and query you wish to plot, and the map
will be updated.  You can also choose to show a difference plot using
the check box.  If you do, then a selector for the difference scenario
will appear.  

At the bottom of the map there are controls for the display.  The
slider bar selects the year to plot.  There is also a selector box for
choosing what part of the map to display (global, or several regions
of interest).

### Plotting data over time

(Not yet implemented.)
