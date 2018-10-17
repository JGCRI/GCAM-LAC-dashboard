# GCAM-LAC Dashboard: An Interactive Dashboard for Exploring GCAM Scenario Data

The GCAM-LAC Dashboard is a scenario explorer for GCAM, with a focus on Latin
America and the Caribbean.  Its purpose is to provide a way to give users a
quick view of the data in a collection of scenarios.  You can get a listing of
the scenarios in a data set and the queries available for each scenario, or
available jointly for a collection of scenarios.  You can plot the queries for a
single scenario in a map view or over time, or you can plot the difference in 
output values between two scenarios in either of the same two views.

## Installation

The easiest way to run the GCAM-LAC Dashboard is using the [R
Studio](https://www.rstudio.com/) IDE.  Download and install R Studio.  Start a
new session in R Studio, and install the `devtools` package, if you don't have
it already.  You can do that by entering:
```R
install.packages('devtools')
```
Next, install the gcamlacdash package:
```R
devtools::install_github('JGCRI/GCAM-LAC-dashboard')
```
This should install all of the required packages that the GCAM
Dashboard needs to operate for you automatically.

## Usage

To run the GCAM Dashboard, from the R command console enter
`gcamlacdash::run()`.

### Uploading data

The GCAM-LAC Dashboard gets its data from the project data files created by the 
[rgcam](https://github.com/JGCRI/rgcam) package.  Add one or more GCAM scenarios
to a project data file using that package.  In the top right of the app's 
display you will see the name of the current file loaded, and a button for 
uploading your own project file. By default, the dashboard comes with several 
data sets pre-loaded.  To upload your own data file, click the upload file
button, and then the 'Browse...' button.  Select your project file using the
file explorer, and it will be uploaded into the app.  If the upload is
successful, the file upload panel will show the name of the data file and the
scenarios contained in it.

### Plotting maps

Switch to the Maps tab.  The control panel at the right of the display has 
widgets for selecting a scenario to plot and a query to plot.  Select the 
scenario and query you wish to plot, and the map will be updated.  The 'Extent' 
option allows you to view different regions of the world (global, or several
regions of interest).  If your data set includes spatial (gridded) data, another
option will appear to change the background regions shown on the map.

At the bottom of the map there is a slider bar selects the year to plot.

### Exploring data in greater detail

The Explore tab provides a view of the query variable over time.  Above the bar 
is a selector box labeled 'Break totals into subcategories by:', which allows
you to change the bar plot into a stacked bar plot, with the elements making up
the stack coming from the variable you select.  To change which variable is
selected, use the 'Plot Variable' dropdown in the top right.

Below are controls for choosing and comparing scenarios.  The option to 'Add
Difference Scenario' allows you to see the change in values from one scenario
against another. Clicking that checkbox creates a second dropdown from which you
can select a second scenario. To compare more than one scenario, use the
'Compare Scenarios' tab.

Filtering can be applied by using the panel to the right of the plot.  By
default, only the Latin America and Caribbean GCAM regions are selected, but you
may explore other world regions as well.  Simply select or deselect the
countries you wish to view, and the plot will update automatically.

For any variation of the bar chart, the values of each bar can be seen by
hovering over the bar or segment of a bar of interest.  To view the dataset as a
whole, click the 'View as Table' button.  This brings up a table of all of the
data currently being plotted, with options to search and filter the raw dataset.
The entire unfiltered dataset can be downloaded as a .csv file with the
'Download' button located beneath the table.

### Comparing multiple scenarios

The Compare Scenarios tab is useful if you wish to see plots of different
scenarios side-by-side.  Like the other tabs, it allows you to choose a specific
variable to plot, but this time for multiple scenarios at once.  To add or
remove scenarios, choose them from the 'Selected Scenarios' dropdown, or click
on one and press delete.

## Creating a project file with rgcam

The `rgcam` package provides functions for extracting GCAM data from GCAM output databases and importing it into R for analysis. The central concept in rgcam is the "project data file", which contains an R-native representation of selected queries for one or more scenarios. The package provides functions to run the GCAM Model Interface to extract data and add the results to a new or existing project data file, as well as to manage previously created project data.

### Installation

To use `rgcam` it is necessary to have R installed, and an IDE such as RStudio is highly recommended. To install rgcam, open an R session and run

```
install_github('JGCRI/rgcam')
```

The package includes a copy of the GCAM Model Interface and BaseX library, so it is not necessary to have it otherwise installed or configured. It also requires having a version of Java that can run the Model Interface, which is a minimum of Java version of 1.7*. 

## Usage

To extract data from a GCAM output database, rgcam uses an XML file containing the queries to retrieve. With that file, it is possible to create or add to a project file using the `addScenario` function. For example:

```
conn <- localDBConn('/path/to/dbs', 'my-gcamdb_basexdb')
prj <- addScenario(conn, 'my-project-name.dat', 'my-scenario-name'
                   'my-batch-queries.xml')
```

This command would run the queries in `my-batch-queries.xml` against the database `my-gcamdb_basexdb`, extract the results for a scenario called "my-scenario-name", and write the results to a file called "my-project-name.dat". Other scenarios, whether in the same database or a different one, can make additional calls to `addScenario` to add them to the project data. The results are also returned and assigned to prj if further processing is desired. Any file created this way can then be used in the GCAM Dashboard.

Existing project data files can be loaded using `loadProject`:

```
prj <- loadProject('my-project-name.dat')
```

The `rgcam` package supports a number of advanced usage modes that are described in a vignette included with the package. Once you have the rgcam installed, run

```
devtools::build_vignettes('rgcam')
browseVignettes('rgcam')
```

For further information, visit https://github.com/JGCRI/rgcam.

