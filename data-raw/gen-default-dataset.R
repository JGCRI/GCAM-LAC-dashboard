library(rgcam)
library(iamrpt)

# This function converts GCAM data from wide form to long form
read.gcam <- function(filename, data.units=NULL, scenario=NULL) {
  f <- read.csv(filename)
  ycols <- grep("(X1)|(X2)", names(f))
  names(f)[ycols] <- substring(names(f)[ycols], 2) # remove the X

  f <- tidyr::gather(f, 'year', 'value', ycols)
  f <- dplyr::mutate_if(f, is.integer, as.numeric)

  if ('longitude' %in% names(f)) {
    f <- dplyr::rename(f, lon=longitude)
  }
  if ('latitude' %in% names(f)) {
    f <- dplyr::rename(f, lat=latitude)
  }
  if (!is.null(data.units)) {
    f$Units <- data.units
  }

  if (!is.null(scenario)) {
    f$scenario <- scenario
  }

  f
}

# Add gridded water data to the default dataset. The three hydrology files
# contain half-degree resolution data on water scarcity, water supply, and water
# demand, which all come from a separate climate and hydrology model.
addHydroData <- function(defaultData) {

  hydro1 <- system.file('extdata/hydrology',
                        'hydro_scarcity-index_REF_IPSL_2010_2050_2090.csv',
                        package = "gcamlacdash")
  hydro2 <- system.file('extdata/hydrology',
                        'hydro_q_mm-per-mth_rollmean20yr_ipsl-cm5a-lr_hist-rcp8p5_2010_2050_2090.csv',
                        package = "gcamlacdash")
  hydro3 <- system.file('extdata/hydrology',
                        'hydro_demand_mm-per-yr_ipsl_cm5a-lr_hist-rcp8p5_2010_2050_2090.csv',
                        package = "gcamlacdash")

  scar <- read.gcam(hydro1, data.units="water scarcity index", scenario="Reference")
  scar <- dplyr::filter(scar, scar$value > 0 & scar$value < 1) # invalid data vals
  supp <- read.gcam(hydro2, data.units="mm per year", scenario="Reference")
  dmnd <- read.gcam(hydro3, data.units="mm per year", scenario="Reference")
  dmnd <- dplyr::filter(dmnd, dmnd$value > 0 & dmnd$value < 50)

  defaultData <- addQueryTable(defaultData, scar, "Water Scarcity", clobber = T)
  defaultData <- addQueryTable(defaultData, supp, "Water Supply")
  defaultData <- addQueryTable(defaultData, dmnd, "Water Demand")
  defaultData
}

# This dataset includes queries for a reference scenario and a scenario
# according to the Paris increased ambitions. The dataset itself is too large to
# include in the package, but here is how the file was created.
#
# conn <- localDBConn('../IDB', 'idb_basexdb')
# dashboard <- addScenario(conn, 'inst/extdata/dashboard.dat', '../latin_america_dashboard_queries.xml')
# defaultData <- loadProject(system.file('extdata/dashboard.dat', package = "gcamlacdash"))
scnctl <- "../dashboard-resources/clean_data/scen.csv"
varctl <- "../dashboard-resources/clean_data/var.csv"
dbfldr <- "../IDB"
generate(scnctl, varctl, dbfldr, fileformat = 'rgcam', wideformat = F)
if(file.exists('inst/extdata/dashboard.dat')) {
  warning("File dashboard.dat already exists, copy aborted.")
} else {
  file.rename('iamrpt.dat', 'inst/extdata/dashboard.dat')
}
defaultData <- loadProject('inst/extdata/dashboard.dat')
defaultData <- addHydroData(defaultData)


# This dataset includes queries for different SSP scenarios. The dataset itself
# is too large to include in the package, but here is how the file was created.
#
# conn <- localDBConn('../IDB/SSP', 'database_basexdb_SSP_water_assumptions')
# for (i in c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")) {
#   ssp <- addScenario(conn, 'inst/extdata/ssp.dat', i, '../IDB/SSP/ssp_water_latin_america_queries.xml')
# }
sspData <- loadProject(system.file('extdata/ssp.dat', package = "gcamlacdash"))


devtools::use_data(sspData, defaultData)
