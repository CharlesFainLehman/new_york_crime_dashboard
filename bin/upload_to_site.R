library(rsconnect)
rsconnect::setAccountInfo(name='manhattan-institute', token='D7E98B14F95BD4122499A048795045DF', secret=commandArgs(trailingOnly=TRUE)[1])
rsconnect::deployApp(
  appFiles = c("app.R",
               "bin/MI_theme.R",
               "dat/weekly_crime_counts_post_processed.csv",
               "www/Master-MI-Black-RGB.png"),
  forceUpdate = TRUE)