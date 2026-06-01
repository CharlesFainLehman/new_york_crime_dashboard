library(rsconnect)
rsconnect::setAccountInfo(name='manhattan-institute',
                          token=Sys.getenv("SHINYAPPS_TOKEN"),
                          secret=Sys.getenv("SHINYAPPS_SECRET_TOKEN"))
rsconnect::deployApp(
  appFiles = c("app.R",
               "bin/MI_theme.R",
               "dat/weekly_crime_counts_post_processed.csv",
               "www/Master-MI-Black-RGB.png"),
  forceUpdate = TRUE)