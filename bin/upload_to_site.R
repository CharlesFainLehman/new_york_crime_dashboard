library(rsconnect)

# The workflow installs packages from Posit's binary RSPM repo for speed, which
# leaves "RSPM" as their recorded source. shinyapps.io's build server can't fetch
# source tarballs from that, so pin a plain CRAN source repo for the manifest.
Sys.unsetenv("RENV_CONFIG_REPOS_OVERRIDE")
options(repos = c(CRAN = "https://cloud.r-project.org"))

rsconnect::setAccountInfo(name='manhattan-institute', token='D7E98B14F95BD4122499A048795045DF', secret=commandArgs(trailingOnly=TRUE)[1])
rsconnect::deployApp(
  appFiles = c("app.R",
               "bin/MI_theme.R",
               "dat/weekly_crime_counts_post_processed.csv",
               "www/Master-MI-Black-RGB.png"),
  forceUpdate = TRUE)