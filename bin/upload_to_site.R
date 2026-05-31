library(rsconnect)

# The workflow installs packages from Posit's binary RSPM repo for speed, so
# their DESCRIPTION records Repository: RSPM. rsconnect maps that name against
# getOption("repos") to build the manifest, so "RSPM" must resolve to a real
# URL that serves source tarballs (PPM "latest" does); otherwise shinyapps.io's
# build server gets the bare name "RSPM" as a URL and can't fetch sources.
Sys.unsetenv("RENV_CONFIG_REPOS_OVERRIDE")
options(repos = c(RSPM = "https://packagemanager.posit.co/cran/latest",
                  CRAN = "https://cloud.r-project.org"))

rsconnect::setAccountInfo(name='manhattan-institute', token='D7E98B14F95BD4122499A048795045DF', secret=commandArgs(trailingOnly=TRUE)[1])

# DEBUG: dump the manifest so we can see exactly how the bsicons repository is recorded
cat("==== getOption(repos) ====\n"); print(getOption("repos"))
rsconnect::writeManifest(appDir = ".",
  appFiles = c("app.R", "bin/MI_theme.R",
               "dat/weekly_crime_counts_post_processed.csv",
               "www/Master-MI-Black-RGB.png"))
cat("==== manifest.json ====\n")
cat(readLines("manifest.json"), sep = "\n")
cat("\n==== end manifest ====\n")

rsconnect::deployApp(
  appFiles = c("app.R",
               "bin/MI_theme.R",
               "dat/weekly_crime_counts_post_processed.csv",
               "www/Master-MI-Black-RGB.png"),
  forceUpdate = TRUE)