library(rsconnect)

# The workflow installs packages from Posit's binary RSPM repo for speed, so
# their DESCRIPTION records Repository: RSPM. rsconnect maps that name against
# getOption("repos") to build the manifest, so "RSPM" must resolve to a real
# URL that serves source tarballs (PPM "latest" does); otherwise shinyapps.io's
# build server gets the bare name "RSPM" as a URL and can't fetch sources.
Sys.unsetenv("RENV_CONFIG_REPOS_OVERRIDE")
options(repos = c(RSPM = "https://packagemanager.posit.co/cran/latest",
                  CRAN = "https://cloud.r-project.org"))

app_files <- c("app.R",
               "bin/MI_theme.R",
               "dat/weekly_crime_counts_post_processed.csv",
               "www/Master-MI-Black-RGB.png")

rsconnect::setAccountInfo(name='manhattan-institute', token='D7E98B14F95BD4122499A048795045DF', secret=commandArgs(trailingOnly=TRUE)[1])

# Prime rsconnect's package-source resolution against the repos set above. Without
# this, deployApp's first availablePackages() pass can fall back to a bare "RSPM"
# source that shinyapps.io's build server can't fetch.
rsconnect::writeManifest(appDir = ".", appFiles = app_files)

rsconnect::deployApp(appFiles = app_files, forceUpdate = TRUE)