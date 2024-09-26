library(rsconnect)
rsconnect::setAccountInfo(name='manhattan-institute', token='D7E98B14F95BD4122499A048795045DF', secret=commandArgs(trailingOnly=TRUE)[1])
rsconnect::deployApp(forceUpdate = TRUE)