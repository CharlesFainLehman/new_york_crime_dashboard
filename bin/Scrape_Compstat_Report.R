library(pdftools)
library(stringr)
library(lubridate)

download.file("https://www.nyc.gov/assets/nypd/downloads/pdf/crime_statistics/cs-en-us-city.pdf",
              "dat/compstat.pdf", mode = 'w+')

compstat_raw <- pdf_text("dat/compstat.pdf")
compstat_rows <- unlist(strsplit(compstat_raw, "\n"))

compstat_dates <- unlist(str_extract_all(compstat_raw, "\\d{1,2}/\\d{1,2}/\\d{4}"))

#extracting the rows for the crimes we're interested in

crimes <- compstat_rows[str_detect(compstat_rows, "Murder|Rape|Robbery|Fel. Assault|Burglary|Gr. Larceny|G.L.A.|Petit Larceny|Misd. Assault")][1:9]

#splitting on white space unless the white space is preceded by a period
#which avoids splitting "Fel. Assault" and "Gr. Larceny" on white space
crimes <- strsplit(crimes, "\\s{2,}", perl = T)

#Grab the offense and the most recent WTD counts
crimes <- lapply(crimes, function(x) x[2:3])

crimes <- as.data.frame(t(as.data.frame(crimes)))
rownames(crimes) <- 1:nrow(crimes)
names(crimes) <- c("Offense", "n")

crimes$n <- gsub(",", "", crimes$n)

date <- unlist(strsplit(compstat_dates[1], "/"))
date <- as.Date(paste(date[3], date[1], date[2], sep = "-"), format = "%Y-%m-%d")

crimes$Year <- year(date)
crimes$Week <- week(date)

if(!dir.exists("dat/individual reports/csv/")) {dir.create("dat/individual reports/csv/")}

write.csv(crimes, paste("dat/individual reports/csv/compstat-", year(date), "-", week(date), ".csv", sep = ""), row.names = F)

file.rename("dat/compstat.pdf", paste('dat/individual reports/pdf/compstat-', year(date), "-", week(date), ".pdf", sep = ""))