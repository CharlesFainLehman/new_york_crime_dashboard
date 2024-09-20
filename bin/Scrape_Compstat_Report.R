library(pdftools)
library(stringr)

download.file("https://home.nyc.gov/assets/nypd/downloads/pdf/crime_statistics/cs-en-us-city.pdf",
              "dat/compstat.pdf", mode = 'wb')

compstat_raw <- pdf_text("dat/compstat.pdf")
compstat_rows <- unlist(strsplit(compstat_raw, "\n"))

compstat_dates <- unlist(str_extract_all(compstat_raw, "\\d{1,2}/\\d{1,2}/\\d{4}"))

#extracting the rows for the crimes we're interested in

crimes <- compstat_rows[str_detect(compstat_rows, "Murder|Rape|Robbery|Fel. Assault|Burglary|Gr. Larceny|G.L.A.|Petit Larceny|Retail Theft|Misd. Assault|Shooting Inc.")][c(1:10, 12)]

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

crimes$Month <- date[1]
crimes$Day <- date[2]
crimes$Year <- date[3]

write.csv(crimes, paste("dat/individual reports/compstat-", paste(date, collapse = "-"), ".csv", sep = ""))

file.remove("dat/compstat.pdf")