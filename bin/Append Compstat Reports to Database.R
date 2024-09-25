library(stringr)
library(dplyr)

weekly_summary <- read.csv("dat/weekly_crime_counts.csv")

compstat_reports <- list.files("dat/individual reports", full.names = T)

for(file in compstat_reports) {
  print(file)
  year = as.numeric(str_extract(file, "\\d\\d\\d\\d"))
  week = as.numeric(str_extract(file, "\\d\\d(?=\\.)"))
  
  #if the combination of year and week is already in the data set
  if(nrow(filter(weekly_summary, Year == year, Week == week)) > 0) {
    print("file already appended")
    break
  } else
  {
    weekly_summary <- bind_rows(weekly_summary, read.csv(file))
    print("appending file")
  }
  
  file.remove(file)
}

write.csv(weekly_summary, "dat/weekly_crime_counts.csv", row.names = F)