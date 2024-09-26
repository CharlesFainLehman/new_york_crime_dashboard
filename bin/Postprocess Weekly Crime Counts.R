library(dplyr)
library(lubridate)
library(zoo)

wcc <- read.csv("dat/weekly_crime_counts.csv")

wcc %>% 
  mutate(Date = as.Date(paste(Year, Week, "1", sep = "-"), format = "%Y-%W-%u"),
         Month = month(Date)) %>% 
  group_by(Offense) %>%
  arrange(Year, Week) %>%
  mutate(rollingavg = rollsum(n, 52, align = 'right', fill = NA)) %>%
  ungroup() %>%
  group_by(Offense, Year) %>%
  arrange(Week) %>%
  mutate(ytd = cumsum(n)) %>%
  group_by(Offense, Year, Month) %>%
  mutate(monthly_n = sum(n)) %>%
  write.csv("dat/weekly_crime_counts_post_processed.csv", row.names = F)