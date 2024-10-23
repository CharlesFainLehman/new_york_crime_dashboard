#This is the script I used to summarise the YTD data
#Those data can be found here: https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Current-Year-To-Date-/5uac-w243/about_data
#It's not used in the app, but I save it here for future reference

library(tidyverse)
library(lubridate)

if(!exists("ytd")) {ytd <- read.csv("dat/NYPD_Complaint_Data_Current__Year_To_Date__20241023.csv")}

ytd %>%
  filter(OFNS_DESC %in% c("MURDER & NON-NEGL. MANSLAUGHTER",
                          "RAPE",
                          "ROBBERY",
                          "FELONY ASSAULT",
                          "BURGLARY",
                          "GRAND LARCENY",
                          "GRAND LARCENY OF MOTOR VEHICLE",
                          "PETIT LARCENY",
                          "ASSAULT 3 & RELATED OFFENSES")) %>%
  mutate(Date = as.Date(CMPLNT_FR_DT, format = "%m/%d/%Y"),
         Week = week(Date),
         Year = year(Date)
         ) %>%
  filter(Year == 2024) %>%
  count(OFNS_DESC, Year, Week) %>%
  mutate(Offense = case_when(
    OFNS_DESC == "MURDER & NON-NEGL. MANSLAUGHTER" ~ "Murder",
    OFNS_DESC == "RAPE" ~ "Rape",
    OFNS_DESC == "ROBBERY" ~ "Robbery",
    OFNS_DESC == "FELONY ASSAULT" ~ "Fel. Assault",
    OFNS_DESC == "BURGLARY" ~ "Burglary",
    OFNS_DESC == "GRAND LARCENY" ~ "Gr. Larceny",
    OFNS_DESC == "GRAND LARCENY OF MOTOR VEHICLE" ~ "G.L.A.",
    OFNS_DESC == "PETIT LARCENY" ~ "Petit Larceny",
    OFNS_DESC == "ASSAULT 3 & RELATED OFFENSES" ~ "Misd. Assault"
  )) %>% 
  select(-OFNS_DESC) %>%
  write.csv('dat/weekly_crime_count_Q1-Q3 2024.csv', row.names = F)

if(!exists('historic')) {historic <- read.csv("dat/NYPD_Complaint_Data_Historic_20241023.csv")}

historic %>%
  filter(OFNS_DESC %in% c("MURDER & NON-NEGL. MANSLAUGHTER",
                          "RAPE",
                          "ROBBERY",
                          "FELONY ASSAULT",
                          "BURGLARY",
                          "GRAND LARCENY",
                          "GRAND LARCENY OF MOTOR VEHICLE",
                          "PETIT LARCENY",
                          "ASSAULT 3 & RELATED OFFENSES")) %>%
  mutate(Date = as.Date(CMPLNT_FR_DT, format = "%m/%d/%Y"),
         Week = week(Date),
         Year = year(Date)
  ) %>%
  count(OFNS_DESC, Year, Week) %>%
  mutate(Offense = case_when(
    OFNS_DESC == "MURDER & NON-NEGL. MANSLAUGHTER" ~ "Murder",
    OFNS_DESC == "RAPE" ~ "Rape",
    OFNS_DESC == "ROBBERY" ~ "Robbery",
    OFNS_DESC == "FELONY ASSAULT" ~ "Fel. Assault",
    OFNS_DESC == "BURGLARY" ~ "Burglary",
    OFNS_DESC == "GRAND LARCENY" ~ "Gr. Larceny",
    OFNS_DESC == "GRAND LARCENY OF MOTOR VEHICLE" ~ "G.L.A.",
    OFNS_DESC == "PETIT LARCENY" ~ "Petit Larceny",
    OFNS_DESC == "ASSAULT 3 & RELATED OFFENSES" ~ "Misd. Assault"
  )) %>% 
  select(-OFNS_DESC) %>%
  filter(Week != 53,
         !is.na(Year), !is.na(Week),
         Year >= 2006) %>%
  write.csv("dat/historic_crime_counts.csv", row.names = F)