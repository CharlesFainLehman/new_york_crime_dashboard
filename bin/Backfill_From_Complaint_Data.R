# One-off backfill for weeks the weekly scraper missed AND that the Internet
# Archive never captured (2024-51, 2025-02, 2025-43). These are reconstructed
# from NYPD Complaint Data (Historic), dataset qgea-i56i, aggregated by
# CompStat's Mon-Sun week window and mapped to the 9 CompStat categories.
#
# NOTE: these weeks are ESTIMATES. Complaint micro-data is finalized and dated
# by occurrence, so it runs slightly higher than the preliminary CompStat
# counts in the rest of the series. Two categories diverge by definition:
# Rape (KY_CD 104 is narrower than CompStat's UCR rape) undercounts, and
# Misd. Assault (KY_CD 344 "Assault 3 & related" is broader) overcounts.

library(stringr)

base <- "https://data.cityofnewyork.us/resource/qgea-i56i.csv"

# KY_CD -> CompStat category
cats <- c("101" = "Murder", "104" = "Rape", "105" = "Robbery",
          "106" = "Fel. Assault", "107" = "Burglary", "109" = "Gr. Larceny",
          "110" = "G.L.A.", "341" = "Petit Larceny", "344" = "Misd. Assault")

# target week -> Mon-Sun window (windows anchored to known CompStat report dates)
weeks <- list(
  list(year = 2024, week = 51, start = "2024-12-16", end = "2024-12-22"),
  list(year = 2025, week = 2,  start = "2025-01-13", end = "2025-01-19"),
  list(year = 2025, week = 43, start = "2025-10-27", end = "2025-11-02")
)

csv_dir <- "dat/individual reports/csv/"

for (w in weeks) {
  where <- sprintf(
    "cmplnt_fr_dt between '%sT00:00:00' and '%sT23:59:59' and ky_cd in('%s')",
    w$start, w$end, paste(names(cats), collapse = "','"))
  q <- paste0(base,
              "?$select=", URLencode("ky_cd,count(1) as n", reserved = TRUE),
              "&$where=", URLencode(where, reserved = TRUE),
              "&$group=ky_cd&$limit=50000")

  res <- read.csv(url(q), colClasses = c(ky_cd = "character"))

  counts <- setNames(res$n, res$ky_cd)
  crimes <- data.frame(
    Offense = unname(cats),
    n = as.integer(counts[names(cats)]),
    Year = w$year,
    Week = w$week,
    stringsAsFactors = FALSE)
  crimes$n[is.na(crimes$n)] <- 0  # category with zero complaints that week

  out <- paste0(csv_dir, "compstat-", w$year, "-", w$week, ".csv")
  write.csv(crimes, out, row.names = FALSE)
  print(paste("WROTE", out, "(total complaints:", sum(crimes$n), ")"))
}

print("Complaint-data backfill complete.")
