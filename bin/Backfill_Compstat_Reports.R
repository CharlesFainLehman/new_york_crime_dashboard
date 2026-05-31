# One-off backfill: recover compstat weeks missed while the scraper was down.
# The live PDF URL only holds the latest report, so missed weeks are pulled
# from the Internet Archive's Wayback Machine. Each distinct archived version
# is parsed with the same logic as Scrape_Compstat_Report.R; the week is taken
# from the date inside the PDF. Weeks that already exist on disk are skipped.

library(pdftools)
library(stringr)
library(lubridate)

orig_url <- "https://www.nyc.gov/assets/nypd/downloads/pdf/crime_statistics/cs-en-us-city.pdf"
ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"

args <- commandArgs(trailingOnly = TRUE)
from <- if (length(args) >= 1) args[1] else "20250101"
to   <- if (length(args) >= 2) args[2] else format(Sys.Date(), "%Y%m%d")

csv_dir <- "dat/individual reports/csv/"
pdf_dir <- "dat/individual reports/pdf/"
dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)

cdx <- sprintf(paste0("http://web.archive.org/cdx/search/cdx?url=%s",
                      "&output=text&fl=timestamp&filter=statuscode:200",
                      "&collapse=digest&from=%s&to=%s"),
               URLencode(orig_url, reserved = TRUE), from, to)

timestamps <- readLines(url(cdx), warn = FALSE)
timestamps <- timestamps[nzchar(timestamps)]
print(paste("Found", length(timestamps), "distinct archived versions"))

for (ts in timestamps) {
  Sys.sleep(3)  # be polite to archive.org; rapid requests get throttled
  wb_url <- sprintf("https://web.archive.org/web/%sid_/%s", ts, orig_url)
  tmp <- tempfile(fileext = ".pdf")
  ok <- tryCatch({
    download.file(wb_url, tmp, mode = "wb", quiet = TRUE,
                  headers = c("User-Agent" = ua))
    TRUE
  }, error = function(e) { print(paste("  download failed for", ts, ":", conditionMessage(e))); FALSE })
  if (!ok) next

  compstat_raw  <- pdf_text(tmp)
  compstat_rows <- unlist(strsplit(compstat_raw, "\n"))
  compstat_dates <- unlist(str_extract_all(compstat_raw, "\\d{1,2}/\\d{1,2}/\\d{4}"))

  crimes <- compstat_rows[str_detect(compstat_rows, "Murder|Rape|Robbery|Fel\\. Assault|Burglary|Gr\\. Larceny|G\\.L\\.A\\.|Petit Larceny|Misd\\. Assault")]
  if (length(crimes) < 9) { print(paste("  skipping", ts, "- found", length(crimes), "crime rows")); next }
  crimes <- crimes[1:9]
  crimes <- strsplit(crimes, "\\s{2,}", perl = TRUE)
  crimes <- lapply(crimes, function(x) x[2:3])
  crimes <- as.data.frame(t(as.data.frame(crimes)))
  rownames(crimes) <- 1:nrow(crimes)
  names(crimes) <- c("Offense", "n")
  crimes$n <- gsub(",", "", crimes$n)

  date <- unlist(strsplit(compstat_dates[1], "/"))
  date <- as.Date(paste(date[3], date[1], date[2], sep = "-"), format = "%Y-%m-%d")
  crimes$Year <- year(date)
  crimes$Week <- week(date)

  csv_path <- paste0(csv_dir, "compstat-", year(date), "-", week(date), ".csv")
  if (file.exists(csv_path)) {
    print(paste("  have", year(date), "week", week(date), "- skipping"))
    next
  }

  write.csv(crimes, csv_path, row.names = FALSE)
  file.copy(tmp, paste0(pdf_dir, "compstat-", year(date), "-", week(date), ".pdf"), overwrite = TRUE)
  print(paste("  WROTE", year(date), "week", week(date), "from snapshot", ts))
}

print("Backfill complete.")
