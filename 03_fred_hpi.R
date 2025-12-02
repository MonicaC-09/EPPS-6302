
source("R/00_setup.R")
library(fredr)

# IMPORTANT:
# To run this script, you must have FRED_API_KEY in your .Renviron
# Example:
# FRED_API_KEY=your-key-here
fredr_set_key(Sys.getenv("FRED_API_KEY"))


fred_series_id <- "ATNHPIUS19124Q"

dallas_hpi <- fredr::fredr(
  series_id         = fred_series_id,
  observation_start = as.Date("1990-01-01")
) %>%
  dplyr::mutate(
    year    = lubridate::year(date),
    quarter = lubridate::quarter(date),
    hpi_id  = fred_series_id
  ) %>%
  dplyr::rename(
    hpi_value = value
  )

print(head(dallas_hpi))

if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

readr::write_csv(
  dallas_hpi,
  "data/processed/dallas_fred_hpi_quarterly_1990_onwards.csv"
)
