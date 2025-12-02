source("R/00_setup.R")

zillow_path <- "C:/Users/migue/Downloads/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"

if (!file.exists(zillow_path)) {
  stop("❌ Zillow file not found at:\n", zillow_path,
       "\nMake sure the file exists and the path is correct.")
}

message("📥 Reading Zillow data from: ", zillow_path)
zillow_raw <- readr::read_csv(zillow_path)

message("Columns in RAW file (first 20):")
print(names(zillow_raw)[1:20])

zillow_clean <- zillow_raw %>% janitor::clean_names()

message("Columns AFTER clean_names (first 20):")
print(names(zillow_clean)[1:20])

required_cols <- c("region_name", "city", "state")

missing_cols <- setdiff(required_cols, names(zillow_clean))
if (length(missing_cols) > 0) {
  warning("⚠ Missing expected columns: ", paste(missing_cols, collapse = ", "))
}

zillow_dallas_zip <- zillow_clean %>%
  dplyr::filter(
    city  == "Dallas",
    state %in% c("TX", "Texas")
  )

message("ZIP rows for Dallas: ", nrow(zillow_dallas_zip))

date_cols <- grep("^x\\d{4}_\\d{2}_\\d{2}$", names(zillow_dallas_zip), value = TRUE)

if (length(date_cols) == 0) {
  stop("❌ No monthly date columns detected (expected names like x2000_01_31).")
}

message("Number of monthly date columns detected: ", length(date_cols))

zillow_long <- zillow_dallas_zip %>%
  tidyr::pivot_longer(
    cols      = dplyr::all_of(date_cols),
    names_to  = "date_col",
    values_to = "zhvi"
  ) %>%
  dplyr::mutate(
    # convert x2000_01_31 -> 2000-01-31 -> Date
    date = as.Date(gsub("^x", "", gsub("_", "-", date_col))),
    year = lubridate::year(date)
  ) %>%
  dplyr::select(region_name, city, state, date, year, zhvi)

message("Long-format sample:")
print(head(zillow_long))

zillow_zip_annual <- zillow_long %>%
  dplyr::group_by(region_name, year) %>%
  dplyr::summarise(
    zhvi_mean = mean(zhvi, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  dplyr::rename(zip = region_name)

message("Annual ZIP-level rows: ", nrow(zillow_zip_annual))



zillow_city_annual <- zillow_long %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    zhvi_mean_city = mean(zhvi, na.rm = TRUE),
    .groups        = "drop"
  )

if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

readr::write_csv(
  zillow_zip_annual,
  "data/processed/dallas_zillow_zhvi_zip_annual.csv"
)

readr::write_csv(
  zillow_city_annual,
  "data/processed/dallas_zillow_zhvi_city_annual.csv"
)