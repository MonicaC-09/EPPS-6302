source("R/00_setup.R")

decennial <- readr::read_csv("data/processed/dallas_decennial_tract_2000_2010_2020.csv")
acs       <- readr::read_csv("data/processed/dallas_acs_tract_2010_2020.csv")

message("Decennial rows: ", nrow(decennial))
message("ACS rows: ", nrow(acs))


census_merged <- dplyr::left_join(
  decennial,
  acs,
  by = c("geoid", "name", "year"),
  suffix = c("_decennial", "_acs")
)

message("Merged rows: ", nrow(census_merged))
print(
  census_merged %>%
    dplyr::count(year)
)

if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

readr::write_csv(
  census_merged,
  "data/processed/dallas_census_merged_tract_2000_2010_2020.csv"
)

