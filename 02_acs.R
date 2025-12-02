source("R/00_setup.R")

acs_vars <- c(
  median_hh_income      = "B19013_001",  
  tenure_total_occupied = "B25003_001",
  tenure_owner          = "B25003_002",
  tenure_renter         = "B25003_003"
)

get_acs_dallas <- function(year) {

  raw <- tidycensus::get_acs(
    geography = "tract",
    state     = "TX",
    county    = "Dallas",
    year      = year,
    survey    = "acs5",
    variables = acs_vars,
    geometry  = FALSE
  )
  
  wide <- raw %>%
    dplyr::select(GEOID, NAME, variable, estimate) %>%
    tidyr::pivot_wider(
      id_cols     = c(GEOID, NAME),
      names_from  = variable,
      values_from = estimate
    )
  
  clean <- wide %>%
    dplyr::mutate(
      year         = year,
      owner_share  = tenure_owner / tenure_total_occupied,
      renter_share = tenure_renter / tenure_total_occupied,
      GEOID        = as.character(GEOID)
    ) %>%
    janitor::clean_names()
  
  return(clean)
}

years_acs <- c(2010, 2020)

acs_dallas <- purrr::map_dfr(years_acs, get_acs_dallas)

print(
  acs_dallas %>% dplyr::count(year)
)

if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

readr::write_csv(
  acs_dallas,
  "data/processed/dallas_acs_tract_2010_2020.csv"
)

