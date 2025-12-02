source("R/00_setup.R")

get_decennial_dallas <- function(year) {
  
  if (year %in% c(2000, 2010)) {
    sumfile <- "sf1"
    
    vars <- c("P001001", "H001001")
  } else if (year == 2020) {
    sumfile <- "pl"
    
    vars <- c("P1_001N", "H1_001N")
  } else {
    stop("Year must be 2000, 2010, or 2020.")
  }
  
  
  raw <- tidycensus::get_decennial(
    geography = "tract",
    state     = "TX",
    county    = "Dallas",
    year      = year,
    variables = vars,
    sumfile   = sumfile,
    geometry  = FALSE
  )
  

  wide <- raw %>%
    dplyr::select(GEOID, NAME, variable, value) %>%
    tidyr::pivot_wider(
      id_cols     = c(GEOID, NAME),
      names_from  = variable,
      values_from = value
    )
  

  if (year %in% c(2000, 2010)) {
    wide <- wide %>%
      dplyr::rename(
        total_pop     = P001001,
        housing_units = H001001
      )
  } else if (year == 2020) {
    wide <- wide %>%
      dplyr::rename(
        total_pop     = P1_001N,
        housing_units = H1_001N
      )
  }
  
  clean <- wide %>%
    dplyr::mutate(
      year  = year,
      GEOID = as.character(GEOID)
    ) %>%
    janitor::clean_names()
  
  return(clean)
}

years_decennial <- c(2000, 2010, 2020)

decennial_dallas <- purrr::map_dfr(years_decennial, get_decennial_dallas)

print(
  decennial_dallas %>%
    dplyr::count(year)
)

if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

readr::write_csv(
  decennial_dallas,
  "data/processed/dallas_decennial_tract_2000_2010_2020.csv"
)
