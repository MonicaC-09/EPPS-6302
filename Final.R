####### Miguel's Data Processing #########

# 1 -  Setup
required_pkgs <- c(
  "tidyverse",   
  "tidycensus",
  "fredr",
  "lubridate",
  "janitor"
)

installed <- rownames(installed.packages())
to_install <- setdiff(required_pkgs, installed)

if (length(to_install) > 0) {
  install.packages(to_install)
}

invisible(lapply(required_pkgs, library, character.only = TRUE))

options(tigris_use_cache = TRUE)

tidycensus::census_api_key(Sys.getenv("5de21d57a80e980580ab0f04b54ae4f3a74e7b50"), install = TRUE, overwrite=TRUE)
fredr::fredr_set_key(Sys.getenv("f5d478a71be3c9bcfdaeeaf09c00cbf3"))

# 2 Decennial Census

source("00_setup.R")

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


# 3 - American Community Survey (ACS)

source("00_setup.R")

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


# 4 - FRED Economic Data


source("00_setup.R")
library(fredr)

# IMPORTANT:
# To run this script, you must have FRED_API_KEY in your .Renviron
# Example:
fredr_set_key(Sys.getenv("f5d478a71be3c9bcfdaeeaf09c00cbf3"))
FRED_API_KEY="f5d478a71be3c9bcfdaeeaf09c00cbf3"
fredr_set_key(FRED_API_KEY)

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

# 5 - Zillow Housing Data

source("00_setup.R")

zillow_path <- "City_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"

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
    region_name  == "Dallas",
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
  dplyr::select(region_name, state, date, year, zhvi)

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

# 6 - Merge and Finalize Datasets

source("00_setup.R")

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
####### End of Miguel's Data Processing ######### 


######## Tryphosa's Data Analysis #########


library(tidyverse)

# 1. Load Data 

# Census data (includes total_pop and housing_units)
decennial_data <- read_csv("dallas_decennial_tract_2000_2010_2020.csv")

# ACS data (includes median_hh_income and tenure/renter share)
acs_data <- read_csv("dallas_acs_tract_2010_2020.csv")

# 2. Data Cleaning and Harmonization

# Clean the column names for consistency
decennial_data <- decennial_data %>%
  rename(total_pop = total_pop, housing_units = housing_units) %>%
  select(geoid, name, year, total_pop, housing_units)

acs_data <- acs_data %>%
  rename(median_income = median_hh_income) %>%
  select(geoid, year, median_income, owner_share, renter_share)

# 3. Merge Datasets into a Unified Demographic Panel

unified_demographics <- full_join(decennial_data, acs_data, by = c("geoid", "year")) %>%
  # Filter out NA rows if they exist, but for now, just clean.
  filter(!is.na(geoid)) %>%
  # Ensure year is treated as a factor for time series analysis
  mutate(year = as.integer(year)) %>%
  # Select final columns for the analysis-ready demographic file
  select(geoid, name, year, total_pop, housing_units, median_income, owner_share, renter_share) %>%
  # Sort for easy viewing
  arrange(geoid, year)

# Print a summary to check the data structure
print(head(unified_demographics))
print(summary(unified_demographics))

# Save the unified demographic data 
write_csv(unified_demographics, "unified_dallas_demographics_1990_2020.csv")

# integrating with the Zillow/FRED housing data

# Load and prepare Zillow City-level data to track the general housing price trend.
zillow_city_data <- read_csv("dallas_zillow_zhvi_city_annual.csv") %>%
  rename(year = year, zhvi_mean_city = zhvi_mean_city)

#4. Final Analysis Dataset (Demographics + City Housing Trend)
final_analysis_data <- unified_demographics %>%
  left_join(zillow_city_data, by = "year")

# Save the final dataset for the next analysis script
write_csv(final_analysis_data, "final_dallas_panel_dataset.csv")

message("Data preparation complete. Saved 'final_dallas_panel_dataset.csv' for analysis.")

# 02Analysis and modeling

library(tidyverse)
library(broom)

# Load Final Dataset 
panel_data <- read_csv("final_dallas_panel_dataset.csv")

# 2. Calculate Change Variables (2010 to 2020)

change_data <- panel_data %>%
  # Filter to the start and end years of the most complete data span
  filter(year %in% c(2010, 2020)) %>%
  # Pivot wider to get 2010 and 2020 values side-by-side for easy calculation
  pivot_wider(
    id_cols = c(geoid, name),
    names_from = year,
    values_from = c(median_income, owner_share, zhvi_mean_city)
  ) %>%
  # Calculate Percentage Change for key variables
  mutate(
    # Change in owner-occupied share (a demographic shift metric)
    delta_owner_share = owner_share_2020 - owner_share_2010,
    
    # Percentage change in median household income (a measure of economic shift/affluence)
    pct_change_median_income = ((median_income_2020 - median_income_2010) / median_income_2010) * 100,
    
    # Change in City-Wide ZHVI (This is constant for all tracts, use as a constant check)
    delta_zhvi_city = zhvi_mean_city_2020 - zhvi_mean_city_2010
  ) %>%
  # Select variables for modeling
  select(geoid, name, delta_owner_share, pct_change_median_income, delta_zhvi_city) %>%
  # Remove tracts with missing data for this period
  drop_na()

message(paste("Analysis dataset created with", nrow(change_data), "Census Tracts."))
print(head(change_data))

#3. Statistical Modeling (Linear Regression)

# Model 1: Predict change in owner share using change in median income
# Hypothesis: Tracts with a larger increase in median income saw a greater change in owner share.
model_1 <- lm(delta_owner_share ~ pct_change_median_income, data = change_data)

# Print detailed summary
cat("\n--- MODEL 1: Owner Share Change vs. Median Income Change ---\n")
print(summary(model_1))

# Use 'broom' to generate a clean table of model results (ready for Quarto report)
model_results <- tidy(model_1)
print(model_results)

# 4. Correlation Analysis
correlation_result <- cor.test(change_data$delta_owner_share, change_data$pct_change_median_income)

cat("\n--- CORRELATION TEST: Owner Share Change vs. Median Income Change ---\n")
print(correlation_result)
message(paste("Correlation Coefficient (R):", round(correlation_result$estimate, 4)))

# Save the change data for visualization
write_csv(change_data, "dallas_tract_change_metrics_2010_2020.csv")

library(tidyverse)
library(sf)

change_data <- read_csv("dallas_tract_change_metrics_2010_2020.csv")

# Scatter Plot: Relationship between Shifts 

# Plot the relationship found in the regression model
scatter_plot <- ggplot(change_data, aes(x = pct_change_median_income, y = delta_owner_share)) +
  geom_point(alpha = 0.6, color = "#0072B2") + # Blue points
  geom_smooth(method = "lm", se = TRUE, color = "#D55E00", linetype = "dashed") + # Orange regression line
  labs(
    title = "Change in Owner Share vs. Percentage Change in Median Income (2010-2020)",
    x = "Percentage Change in Median Household Income (%)",
    y = "Change in Owner-Occupied Housing Share (Percentage Points)",
    caption = paste("R =", round(cor(change_data$pct_change_median_income, change_data$delta_owner_share), 3))
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic")
  )

print(scatter_plot)

# Save the scatter plot
ggsave("scatter_plot_income_vs_owner_share.png", scatter_plot, width = 8, height = 6)

# 3.Mapping and Graphs
# Description: Create shapefile (study_geo.shp) and basic visualizations.

library(tidyverse)
library(sf)
library(tigris)
library(ggplot2)
library(viridis)   # for scale_fill_viridis_c()

options(tigris_use_cache = TRUE)

# 1. Load Change Metrics

change_data <- read_csv("dallas_tract_change_metrics_2010_2020.csv") %>%
  mutate(geoid = as.character(geoid))

cat("\n--- Change Data (preview) ---\n")
print(head(change_data))

# 2. Get 2020 Dallas County Tract Geometry

# Texas = "TX", Dallas County FIPS = 113
dallas_tracts <- tracts(
  state  = "TX",
  county = "Dallas",
  year   = 2020,
  cb     = TRUE,
  class  = "sf"
) %>%
  st_transform(4326) %>%        # standard WGS84 lat/lon
  mutate(GEOID = as.character(GEOID))

cat("\n--- Dallas Tracts (preview) ---\n")
print(head(dallas_tracts[, c("GEOID")]))

# 3. Join Attributes to Geometry

study_geo <- dallas_tracts %>%
  left_join(
    change_data,
    by = c("GEOID" = "geoid")
  )

cat("\n--- Joined sf object (preview) ---\n")
print(head(study_geo))

# 4. Clean Fields for Shapefile Export

study_geo_export <- study_geo %>%
  select(
    GEOID,                        # tract id
    tractname = name,             # tract label
    d_ownshare = delta_owner_share,
    pct_incmi  = pct_change_median_income,
    d_zhvi     = delta_zhvi_city,
    geometry
  )

cat("\n--- Export-ready sf (names) ---\n")
print(names(study_geo_export))

# 5. Ensure Output Directory & Remove Old Shapefile (if any)

dir.create("outputs_shapefiles", showWarnings = FALSE, recursive = TRUE)

old_files <- list.files(
  "outputs_shapefiles",
  pattern = "^study_geo\\.",
  full.names = TRUE
)

if (length(old_files) > 0) {
  file.remove(old_files)
  message("Removed old study_geo.* files in outputs_shapefiles/")
}

# 6. Write Shapefile

st_write(
  study_geo_export,
  "outputs_shapefiles/study_geo.shp"
)

message("Shapefile written to 'outputs_shapefiles/study_geo.shp'.")

# 7. Scatter Plot (Change vs Income)

scatter_plot <- ggplot(change_data, aes(x = pct_change_median_income, y = delta_owner_share)) +
  geom_point(alpha = 0.6, color = "#0072B2") +
  geom_smooth(method = "lm", se = TRUE, color = "#D55E00", linetype = "dashed") +
  labs(
    title   = "Change in Owner Share vs. % Change in Median Income (2010–2020)",
    x       = "Percentage Change in Median Household Income (%)",
    y       = "Change in Owner-Occupied Share (percentage points)",
    caption = paste("R =",
                    round(cor(change_data$pct_change_median_income,
                              change_data$delta_owner_share), 3))
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.4),
    plot.caption = element_text(face = "italic")
  )

ggsave("scatter_plot_income_vs_owner_share.png", scatter_plot, width = 8, height = 6)
message("Saved 'scatter_plot_income_vs_owner_share.png'.")

# 8. Basic Choropleth Map

map_owner_change <- ggplot(study_geo_export) +
  geom_sf(aes(fill = d_ownshare), color = NA) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  labs(
    title = "Change in Owner-Occupied Housing Share by Census Tract (2010–2020)",
    fill  = "Δ Owner Share\n(percentage points)"
  ) +
  theme_minimal()

ggsave("map_owner_share_change.png", map_owner_change, width = 8, height = 6)
message("Saved 'map_owner_share_change.png'.")

# 9. Choropleth: % Change in Median Income

map_income_change <- ggplot(study_geo_export) +
  geom_sf(aes(fill = pct_incmi), color = NA) +
  scale_fill_viridis_c(
    option   = "magma",
    na.value = "grey80",
    labels   = scales::label_number(accuracy = 1)
  ) +
  labs(
    title = "% Change in Median Household Income by Census Tract (2010–2020)",
    fill  = "% Change\nMedian Income"
  ) +
  theme_minimal()

ggsave(
  "map_income_change_2010_2020.png",
  map_income_change,
  width  = 8,
  height = 6
)

message("Saved 'map_income_change_2010_2020.png'.")

# 10. Categorical Map: Gentrification Pattern Types

median_inc_change  <- median(study_geo_export$pct_incmi,  na.rm = TRUE)
median_own_change  <- median(study_geo_export$d_ownshare, na.rm = TRUE)

gentrification_geo <- study_geo_export %>%
  mutate(
    gent_cat = case_when(
      pct_incmi >= median_inc_change & d_ownshare >= median_own_change ~
        "High income ↑ / Owner ↑",
      pct_incmi >= median_inc_change & d_ownshare <  median_own_change ~
        "High income ↑ / Owner ↓",
      pct_incmi <  median_inc_change & d_ownshare >= median_own_change ~
        "Low income ↑ / Owner ↑",
      pct_incmi <  median_inc_change & d_ownshare <  median_own_change ~
        "Low income ↑ / Owner ↓",
      TRUE ~ "Missing data"
    ),
    gent_cat = factor(
      gent_cat,
      levels = c(
        "High income ↑ / Owner ↑",
        "High income ↑ / Owner ↓",
        "Low income ↑ / Owner ↑",
        "Low income ↑ / Owner ↓",
        "Missing data"
      )
    )
  )

map_gentrification_types <- ggplot(gentrification_geo) +
  geom_sf(aes(fill = gent_cat), color = NA) +
  scale_fill_viridis_d(
    option   = "turbo",
    direction = -1,
    na.value = "grey80"
  ) +
  labs(
    title = "Income & Owner-Occupied Change Pattern by Census Tract (2010–2020)",
    fill  = "Pattern Type"
  ) +
  theme_minimal()

ggsave(
  "map_gentrification_pattern_types.png",
  map_gentrification_types,
  width  = 9,
  height = 6
)

message("Saved 'map_gentrification_pattern_types.png'.")

# 11. Faceted Map: Compare Owner Share Change vs. % Income Change

# Prepare long-format data for faceting
long_change_sf <- study_geo_export %>%
  select(GEOID, d_ownshare, pct_incmi, geometry) %>%
  pivot_longer(
    cols      = c(d_ownshare, pct_incmi),
    names_to  = "indicator",
    values_to = "value"
  ) %>%
  mutate(
    indicator = recode(
      indicator,
      d_ownshare = "Δ Owner-Occupied Share (pp)",
      pct_incmi  = "% Change in Median Income"
    )
  )

map_faceted_change <- ggplot(long_change_sf) +
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c(
    option   = "plasma",
    na.value = "grey80"
  ) +
  facet_wrap(~ indicator, ncol = 2) +
  labs(
    title = "Change in Owner Share and Median Income by Census Tract (2010–2020)",
    fill  = "Value"
  ) +
  theme_minimal()

ggsave(
  "map_faceted_owner_vs_income_change_two.png",
  map_faceted_change,
  width = 8, height = 6)

message("Saved 'map_faceted_owner_vs_income_change_two.png'.")

# 12. Line Graph: City-Level Housing Price Trend (ZHVI)


zillow_city <- read_csv("dallas_zillow_zhvi_city_annual.csv")

line_housing <- ggplot(zillow_city, aes(x = year, y = zhvi_mean_city)) +
  geom_line(color = "#1f78b4", size = 1.2) +
  geom_point(color = "#1f78b4", size = 2) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Dallas City ZHVI Trend (2000–2020)",
    x = "Year",
    y = "Median Home Value (ZHVI)",
    caption = "Source: Zillow ZHVI City-Level Data"
  ) +
  theme_minimal(base_size = 10)

ggsave("line_graph_city_housing_trend.png", line_housing, width = 8, height = 6)
message("Saved 'line_graph_city_housing_trend.png'.")

# 13. Line Graph: Tract-Level Median Income Trend (Average Across Tracts)

panel_data <- read_csv("final_dallas_panel_dataset.csv")

income_trend <- panel_data %>%
  group_by(year) %>%
  summarize(mean_income = mean(median_income, na.rm = TRUE))

line_income <- ggplot(income_trend, aes(x = year, y = mean_income)) +
  geom_line(color = "#33a02c", size = 1.2) +
  geom_point(color = "#33a02c", size = 2) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Average Median Household Income in Dallas Census Tracts (2010–2020)",
    x = "Year",
    y = "Mean Median Income",
    caption = "Source: ACS 2010–2020"
  ) +
  theme_minimal(base_size = 14)

ggsave("line_graph_income_trend.png", line_income, width = 8, height = 6)
message("Saved 'line_graph_income_trend.png'.")
######## End of Tryphosa's Data Analysis #########
