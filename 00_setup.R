
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

