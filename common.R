library(tidyverse)
library(lubridate)
library(here)


### Settings ###
input_filenames <- list(
  table_age="Cases_by_age.csv",
  table_sex="Cases_by_sex.csv",
  table_deaths="Deaths_over_time.csv",
  table_municipality="Municipality_test_pos.csv"
)

output_filenames <- list(
  table_municipality="municipality_data"
)

settings <- list(
  # dirs
  download_dir='data/downloads/',
  unzip_dir='data/unzipped/',
  base_dir='data/base/',
  tmp_dir='tmp/',
  release_dir='release/',
  
  # files
  sources_filename="sources.csv"
)

### Common functions ###

# Useful
`%!in%` = Negate(`%in%`)


### Set up environment ###

# Dir paths
project_path <- here()
download_path <- file.path(project_path, settings$download_dir)
unzip_path <- file.path(project_path, settings$unzip_dir)
sources_path <- file.path(project_path, settings$sources_filename)
tmp_path <- file.path(project_path, settings$tmp_dir)
base_path <- file.path(project_path, settings$base_dir)
release_path <- file.path(project_path, settings$release_dir)
dir.create(tmp_path, showWarnings = F)

# Cleanup
rm(settings)

# Set working dir
setwd(project_path)

# Function to load the sources list
load_sources <- function() {
  if (file.exists(sources_path)) {
    sources <- read_csv(
      sources_path,
      locale=locale(tz="Europe/Berlin"),
      col_types = list(
        col_character(),
        col_character(),
        col_logical(),
        col_logical(),
        col_datetime(),
        col_date(),
        col_character(),
        col_datetime(),
        col_logical()
      ))  
  } else {
    sources <- tibble(
      url = character(),
      path = character(),
      downloaded = logical(),
      unzipped = logical(),
      accessed = as.POSIXct(NA),
      release_date = as_date(NA),
      dir = character(),
      updated = as.POSIXct(NA),
      newest_release = logical()
    )
  }
}

release_path_from_date <- function(date) {
  y = paste0(year(date))
  m = paste0(month(date))
  d = file.path(release_path, y, m)
  dir.create(d, showWarnings = F, recursive = T)
  d
}
