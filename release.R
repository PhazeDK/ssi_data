# Goes through sources and cleans up all tables to a unified format
# For data that can be compiled together, this is done
# Then moves files to release folder
library(tidyverse)
source("common.R")

sources <- load_sources()

# Tables:
# Cases_by_age
# Cases_by_sex
# Deaths_over_time
# Municipality_test_pos
# Newly_admitted_over_time
# Region_summary
# Test_pos_over_time
# Test_regioner

unzipped_files <- sources %>% filter(unzipped)
table_dates <- unzipped_files$release_date
tables <- list()
collated <- list()

## Cases_by_municipality
stopifnot(
 all(file.exists(file.path(unzip_path, unzipped_files %>% pull(dir), input_filenames$table_municipality)))
)

tables$municipality <- lapply(file.path(unzip_path, unzipped_files %>% pull(dir), input_filenames$table_municipality), read_csv2)

stopifnot(
 all(mapply(colnames, tables$municipality) == colnames(first(tables$municipality))))

# "Kommune_(id)" "Kommune_(navn)" "Antal_testede" "Antal_bekræftede_COVID-19" "Befolkningstal" "Kumulativ_incidens_(per_100000)"
table_mun_col_names <- c("mun_code", "mun_name", "tests", "cases", "population", "cases_per_100K")
tables$municipality <- lapply(tables$municipality, setNames, table_mun_col_names)

collated$municipality <- do.call(
 rbind, 
 mapply(cbind, tables$municipality, date=table_dates, SIMPLIFY=F)
) %>% arrange(date, mun_code) %>% select(date, mun_code, mun_name, tests, cases, population, cases_per_100K)

# Fixing some punctuation (don't know if the error comes from SSI)
collated$municipality$mun_name <- str_replace_all(collated$municipality$mun_name, '[:punct:]', ".")
collated$municipality$mun_name <- str_replace_all(collated$municipality$mun_name, '[:punct:]', "-")

collated$municipality <- collated[["municipality"]] %>% mutate(
  tests = as.integer(str_replace_all(tests, '\\.', '')),
  cases = as.integer(str_replace_all(cases, '\\.', '')),
  cases_per_100K = as.double(cases_per_100K)
)

# Add base if any
base_table_path <- file.path(base_path, paste0(output_filenames$table_municipality, ".csv"))
if (file.exists(base_table_path)) {
  base_table_municipality <- read.csv(base_table_path, encoding = "UTF-8")
  collated$municipality <- rbind(collated[["municipality"]], base_table_municipality) %>% arrange(date, mun_code) 
}

base_municipalities_path <- file.path(base_path, "municipalities.csv")
if (file.exists(base_municipalities_path)) {
  base_muns <- read.csv(base_municipalities_path, encoding = "UTF-8")
  collated$municipality <- collated$municipality %>% 
    select(-mun_name) %>% 
    left_join(base_muns, by="mun_code") %>% 
    select(date, mun_code, mun_name, tests, cases, population, cases_per_100K)
}

# RELEASE
t <- collated[["municipality"]]
d <- max(t$date)
path_dir <- release_path_from_date(d)
file_name <- paste0(output_filenames$table_municipality, '_', d, '.csv')
path_file <- file.path(path_dir, file_name)

if (!file.exists(path_file)) {
  # Write file
  write.csv(t, path_file, row.names = FALSE, fileEncoding="UTF-8")
  
  # Overwrite latest
  latest <- file.path(release_path, "latest", paste0(output_filenames$table_municipality, ".csv"))
  if (file.exists(latest)) {
    file.remove(latest)
  }
  write.csv(t, latest, row.names = FALSE, fileEncoding="UTF-8")
}

# ## Cases_by_age ##
# # TODO: only handle new files, eg: set "copied"/"released" to True etc. -- LETS NOT. Always start from the beginning,
# # but only if new. 
# stopifnot(
#   all(file.exists(file.path(unzip_path, unzipped_files %>% pull(dir), input_filenames$table_age)))
# )
# 
# tables$age <- lapply(file.path(unzip_path, unzipped_files %>% pull(dir), input_filenames$table_age), read_csv2)
# 
# stopifnot(
#   all(mapply(colnames, tables$age) == colnames(first(tables$age))))
# 
# # [1] "Aldersgruppe" "Antal_bekræftede_COVID-19" "Antal_testede" "Procent_positive"
# table_age_col_names <- c("age_group", "cases", "tested", "percentage_positive")
# tables$age <- lapply(tables$age, setNames, table_age_col_names)
# 
# collated$age <- do.call(
#   rbind, 
#   mapply(cbind, tables$age, date=table_dates, SIMPLIFY=F)
# ) %>% select(-percentage_positive) %>% arrange(date, age_group)
# 
# # TODO: save tables in releases
# # TODO: add baseline from pdf
# 
# 
# ## Cases_by_sex
# stopifnot(
#   all(file.exists(file.path(unzip_path, unzipped_files %>% pull(dir), input_filenames$table_sex)))
# )
# 
# tables$sex <- lapply(file.path(unzip_path, unzipped_files %>% pull(dir), input_filenames$table_sex), read_csv2)
# 
# stopifnot(
#   all(mapply(colnames, tables$sex) == colnames(first(tables$sex))))
# 
# table_sex_col_names <- c("age_group", "female_cases_percentage", "male_cases_percentage", "percentage_positive")
# tables$sex <- lapply(tables$sex, setNames, table_sex_col_names)
# 
# collated$sex <- do.call(
#   rbind, 
#   mapply(cbind, tables$sex, date=table_dates, SIMPLIFY=F)
# ) %>% select(-percentage_positive) %>% arrange(date, age_group)
# 
# 
# ## Deaths_over_time
# stopifnot(
#   all(file.exists(file.path(unzip_path, unzipped_files %>% pull(dir), input_filenames$table_deaths)))
# )
# 
# tables$deaths <- lapply(file.path(unzip_path, unzipped_files %>% pull(dir), input_filenames$table_deaths), read_csv2)
# 
# stopifnot(
#   all(mapply(colnames, tables$deaths) == colnames(first(tables$deaths))))
# 
# table_deaths_col_names <- c("date", "deaths")
# tables$deaths <- lapply(tables$deaths, setNames, table_deaths_col_names)
# collated$deaths <- tibble(table=tables$deaths, date=table_dates) %>% arrange(desc(date)) %>% slice(1L) %>% pull(table) %>% first()
# 
# # There are no uncollated deaths tables
# # TODO: fix dates
