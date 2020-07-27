# Update source list from SSI data, download zip files and unzip.
# Keeps a record of all sources previously downloaded.
# Does not (yet) handle duplicate data for the same dates, which might arise later.

library(tidyverse)
library(lubridate)
library(rvest)
library(urltools)

source("common.R")

# Load sources file
sources <- load_sources()

loaded_sources <- sources

# Get urls online
base_url<- "https://www.ssi.dk/sygdomme-beredskab-og-forskning/sygdomsovervaagning/c/covid19-overvaagning/arkiv-med-overvaagningsdata-for-covid19"

# Get download links
pg <- read_html(base_url)
links <- html_attr(html_nodes(pg, "a"), "href")
links_fixed <- str_remove_all(links, "\\.zip.*$")
zip_links <- unique(paste0(links_fixed[grepl("data-epidemiologiske?-rapport", links_fixed, ignore.case = T)], ".zip"))

new_files <- tibble(url=zip_links[zip_links %!in% sources$url]) %>% mutate(
  path = sub(".*/", "", url_parse(url)$path),
  downloaded = FALSE,
  unzipped = if_else(str_detect(path,".zip$"), FALSE, NA),
  accessed = as.POSIXct(NA),
  release_date = as_date(str_match(path, "\\d{8}"), format="%d%m%Y"),
  dir = as.character(NA),
  updated = as.POSIXct(NA),
  newest_release = FALSE,
)

# Append to souces
sources <- rbind(sources, new_files)

# Any new files to download?
to_download <- sources %>% filter(!downloaded) 
if ( to_download %>% tally() > 0) {
  # If so, download them
  cat("# Files to download:", to_download %>% tally() %>% pull(n), "\n")
  download.file(to_download$url, file.path(download_path, to_download$path), method = "libcurl")
  
  # Update sources
  sources <- sources %>% mutate(
    downloaded = path %in% list.files(download_path),
    accessed = if_else(is.na.POSIXlt(accessed), file.info(file.path(download_path, sources$path))$ctime, accessed)
  )
} else {
  cat("No new files to download.\n")
}

# Any new files to unzip?
to_unzip <- sources %>% filter(str_detect(sources$path,".zip$"), !unzipped, downloaded)
if ( to_unzip %>% tally() > 0) {
  # If so, unzip them
  cat("# Files to unzip:", to_unzip %>% tally() %>% pull(n), "\n")
  
  updated <- list()
  for (source_path in to_unzip$path) {
    dir_name <- sub('\\.zip$', '', source_path)
    dir_path <- file.path(unzip_path, dir_name)
    dir.create(dir_path, showWarnings = F)
    unzip(file.path(download_path, source_path), exdir=dir_path)
    cat("Unzipped", source_path, "into", dir_path, "\n")
    updated[source_path] <- as_datetime(max(unzip(file.path(download_path, source_path), list=T)$Date))
  }
  
  # Update sources
  sources <- sources %>% mutate(
    unzipped = file.path(unzip_path, path) %in% paste0(list.dirs(unzip_path), ".zip"),
    dir = if_else(is.na(dir) & unzipped, sub('\\.zip$', '', path), dir)
  )
  
  # Set last updated
  updated <- rownames_to_column(stack(tibble(updated)), "path") %>%
    transmute(path, updated=as_datetime(values))
  
  sources <- sources %>%
    left_join(updated, by="path") %>% 
    mutate(
      updated = if_else(is.na(updated.x), updated.y, updated.x)
    ) %>% 
    select(-updated.x, -updated.y)
  
  # newest by releasedate?
  sources <- sources %>% 
    select(-newest_release) %>% 
    group_by(release_date) %>% 
    arrange(desc(updated)) %>% 
    mutate(
      newest_release = updated == max(updated)
      ) %>% 
    ungroup()
  
} else {
  cat("No new files to unzip.\n")
}

# Save sources
if ( identical(sources, loaded_sources) ) {
  cat("Sources unchanged.\n")
} else {
  write_csv(sources, sources_path)
  cat("Sources updated in", sources_path, ".\n")
}


