## Compiling CPCe data
## Description: Script looping through each of the site level raw point level CPCe analyzed files for CRAG/R2R data in 2016
## Juliette Verstaen | juliette.verstaen@noaa.gov
## May 16th, 2023


## Load libraries ------------------------------------------
library(tidyverse)
library(readxl)
library(doParallel)


## 1. Set up everything for the loop to run properly ------------------------------------------

# a. Grab the names of all the files to get all the sites we will be extracting data from
site_list <-  list.files(path = paste0(jv_path, "raw_data/biological/CRAG/ASCRMP_tutuila/photoquad_labeled/2016/site_level/", sep = "" ),
                         full.names = FALSE,
                         include.dirs = FALSE) %>% 
  str_remove_all(".xlsx")

# b. create vector of all the transects we will loop through 
transect_list <- c(1:6)

# c. Create empty df that we will fill with the extracted data
compiled_data <- data.frame(site = NA,
                            year = NA, 
                            transect = NA,
                            photo_number = NA,
                            taxon_raw = NA,
                            major_cat = NA)


## 2. Loop through each site and for each transect grab the data we are interested in ------------------------------------------

foreach(site = site_list) %:% 
  
  # site <- site_list[28] # These are just here for troublshooting purposes
  
  foreach(transect = transect_list) %do% {
    
    # transect = transect_list[3] # These are just here for troublshooting purposes
    transect_name = paste0("T", transect) 
    
    ## grab the total number of frames for that transect 
    total_frames <- read_xlsx(path = paste0(jv_path, "raw_data/biological/CRAG/ASCRMP_tutuila/photoquad_labeled/2016/site_level/",
                                            site,
                                            ".xlsx",
                                            sep = "" ),
                              sheet = 'Data Summary',
                              skip = 5) %>%
      rename(column_1 = 'TRANSECT NAME') %>%  ## idk why I've always had issues with the filter function when the column is not machine readable
      filter(column_1 == "Number of frames") %>% 
      pull(transect_name)
    
    
    ## Read in the raw data and do some wrangling
    data <- read_xlsx(path = paste0(jv_path, "raw_data/biological/CRAG/ASCRMP_tutuila/photoquad_labeled/2016/site_level/",
                                    site,
                                    ".xlsx",
                                    sep = ""),
                      sheet = paste0(transect_name, "_raw")) %>% 
      dplyr::select(taxon_raw = 'Raw Data',
                    major_cat = 'Major Category') %>% 
      mutate(transect = transect, 
             site = site,
             year = 2016,
             photo_number = rep(c(1:total_frames), each = 5)) 
    
    ## add it into the empty df
    compiled_data <- rbind(compiled_data, data) %>% 
      drop_na() ## get rid of that one all NA row that's from creating the empty df
    
  }

## 3. Save the newly compiled data! ------------------------------------------

write_csv(compiled_data, paste0(jv_path, "raw_data/biological/CRAG/ASCRMP_tutuila/photoquad_labeled/2016/compile_2016.csv"))