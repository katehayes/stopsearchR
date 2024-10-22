# library(usethis)
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(zoo)
library(sf)
# use_readme_md()

# First, exploring the police API.....

# force_list <- GET("https://data.police.uk/api/forces") %>% 
#   content("text") %>% # extract the JSON
#   fromJSON() # convert the JSON string to a list

# query_params <- list(force="metropolitan")
# response <- GET("https://data.police.uk/api/stops-force", query = query_params)
# body <- content(response, "text")
# parsed_data <- fromJSON(body)

# dates_list <- GET("https://data.police.uk/api/crimes-street-dates") %>% 
#   content("text")  %>% # extract the JSON
#   fromJSON() # convert the JSON string to a list

# check <- dates_list$`stop-and-search`[[12]]
# date_range <- data.frame(months = dates_list$date) # API only gives back until 2021

# ...... but the API only allows access to data back to 2021
# As a result, need to extract data from the archive page

temp1 <- tempfile()
temp2 <- tempfile()
temp3 <- tempfile()
temp4 <- tempfile()

options(timeout = 1000) # download kept timing out when limit was at 60 second default
download.file("https://data.police.uk/data/archive/2024-08.zip", temp1)
download.file("https://data.police.uk/data/archive/2021-08.zip", temp2)
download.file("https://data.police.uk/data/archive/2018-08.zip", temp3)
download.file("https://data.police.uk/data/archive/2015-08.zip", temp4)

# You can find the code for each police force w the following:
# force_list <- GET("https://data.police.uk/api/forces") %>% 
#   content("text") %>% # extract the JSON
#   fromJSON() # convert the JSON string to a list

# should be able to extract either by force or by data type
# COME BACK HERE AND CHANGE FUNCTION EXTRACT SO THAT YOU CAN GIVE IT A LIST OF ZIPPED FOLDERS IN TEMPFILES
raw_met_ss <- extract(zipped_folder = temp1, police = "metropolitan", data_type = "stop-and-search") %>% 
  bind_rows(extract(zipped_folder = temp2,  police = "metropolitan", data_type = "stop-and-search")) %>% 
  bind_rows(extract(zipped_folder = temp3,  police = "metropolitan", data_type = "stop-and-search")) %>% 
  bind_rows(extract(zipped_folder = temp4,  police = "metropolitan", data_type = "stop-and-search"))

# save(raw_met_ss, file = "data/raw_met_ss.RData")



# year <- "2015"
# month <- "12"
# police <- "metropolitan"
# data <- "stop-and-search"
# my_path <- paste(year, "-", month, "/", year, "-", month, "-", police, "-", data, ".csv$", sep = "")


# zipped_files <- list(temp1, temp2, temp3)
# 
# file_list <- append(lapply(zipped_files, function(fn) unzip(get(fn), list = TRUE)$Name))
# 
# l <- length(zipped_files)
# 
# 
# check <- append(fl1, fl2)
# fl1 <- unzip(temp1, list = TRUE)$Name
# fl2 <- unzip(temp2, list = TRUE)$Name
# class(temp1)
