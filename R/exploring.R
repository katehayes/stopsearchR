# https://stackoverflow.com/questions/67797842/how-to-use-r-to-identify-most-up-to-date-downloadable-file-on-a-web-page

library(rvest)
library(stringr)
library(xml2)


force_list <- GET("https://data.police.uk/api/forces") %>%
  content("text") %>% # extract the JSON
  fromJSON() # convert the JSON string to a list


page <- read_html("https://data.police.uk/data/archive/")

check <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("/[0-9]{4}\\-[0-9]{2}\\.zip$") # find those that end in 'year-month.zip'
  check
  
  check
  
  temp1 <- tempfile()
  
  options(timeout = 1000) # download kept timing out when limit was at 60 second default
  download.file("https://data.police.uk/data/archive/2024-08.zip", temp1)
  
 files_list
    
    files_list <- unzip(temp1, list = TRUE)$Name
    files_list <- grep(police, files_list, value = TRUE)
    files_list <- grep(data_type, files_list, value = TRUE)
    
    df <- bind_rows(lapply(files_list, function(fn) read_csv(unz(zipped_folder, fn), col_types = cols(Latitude = col_double(), Longitude = col_double()))))
    

    
    list_files <- function(zipped_folder) {
      
      files_list <- unzip(zipped_folder, list = TRUE)$Name
      
      for (i in files_list) {
        
      }
      
      files_list <- grep(police, files_list, value = TRUE)
      files_list <- grep(data_type, files_list, value = TRUE)
      
      df <- bind_rows(lapply(files_list, function(fn) read_csv(unz(zipped_folder, fn), col_types = cols(Latitude = col_double(), Longitude = col_double()))))
      
      return(df)
      
    }
  
