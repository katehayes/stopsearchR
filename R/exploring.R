# https://stackoverflow.com/questions/67797842/how-to-use-r-to-identify-most-up-to-date-downloadable-file-on-a-web-page
library(tidyverse)
library(rvest)
library(stringr)
library(xml2)
library(purrr)
library(zoo)

force_list <- GET("https://data.police.uk/api/forces") %>%
  content("text") %>% # extract the JSON
  fromJSON() # convert the JSON string to a list


series_list <- c("stop-and-search", "outcomes", "street")

page <- read_html("https://data.police.uk/data/archive/")

zip_list <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("/[0-9]{4}\\-[0-9]{2}\\.zip$") # find those that end in 'year-month.zip'

  

  
  temp <- tempfile()
  
  
  options(timeout = 1000) # download kept timing out when limit was at 60 second default
  download.file("https://data.police.uk/data/archive/2024-08.zip", temp)
  zip0824 <- read_csv(unz(temp, "2021-09/2021-09-west-midlands-stop-and-search.csv"))
  
  
  download.file("https://data.police.uk/data/archive/2021-09.zip", temp)
  zip0921 <- read_csv(unz(temp, "2021-09/2021-09-west-midlands-stop-and-search.csv"))
  
  files_list <- as.data.frame(files_list)
  
  # so first q is: are there any files that appear in one zipped folder but are missing from another? (one that they should be in)
  # if entry in col A can be found in list of characters, enter TRUE in col B
  
  # (second q should be something like: for each piece of data, are all the versions the same?)
  
  
  # it should be in the package whereby if you want to make an updated df 
  # checking file presence/absence you can 
  # but otherwise you can use my saved version??
  
  # actually, write a function that tells people what has changed since the last time
  # i did this file accounting
  
  
  # files_list <- unzip(temp, list = TRUE)$Name %>% 
  #   str_sub(1,7) %>% 
  #   unique()
  # files_list
  
  
  accounting <- crossing(year = 2010:2024, month = c("01", "02", "03", "04", 
                                                     "05", "06", "07", "08",
                                                     "09", "10", "11", "12"), 
                         police = force_list$id, series = series_list) %>% 
    filter(year != 2010 | year == 2010 & month == "12") %>% 
    filter(year != 2024 | year == 2024 & as.numeric(month) <= 8) %>% 
    arrange(desc(year), desc(month)) %>% 
    mutate(file_name = paste(year, "-", month, "/", year, "-", month, "-", police, "-", series, ".csv", sep = "")) 
  
    

  
    zip_list <- zip_list[1:10]
    
    zip_list <- paste("https://data.police.uk", zip_list, sep = "")
    
    folders <- list()
    j = 1
    
    
    
  for (i in zip_list) {
    
    col_nm <- paste("zip", str_sub(i, 37, 43), sep = "")
    
    download.file(i, temp)
    # folders[[j]] <- temp
    # j = j + 1

    files_list <- unzip(temp, list = TRUE)$Name
    
    accounting <- accounting %>% 
      mutate(!!col_nm := ifelse(file_name %in% files_list, T, F))
      
  }
    
    # save(accounting, file = "data/accounting.RData")
  
    #change the way zip is named to yearmonth 
    
    zip_list
    
    
    check <- read_csv(unz(folders[[[1]]], "2024-08/2024-08-avon-and-somerset-outcomes.csv"))
    
  
    # for a police force, data type, time span, say which months are present sometimes, never, always. 
    # and where.
    
    # take accounting and return a vector of zip links to most recent instances
    # of a certain force and data type (dates between two points?)
    
    
    # oh lol, an option also to do it in the least amount of downloads?
    # or is that fairly useless
    
    
    
  more <- accounting %>% 
    mutate(month = paste(year, month, sep="-"),
           file_name = substr(file_name, 9, nchar(file_name))) %>% 
    select(-c(year)) %>% 
    pivot_longer(c(starts_with("zip")),
                 names_to = "folder",
                 values_to = "contains") %>% 
    group_by(folder) %>% 
    mutate(folder_min = min(month[contains == T]),
           folder_max = max(month[contains == T])) %>% 
    ungroup() %>% 
    mutate(missing = ifelse(month >= folder_min & month <= folder_max & contains == F,
                                   T, F)) %>% 
    # JUST LOOKING AT MISSING THINGS - THROWING AWAY ALOT OF THE DATASET
    group_by(file_name) %>% 
    filter(any(missing == T)) %>% 
    mutate(status = ifelse(contains == T, "present", ifelse(missing == F, "outside_range", "missing"))) %>% 
    select(-c(police, series, folder_min, folder_max, contains, missing)) %>% 
    filter(any(status == "present")) %>% 
    ungroup() %>% 
    filter(status != "outside_range") %>% 
    group_by(file_name) %>% 
    mutate(check = ifelse(max(month[status == "missing"]) > max(month[status == "present"]), T, F))
  # so basically there are no files that are in a folder present and then in a LATER folder (innappropriately) missing
  

  
  
  
  
  more <- accounting %>% 
    mutate(month = paste(year, month, sep="-"),
           file_name = substr(file_name, 9, nchar(file_name))) %>% 
    select(-c(year)) %>% 
    pivot_longer(c(starts_with("zip")),
                 names_to = "folder",
                 values_to = "contains") %>% 
    group_by(folder) %>% 
    mutate(folder_min = min(month[contains == T]),
           folder_max = max(month[contains == T])) %>% 
    ungroup() %>% 
    mutate(missing = ifelse(month >= folder_min & month <= folder_max & contains == F,
                            T, F)) %>% 
    # JUST LOOKING AT MISSING THINGS - THROWING AWAY ALOT OF THE DATASET
    group_by(file_name) %>% 
    filter(any(missing == T)) %>% 
    mutate(status = ifelse(contains == T, "present", ifelse(missing == F, "outside_range", "missing"))) %>% 
    mutate(sometimes_present = any(status == "present"))
  
  
  never_present <- more %>% 
    filter(sometimes_present == F) %>% 
    distinct(file_name)
  
  sometimes_present <- more %>% 
    filter(sometimes_present == T) %>% 
    distinct(file_name)
  
  #in the final dataset i want to have a tag against each file that says whether sometimes, never, always present 
  
  
  
  
  more <- accounting %>% 
    mutate(month = paste(year, month, sep="-"),
           file_name = substr(file_name, 9, nchar(file_name))) %>% 
    select(-c(year)) %>% 
    pivot_longer(c(starts_with("zip")),
                 names_to = "folder",
                 values_to = "contains") %>% 
    group_by(folder) %>% 
    mutate(folder_min = min(month[contains == T]),
           folder_max = max(month[contains == T])) %>% 
    ungroup() %>% 
    mutate(missing = ifelse(month >= folder_min & month <= folder_max & contains == F,
                            T, F)) %>% 
    # JUST LOOKING AT MISSING THINGS - THROWING AWAY ALOT OF THE DATASET
    group_by(file_name) %>% 
    filter(any(missing == T)) %>% 
    mutate(status = ifelse(contains == T, "present", ifelse(missing == F, "outside_range", "missing"))) %>% 
    mutate(sometimes_present = any(status == "present"))
  
  
  pivot_wider(names_from = folder,
              values_from = status) %>% 
    # mutate(sometimes_present = if_any(starts_with("zip"),  ~. == "present")) %>% 
    filter(if_any(starts_with("zip"),  ~. == "present")) %>% 
    # mutate(check = "outside_range") %>%  -- so all of these zip folders have something
    # present in them that is missing incorrectly elsewhere????
    select(!(where(~ all(.x == "outside_range")))) 
    # is there anything present and LATER missing
    
    
  
    more <- accounting %>% 
    mutate(month = paste(year, month, sep="-"),
           file_name = substr(file_name, 9, nchar(file_name))) %>% 
    select(-c(year)) %>% 
    pivot_longer(c(starts_with("zip")),
                 names_to = "folder",
                 values_to = "contains") %>% 
    group_by(folder) %>% 
    mutate(folder_min = min(month[contains == T]),
           folder_max = max(month[contains == T])) %>% 
    ungroup() %>% 
    mutate(missing = ifelse(month >= folder_min & month <= folder_max & contains == F,
                            T, F)) %>% 
    # JUST LOOKING AT MISSING THINGS - THROWING AWAY ALOT OF THE DATASET
    group_by(file_name) %>% 
    filter(any(missing == T)) %>% 
    mutate(status = ifelse(contains == T, "present", ifelse(missing == F, "outside_range", "missing"))) %>% 
    mutate(sometimes_present = any(status == "present"))
  
  
one_file <- more %>% 
  filter(file_name == "2023-05-cheshire-stop-and-search.csv") %>% 
  filter(status != "outside_range")


one_file <- accounting %>% 
  filter(police == "cheshire",
         series == "stop-and-search",
         year == 2023,
         month == "05")


one_file <- more %>% 
  filter(file_name == "2015-04-gwent-stop-and-search.csv") %>% 
  filter(status != "outside_range")


# always check the diff between first and last instance of a file to begin with

download.file("https://data.police.uk/data/archive/2023-09.zip", temp)
zip0923 <- read_csv(unz(temp, "2023-05/2023-05-cheshire-stop-and-search.csv"))


download.file("https://data.police.uk/data/archive/2024-08.zip", temp)
zip0824 <- read_csv(unz(temp, "2023-05/2023-05-cheshire-stop-and-search.csv"))

check <- all.equal(zip0923, zip0824)


download.file("https://data.police.uk/data/archive/2015-06.zip", temp)
zip0615 <- read_csv(unz(temp, "2015-04/2015-04-gwent-stop-and-search.csv"))

download.file("https://data.police.uk/data/archive/2018-03.zip", temp)
zip0318 <- read_csv(unz(temp, "2015-04/2015-04-gwent-stop-and-search.csv"))

check <- all.equal(zip0318, zip0615)  

check <- zip0318 %>% 
  left_join(zip0615 %>% 
              mutate(in_0615 = T)) 

# none match precisely



check <- zip0318 %>% 
  select(Date) %>% 
  left_join(zip0615 %>% 
              select(Date) %>% 
              mutate(in_0615 = T)) 
# ah ok by 2018 they've blanked out the precise time

check <- zip0318 %>% 
  select(!Date) %>% 
  left_join(zip0615 %>% 
              select(!Date) %>% 
              mutate(in_0615 = T)) 
# and age range is missing in 2018


download.file("https://data.police.uk/data/archive/2018-02.zip", temp)
zip0218 <- read_csv(unz(temp, "2015-04/2015-04-gwent-stop-and-search.csv"))

download.file("https://data.police.uk/data/archive/2016-07.zip", temp)
zip0716 <- read_csv(unz(temp, "2015-04/2015-04-gwent-stop-and-search.csv"))

download.file("https://data.police.uk/data/archive/2015-12.zip", temp)
zip1215 <- read_csv(unz(temp, "2015-04/2015-04-gwent-stop-and-search.csv"))

download.file("https://data.police.uk/data/archive/2015-07.zip", temp)
zip0715 <- read_csv(unz(temp, "2015-04/2015-04-gwent-stop-and-search.csv"))

download.file("https://data.police.uk/data/archive/2015-09.zip", temp)
zip0915 <- read_csv(unz(temp, "2015-04/2015-04-gwent-stop-and-search.csv"))

download.file("https://data.police.uk/data/archive/2015-08.zip", temp)
zip0815 <- read_csv(unz(temp, "2015-04/2015-04-gwent-stop-and-search.csv"))




check <- all.equal(zip0716, zip0218)  

check <- all.equal(zip0815, zip0915) 

date_change <- zip0716 %>% 
  rename(date_0716 = Date) %>% 
  mutate(n = row_number()) %>% 
  full_join(zip0615 %>% 
              rename(date_0615 = Date) %>% 
              mutate(n = row_number())) %>% 
  select(starts_with("date")) %>% 
  mutate(changed = ifelse(date_0716 == date_0615, F, T))

# that's honestly mad.... what's the story there 
# they added an hour onto everhtning.. why??


check <- all.equal(zip0716, zip1215) 



# q's we might want to ask:
# what are all the versions of a dataset
# where is a dataset missing incorrectly and then present
# what is the police force with the most missing
# which files are missing forever and which are only sometimes missing
 # return a list of all zips in which a certain file is present
      # return name of most recent zip in which file is present. 


# if we are using data, we might want to get a little summary report
# detailing what month everything was added, how many months late if late, etc
      
    
  
  missing <- more %>% 
    filter(missing )
    
    


  
    
    
  
  check <- more %>% 
    filter(contains == T)
  
  
  
  # %>% 
    pivot_wider(names_from = date,
                values_from = contains)
    
    
    
    
    
    
    
    
    
    
  
 files_list
    
    files_list <- unzip(temp, list = TRUE)$Name
    files_list <- grep(police, files_list, value = TRUE)
    files_list <- grep(data_type, files_list, value = TRUE)
    
    
    
    
    # So i want it like this:
    # 
    

    
    
    files_list <- grep(police, files_list, value = TRUE)
    files_list <- grep(data_type, files_list, value = TRUE)
    
    df <- bind_rows(lapply(files_list, function(fn) read_csv(unz(zipped_folder, fn), col_types = cols(Latitude = col_double(), Longitude = col_double()))))
    

    files_list <- unzip(temp, list = TRUE)$Name %>% 
      str_sub(1,7) %>% 
      unique()
    
    files_list
    
    list_files <- function(zipped_folder) {
      
      files_list <- unzip(zipped_folder, list = TRUE)$Name %>%
      months_list <- unzip(zipped_folder, list = TRUE)$Name %>% 
        str_sub(1,7) %>% 
        unique()
      
      months_list <- 
      
      disp_data_split_ew <- list()
      disp_data_split_e <- list()
      disp_data_split_w <- list()
      
      for (i in files_list) {
        
      }
      
      files_list <- grep(police, files_list, value = TRUE)
      files_list <- grep(data_type, files_list, value = TRUE)
      
      df <- bind_rows(lapply(files_list, function(fn) read_csv(unz(zipped_folder, fn), col_types = cols(Latitude = col_double(), Longitude = col_double()))))
      
      return(df)
      
    }
  
