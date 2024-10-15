# install.packages("ggmap")
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("curl")
library(ggplot2)
library(ggmap)
# install.packages("osmdata")
library(osmdata)

# install.packages("sf")
library(sf)
# install.packages("lubridate")
library(lubridate)
library(tidyverse)
library(readxl)
library(viridis)
library(httr)
library(jsonlite)

library(curl)

##------##------##------##------##------##------##------##------##------##------##------##------##------##------##------
##-------------###CLEANING STOP AND SEARCH SPATIAL DATA###--------------------------------------------------------------------
##------##------##------##------##------##------##------##------##------##------##------##------##------##------##------

# missing lsoas to black

add_missing_lsoa <- function(ss_data, full_lsoa_list, full_lsoa_shapes) {
  
  present_lsoa <- ss_data %>% 
    st_drop_geometry() %>% 
    distinct(lsoa)
  
  missing_shapes <- full_lsoa_list %>% 
    filter(!(lsoa %in% present_lsoa$lsoa)) %>% 
    left_join(full_lsoa_shapes %>% 
                select(LSOA11NM) %>% 
                rename(lsoa = LSOA11NM)) %>% 
    rename(lsoa_shape = geometry) %>% 
    mutate(count = 0) %>% 
    st_as_sf() %>% 
    st_transform(crs = 4326)
  
  return(missing_shapes)
  
}


extract <- function(zipped_folder, police = ".", data_type = ".") {
    
    files_list <- unzip(zipped_folder, list = TRUE)$Name
    files_list <- grep(police, files_list, value = TRUE)
    files_list <- grep(data, files_list, value = TRUE)

    df <- bind_rows(lapply(files_list, function(fn) read_csv(unz(zipped_folder, fn), col_types = cols(Latitude = col_double(), Longitude = col_double()))))
     
    return(df)
     
}


# extract <- function(zipped_files, police = ".", data_type = ".") {
#   
#   # l <- length(zipped_files)
#   
#   
#   file_list <- append(lapply(zipped_files, function(fn) unzip(fn, list = TRUE)$Name))
#   file_list <- grep(police, file_list, value = TRUE)
#   file_list <- grep(data, file_list, value = TRUE)
# 
# 
#   df <- bind_rows(lapply(file_list, function(fn) read_csv(unz(zipped_files[[1]], fn), col_types = cols(Latitude = col_double(), Longitude = col_double())))) %>% 
#     bind_rows(lapply(file_list, function(fn) read_csv(unz(zipped_files[[2]], fn), col_types = cols(Latitude = col_double(), Longitude = col_double())))) 
# 
#   return(df)
#   
# }


# extract <- function(zf1, zf2, zf3, zf4, police = ".", data_type = ".") {
#   
#   fl1 <- unzip(zf1, list = TRUE)$Name
#   fl1 <- grep(police, fl1, value = TRUE)
#   fl1 <- grep(data, fl1, value = TRUE)
#   
#   fl2 <- unzip(zf2, list = TRUE)$Name
#   fl2 <- grep(police, fl2, value = TRUE)
#   fl2 <- grep(data, fl2, value = TRUE)
#   
#   fl3 <- unzip(zf2, list = TRUE)$Name
#   fl3 <- grep(police, fl2, value = TRUE)
#   fl3 <- grep(data, fl2, value = TRUE)
#   
#   fl4 <- unzip(zf2, list = TRUE)$Name
#   fl4 <- grep(police, fl2, value = TRUE)
#   fl4 <- grep(data, fl2, value = TRUE)
#   
#   df <- bind_rows(lapply(fl1, function(fn) read_csv(unz(zf1, fn), col_types = cols(Latitude = col_double(), Longitude = col_double())))) %>% 
#     bind_rows(lapply(fl2, function(fn) read_csv(unz(zf2, fn), col_types = cols(Latitude = col_double(), Longitude = col_double())))) %>% 
#     bind_rows(lapply(fl3, function(fn) read_csv(unz(zf3, fn), col_types = cols(Latitude = col_double(), Longitude = col_double())))) %>% 
#     bind_rows(lapply(fl4, function(fn) read_csv(unz(zf4, fn), col_types = cols(Latitude = col_double(), Longitude = col_double()))))
#   
#   return(df)
#   
# }



extract_ss <- function(common_path, file_pattern = "-metropolitan-stop-and-search.csv$") {
  
  files_to_read <- list.files(
    path = common_path,        # directory to search within
    pattern = file_pattern,
    recursive = TRUE,          # search subdirectories
    full.names = TRUE          # return the full path
  )

  # In this case, if the root cause is reading a 0-row csv file,
  # I think the solution is to fix the problem upstream by (e.g.) using
  # col_types in readr::read_csv() to ensure that even
  # empty data frames get the correct column types.
  
  ss_geom <- lapply(files_to_read, read_csv, col_types = cols(`Outcome linked to object of search` = col_character())) %>%
    bind_rows()
  
  return(ss_geom)
  
}

# 
# missing_lsoa_shape <- th_lsoa_list %>% 
#   filter(!(lsoa %in% ss_lsoa_list$lsoa)) %>% 
#   left_join(lsoa_shape %>% 
#               select(LSOA11NM) %>% 
#               rename(lsoa = LSOA11NM)) %>% 
#   rename(lsoa_shape = geometry) %>% 
#   mutate(count = 0) %>% 
#   st_as_sf() %>% 
#   st_transform(crs = 4326)






##------##------##------##------##------##------##------##------##------##------##------##------##------##------##------
##-------------###OLD OLD OLD###--------------------------------------------------------------------
##------##------##------##------##------##------##------##------##------##------##------##------##------##------##------
# lonlat_to_lsoa <- function(ss_sf,
#                            LSOA_shape = lsoa_shape,
#                            name_col = "LSOA11NM") {
#   lsoa_trans <- st_transform(lsoa_shape, crs = 4326)
#   # ss_trans <- st_transform(ss_sf, crs = 3857)
#   ss_trans <- ss_sf
#   lsoa_names <- lsoa_trans[[name_col]]
#   ii <- as.integer(st_intersects(ss_trans, lsoa_trans))
#   lsoa_names[ii]
# }


lonlat_to_lsoa <- function(ss_sf,
LSOA_shape = lsoa_shape,
name_col = "LSOA11NM") {
  lsoa_trans <- st_transform(lsoa_shape, crs = 3857)
  ss_trans <- st_transform(ss_sf, crs = 3857)
  lsoa_names <- lsoa_trans[[name_col]]
  ii <- as.integer(st_intersects(ss_trans, lsoa_trans))
  lsoa_names[ii]
}


lonlat_to_lsoashape <- function(ss_sf,
                                LSOA_shape = lsoa_shape,
                                shape_col = "geometry") {
  lsoa_trans <- st_transform(lsoa_shape, crs = 3857)
  ss_trans <- st_transform(ss_sf, crs = 3857)
  shape <- lsoa_trans[[shape_col]]
  ii <- as.integer(st_intersects(ss_trans, lsoa_trans))
  shape[ii]
}

# lonlat_to_ward <- function(ss_sf,
#                            ward_shape = w_shape,
#                            name_col = "WD11NM") {
#   ward_trans <- st_transform(ward_shape, crs = 3857)
#   ss_trans <- st_transform(ss_sf, crs = 3857)
#   ward_names <- ward_trans[[name_col]]
#   ii <- as.integer(st_intersects(ss_trans, ward_trans))
#   ward_names[ii]
# }

# lonlat_to_wshape <- function(ss_sf,
#                            ward_shape = w_shape,
#                            shape_col = "geometry") {
#   ward_trans <- st_transform(ward_shape, crs = 3857)
#   ss_trans <- st_transform(ss_sf, crs = 3857)
#   shape <- ward_trans[[shape_col]]
#   ii <- as.integer(st_intersects(ss_trans, ward_trans))
#   shape[ii]
# }

# hahaahhahha no, not even nearly
lonlat_to_wshape <- function(ss_sf,
                           ward_shape = w_shape,
                           shape_col = "geometry") {
  ward_trans <- st_transform(ward_shape, crs = 3857)
  ss_trans <- st_transform(ss_sf, crs = 3857)
  shape <- ward_trans[[shape_col]]
  ii <- as.integer(grepl(ward_trans, ss_trans))
  shape[ii]
}

lonlat_to_LA <- function(ss_sf,
                         ward_shape = w_shape,
                         name_col = "LAD22NM") {
  ward_trans <- st_transform(ward_shape, crs = 3857)
  ss_trans <- st_transform(ss_sf, crs = 3857)
  ward_names <- ward_trans[[name_col]]
  ii <- as.integer(st_intersects(ss_trans, ward_trans))
  ward_names[ii]
}
# lonlat_to_shape <- function(ss_sf,
# ward_shape = w_shape,
# shape_col = "geometry") {
#
# ward_trans <- st_transform(ward_shape, crs = 3857)
# ss_trans <- st_transform(ss_sf, crs = 3857)
#
# ward_shape <- ward_trans[[shape_col]]
#
# ii <- as.integer(st_intersects(ss_trans, ward_trans))
# ward_shape[ii]
#
# }
# function that goes through the CSV files
# makes note of the missingness of the age info (CHANGE THIS TO among the person searches!!!)
# converts everything into as above, date gender reason outcome ward location
ss_clean <- function(raw_list) {
  clean_list <- list()
  for (i in 1:length(raw_list)) {
    ss <- raw_list[[i]]
    ss_na <- ss %>%
      mutate(Age.range = ifelse(Age.range == "", NA, Age.range))
    pc <- colMeans(is.na(ss_na))
    age_pc <- pc[["Age.range"]]
    # missing overall, not among birmingham non-vehicle searches...
    # maybe you should change that
    sssf <- st_as_sf(ss, coords = c("Longitude", "Latitude"), crs = 4326)
    sssf$LA <- lonlat_to_LA(ss_sf = sssf)
    ss_birm <- sssf %>%
      filter(LA == "Birmingham",
             Type == "Person search",
             Age.range == "10-17")
    ss_birm$lsoa <- lonlat_to_lsoa(ss_sf = ss_birm)
    ss_birm$ward <- lonlat_to_ward(ss_sf = ss_birm)
    ss_birm <- ss_birm %>%
      mutate(Date = as.Date(Date)) %>%
      select(Date, Gender, Object.of.search, Outcome, ward, lsoa) %>%
      rename(date = Date, gender = Gender, reason = Object.of.search, outcome = Outcome) %>%
      mutate(age_missing_pc = age_pc)
    clean_list[[i]] <- ss_birm
  }
  return(clean_list)
}