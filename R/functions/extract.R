extract <- function(zipped_folder, police = ".", data_type = ".") {
  
  files_list <- unzip(zipped_folder, list = TRUE)$Name
  files_list <- grep(police, files_list, value = TRUE)
  files_list <- grep(data, files_list, value = TRUE)
  
  df <- bind_rows(lapply(files_list, function(fn) read_csv(unz(zipped_folder, fn), col_types = cols(Latitude = col_double(), Longitude = col_double()))))
  
  return(df)
  
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

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

