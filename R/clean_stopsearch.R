w_shape <- st_read("/Users/katehayes/Library/CloudStorage/GoogleDrive-khayes2@sheffield.ac.uk/My Drive/CL_drive_data/Wards_December_2022_Boundaries_UK_BFC_-3416072881830331872 (1)/WD_DEC_2022_UK_BFC.shp")
# going to use ward boundaries from the same place as below, the place the police linked, and hope it works
# actually maybe I've changed back
# w_shape <- st_read("/Users/katehayes/THdata/Wards_(E+W)_2011_Boundaries_(Full_Extent)/WD_DEC_2011_EW_BFE.shp")

# lsoa_shape <- st_read("/Users/katehayes/CLmodelR/temp_data/LSOA_Dec_2021_Boundaries_Full_Clipped_EW_BFC_2022_4005706377815092351/LSOA_2021_EW_BFC_V7.shp")
# lsoa_shape <- st_read("/Users/katehayes/THdata/LSOA_2011_EW_BFC_shp/LSOA_2011_EW_BFC.shp")
# god, police are using 2011 LSOAs for the data release..
# https://webarchive.nationalarchives.gov.uk/ukgwa/20160110200248/http://www.ons.gov.uk/ons/guide-method/geography/products/census/spatial/2011/index.html
# this is the link from the police data website
lsoa_shape <- st_read("/Users/katehayes/Library/CloudStorage/GoogleDrive-khayes2@sheffield.ac.uk/My Drive/THdata/Lower_layer_super_output_areas_(E+W)_2011_Boundaries_(Full_Extent)_V2/LSOA_2011_EW_BFE_V2.shp")
# lsoa2LA <- read.csv("/Users/katehayes/THdata/OAs_to_LSOAs_to_MSOAs_to_LEP_to_LAD_(December_2022)_Lookup_in_England_(V2).csv")
lsoa2LA <- read_xlsx("/Users/katehayes/Library/CloudStorage/GoogleDrive-khayes2@sheffield.ac.uk/My Drive/THdata/LSOA11_WD21_LAD21_EW_LU_V2.xlsx")
# lsoa2ward <- read.csv("/Users/katehayes/THdata/LSOA_(2021)_to_Ward_to_Lower_Tier_Local_Authority_(May_2022)_Lookup_for_England_and_Wales.csv")
# lsoa2lsoa <- read.csv("/Users/katehayes/THdata/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales_(Version_2).csv")


save(w_shape, file = "Output/Data/Cleaned/w_shape.Rdata")
save(lsoa_shape, file = "Output/Data/Cleaned/lsoa_shape.Rdata")
save(lsoa2LA, file = "Output/Data/Cleaned/lsoa2LA.Rdata")
# this downloaded set is mising november 2022...
# long lat data goes missing in 2016...

ss_geom <- extract_ss(common_path = "/Users/katehayes/THdata/f3eb9ff0eecb21cb2136c6c18dc78ff364a463b4") %>% 
bind_rows(extract_ss(common_path = "/Users/katehayes/THdata/2020-04")) %>%
  bind_rows(extract_ss(common_path = "/Users/katehayes/THdata/2017-04")) %>%
  filter(!is.na(Latitude),
         !is.na(Longitude)) %>%
  distinct(Latitude, Longitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)


ss_geom$lsoa <- lonlat_to_lsoa(ss_sf = ss_geom)
ss_geom$lsoa_shape <- lonlat_to_lsoashape(ss_sf = ss_geom)

ss_geom <- ss_geom %>%
  left_join(lsoa2LA %>%
              select(LSOA11NM, LAD21NM, WD21NM) %>%
              rename(lsoa = LSOA11NM,
                     LA = LAD21NM,
                     ward = WD21NM))




ss_raw_0516to0322 <- extract_ss(common_path = "/Users/katehayes/Library/CloudStorage/GoogleDrive-khayes2@sheffield.ac.uk/My Drive/THdata/f3eb9ff0eecb21cb2136c6c18dc78ff364a463b4") %>%
  bind_rows(extract_ss(common_path = "/Users/katehayes/Library/CloudStorage/GoogleDrive-khayes2@sheffield.ac.uk/My Drive/THdata/2020-04"))%>%
  bind_rows(extract_ss(common_path = "/Users/katehayes/Library/CloudStorage/GoogleDrive-khayes2@sheffield.ac.uk/My Drive/THdata/2017-04")) %>%
  filter(!is.na(Latitude),
         !is.na(Longitude)) %>%
  rename(outcome = Outcome,
         powers = Legislation,
         search_type = Type,
         date = Date,
         gender = Gender,
         age = `Age range`,
         ethnicity_self = `Self-defined ethnicity`,
         ethnicity_officer = `Officer-defined ethnicity`,
         operation = `Part of a policing operation`,
         link = `Outcome linked to object of search`,
         remove_clothing = `Removal of more than just outer clothing`,
         op_name = `Policing operation`,
         reason = `Object of search`) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_join(ss_geom)
# have not yet managed to add ward shape - but thats fine


stop_search_0516to0322 <- ss_raw_0516to0322 %>%
  mutate(long_date = ymd_hms(date),
         short_date = as.Date(date)) %>%
  mutate(time = hour(long_date),
         day = wday(long_date, label = TRUE),
         month = month(long_date, label = TRUE)) %>%
  arrange(date)



save(stop_search_0516to0322, file = "Output/Data/Cleaned/stop_search_0516to0322.Rdata")

ss_th <- stop_search_0516to0322 %>% 
  filter(LA == "Tower Hamlets")

tf <- stop_search_0516to0322 %>% 
  st_drop_geometry() %>% 
  filter(LA == "Bexley")


library(sf)
library(ggplot2)
library(tidyverse)

# check <- ss_th %>% 
#   st_transform(crs = 3857)
# st_geometry(ss_th) <- "lsoa_shape"


th_lsoa_list <- lsoa2LA %>% 
  filter(LAD21NM == "Tower Hamlets") %>% 
  select(LSOA11NM, WD21NM) %>% 
  rename(lsoa = LSOA11NM,
         ward = WD21NM)

  

ss_th_car <- ss_th %>% 
  filter(search_type %in% c("Person and Vehicle search", "Vehicle search"))

ss_th_person <- ss_th %>% 
  filter(search_type == "Person search")


ss_lsoa_list <- ss_th %>% 
  st_drop_geometry() %>% 
  distinct(lsoa)

sec60_lsoa_list <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(powers == "Criminal Justice and Public Order Act 1994 (section 60)") %>% 
  distinct(lsoa)

PACE_lsoa_list <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(powers == "Police and Criminal Evidence Act 1984 (section 1)") %>% 
  distinct(lsoa)

sec23_lsoa_list <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(powers == "Misuse of Drugs Act 1971 (section 23)") %>% 
  distinct(lsoa)

sec47_lsoa_list <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(powers == "Firearms Act 1968 (section 47)") %>% 
  distinct(lsoa)


missing_lsoa_shape <- th_lsoa_list %>% 
  filter(!(lsoa %in% ss_lsoa_list$lsoa)) %>% 
  left_join(lsoa_shape %>% 
              select(LSOA11NM) %>% 
              rename(lsoa = LSOA11NM)) %>% 
  rename(lsoa_shape = geometry) %>% 
  mutate(count = 0) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326)

missing_sec60_lsoa_shape <- th_lsoa_list %>% 
  filter(!(lsoa %in% sec60_lsoa_list$lsoa)) %>% 
  left_join(lsoa_shape %>% 
              select(LSOA11NM) %>% 
              rename(lsoa = LSOA11NM)) %>% 
  rename(lsoa_shape = geometry) %>% 
  mutate(count = 0) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326)

missing_PACE_lsoa_shape <- th_lsoa_list %>% 
  filter(!(lsoa %in% PACE_lsoa_list$lsoa)) %>% 
  left_join(lsoa_shape %>% 
              select(LSOA11NM) %>% 
              rename(lsoa = LSOA11NM)) %>% 
  rename(lsoa_shape = geometry) %>% 
  mutate(count = 0) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326)

missing_sec23_lsoa_shape <- th_lsoa_list %>% 
  filter(!(lsoa %in% sec23_lsoa_list$lsoa)) %>% 
  left_join(lsoa_shape %>% 
              select(LSOA11NM) %>% 
              rename(lsoa = LSOA11NM)) %>% 
  rename(lsoa_shape = geometry) %>% 
  mutate(count = 0) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326)

missing_sec47_lsoa_shape <- th_lsoa_list %>% 
  filter(!(lsoa %in% sec47_lsoa_list$lsoa)) %>% 
  left_join(lsoa_shape %>% 
              select(LSOA11NM) %>% 
              rename(lsoa = LSOA11NM)) %>% 
  rename(lsoa_shape = geometry) %>% 
  mutate(count = 0) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326)
  

# OK I THINK THE ISSUE IS PROBABLY - THERE ARE MUTLIPLE WARDS CALLED THE SAME NAME
# just if you want to make a big map, which isnt the priority actually
london_la_list <- ss_london %>% 
  st_drop_geometry() %>% 
  distinct(LA) 

gone_wrong_list <- c("Central Bedfordshire",
                     "Brighton and Hove",
                     "Buckinghamshire",
                     "North East Derbyshire",
                     "Oxford",
                     "Nottingham")






# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # #the last day # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 









# files_to_read <- list.files(
#   path = common_path,        # directory to search within
#   pattern = "-metropolitan-stop-and-search.csv$",
#   recursive = TRUE,          # search subdirectories
#   full.names = TRUE          # return the full path
# )

ss_geom <- lapply(files_to_read, read.csv) %>% 
  bind_rows() %>% 
  filter(!is.na(Latitude), 
         !is.na(Longitude)) %>% 
  distinct(Latitude, Longitude) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) # 4326


# ss_geom$ward <- lonlat_to_ward(ss_sf = ss_geom)
# ss_geom$ward_shape <- lonlat_to_wshape(ss_sf = ss_geom)
ss_geom$lsoa <- lonlat_to_lsoa(ss_sf = ss_geom)
ss_geom$lsoa_shape <- lonlat_to_lsoashape(ss_sf = ss_geom)

ss_geom <- ss_geom %>% 
  left_join(lsoa2LA %>% 
              select(LSOA11NM, LAD21NM, WD21NM) %>% 
              rename(lsoa = LSOA11NM,
                     LA = LAD21NM,
                     ward = WD21NM))


ss_raw_03_22_03_23 <- lapply(files_to_read, read.csv) %>% 
  bind_rows() %>% 
  filter(!is.na(Latitude), 
         !is.na(Longitude)) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_join(ss_geom)
# have not yet managed to add ward shape - but thats fine


ss_london <- ss_raw_03_22_03_23 %>% 
  mutate(long_date = ymd_hms(Date),
         short_date = as.Date(Date)) %>% 
  mutate(time = hour(long_date),
         day = wday(long_date, label = TRUE),
         month = month(long_date, label = TRUE)) %>% 
  select(-c(Part.of.a.policing.operation, Policing.operation, 
            Outcome.linked.to.object.of.search, Removal.of.more.than.just.outer.clothing)) %>%  # these are all blank/non-informative
  rename(outcome = Outcome,
         powers = Legislation,
         search_type = Type,
         date = Date,
         gender = Gender,
         age = Age.range,
         ethnicity_self = Self.defined.ethnicity,
         ethnicity_officer = Officer.defined.ethnicity,
         reason = Object.of.search)
