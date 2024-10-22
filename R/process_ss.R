
# Adding more spatial info/details
# Police give long/lat, the centre points of the LSOA
# Want to add polygon as well, so that we can create maps
ss_geom <- raw_met_ss %>% 
  rename(lat = Latitude,
         lng = Longitude) %>% 
  filter(!is.na(lat),
         !is.na(lng)) %>%
  distinct(lat, lng)  %>%
  st_as_sf(coords = c("lng", "lat"),
           crs = 4326)

ss_geom <- ss_geom %>% 
  mutate(lsoa = lnglat_to_lsoa(stop_search_sf = ss_geom, lsoa_shp_file = lsoa11_shape, col = "LSOA11NM"),
         lsoa_shape = lnglat_to_lsoa(stop_search_sf = ss_geom, lsoa_shp_file = lsoa11_shape, col = "geometry")) %>% 
  left_join(lsoa2LA %>%
              select(LSOA11NM, LAD21NM, WD21NM) %>%
              rename(lsoa = LSOA11NM,
                     la = LAD21NM,
                     ward = WD21NM))


met_ss <- raw_met_ss %>% 
  rename(outcome = Outcome,
         powers = Legislation,
         search_type = Type,
         date = Date,
         gender = Gender,
         age = `Age range`,
         ethnicity_self = `Self-defined ethnicity`,
         ethnicity_officer = `Officer-defined ethnicity`,
         reason = `Object of search`,
         link = `Outcome linked to object of search`,
         remove_clothing = `Removal of more than just outer clothing`,
         lat = Latitude,
         lng = Longitude) %>% 
  select(-c(`Part of a policing operation`, `Policing operation`)) %>% 
  mutate(time = format(as.POSIXct(date), format = "%H:%M:%S"),
         date = format(as.POSIXct(date), format = "%Y-%m-%d")) %>% 
  filter(!is.na(lat),
         !is.na(lng)) %>%
  st_as_sf(coords = c("lng", "lat"),
           crs = 4326) %>% 
  st_join(ss_geom)

# save(met_ss, file = "data/met_ss.RData")


th_lsoa_shape <- lsoa2LA %>% 
  filter(LAD21NM == "Tower Hamlets") %>% 
  select(LSOA11NM, WD21NM) %>% 
  rename(lsoa = LSOA11NM,
         ward = WD21NM) %>% 
  left_join(lsoa11_shape %>% 
              select(LSOA11NM) %>% 
              rename(lsoa = LSOA11NM)) %>% 
  rename(lsoa_shape = geometry) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326)

