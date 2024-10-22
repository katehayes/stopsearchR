
lnglat_to_lsoa <- function(stop_search_sf, lsoa_shp_file, col) {
  
  lsoa_trans <- st_transform(lsoa_shp_file, crs = 3857)
  ss_trans <- st_transform(stop_search_sf, crs = 3857)
  lsoa_names <- lsoa_trans[[col]]
  ii <- as.integer(st_intersects(ss_trans, lsoa_trans))
  lsoa_names[ii]
  
}
