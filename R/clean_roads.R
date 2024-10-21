# install.packages("osmdata")
library(osmdata)
library(sf)
library(ggplot2)
library(dplyr)

# diff between 1 and 2?


# q <- opq(bbox = 'greater london uk') %>%
#   add_osm_feature(key = 'highway') %>%
#   osmdata_sf()

TH021D_roadlist <- c("Adler Street",
                     "Alie Street",
                     "Assam Street",
                     "Buckle Street",
                     "Coke Street",
                     "Commercial Road",
                     "Commercial Street",
                     "Feather Mews",
                     "Fieldgate Street",
                     "Greenfield Road",
                     "Mears Close",
                     "Myrdle Street",
                     "New Drum Street",
                     "New Road",
                     "Parfett Street",
                     "Plumbers Row",
                     "Settles Street",
                     "Vine Court",
                     "Wapping High Street",
                     "White Church Lane",
                     "Whitechapel High Street",
                     "Whitechapel Road")

bb <- getbb('London Borough of Tower Hamlets') 
# %>% 
#   as.data.frame() %>% 
#   mutate(min = min - 0.015,
#          max = max + 0.015) %>% 
#   as.matrix()


w_shape <- st_read("/Users/katehayes/Library/CloudStorage/GoogleDrive-khayes2@sheffield.ac.uk/My Drive/temp_data/Wards_December_2022_Boundaries_UK_BFC_-3416072881830331872/WD_DEC_2022_UK_BFC.shp")
lsoa_shape <- st_read("/Users/katehayes/Library/CloudStorage/GoogleDrive-khayes2@sheffield.ac.uk/My Drive/THdata/Lower_layer_super_output_areas_(E+W)_2011_Boundaries_(Full_Extent)_V2/LSOA_2011_EW_BFE_V2.shp")

TH021D_shape <- lsoa_shape %>% 
  filter(LSOA11NM == "Tower Hamlets 021D")


q <- opq(bbox = 'greater london uk') %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "motorway_link")) %>%
  add_osm_feature(key = 'name') %>%
  osmdata_xml(filename = 'bigroads.osm')

bigroads <- sf::st_read('bigroads.osm', layer = 'lines') 
save(bigroads, file = "Output/Data/Cleaned/bigroads.Rdata")



q2 <- opq(bbox = 'greater london uk') %>%
  add_osm_feature(key = "highway", 
                  value = c("primary", "primary_link")) %>%
  add_osm_feature(key = 'name') %>%
  osmdata_xml(filename = 'bigroads2.osm')

bigroads2 <- sf::st_read('bigroads2.osm', layer = 'lines') 
save(bigroads2, file = "Output/Data/Cleaned/bigroads2.Rdata")

m <- opq(bbox = 'greater london uk') %>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "secondary_link")) %>%
  add_osm_feature(key = 'name') %>%
  osmdata_xml(filename = 'medroads.osm')

medroads <- sf::st_read('medroads.osm', layer = 'lines') 
save(medroads, file = "Output/Data/Cleaned/medroads.Rdata")

m2 <- opq(bbox = 'greater london uk') %>%
  add_osm_feature(key = "highway", 
                  value = c("tertiary", "tertiary_link")) %>%
  add_osm_feature(key = 'name') %>%
  osmdata_xml(filename = 'medroads2.osm')

medroads2 <- sf::st_read('medroads2.osm', layer = 'lines') 
save(medroads2, file = "Output/Data/Cleaned/medroads2.Rdata")


s <- opq(bbox = 'greater london uk') %>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street")) %>%
  add_osm_feature(key = 'name') %>%
  osmdata_xml(filename = 'smallroads.osm')

smallroads <- sf::st_read('smallroads.osm', layer = 'lines') 
save(smallroads, file = "Output/Data/Cleaned/smallroads.Rdata")


s2 <- opq(bbox = 'greater london uk') %>%
  add_osm_feature(key = "highway", 
                  value = c("unclassified",
                            "service", "footway")) %>%
  add_osm_feature(key = 'name') %>%
  osmdata_xml(filename = 'smallroads2.osm')

smallroads2 <- sf::st_read('smallroads2.osm', layer = 'lines') 
save(smallroads2, file = "Output/Data/Cleaned/smallroads2.Rdata")


r <- opq(bbox = 'London Borough of Tower Hamlets') %>%
  add_osm_feature(key = "waterway") %>%
  add_osm_feature(key = 'name') %>%
  osmdata_xml(filename = 'river.osm')
  # osmdata_sf()


river <- sf::st_read('river.osm', layer = 'lines', quiet = TRUE) %>% 
  filter(name == "River Thames")
save(river, file = "Output/Data/Cleaned/river.Rdata")


p <- opq(bbox = 'greater london uk') %>%
  add_osm_feature(key = 'building') %>%
  add_osm_feature(key = 'name') %>%
  osmdata_xml(filename = 'buildings.osm')

buildings <- sf::st_read('buildings.osm', layer = 'multipolygons', quiet = TRUE) %>% 
  filter(amenity=='police')
save(buildings, file = "Output/Data/Cleaned/buildings.Rdata")


x <- opq(bbox = 'London Borough of Tower Hamlets') %>%
  add_osm_feature(key = "railway") %>%
  add_osm_feature(key = 'name') %>%
  osmdata_xml(filename = 'railway.osm')

stations <- sf::st_read('railway.osm', layer = 'points', quiet = TRUE) 

# %>% 
  filter(amenity=='police')


ss_th_count <- ss_th %>% 
  st_drop_geometry() %>% 
  group_by(lsoa, ward, lsoa_shape) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(missing_lsoa_shape)  

# %>% 
  ggplot(aes(fill=count)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "magma")


  ss_towerh <-  ggplot() +
  geom_sf(data = river,
          inherit.aes = FALSE,
          color = "#C6CCF0",
          linewidth = 5.5,
          alpha = 0.5) +
  geom_sf(data = ss_th_count,
          aes(fill=count),
          inherit.aes = FALSE,
          color = NA,
          linewidth = 0.1,
          alpha = 0.9) +
          scale_fill_viridis(option = "rocket",
                             direction = -1,
                             begin = 0.2,
                             end = 0.97) +
  # geom_sf(data = w_shape %>% 
  #           filter(LAD22NM == "Tower Hamlets"),
  #         inherit.aes = FALSE,
  #         fill = NA,
  #         color = "grey",
  #         linewidth = 0.5,
  #         alpha = 0.1) +
  geom_sf(data = river,
          inherit.aes = FALSE,
          color = "#C6CCF0",
          linewidth = 5.5,
          alpha = 1) +
  geom_sf(data = bigroads,
          inherit.aes = FALSE,
          fill = NA,
          color = "black",
          linewidth = 0.35,
          alpha = 1) +
  geom_sf(data = bigroads2,
          inherit.aes = FALSE,
          fill = NA,
          color = "black",
          linewidth = 0.35,
          alpha = 1) +
  geom_sf(data = medroads,
          inherit.aes = FALSE,
          fill = NA,
          color = "black",
          linewidth = 0.25,
          alpha = 1) +
  geom_sf(data = medroads2,
          inherit.aes = FALSE,
          fill = NA,
          color = "black",
          linewidth = 0.25,
          alpha = 1) +
  geom_sf(data = smallroads,
          inherit.aes = FALSE,
          fill = NA,
          color = "black",
          linewidth = 0.1,
          alpha = 1) +
  geom_sf(data = smallroads2,
          inherit.aes = FALSE,
          fill = NA,
          color = "black",
          linewidth = 0.1,
          alpha = 1) +
  geom_sf(data = buildings,
          inherit.aes = FALSE,
          fill = NA,
          color = "blue",
          linewidth = 0.85,
          alpha = .6) +
  # geom_sf(data = stations,
  #         inherit.aes = FALSE,
  #         fill = NA,
  #         color = "blue",
  #         linewidth = 0.85,
  #         alpha = .6) +
  # coord_sf(xlim = c(bb[1, 1], bb[1, 2]), 
  #          ylim = c(bb[2, 1], bb[2, 2]),
  #          expand = FALSE) 
  coord_sf(xlim = c(-0.145, 0.075),
           ylim = c(51.475, 51.579),
           expand = FALSE) +
  # coord_sf(xlim = c(-0.085, 0.015), 
  #          ylim = c(51.482, 51.54879),
  #          expand = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill = 'white'))

  ss_towerh
  
  library(viridis)
  
ggsave(ss_towerh, file = "Output/ss_towerh.png")

# robin lovelace - from any coord to any other coord distance from different 
# 
# Myrdle Street