# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # #graphs for training # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



ss_th %>%
  st_drop_geometry() %>% 
  mutate(ethnicity_officer = ifelse(ethnicity_officer == "Other", NA, ethnicity_officer)) %>% 
  filter(!is.na(ethnicity_officer)) %>% 
  mutate(year = substr(short_date, start = 1, stop = 4),
         count = 1) %>% 
  # group_by(ethnicity_officer, year, month) %>% 
  # summarise(count = sum(count)) %>% 
  ggplot() +
  geom_bar(aes(x = interaction(month, year)), position = "dodge") +
  facet_wrap(~ethnicity_officer, ncol = 1) +
  theme_bw() +
  theme(title = element_blank(),
        axis.title = element_blank()) +
  scale_y_continuous(name = "",
                     expand = c(0, 0), 
                     limits = c(0,1350)) 



# the average day? this year?
av_day <- ss_th %>%
  st_drop_geometry() %>% 
  filter(short_date >= "2022-03-31") %>% 
  mutate(count = 1) %>% 
  group_by(short_date, gender, age, ethnicity_officer) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  group_by(gender, age, ethnicity_officer) %>% 
  summarise(count = mean(count)) %>% 
  mutate(age = ifelse(is.na(age), "Unknown", age)) %>% 
  mutate(age = factor(age, levels = c("under 10", "10-17", 
                                      "18-24", "25-34", 
                                      "over 34", "Unknown"))) %>% 
  mutate(ethnicity_officer = ifelse((is.na(ethnicity_officer) | ethnicity_officer == "Other"), 
                                    "Unknown", ethnicity_officer)) %>% 
  mutate(gender = ifelse((is.na(gender) | gender == "Other"), 
                                    "Unknown", gender)) %>% 
  ggplot() +
  geom_bar(aes(x = age, fill = gender), position = "stack") +
  # facet_wrap(~ethnicity_officer, ncol = 1) +
  theme_bw()

av_day 


race <- ss_th %>%
  st_drop_geometry() %>%
  mutate(ethnicity_officer = ifelse(ethnicity_officer == "Other", NA, ethnicity_officer)) %>% 
  # filter(!is.na(ethnicity_officer)) %>% 
  mutate(count = 1) %>%
  group_by(ethnicity_officer) %>% 
  summarise(count = sum(count))

race <- race %>% 
  ungroup() %>% 
  mutate(pc = count / sum(count))


months <- ss_th %>%
  st_drop_geometry() %>% 
  mutate(year = substr(short_date, start = 1, stop = 4)) %>% 
  distinct(year, month)


av_day <- ss_th %>%
  st_drop_geometry() %>% 
  mutate(count = 1) %>% 
  group_by(short_date, outcome) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  group_by(outcome) %>% 
  summarise(count = mean(count)) %>% 
  ungroup() %>% 
  mutate(pc = count/sum(count))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # #making map of the LSOAs and wards# # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

th_map_lsoa <- ss_th %>% 
  st_drop_geometry() %>% 
  group_by(lsoa, lsoa_shape) %>% 
  mutate(count = 0) %>% 
  summarise(count = sum(count)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(missing_lsoa_shape)

  
th_map_ward <- ss_th %>% 
  st_drop_geometry() %>% 
  group_by(ward) %>% 
  mutate(count = 0) %>% 
  summarise(count = sum(count)) %>% 
  left_join(w_shape %>% 
              filter(LAD22NM == "Tower Hamlets") %>% 
              select(WD22NM, geometry) %>% 
              rename(ward = WD22NM)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326)


check <- w_shape %>% 
  filter(LAD22NM == "Tower Hamlets") %>% 
  st_transform(crs = 4326) %>% 
  ggplot() + 
  geom_sf(fill = NA, color = "black", linewidth = 0.5) +
  geom_sf_text(aes(label = str_wrap(WD22NM, 2)), colour = "black", size = 2.5) +
  theme_bw()
  check
  

  



th_map_ward <- ss_london %>% 
  st_drop_geometry() %>% 
  filter(LA %in% c("Tower Hamlets", "Hackney", "City of London"),
         !(LA == "Hackney" & !(ward %in% c('Hoxton East & Shoreditch',
                                           'Haggerston',
                                           'London Fields',
                                           'Victoria',
                                           'Hackney Wick')))) %>% 
  group_by(ward) %>% 
  mutate(count = 0) %>% 
  summarise(count = sum(count)) %>% 
  left_join(w_shape %>% 
              filter(LAD22NM %in% c("Tower Hamlets", "Hackney", "City of London")) %>% 
              select(WD22NM, geometry) %>% 
              rename(ward = WD22NM)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326)


th_map <- ggplot() + 
  geom_sf(data = th_map_ward, fill = "grey", color = "grey") +
  geom_sf(data = th_map_lsoa, fill = NA, colour="white", linewidth = 0.2) +
  geom_sf(data = th_map_ward, fill = NA, color = "black", linewidth = 0.5) +
  # geom_text(data =ward_coords, aes(x = X, y = Y, label = ward)) +
  geom_sf_text(data = th_map_ward, aes(label = str_wrap(ward, 2)), colour = "black", size = 2.5) +
  theme_bw()



th_map 


check <- ss_london %>% 
  filter(is.na(ward))

check <- lsoa2LA %>% 
  filter(WD21NM == "Aldgate")

# ward_centroids <- st_centroid(th_map_ward)
# ward_coords <- as.data.frame(st_coordinates(ward_centroids))
# ward_coords$ward <- ward_centroids$ward

  
london_map <- road_lines %>% 
  ggplot() + 
  geom_sf()

london_map

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # #Looking at relative frequency of stop and search # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


ss_map <- ss_th %>% 
  st_drop_geometry() %>% 
  group_by(lsoa, ward, lsoa_shape) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(missing_lsoa_shape)  %>% 
  ggplot(aes(fill=count)) +
    geom_sf(colour="white") +
    scale_fill_viridis(option = "magma")

ss_map


ss_map <- ss_th %>% 
  st_drop_geometry() %>% 
  group_by(lsoa, ward, lsoa_shape) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  filter(lsoa != "Tower Hamlets 021D") %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(missing_lsoa_shape)  %>% 
  ggplot(aes(fill=count)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "magma")

ss_map



# number of days in last year (excl november) that there was a s and s
ss_day_map <- ss_th %>% 
  st_drop_geometry() %>% 
  distinct(lsoa, lsoa_shape, short_date) %>% 
  mutate(count = 1) %>% 
  group_by(lsoa, lsoa_shape) %>% 
  summarise(count = sum(count)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(missing_lsoa_shape) %>% 
  ggplot(aes(fill=count)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "magma")

ss_day_map


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # #Looking at use of stop and search powers # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

ss_sec60day_map <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(powers == "Criminal Justice and Public Order Act 1994 (section 60)") %>% 
  distinct(lsoa, lsoa_shape, short_date) %>% 
  mutate(count = 1) %>% 
  group_by(lsoa, lsoa_shape) %>% 
  summarise(count = sum(count)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(missing_sec60_lsoa_shape) %>% 
  ggplot(aes(fill=count)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "magma")

ss_sec60day_map

ss_PACEday_map <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(powers == "Police and Criminal Evidence Act 1984 (section 1)") %>% 
  distinct(lsoa, lsoa_shape, short_date) %>% 
  mutate(count = 1) %>% 
  group_by(lsoa, lsoa_shape) %>% 
  summarise(count = sum(count)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(missing_PACE_lsoa_shape) %>% 
  ggplot(aes(fill=count)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "magma")

ss_PACEday_map

ss_sec23day_map <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(powers == "Misuse of Drugs Act 1971 (section 23)") %>% 
  distinct(lsoa, lsoa_shape, short_date) %>% 
  mutate(count = 1) %>% 
  group_by(lsoa, lsoa_shape) %>% 
  summarise(count = sum(count)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(missing_sec23_lsoa_shape) %>% 
  ggplot(aes(fill=count)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "magma")

ss_sec23day_map

ss_sec47day_map <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(powers == "Firearms Act 1968 (section 47)") %>% 
  distinct(lsoa, lsoa_shape, short_date) %>% 
  mutate(count = 1) %>% 
  group_by(lsoa, lsoa_shape) %>% 
  summarise(count = sum(count)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(missing_sec47_lsoa_shape) %>% 
  ggplot(aes(fill=count)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "magma")

ss_sec47day_map







# day hour month? to find any patterns/temporal hotspots
ss_dhm_map <- ss_th %>% 
  st_drop_geometry() %>% 
  mutate(count = 1) %>% 
  group_by(time, day, month, lsoa, lsoa_shape) %>% 
  summarise(count = sum(count)) %>% 
  filter(lsoa == "Tower Hamlets 021C") %>% 
  ggplot() +
  geom_bar(aes(x = time, y = count), stat = "identity", position = "dodge2") +
  facet_wrap(~interaction(day, month))
ss_dhm_map





ss_map_month <- ss_th  %>% 
  group_by(lsoa, month) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  ggplot(aes(fill=count)) +
  geom_sf(colour="white") +
  facet_wrap(~month) +
  scale_fill_viridis(option = "plasma")

ss_map_month

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # #Looking at use of different type of police powers# # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
ss_th_powers_pc <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(powers != "") %>% 
  group_by(lsoa, lsoa_shape, powers) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = powers,
              values_from = count,
              values_fill = 0) %>% 
  mutate(tot = `Police and Criminal Evidence Act 1984 (section 1)` + `Misuse of Drugs Act 1971 (section 23)` +
           `Criminal Justice and Public Order Act 1994 (section 60)` + `Firearms Act 1968 (section 47)`,
         `Police and Criminal Evidence Act 1984 (section 1)` = `Police and Criminal Evidence Act 1984 (section 1)`/tot,
         `Misuse of Drugs Act 1971 (section 23)` = `Misuse of Drugs Act 1971 (section 23)`/tot,
         `Criminal Justice and Public Order Act 1994 (section 60)` = `Criminal Justice and Public Order Act 1994 (section 60)`/tot,
         `Firearms Act 1968 (section 47)` = `Firearms Act 1968 (section 47)`/tot) %>% 
  select(-tot) %>% 
  pivot_longer(-c(lsoa, lsoa_shape),
             names_to = "powers",
             values_to = "pc") %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  ggplot(aes(fill = pc)) +
  geom_sf(colour = "white") +
  scale_fill_viridis(option = "magma") +
  facet_wrap(~powers) +
  geom_sf(data = missing_lsoa_shape, fill = "grey", color = "white")

ss_th_powers_pc

ss_powerspc <- ss_th %>% 
  group_by(lsoa, powers) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "powers",
              values_from = "count",
              values_fill = 0) %>% 
  mutate(tot = `Police and Criminal Evidence Act 1984 (section 1)` + `Misuse of Drugs Act 1971 (section 23)` +
           `Criminal Justice and Public Order Act 1994 (section 60)` + `Firearms Act 1968 (section 47)`,
         `Police and Criminal Evidence Act 1984 (section 1)` = `Police and Criminal Evidence Act 1984 (section 1)`/tot,
         `Misuse of Drugs Act 1971 (section 23)` = `Misuse of Drugs Act 1971 (section 23)`/tot,
         `Criminal Justice and Public Order Act 1994 (section 60)` = `Criminal Justice and Public Order Act 1994 (section 60)`/tot,
         `Firearms Act 1968 (section 47)` = `Firearms Act 1968 (section 47)`/tot) %>% 
  select(-tot) %>% 
  pivot_longer(-c(lsoa, lsoa_shape),
               names_to = "powers",
               values_to = "pc") 


ss_power1 <- ss_powerspc %>% 
  filter(powers == "Police and Criminal Evidence Act 1984 (section 1)") %>% 
  ggplot(aes(fill=pc)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "plasma")

ss_power1


ss_power2 <- ss_powerspc %>% 
  filter(powers == "Misuse of Drugs Act 1971 (section 23)") %>% 
  ggplot(aes(fill=pc)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "plasma")

ss_power2


ss_power3 <- ss_powerspc %>% 
  filter(powers == "Criminal Justice and Public Order Act 1994 (section 60)") %>% 
  ggplot(aes(fill=pc)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "plasma")

ss_power3

ss_power4 <- ss_powerspc %>% 
  filter(powers == "Firearms Act 1968 (section 47)") %>% 
  ggplot(aes(fill=pc)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "plasma")

ss_power4

# Police and Criminal Evidence Act 1984 (section 1)
# Misuse of Drugs Act 1971 (section 23)
# Criminal Justice and Public Order Act 1994 (section 60)
# Firearms Act 1968 (section 47)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # #Looking at use type of searches # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


ss_th_car <- ss_th %>% 
  filter(search_type %in% c("Person and Vehicle search", "Vehicle search")) %>% 
  st_drop_geometry() %>% 
  group_by(lsoa, ward, lsoa_shape) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(add_missing_lsoa(ss_data = ss_th %>% 
                               filter(search_type %in% c("Person and Vehicle search", "Vehicle search")),
                             full_lsoa_list = th_lsoa_list, full_lsoa_shapes = lsoa_shape)) %>% 
  ggplot(aes(fill=count)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "magma")

ss_th_car


ss_th_car_days <- ss_th %>% 
  filter(search_type %in% c("Person and Vehicle search", "Vehicle search")) %>% 
  st_drop_geometry() %>% 
  distinct(lsoa, lsoa_shape, short_date) %>% 
  mutate(count = 1) %>% 
  group_by(lsoa, lsoa_shape) %>% 
  summarise(count = sum(count)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(add_missing_lsoa(ss_data = ss_th %>% 
                               filter(search_type %in% c("Person and Vehicle search", "Vehicle search")),
                             full_lsoa_list = th_lsoa_list, full_lsoa_shapes = lsoa_shape)) %>% 
  ggplot(aes(fill=count)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "magma")

ss_th_car_days


ss_th_person <- ss_th %>% 
  filter(search_type == "Person search") %>% 
  st_drop_geometry() %>% 
  group_by(lsoa, ward, lsoa_shape) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(add_missing_lsoa(ss_data = ss_th %>% 
                               filter(search_type == "Person search"),
                               full_lsoa_list = th_lsoa_list, full_lsoa_shapes = lsoa_shape)) %>% 
  ggplot(aes(fill=count)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "magma")


ss_th_person


ss_th_person_days <- ss_th %>% 
  filter(search_type == "Person search") %>% 
  st_drop_geometry() %>% 
  distinct(lsoa, lsoa_shape, short_date) %>% 
  mutate(count = 1) %>% 
  group_by(lsoa, lsoa_shape) %>% 
  summarise(count = sum(count)) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  bind_rows(add_missing_lsoa(ss_data = ss_th %>% 
                               filter(search_type == "Person search"),
                             full_lsoa_list = th_lsoa_list, full_lsoa_shapes = lsoa_shape)) %>% 
  ggplot(aes(fill=count)) +
  geom_sf(colour="white") +
  scale_fill_viridis(option = "magma")


ss_th_person_days


ss_th_car_pc <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(search_type != "") %>% 
  mutate(search_type = ifelse(search_type == "Person search", "Person", "Car")) %>% 
  group_by(lsoa, lsoa_shape, search_type) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = search_type,
              values_from = count,
              values_fill = 0) %>% 
  mutate(tot = Person + Car,
         Person = Person/tot,
         Car = Car/tot) %>% 
  select(-tot) %>% 
  pivot_longer(-c(lsoa, lsoa_shape),
               names_to = "search_type",
               values_to = "pc") %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  ggplot(aes(fill = pc)) +
  geom_sf(colour = "white") +
  scale_fill_viridis(option = "magma") +
  facet_wrap(~search_type) +
  geom_sf(data = missing_lsoa_shape, fill = "grey", color = "white")

ss_th_car_pc


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # #INVESTIGATING AGE GROUP # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
ss_th_age <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(age != "") %>% 
  group_by(lsoa, ward, lsoa_shape, age) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = age,
              values_from = count,
              values_fill = 0) %>% 
  pivot_longer(c(`10-17`, `18-24`, `25-34`, `over 34`),
               names_to = "age",
               values_to = "count") %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  ggplot(aes(fill = count)) +
  geom_sf(colour = "white") +
  scale_fill_viridis(option = "magma") +
  facet_wrap(~age) +
  geom_sf(data = missing_lsoa_shape, fill = "grey", color = "white")

ss_th_age


ss_th_age_pc <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(age != "") %>% 
  group_by(lsoa, ward, lsoa_shape, age) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = age,
              values_from = count,
              values_fill = 0) %>% 
  mutate(tot = `under 10` + `10-17` + `18-24` + `25-34` + `over 34`,
         `under 10` = `under 10`/tot,
         `10-17` = `10-17`/tot,
         `18-24` = `18-24`/tot,
         `25-34` = `25-34`/tot,
         `over 34` = `over 34`/tot) %>% 
  select(-tot) %>% 
  pivot_longer(c(`under 10`, `10-17`, `18-24`, `25-34`, `over 34`),
               names_to = "age",
               values_to = "pc") %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  ggplot(aes(fill = pc)) +
  geom_sf(colour = "white") +
  scale_fill_viridis(option = "magma") +
  facet_wrap(~age) +
  geom_sf(data = missing_lsoa_shape, fill = "grey", color = "white")

ss_th_age_pc

ss_th_age_missing <- ss_th %>% 
  st_drop_geometry() %>% 
  mutate(age_missing = ifelse(is.na(age), "Missing", "Recorded")) %>% 
  group_by(lsoa, ward, lsoa_shape, age_missing) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = age_missing,
              values_from = count,
              values_fill = 0) %>% 
  mutate(tot = Missing + Recorded,
         Missing = Missing/tot,
         Recorded = Recorded/tot) %>% 
  select(-tot) %>% 
  pivot_longer(c(Missing, Recorded),
               names_to = "age_missing",
               values_to = "pc") %>% 
  filter(age_missing == "Missing") %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  ggplot(aes(fill = pc)) +
  geom_sf(colour = "white") +
  scale_fill_viridis(option = "magma") +
  geom_sf(data = missing_lsoa_shape, fill = "grey", color = "white")
  
ss_th_age_missing
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  # # # # # # #INVESTIGATING missing # # # # # # # # # # # # # # # 
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  
  # look at how missingness of various categories changes over time?

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # #INVESTIGATING reasons # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

check <- ss_th %>% 
  st_drop_geometry() %>% 
  distinct(reason)

# Stolen goods
# Controlled drugs
# Offensive weapons
# Evidence of offences under the Act
# Articles for use in criminal damage
# Anything to threaten or harm anyone
# Firearms
# Fireworks

ss_th_reason_missing <- ss_th %>% 
  st_drop_geometry() %>% 
  mutate(reason_missing = ifelse(is.na(reason), "Missing", "Recorded")) %>% 
  group_by(lsoa, ward, lsoa_shape, reason_missing) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = reason_missing,
              values_from = count,
              values_fill = 0) %>% 
  mutate(tot = Missing + Recorded,
         Missing = Missing/tot,
         Recorded = Recorded/tot) %>% 
  select(-tot) %>% 
  pivot_longer(c(Missing, Recorded),
               names_to = "reason_missing",
               values_to = "pc") %>% 
  filter(reason_missing == "Missing") %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  ggplot(aes(fill = pc)) +
  geom_sf(colour = "white") +
  scale_fill_viridis(option = "magma") +
  geom_sf(data = missing_lsoa_shape, fill = "grey", color = "white")

ss_th_reason_missing
# reason is pretty rarely missing

ss_th_reason_pc_weapon <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(!is.na(reason)) %>% 
  group_by(lsoa, ward, lsoa_shape, reason) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = reason,
              values_from = count,
              values_fill = 0) %>% 
  mutate(tot = `Stolen goods` + `Controlled drugs` + `Offensive weapons` +
           `Evidence of offences under the Act` + `Articles for use in criminal damage` + 
           `Anything to threaten or harm anyone` + `Firearms` + `Fireworks`,
         `Stolen goods` = `Stolen goods`/tot,
         `Controlled drugs` = `Controlled drugs`/tot,
         `Offensive weapons` = `Offensive weapons`/tot,
         `Evidence of offences under the Act` = `Evidence of offences under the Act`/tot,
         `Articles for use in criminal damage` = `Articles for use in criminal damage`/tot,
         `Anything to threaten or harm anyone` = `Anything to threaten or harm anyone`/tot,
         `Firearms` = `Firearms`/tot,
         `Fireworks` = `Fireworks`/tot) %>% 
  select(-tot) %>% 
  pivot_longer(c(`Stolen goods`, `Controlled drugs`, `Offensive weapons`,
                   `Evidence of offences under the Act`, `Articles for use in criminal damage`, 
                   `Anything to threaten or harm anyone`, `Firearms`, `Fireworks`),
               names_to = "reason",
               values_to = "pc") %>% 
  filter(reason %in% c("Anything to threaten or harm anyone", "Firearms",
                       "Offensive weapons")) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  ggplot(aes(fill = pc)) +
  geom_sf(colour = "white") +
  scale_fill_viridis(option = "magma") +
  facet_wrap(~reason) +
  geom_sf(data = missing_lsoa_shape, fill = "grey", color = "white")

ss_th_reason_pc_weapon


ss_th_reason_pc_drug <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(!is.na(reason)) %>% 
  group_by(lsoa, ward, lsoa_shape, reason) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = reason,
              values_from = count,
              values_fill = 0) %>% 
  mutate(tot = `Stolen goods` + `Controlled drugs` + `Offensive weapons` +
           `Evidence of offences under the Act` + `Articles for use in criminal damage` + 
           `Anything to threaten or harm anyone` + `Firearms` + `Fireworks`,
         `Stolen goods` = `Stolen goods`/tot,
         `Controlled drugs` = `Controlled drugs`/tot,
         `Offensive weapons` = `Offensive weapons`/tot,
         `Evidence of offences under the Act` = `Evidence of offences under the Act`/tot,
         `Articles for use in criminal damage` = `Articles for use in criminal damage`/tot,
         `Anything to threaten or harm anyone` = `Anything to threaten or harm anyone`/tot,
         `Firearms` = `Firearms`/tot,
         `Fireworks` = `Fireworks`/tot) %>% 
  select(-tot) %>% 
  pivot_longer(c(`Stolen goods`, `Controlled drugs`, `Offensive weapons`,
                 `Evidence of offences under the Act`, `Articles for use in criminal damage`, 
                 `Anything to threaten or harm anyone`, `Firearms`, `Fireworks`),
               names_to = "reason",
               values_to = "pc") %>% 
  filter(reason %in% c("Controlled drugs")) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  ggplot(aes(fill = pc)) +
  geom_sf(colour = "white") +
  scale_fill_viridis(option = "magma") +
  facet_wrap(~reason) +
  geom_sf(data = missing_lsoa_shape, fill = "grey", color = "white")

ss_th_reason_pc_drug


ss_th_reason_pc_stolen <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(!is.na(reason)) %>% 
  group_by(lsoa, ward, lsoa_shape, reason) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = reason,
              values_from = count,
              values_fill = 0) %>% 
  mutate(tot = `Stolen goods` + `Controlled drugs` + `Offensive weapons` +
           `Evidence of offences under the Act` + `Articles for use in criminal damage` + 
           `Anything to threaten or harm anyone` + `Firearms` + `Fireworks`,
         `Stolen goods` = `Stolen goods`/tot,
         `Controlled drugs` = `Controlled drugs`/tot,
         `Offensive weapons` = `Offensive weapons`/tot,
         `Evidence of offences under the Act` = `Evidence of offences under the Act`/tot,
         `Articles for use in criminal damage` = `Articles for use in criminal damage`/tot,
         `Anything to threaten or harm anyone` = `Anything to threaten or harm anyone`/tot,
         `Firearms` = `Firearms`/tot,
         `Fireworks` = `Fireworks`/tot) %>% 
  select(-tot) %>% 
  pivot_longer(c(`Stolen goods`, `Controlled drugs`, `Offensive weapons`,
                 `Evidence of offences under the Act`, `Articles for use in criminal damage`, 
                 `Anything to threaten or harm anyone`, `Firearms`, `Fireworks`),
               names_to = "reason",
               values_to = "pc") %>% 
  filter(reason %in% c("Stolen goods")) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  ggplot(aes(fill = pc)) +
  geom_sf(colour = "white") +
  scale_fill_viridis(option = "magma") +
  facet_wrap(~reason) +
  geom_sf(data = missing_lsoa_shape, fill = "grey", color = "white")

ss_th_reason_pc_stolen


ss_th_reason_pc_three <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(!is.na(reason)) %>% 
  group_by(lsoa, ward, lsoa_shape, reason) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = reason,
              values_from = count,
              values_fill = 0) %>% 
  mutate(tot = `Stolen goods` + `Controlled drugs` + `Offensive weapons` +
           `Evidence of offences under the Act` + `Articles for use in criminal damage` + 
           `Anything to threaten or harm anyone` + `Firearms` + `Fireworks`,
         `Stolen goods` = `Stolen goods`/tot,
         `Controlled drugs` = `Controlled drugs`/tot,
         `Offensive weapons grouping` = (`Offensive weapons` + `Anything to threaten or harm anyone` + `Firearms`)/tot,
         `Evidence of offences under the Act` = `Evidence of offences under the Act`/tot,
         `Articles for use in criminal damage` = `Articles for use in criminal damage`/tot,
         `Anything to threaten or harm anyone` = `Anything to threaten or harm anyone`/tot,
         `Firearms` = `Firearms`/tot,
         `Fireworks` = `Fireworks`/tot) %>% 
  select(-tot) %>% 
  pivot_longer(c(`Stolen goods`, `Controlled drugs`, `Offensive weapons`,
                 `Evidence of offences under the Act`, `Articles for use in criminal damage`, 
                 `Anything to threaten or harm anyone`, `Firearms`, `Fireworks`, `Offensive weapons grouping`),
               names_to = "reason",
               values_to = "pc") %>% 
  filter(reason %in% c("Stolen goods", "Controlled drugs", "Offensive weapons grouping")) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  ggplot(aes(fill = pc)) +
  geom_sf(colour = "white") +
  scale_fill_viridis(option = "magma") +
  facet_wrap(~reason) +
  geom_sf(data = missing_lsoa_shape, fill = "grey", color = "white")

ss_th_reason_pc_three

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # #INVESTIGATING OUTCOMES # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

check <- ss_th %>% 
  st_drop_geometry() %>% 
  distinct(outcome)
# no missing i think!

# Arrest
# A no further action disposal
# Community resolution
# Summons / charged by post
# Penalty Notice for Disorder
# Caution (simple or conditional)


ss_th_outcome_pc <- ss_th %>% 
  st_drop_geometry() %>% 
  group_by(lsoa, ward, lsoa_shape, outcome) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = outcome,
              values_from = count,
              values_fill = 0) %>% 
  mutate(tot = `Arrest` +
               `A no further action disposal` +
               `Community resolution` +
               `Summons / charged by post` +
               `Penalty Notice for Disorder` +
               `Caution (simple or conditional)`,
         `Arrest` = `Arrest`/tot,
         `A no further action disposal` =  `A no further action disposal`/tot,
         `Community resolution` = `Community resolution`/tot,
         `Summons / charged by post` = `Summons / charged by post`/tot,
         `Penalty Notice for Disorder` = `Penalty Notice for Disorder`/tot,
         `Caution (simple or conditional)` = `Caution (simple or conditional)`/tot) %>% 
  select(-tot) %>% 
  pivot_longer(c(`Arrest`,
                 `A no further action disposal`,
                 `Community resolution`,
                 `Summons / charged by post`,
                 `Penalty Notice for Disorder`,
                 `Caution (simple or conditional)`),
               names_to = "outcome",
               values_to = "pc") %>% 
  # filter(outcome %in% c("Stolen goods")) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  ggplot(aes(fill = pc)) +
  geom_sf(colour = "white") +
  scale_fill_viridis(option = "magma") +
  facet_wrap(~outcome) +
  geom_sf(data = missing_lsoa_shape, fill = "grey", color = "white")

ss_th_outcome_pc
# no further action by far most common

ss_th_outcome_pc_three <- ss_th %>% 
  st_drop_geometry() %>% 
  group_by(lsoa, ward, lsoa_shape, outcome) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = outcome,
              values_from = count,
              values_fill = 0) %>% 
  mutate(tot = `Arrest` +
           `A no further action disposal` +
           `Community resolution` +
           `Summons / charged by post` +
           `Penalty Notice for Disorder` +
           `Caution (simple or conditional)`,
         `Arrest` = `Arrest`/tot,
         `A no further action disposal` =  `A no further action disposal`/tot,
         `Community resolution` = `Community resolution`/tot,
         `Summons / charged by post` = `Summons / charged by post`/tot,
         `Penalty Notice for Disorder` = `Penalty Notice for Disorder`/tot,
         `Caution (simple or conditional)` = `Caution (simple or conditional)`/tot) %>% 
  select(-tot) %>% 
  pivot_longer(c(`Arrest`,
                 `A no further action disposal`,
                 `Community resolution`,
                 `Summons / charged by post`,
                 `Penalty Notice for Disorder`,
                 `Caution (simple or conditional)`),
               names_to = "outcome",
               values_to = "pc") %>% 
  filter(outcome %in% c("Arrest", "Community resolution", "Penalty Notice for Disorder", "Caution (simple or conditional)")) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  ggplot(aes(fill = pc)) +
  geom_sf(colour = "white") +
  scale_fill_viridis(option = "magma") +
  facet_wrap(~outcome) +
  geom_sf(data = missing_lsoa_shape, fill = "grey", color = "white")

ss_th_outcome_pc_three


ss_th_outcome_pc_no_byage <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(!is.na(age)) %>% 
  group_by(lsoa, ward, lsoa_shape, outcome, age) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = outcome,
              values_from = count,
              values_fill = 0) %>% 
  mutate(tot = `Arrest` +
           `A no further action disposal` +
           `Community resolution` +
           `Summons / charged by post` +
           `Penalty Notice for Disorder` +
           `Caution (simple or conditional)`,
         `Arrest` = `Arrest`/tot,
         `A no further action disposal` =  `A no further action disposal`/tot,
         `Community resolution` = `Community resolution`/tot,
         `Summons / charged by post` = `Summons / charged by post`/tot,
         `Penalty Notice for Disorder` = `Penalty Notice for Disorder`/tot,
         `Caution (simple or conditional)` = `Caution (simple or conditional)`/tot) %>% 
  select(-tot) %>% 
  pivot_longer(c(`Arrest`,
                 `A no further action disposal`,
                 `Community resolution`,
                 `Summons / charged by post`,
                 `Penalty Notice for Disorder`,
                 `Caution (simple or conditional)`),
               names_to = "outcome",
               values_to = "pc") %>% 
  filter(outcome %in% c("A no further action disposal")) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  ggplot(aes(fill = pc)) +
  geom_sf(data = th_map_lsoa, fill = "grey", color = "white") +
  geom_sf(colour = "white") +
  scale_fill_viridis(option = "magma") +
  facet_wrap(~age)

ss_th_outcome_pc_no_byage



ss_th_outcome_pc_no_byreason <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(!is.na(age),
         reason %in% c("Controlled drugs", "Stolen goods", "Offensive weapons", "Anything to threaten or harm anyone", "Firearms")) %>% 
  mutate(reason = ifelse(reason %in% c("Offensive weapons", "Anything to threaten or harm anyone", "Firearms"), 
                         "Weapons grouping", reason)) %>% 
  group_by(lsoa, ward, lsoa_shape, outcome, reason) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = outcome,
              values_from = count,
              values_fill = 0) %>% 
  mutate(tot = `Arrest` +
           `A no further action disposal` +
           `Community resolution` +
           `Summons / charged by post` +
           `Penalty Notice for Disorder` +
           `Caution (simple or conditional)`,
         `Arrest` = `Arrest`/tot,
         `A no further action disposal` =  `A no further action disposal`/tot,
         `Community resolution` = `Community resolution`/tot,
         `Summons / charged by post` = `Summons / charged by post`/tot,
         `Penalty Notice for Disorder` = `Penalty Notice for Disorder`/tot,
         `Caution (simple or conditional)` = `Caution (simple or conditional)`/tot) %>% 
  select(-tot) %>% 
  pivot_longer(c(`Arrest`,
                 `A no further action disposal`,
                 `Community resolution`,
                 `Summons / charged by post`,
                 `Penalty Notice for Disorder`,
                 `Caution (simple or conditional)`),
               names_to = "outcome",
               values_to = "pc") %>% 
  filter(outcome %in% c("A no further action disposal")) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  ggplot(aes(fill = pc)) +
  geom_sf(data = th_map_lsoa, fill = "grey", color = "white") +
  geom_sf(colour = "white") +
  scale_fill_viridis(option = "magma") +
  facet_wrap(~reason)

ss_th_outcome_pc_no_byreason



ss_th_outcome_pc_arrest_byreason <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(!is.na(age),
         reason %in% c("Controlled drugs", "Offensive weapons", "Anything to threaten or harm anyone", "Firearms")) %>% # "Stolen goods",
  mutate(reason = ifelse(reason %in% c("Offensive weapons", "Anything to threaten or harm anyone", "Firearms"), 
                         "Weapons grouping", reason)) %>% 
  group_by(lsoa, ward, lsoa_shape, outcome, reason) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = outcome,
              values_from = count,
              values_fill = 0) %>% 
  mutate(tot = `Arrest` +
           `A no further action disposal` +
           `Community resolution` +
           `Summons / charged by post` +
           `Penalty Notice for Disorder` +
           `Caution (simple or conditional)`,
         `Arrest` = `Arrest`/tot,
         `A no further action disposal` =  `A no further action disposal`/tot,
         `Community resolution` = `Community resolution`/tot,
         `Summons / charged by post` = `Summons / charged by post`/tot,
         `Penalty Notice for Disorder` = `Penalty Notice for Disorder`/tot,
         `Caution (simple or conditional)` = `Caution (simple or conditional)`/tot) %>% 
  select(-tot) %>% 
  pivot_longer(c(`Arrest`,
                 `A no further action disposal`,
                 `Community resolution`,
                 `Summons / charged by post`,
                 `Penalty Notice for Disorder`,
                 `Caution (simple or conditional)`),
               names_to = "outcome",
               values_to = "pc") %>% 
  filter(outcome %in% c("Arrest")) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  ggplot(aes(fill = pc)) +
  geom_sf(data = th_map_lsoa, fill = "grey", color = "white") +
  geom_sf(colour = "white") +
  scale_fill_viridis(option = "magma") +
  facet_wrap(~reason)

ss_th_outcome_pc_arrest_byreason


ss_th_outcome_pc_arrest_byage <- ss_th %>% 
  st_drop_geometry() %>% 
  filter(!is.na(age)) %>% 
  group_by(lsoa, ward, lsoa_shape, outcome, age) %>% 
  mutate(count = 1) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = outcome,
              values_from = count,
              values_fill = 0) %>% 
  mutate(tot = `Arrest` +
           `A no further action disposal` +
           `Community resolution` +
           `Summons / charged by post` +
           `Penalty Notice for Disorder` +
           `Caution (simple or conditional)`,
         `Arrest` = `Arrest`/tot,
         `A no further action disposal` =  `A no further action disposal`/tot,
         `Community resolution` = `Community resolution`/tot,
         `Summons / charged by post` = `Summons / charged by post`/tot,
         `Penalty Notice for Disorder` = `Penalty Notice for Disorder`/tot,
         `Caution (simple or conditional)` = `Caution (simple or conditional)`/tot) %>% 
  select(-tot) %>% 
  pivot_longer(c(`Arrest`,
                 `A no further action disposal`,
                 `Community resolution`,
                 `Summons / charged by post`,
                 `Penalty Notice for Disorder`,
                 `Caution (simple or conditional)`),
               names_to = "outcome",
               values_to = "pc") %>% 
  filter(outcome %in% c("Arrest")) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326) %>% 
  ggplot(aes(fill = pc)) +
  geom_sf(data = th_map_lsoa, fill = "grey", color = "white") +
  geom_sf(colour = "white") +
  scale_fill_viridis(option = "magma") +
  facet_wrap(~age)

ss_th_outcome_pc_arrest_byage
