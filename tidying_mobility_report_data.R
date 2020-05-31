
# PACKAGES ----------------------------------------------------------------
setwd("C:/Users/chfal/Desktop/ICOMPBIO/CLEOS_RESEARCH_PJECT/")
require(tidyverse)
require(ggmap)
require(gganimate)
require(urbnmapr)
require(sf)
#devtools::install_github("UrbanInstitute/urbanmapr")


# CLEANING DATA  -----------------------------------------------------


#apple data cleaned and prepared for full join by renaming columns
apple_mobility <- read_csv("applemobilitytrends-2020-05-23.csv") %>%
  filter(country=="United States") %>%
  filter(geo_type=="county") %>%
  pivot_longer(8:138,names_to = "date", values_to = "difference_from_baseline_apple") %>%
  rename("baseline_apple" = "1/13/2020") %>%
  rename("county"="region") %>%
  rename("state"="sub-region") %>%
  rename("transportation_type_apple"="transportation_type") %>%
  select(-c("geo_type","alternative_name"))

#google data cleaned and prepared for full join by renaming columns. col types argument is necessary because readr thinks default is logical because the first 1000 rows are full of NA's but we actually know it's not logical - it's characters!
google_mobility <- read_csv("Global_Mobility_Report.csv", col_types = cols("sub_region_2" = col_character())) %>%
  filter(country_region=="United States") %>%
  filter(!is.na(sub_region_2)) %>%
  rename("county"="sub_region_2") %>%
  rename("country"="country_region") %>%
  rename("state"="sub_region_1")


#check to make sure it looks okay, then join together google and apple data
head(google_mobility)
head(apple_mobility)
total_mobility <- inner_join(google_mobility, apple_mobility, keep=FALSE)
total_mobility <- total_mobility[,2:14]
total_mobility %>%
  mutate(date=as.Date(total_mobility$date, "%m/%d/%Y"))

#write_csv(total_mobility,"total_mobility.csv")


# you can also load to here by running the below code
total_mobility <- read_csv("total_mobility.csv")
total_mobility %>%
  mutate(date=as.Date(total_mobility$date, "%m/%d/%Y")) # for some reason when you reload total_mobility sometimes it is not as a date and it must be as a date for this to work


# get shape files 
counties_sf <- get_urbn_map("counties",sf=TRUE)  %>%
  rename("county"="county_name") %>%
  rename("state"="state_name")
counties_sf

counties_total <- left_join(total_mobility, counties_sf, by=c("county", "state"))



# GRAPH OF THE WHOLE US ON A CERTAIN DAY ----------------------------------

day_x <- counties_total %>%
  subset(date=="4/15/2020")
ggplot() +
  geom_sf(data=day_x$geometry, mapping=aes(fill=day_x$workplaces_percent_change_from_baseline)) +
  labs(fill="% change workplace", title="US % change from baseline to the workplace on 4/15/2020") +
  ggsave("graph_1.png")

# we should think about how to subset becausee this graph is going to be busy as hell

# now try to animate it?

# counties_total <- st_as_sf(counties_total)
# 
# 
# covid_map <- ggplot() +
#   geom_sf(data=counties_sf, fill="white") +
#   geom_sf(counties_total, mapping=aes(fill="grocery_and_pharmacy_percent_change_from_baseline")) +
#   transition_manual(date)
# 
# 
# animate(covid_map, nframe=15, fps=2, end_pause=15)
# 

#let's just try massachusetts b/c i think my computer will explode



# MASSACHUSETTS ONLY ------------------------------------------------------


ma_counties <- counties_total %>%
  subset(counties_total$state=="Massachusetts") %>%
  mutate(date=as.Date(date, "%m/%d/%Y")) %>%
  st_as_sf()

ma_counties2 <- counties_sf %>%
  subset(state=="Massachusetts")

covid_map_ma <- ggplot() +
  geom_sf(data=ma_counties2, fill="white") +
  geom_sf(data=ma_counties, aes(fill=ma_counties$difference_from_baseline_apple)) +
  labs(fill="diff from baseline", subtitle="date: {current_frame}") +
  transition_manual(date)
  
animate(covid_map_ma, duration=60)

#anim_save("ma_covid_1.gif", animation=last_animation())

covid_map_ma2 <- ggplot() +
  geom_sf(data=ma_counties2, fill="white") +
  geom_sf(data=ma_counties, aes(fill=ma_counties$grocery_and_pharmacy_percent_change_from_baseline)) +
  labs(fill="% change grocery/pharmacy", subtitle="date: {current_frame}") +
  transition_manual(date)

animate(covid_map_ma2, duration=30)

#anim_save("ma_covid_2.gif", animation=last_animation())
