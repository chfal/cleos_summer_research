library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(chron)

#read r0 data
r0 <- read_csv("county_r0_estimates_w_latlong.csv")

r0 <-mutate(r0, dates=as.Date(r0$dates, "%m/%d/%y"))

#read timepoints data. if this doesn't run, run each line of it by itself - had some issues referring to date in 3rd column
timepoints_39 <- read_csv("aggregated_data_39_timepoints.csv")

timepoints_39 <- timepoints_39 %>%
  pivot_longer(c(13:51), names_to="date", values_to="temp_in_K")

timepoints_39 <- timepoints_39 %>%
  mutate(date=ymd(timepoints_39$date))

#filter timepoints data
timepoints_39 <- timepoints_39 %>%
  filter(date>="2020-02-05")

#cut by 14 days
timepoints_39$start_date=cut(timepoints_39$date, breaks="14 days")

#make this a date
timepoints_39 <- mutate(timepoints_39, start_date=as.Date(timepoints_39$start_date, "%Y-%m-%d"))

# had to subtract one day from start date to align with sliding window
timepoints_39$start_date = as.Date(timepoints_39$start_date) - 1


timepoints_39 

timepoints_39_total <- timepoints_39 %>% dplyr::select("Combined_Key", "date", "temp_in_K","start_date", "Lat", "Long_", "Admin2", "Province_State")

timepoints_39_final <- left_join(timepoints_39_total, r0, by=c("Combined_Key"="County", "start_date"="dates", "Lat"="Lat", "Long_"="Long_", "Province_State" ="State")) %>%
  group_by(start_date, Combined_Key, Province_State, Admin2) %>%
  summarize_if(is.numeric, mean, na.rm=T)

write_csv(timepoints_39_final, "timepoints_39_final_with_r0.csv")


# now work with long data
timepoints_156 <- read_csv("aggregated_data_156_timepoints.csv")

timepoints_156 <- timepoints_156 %>%
  pivot_longer(c(13:168),names_to="date",values_to="temp_in_K")

timepoints_156 <- timepoints_156 %>%
  mutate(date=ymd(timepoints_156$date))

timepoints_156 <- timepoints_156 %>%
  filter(date>="2020-02-05")

timepoints_156$start_date =cut(timepoints_156$date, breaks="14 days")

timepoints_156 <- mutate(timepoints_156, start_date=as.Date(timepoints_156$start_date, "%Y-%m-%d"))

timepoints_156_total <- timepoints_156 %>% dplyr::select("Combined_Key", "date", "temp_in_K","start_date", "Lat", "Long_", "Admin2", "Province_State")

timepoints_156_final <- left_join(timepoints_156_total, r0, by=c("Combined_Key"="County", "start_date"="dates", "Lat"="Lat", "Long_"="Long_", "Province_State" ="State")) %>%
  group_by(start_date, Combined_Key, Province_State, Admin2) %>%
  summarize_if(is.numeric, mean, na.rm=T)

timepoints_156_final$county = paste(timepoints_156_final$Admin2, "County", sep=" ")

write_csv(timepoints_156_final, "timepoints_156_final_with_r0.csv")


# read in apple data by county
apple_mobility_counties <- read_csv("applemobilitytrends-2020-06-23.csv") %>%
  filter(geo_type=="county") %>%
  pivot_longer(7:169, names_to="date", values_to="mobility_score") %>%
  mutate(date=as.Date(date)) %>%
  filter(date>="2020-02-05")

apple_mobility_counties$start_date = cut(apple_mobility_counties$date, "14 days")

apple_mobility_counties$Combined_Key = paste(apple_mobility_counties$region)

#read in google data by county

google_mobility <- read_csv("Global_Mobility_Report.csv", col_types = cols("sub_region_2" = col_character())) %>%
  filter(country_region=="United States") %>%
  filter(!is.na(sub_region_2)) %>%
  rename("county"="sub_region_2") %>%
  rename("country"="country_region") %>%
  rename("state"="sub_region_1") %>%
  filter(date>="2020-02-19")

google_mobility$start_date = cut(google_mobility$date, "14 days")

google_mobility_final <- google_mobility %>%
  mutate(start_date=as.Date(start_date)) %>%
  group_by(state, county, country, start_date) %>%
  summarize_if(is.numeric, mean, na.rm=T)


ready_for_correlation <- left_join(timepoints_156_final, google_mobility_final, by=c("start_date"="start_date","county"="county", "Province_State"="state"))


write_csv(ready_for_correlation, "r0_county_temperature_google_mobility.csv")
