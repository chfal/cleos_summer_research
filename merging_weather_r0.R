library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(chron)

#read r0 data
r0 <- read_csv("county_r0_estimates_w_latlong.csv") %>%
  mutate(dates=as.Date(r0$dates, "%m/%d/%y"))

#read timepoints data. if this doesn't run, run each line of it by itself - had some issues referring to date in 3rd column
timepoints_39 <- read_csv("aggregated_data_39_timepoints.csv") %>%
  pivot_longer(c(13:51), names_to="date", values_to="temp_in_K") %>%
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


timepoints_39_total <- timepoints_39 %>% select("Combined_Key", "date", "temp_in_K","start_date")

timepoints_39_final <- left_join(timepoints_39_total, r0, by=c("Combined_Key"="County", "start_date"="dates"))
write_csv(timepoints_39_final, "timepoints_39_final_with_r0_not_grouped.csv")


timepoints_39_final2 <- timepoints_39_total %>%
  group_by(start_date, Combined_Key) %>%
  summarize_if(is.numeric, mean, na.rm=T)

timepoints_39_final2 <- left_join(timepoints_39_final2, r0, by=c("Combined_Key"="County", "start_date"="dates"))
write_csv(timepoints_39_final2, "timepoints_39_final_with_r0.csv")


# now work with long data
timepoints_156 <- read_csv("aggregated_data_156_timepoints.csv") %>%
  pivot_longer(c(13:168),names_to="date",values_to="temp_in_K") %>%
  mutate(date=ymd(timepoints_156$date))

timepoints_156 <- timepoints_156 %>%
  filter(date>="2020-02-05")

timepoints_156$start_date =cut(timepoints_156$date, breaks="14 days")

timepoints_156 <- mutate(timepoints_156, start_date=as.Date(timepoints_156$start_date, "%Y-%m-%d"))

timepoints_156_total <- timepoints_156 %>% select("Combined_Key", "date", "temp_in_K","start_date")

#two versions; one is summarized by the week
timepoints_156_final <- left_join(timepoints_156_total, r0, by=c("Combined_Key"="County", "start_date"="dates"))

write_csv(timepoints_156_final, "timepoints_156_final_with_r0_not_grouped.csv")

timepoints_156_final2 <- timepoints_156_total %>%
  group_by(start_date, Combined_Key) %>%
  summarize_if(is.numeric, mean, na.rm=T)

timepoints_156_final2 <- left_join(timepoints_156_final2, r0, by=c("Combined_Key"="County", "start_date"="dates"))
write_csv(timepoints_156_final2, "timepoints_156_final_with_r0.csv")
