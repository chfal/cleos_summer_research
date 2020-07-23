#quick and dirty analyses
library(covid19mobility) #for getting mobility data
library(tidyverse) # for tidying everything!
library(urbnmapr) # for getting shapefiles of states and counties to map
library(sf) # for working with shapefiles
library(lubridate) # for working with dates
library(R0) #for calculating R0
library(chron) #for working with dates
library(gganimate) # for animating / saving plots
library(R0) #for calculating R0

# Get data i hate this package
# google_mobility <- covid19mobility::refresh_covid19mobility_google_us_counties()
# write_csv(google_mobility, "google_mobility.csv")

google_mobility <- read_csv("google_mobility.csv")
#clean google data
google_mobility <- google_mobility %>%
  group_by(state, date, data_type) %>%
  summarize_if(is.numeric, mean, na.rm=T) %>%
  pivot_wider(names_from=data_type) %>%
  filter(date>="2020-02-15")

#Get apple data from the covid19mobility package.
# apple_mobility <- covid19mobility::refresh_covid19mobility_apple_subregion()
# write_csv(apple_mobility, "apple_mobility.csv")

apple_mobility <- read_csv("apple_mobility.csv")
#Clean apple data

apple_mobility <- apple_mobility %>%
  filter(country=="United States") %>%
  dplyr::select(c(1,2,6,7)) %>%
  rename("state"="location") %>%
  filter(date>="2020-02-15")

#Get data by every week, starting from the 15th of February. We do this by adding a "start date" column.

google_mobility$start_date = cut(google_mobility$date, breaks="7 days")
apple_mobility$start_date = cut(apple_mobility$date, breaks="7 days")

google_mobility <- group_by(google_mobility, state, start_date) %>% summarize_if(is.numeric, mean, na.rm=T)

apple_mobility <- group_by(apple_mobility, state, start_date, data_type) %>% summarize_if(is.numeric, mean, na.rm=T)

#original data from Johns Hopkins
original_data <- read_csv("time_series_covid19_confirmed_US.csv")

#population data from 2019 US government database
pop_est <- read_csv("pop_est.csv") %>%
  dplyr::select(c(NAME,POPESTIMATE2019)) %>%
  rename(Province_State=NAME)

#original data, but with population appended at the end
cases_with_pop <- left_join(original_data, pop_est) %>%
  group_by(Province_State) %>%
  summarize_if(is.numeric, sum, na.rm=TRUE)

#selecting only the case counts, population data, and state to make a new matrix
case_counts_raw = cases_with_pop[,30:188]
case_counts_raw <- as.matrix(case_counts_raw)
rownames(case_counts_raw) = cases_with_pop$Province_State

# taking the difference of case counts rather than the cumulative case counts for a final matrix
case_counts <- apply(case_counts_raw,1, diff) %>%
  t()

# filters any negative values from the difference function that may have been introduced. if there's anything in case_counts that's less than zero, we're just going to set it equal to 0.
case_counts[case_counts<0]=0


#GENERATION TIME FUNCTION
mGT = generation.time("gamma", c(3, 1.5))

# FOR LOOP ----------------------------------------------------------------

# counters / setup for for-loop

#reset everything in case_counts that's less than 0 to be equal to 0 again because sometimes it gets reset

case_counts[case_counts<0]=0

# removed all cases where you didn't know what the population was
case_counts <- case_counts[case_counts[,158] !=0,]

weekly <- seq(0,ncol(case_counts), by=7)

i=1
j=1
k=1
l=1

r0 <- data.frame(State=character(), Week=character(),R0=double())

# implement case count to be lower
for (i in 1:nrow(case_counts)){ # we are going to iterate over all the states
  for (j in 1:21){ #add cases to be cumulative across the week
    x=case_counts[i,(weekly[j]+1):weekly[j+1]]
    if (sum(x)<=10) { #cutoff is now less than 10 cases in a week
      R0=NA
    }
    else{
      R0EG = estimate.R(case_counts[i,], GT=mGT,  methods=c('EG'), pop.size = case_counts[,158], nsim=10, begin=weekly[j]+1, end=weekly[j+1])
      R0 = R0EG$estimates$EG$R
    }
    r0[l,1]=rownames(case_counts)[i]
    r0[l,2]=j
    r0[l,3]=R0
    l=l+1
  }
}

r0 <- r0 %>%
  tibble()

#cut the result to quantiles - remove anything in the 95th percentile and the 5th percentile
# r0$R0[r0$R0>quantile(r0$R0, .95, na.rm=T)] <- NA
# r0$R0[r0$R0<quantile(r0$R0, .05, na.rm=T)] <- NA

#add labels to each week's start date
Week <- as.character(c(1:22))
dates <- colnames(case_counts[,c(1,8,15,22,29,36,43,50,57,64,71,78,85,92,99,106,113,120,127,134, 141,148)])

positions <- data.frame(Week, dates)

#write csv to save it
r0 <- left_join(r0, positions)
write_csv(r0, "r0_result_data_frame.csv")

r0 <- read_csv("r0_result_data_frame.csv") %>%
  mutate(date=as.Date(dates, "%m/%d/%y")) %>%
  dplyr::select(c("State","R0","date"))

final_mobility_apple <-apple_mobility %>%
  mutate(start_date=as.Date(start_date)) %>%
  dplyr::select(c("start_date","data_type","value"))

final_mobility_google <-google_mobility %>%
  mutate(start_date=as.Date(start_date))

corr_apple <- left_join(final_mobility_apple, r0, by =c("start_date"="date", "state"="State"))

corr_google <- right_join(final_mobility_google,r0, by=c("start_date"="date","state"="State"))

weather <- read_csv("aggregated_data_156_timepoints.csv")

weather <- weather %>%
  pivot_longer(c(13:168),names_to="date",values_to="temp_in_K")

weather <- weather %>%
  mutate(date=ymd(weather$date))

weather <- weather %>%
  filter(date>="2020-02-15")

weather$start_date =cut(weather$date, breaks="7 days")

weather <- weather %>%
  group_by(Province_State, start_date) %>%
  summarize_if(is.numeric, mean, na.rm=T)

weather$start_date <- as.Date(weather$start_date, "%Y-%m-%d")

weather_total <- weather %>% dplyr::select("Province_State", "start_date", "temp_in_K") %>%
  mutate(temp_in_c=temp_in_K - 273.15)

#read in mask data
masks <- read_csv("maskdata.csv") %>%
  dplyr::select(state, date) %>%
  mutate(mask_date=as.Date(date, "%m/%d/%y")) %>%
  dplyr::select(state, mask_date)

#join to correlation
corr_google2 <- full_join(corr_google, masks, by=c("state"="state"))
corr_apple2 <- full_join(corr_apple, masks, by=c("state"="state"))

#add logical vector masks_worn which asks whether masks were worn or not by state mandate
corr_google2$masks_worn <-(corr_google2$start_date > corr_google2$mask_date)
corr_apple2$masks_worn <- (corr_apple2$start_date > corr_apple2$mask_date)

#if all NA that means masks were never worn, so set it to false
corr_google2[,11][is.na(corr_google2[,11])] <- FALSE
corr_apple2[,6][is.na(corr_apple2[,6])] <- FALSE

final_google_corr <- left_join(corr_google2, weather_total, by=c("state"="Province_State", "start_date"="start_date"))
write_csv(final_google_corr, "final_google_corr.csv")

final_apple_corr <- left_join(corr_apple2, weather_total, by=c("state"="Province_State", "start_date"="start_date"))
write_csv(final_apple_corr, "final_apple_corr.csv")

#calculate percent change in a for loop
percent_increase <- data.frame(State=character(), Week=character(),sum=double())
i=1
j=1
k=1
l=1

for (i in 1:nrow(case_counts)){ # we are going to iterate over all the states
  for (j in 1:21){ #add cases to be cumulative across the week
    Weekly_Sum=sum(case_counts[i,(weekly[j]+1):weekly[j+1]])
    percent_increase[l,1]=rownames(case_counts)[i]
    percent_increase[l,2]=j
    percent_increase[l,3]=Weekly_Sum
    l=l+1
  }
}

Week <- as.character(c(1:22))
dates <- colnames(case_counts[,c(1,8,15,22,29,36,43,50,57,64,71,78,85,92,99,106,113,120,127,134, 141, 148)])

positions <- data.frame(Week, dates)

percent_increase <- left_join(percent_increase, positions) %>%
  rename(weekly_increase = sum)

percent_increase <-
  percent_increase %>%
  mutate(weekly_increase = as.numeric(weekly_increase)) %>%
  mutate(dates=as.Date(dates, "%m/%d/%y"))

percent_increase <- percent_increase %>%
  group_by(State) %>%
  mutate(pct_change = ((weekly_increase/lag(weekly_increase)-1)*100))

#apple and google join together into total data
final_google_corr2 <- left_join(final_google_corr, percent_increase, by=c("state"="State", "start_date"="dates"))
write_csv(final_google_corr, "final_google_corr.csv")

final_apple_corr2 <- left_join(final_apple_corr, percent_increase, by=c("state"="State", "start_date"="dates"))
write_csv(final_apple_corr2, "final_apple_corr.csv")

total_data <- left_join(final_apple_corr2, final_google_corr2)

#all the google mobility that should decrease (i.e. everything but housing)
total_data$expected_decrease <- base::rowMeans(total_data[,c(16,14,13,12,11)], na.rm=T)

#lag data
total_data <- total_data %>%
  group_by(state) %>%
  mutate(lagged_R0 = lag(R0)) %>%
  mutate(lagged_R0 = lag(lagged_R0))

#cut percent change to quantiles
total_data <- total_data %>%
  mutate_if(is.numeric, list(~na_if(., Inf)))

write_csv(total_data, "total_joined_data.csv")