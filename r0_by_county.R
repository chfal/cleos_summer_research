
# TROUBLESHOOTING ---------------------------------------------------------

# still figuring out what the cutoff should be, what MGT i should use, and if this data is reliable because i don't know if it is
# other things: some are just "unassigned counties" which is an issue
# generate distribution and use a quantile to do it because number is so few it is not accurate (still need to do)
# cutoff: upper 95%, lower 5%
# adjust for starting date (waiting on what date the weather data is)
# let date be flexible inside county loop (maybe not possible) 

#packages
library(tidyverse)
library(R0)

#original data
raw_cases = read_csv("time_series_covid19_confirmed_US.csv")
raw_cases$Admin2 <- paste(raw_cases$Admin2, "County")
raw_cases <- raw_cases %>% rename("County" = "Admin2") %>% rename("State"= "Province_State")

county_est <- read_csv("county_pop_est.csv") %>%
  dplyr::select(c(CTYNAME,POPESTIMATE2019, STNAME)) %>%
  rename(County=CTYNAME) %>%
  rename(State=STNAME)

latlong <- raw_cases %>%
  dplyr::select(c(Combined_Key, State, Lat, Long_))


#original data, but with population appended at the end
raw_cases_county <- left_join(raw_cases, county_est)

# the problem is that some of the counties don't have a population estimate for some reason after the left join. This causes a check.incid error later on, causing the loop to stop, because the population size is now NA later on when we try to run the for loop
raw_cases_na_removed <- na.omit(raw_cases_county)

# in order to do this we can either set the population to 1000000 for any NAs in the population OR we can just remove them. we could do it two ways depending on what works better. for now, let's just trim the data set but i'll ask dr. qin on Monday



#selecting only the case counts, population data, and state to make a new matrix
case_counts_edited = raw_cases_na_removed[,c(12:150)]
case_counts_edited <- as.matrix(case_counts_edited)
rownames(case_counts_edited) = raw_cases_na_removed$Combined_Key

# taking the difference of case counts rather than the cumulative case counts for a final matrix
case_counts_final <- apply(case_counts_edited,1, diff) %>%
  t()


# filters any negative values from the difference function that may have been introduced. if there's anything in tb4 that's less than zero, we're just going to set it equal to 0.
case_counts_final[case_counts_final<0]=0

head(case_counts_final)

#GENERATION TIME FUNCTION - why is this the best function?
mGT = generation.time("gamma", c(3, 1.5))

# FOR LOOP ----------------------------------------------------------------

# counters / setup for for-loop

#reset everything in tb4 that's less than 0 to be equal to 0 again because sometimes it gets reset
case_counts_final[case_counts_final<0]=0

fortnight <- seq(0,ncol(case_counts_final), by=14)

i=1
j=1
l=1

result_df <- data.frame(County=character(), Week=character(),R0=double())


# implement case count to be lower
for (i in 1:nrow(case_counts_final)){ # we are going to iterate over all the counties
    for (j in 1:9){ #add cases to be cumulative across the fortnight
    x=case_counts_final[i,(fortnight[j]+1):fortnight[j+1]]
    if (sum(x)<=10) { #cutoff is now less than 10 cases in a fortnight
      R0=NA
    }
    else{
      R0EG = estimate.R(case_counts_final[i,], GT=mGT,  methods=c('EG'), pop.size = tb4[,138], nsim=10, begin=fortnight[j]+1, end=fortnight[j+1])
      R0 = R0EG$estimates$EG$R
    }
    result_df[l,1]=rownames(case_counts_final)[i]
    result_df[l,2]=j
    result_df[l,3]=R0
    l=l+1
  }
}

#merge result df back to longitude/latitude

result_df <- left_join(result_df, latlong, by=c("County"="Combined_Key"))

# cut quantiles
result_df$R0[result_df$R0>quantile(result_df$R0, .95, na.rm=T)] <- NA
result_df$R0[result_df$R0<quantile(result_df$R0, .05, na.rm=T)] <- NA

result_df 

# this is what the result looks like, write it to the working directory
write_csv(result_df, "county_r0_estimates_w_latlong.csv")
