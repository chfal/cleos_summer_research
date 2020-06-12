
# TROUBLESHOOTING ---------------------------------------------------------

# still figuring out how to back merge the dates to the number of weeks
# still figuring out what the cutoff should be, what MGT i should use, and if this data is reliable because i don't know if it is

# SETUP -------------------------------------------------------------------

# generate distribution and use a quantile to do it because number is so few it is not accurate (still need to do)
# upper 75%, upper 50% (still need to do)
# adjust for starting date (not sure how to do - maybe dr. qin said not to do)
# let date be flexible inside county loop (maybe not to do)

library(tidyverse)
library(R0)

#original data
tb = read_csv("time_series_covid19_confirmed_US.csv")
tb$Admin2 <- paste(tb$Admin2, "County")
tb <- tb %>% rename("County" = "Admin2") %>% rename("State"= "Province_State")

county_est <- read_csv("county_est.csv") %>%
  dplyr::select(c(CTYNAME,POPESTIMATE2019, STNAME)) %>%
  rename(County=CTYNAME) %>%
  rename(State=STNAME)

latlong <- tb %>%
  dplyr::select(c(County, State, Lat, Long_)) %>%
  left_join(county_est)


#original data, but with population appended at the end
tb2 <- left_join(tb, county_est)

#selecting only the case counts, population data, and state to make a new matrix
tb3 = tb2[,c(12:150)]
tb3 <- as.matrix(tb3)
rownames(tb3) = tb2$Combined_Key

# taking the difference of case counts rather than the cumulative case counts for a final matrix
tb4 <- apply(tb3,1, diff) %>%
  t()

tb4 <- tb4[6:3261,]

# filters any negative values from the difference function that may have been introduced. if there's anything in tb4 that's less than zero, we're just going to set it equal to 0.
tb4[tb4<0]=0

head(tb4)

#GENERATION TIME FUNCTION - why is this the best function?
mGT = generation.time("gamma", c(3, 1.5))

# FOR LOOP ----------------------------------------------------------------

# counters / setup for for-loop

#reset everything in tb4 that's less than 0 to be equal to 0 again because sometimes it gets reset
tb4[tb4<0]=0

fortnight <- seq(0,ncol(tb4), by=14)

i=1
j=1
l=1

result_df <- data.frame(County=character(), Week=character(),R0=double())


# implement case count to be lower
for (i in 1:nrow(tb4)){ # we are going to iterate over all the counties
    for (j in 1:9){ #add cases to be cumulative across the fortnight
    x=tb4[i,(fortnight[j]+1):fortnight[j+1]]
    if (sum(x)<=25) { #cutoff is now less than 10 cases in a fortnight
      R0=NA
    }
    else{
      R0EG = estimate.R(tb4[i,], GT=mGT,  methods=c('EG'), pop.size = tb4[,138], nsim=10, begin=fortnight[j]+1, end=fortnight[j+1])
      R0 = R0EG$estimates$EG$R
    }
    result_df[l,1]=rownames(tb4)[i]
    result_df[l,2]=j
    result_df[l,3]=R0
    l=l+1
  }
}

#write results
write_csv(result_df, "result_data_frame.csv")

# # GET LATITUTDE/LONGITUDE DATA
# need to rejoin wit original tibble