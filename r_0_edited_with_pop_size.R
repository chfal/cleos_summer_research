# SETUP -------------------------------------------------------------------

library(tidyverse)
library(R0)

#original data
tb = read_csv("time_series_covid19_confirmed_US.csv")

pop_est <- read_csv("pop_est.csv") %>%
  dplyr::select(c(NAME,POPESTIMATE2019)) %>%
  rename(Province_State=NAME)

#original data, but with population appended at the end
tb2 <- left_join(tb, pop_est) %>%
  group_by(Province_State) %>%
  summarize_if(is.numeric, sum, na.rm=TRUE)

#selecting only the case counts, population data, and state to make a new matrix
tb3 = tb2[,7:145]
tb3 <- as.matrix(tb3)
rownames(tb3) = tb2$Province_State

# taking the difference of case counts rather than the cumulative case counts for a final matrix
tb4 <- apply(tb3,1, diff) %>%
  t()

# filters any negative values from the difference function that may have been introduced. if there's anything in tb4 that's less than zero, we're just going to set it equal to 0.
tb4[tb4<0]=0

head(tb4)

#GENERATION TIME FUNCTION - why is this the best function?
mGT = generation.time("gamma", c(3, 1.5))

# SPECIFIC STATES ---------------------------------------------------------
new_york <- tb4["New York",]
R0EG = estimate.R(new_york, GT=mGT,  methods=c('EG'), pop.size = 1244690849, nsim=10, begin=40, end=54)
R0EG$estimates$EG$R


arkansas <- tb4["Arkansas",]
R0EG = estimate.R(arkansas, GT=mGT,  methods=c('EG'), pop.size = 232366865, nsim=10, begin=40, end=54)
R0EG$estimates$EG$R

# this code doesn't work because both start/end is 0, and 0 throughout
# west_virginia <- tb4["West Virginia",]
# R0EG = estimate.R(west_virginia, GT=mGT,  methods=c('EG'), pop.size = 232366865, nsim=10, begin=40, end=54)


# FOR LOOP ----------------------------------------------------------------

# counters / setup for for-loop

#reset everything in tb4 that's less than 0 to be equal to 0 again because sometimes it gets reset
tb4[tb4<0]=0

fortnight <- seq(0,ncol(tb4), by=14)

i=1
j=1
k=1
l=1

result_df <- data.frame(State=character(), Week=character(),R0=double())


# implement case count to be lower
for (i in 1:nrow(tb4)){ # we are going to iterate over all the states
  for (j in 1:9){ #add cases to be cumulative across the fortnight
    x=tb4[i,(fortnight[j]+1):fortnight[j+1]]
    if (sum(x)<=25) { #cutoff is now less than 10 cases in a fortnight
      R0=0
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

tb4[,c(0,14,28,42,56,70,84,98)]

colnames(tb4[,c(0,14,28,42,56,70,84,98)])



plot(result_df$Week, result_df$R0)
# result_3 <- c("1"="2/5/20","2"="2/19/20","3"="3/4/20","4"="3/18/20","5"="4/1/20","6"="4/15/20","7"="4/29/20")


# TROUBLESHOOTING ---------------------------------------------------------

# WHERE DO THE HIGHEST VALUES COME FROM?
# DIAMOND PRINCESS, WEEK 3 - BECAUSE ALL THE INFECTIONS OCCURED IN A TWO-WEEK PERIOD AND THE POPULATION WAS SET TO 0 BY THE CODE
# what happens when we run the simulation again?
# could be because of the testing issues - there were no tests and they had to airdrop the tests to the princess and they administered them all at once, and got the results back on the same day

diamond_princess <- tb4["Diamond Princess",]
R0EG = estimate.R(epid = diamond_princess,GT=mGT, method="EG", begin=0, end=118, pop.size=3,711, nsim=10)
R0EG$estimates$EG$R

# influenza data
# try influenza data, try mobility data

# GERMANY 1918 ------------------------------------------------------------


# compare to germany.1918
data("Germany.1918")
estimate.R(Germany.1918, method = "EG", GT=mGT)
estimate.R(Germany.1918, method = "EG", GT=mGT)
fortnight2 <- seq(0,length(Germany.1918), by=14)
i=1
j=1
germany_r0 <- data.frame(R0=double())
for (j in 1:8){
  x=Germany.1918[(fortnight2[j]+1):fortnight2[j+1]]
  R0EG = estimate.R(Germany.1918, GT=mGT,  methods=c('EG'), nsim=10, begin=fortnight[j]+1, end=fortnight[j+1])
    R0 = R0EG$estimates$EG$R
    print(R0)
}

for (j in 1:8){
  x=Germany.1918[(fortnight2[j]+1):fortnight2[j+1]]
  R0EG = estimate.R(Germany.1918, GT=mGT,  methods=c("ML"), nsim=10, begin=fortnight[j]+1, end=fortnight[j+1])
  R0 = R0EG$estimates$ML$R
  print(R0)
  R0_ML <- as.vector(R0)
}



# still figuring out how to back merge the dates to the number of weeks
# still figuring out what the cutoff should be, what MGT i should use, and if this data is reliable because i don't know if it is