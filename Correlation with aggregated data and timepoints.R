#correlating r0 by temperature with aggregated data and temp data 

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(chron)
library(correlation)

#import the data: I didn't understand how you read in the data. You should use read_csv from the tidyverse, it's faster. I specified column type for the R0.
correlated_156_timepoints <- read_csv("timepoints_156_final_with_r0.csv", col_types=cols("R0"=col_double()))

correlated_39_timpeoints <- read_csv("timepoints_39_final_with_r0.csv", col_types=cols("R0"=col_double()))

#attach the data
# it's not necessary to attach here, because both of the datasets have the same column names, so they'll override each other. that may be your problem
# attach(timepoints_39_final_with_r0)
# attach(aggregated_data_39_timepoints.(1))

#correlate: I think we should make linear models that correlate R0 with temperature


# we think temperature predicts R0, so R0 is y and temperature is X.
plot(correlated_156_timepoints$temp_in_K, correlated_156_timepoints$R0) #let's plot the data together to see what it looks like. we can see there's a lot of noise in this data - we should run it by dr. qin
lm(correlated_156_timepoints$R0~correlated_156_timepoints$temp_in_K) #these are linear models, for nonlinear data unfortunately
summary(lm(correlated_156_timepoints$R0~correlated_156_timepoints$temp_in_K)
) #this makes a summary of the lm, I would read the documentation of how to interpret a linear model in R. 
plot(lm(correlated_156_timepoints$R0~correlated_156_timepoints$temp_in_K)
) #lastly these are plots that show the residuals of the linear model. the data is not normal, so that's why it looks weird

#then I would do this for the 39 timepoints data set. I'm not sure if working with the unaggregated or the aggregated datat is better. I would ask Dr. qin. I would also show him these plots so he can give input on what to do next!



#format? when doing research I seen this format but I keep getting an error message 
correlation <- c( timepoints_156_final_with_r0,aggregated_data_156_timepoints)
correlation <- c(timepoints_39_final_with_r0, aggregated_data_39_timepoints)



