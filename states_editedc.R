library(lubridate)
library(tidyverse)
total_mobility <- read_csv("total_mobility.csv")
total_mobility %>%
  mutate(date=as.Date(total_mobility$date, "%m/%d/%Y")) # for some reason when you reload total_mobility sometimes   


#this creates a data frame of mobility by state.
state_summaries <- total_mobility %>%
  group_by(state, date) %>%
  summarize_if(is.numeric, mean,na.rm=TRUE)

states_sf <- get_urbn_map("states", sf=TRUE) %>%
  st_as_sf()


#this creates a data frame with sf mapping
state_summaries <- left_join(state_summaries, states_sf, by=c("state"="state_name")) %>%
  mutate(date=as.Date(date, "%m/%d/%Y"))
    
#this creates the every two week interval period and selects only the first day - need to ask if this is what he is looking for
every_two_weeks <- state_summaries %>%
  mutate(TwoWeeks=round_date(date, "2 weeks")) %>%
  group_by(TwoWeeks) %>%
  filter(date==min(TwoWeeks))

#this creates some graphs based on the fact that they are animated / will change over time (the "earliest" date in the 2-week period selected above). because each column must be referred to directly, i've been doing this by hand (slow) without a for loop.
diff_from_baseline <- ggplot() +
  geom_sf(data=states_sf, fill="white") +
  geom_sf(data=every_two_weeks, aes(fill=every_two_weeks$difference_from_baseline_apple)) +
  labs(fill="diff from baseline", title="date: {current_frame}") +
  transition_manual(date)

animate(diff_from_baseline, nframes = 13, device = "png",
        renderer = file_renderer("C:/Users/chfal/Desktop/ICOMPBIO/CLEOS_RESEARCH_PJECT/" , prefix = "diff_from_baseline", overwrite = TRUE))
