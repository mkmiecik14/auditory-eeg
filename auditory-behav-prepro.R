# Auditory behavioral task preprocessing script
# This script takes the merged e-data aid file and preprocesses it
# for analysis in R
#
# Matt Kmiecik
# Started 19 JAN 2021

# Prepares workspace ----
source("r-workspace-prep.R")

# Loads data and processes----
behav_data <- 
  read_delim(
    file = "../data/eprime/auditory-behav-data.txt", 
    delim = "\t"
    ) %>%
  # selects relevant variables
  select(
    ss = Subject,
    date = SessionDate,
    order = Block,
    intensity = BlockList,
    rating_0 = RateSc.RESP,
    rt_0 = RateSc.RT,
    rating_1 = RateSc1.RESP,
    rt_1 = RateSc1.RT,
    rating_2 = RateSc2.RESP,
    rt_2 = RateSc2.RT,
    rating_3 = RateSc3.RESP,
    rt_3 = RateSc3.RT,
    rating_4 = RateSc4.RESP,
    rt_4 = RateSc4.RT
  ) %>%
  mutate(across(starts_with("rating"), as.numeric)) %>% # changes ratings to num
  pivot_longer(
    cols = starts_with(c("rating","rt")),
  ) %>%
  filter(complete.cases(value)) %>% # gets rid of missing values
  separate(name, into = c("meas", "delete")) %>% # simplifies
  select(-delete) %>% # gets rid of useless column
  arrange(ss, intensity) %>% # arranges for aesthetics
  # decided to get rid of RT as this was entered by testers and is therefore 
  # not meaningful
  filter(meas == "rating") 
  
# Saves out cleaned and processed data ----
save(behav_data, file = "../output/behav-data.RData")

# Cleans workspace ----
rm(behav_data)

