# Data Analysis 1
# This script performs an analysis on the behavioral unpleansantness ratings
# across the various groups using a bootstrapping procedure
# 
# Also investigates the effect of order and effect of age
#
# Matt Kmiecik
# Started 25 JAN 2021

# Prepares workspace ----
source("r-workspace-prep.R")

# Load data ----
load("../output/behav-data.RData") # behav data
load("../output/ss-groups.RData") # participant groups

# MLM ----

# Level 1 mods
lvl1_mod <- 
  behav_data %>%
  mutate(int_mc = intensity - 3) %>% # this mean centers intensity 
  nest_by(ss) %>%
  mutate(mod = list(lm(value ~ 1 + int_mc, data = data)))

# Level 1 estimates (if you need them at some point like this)
lvl1_est <- 
  lvl1_mod %>% 
  summarise(broom::tidy(mod)) %>%
  ungroup() %>%
  mutate(term = case_when(
    term == "(Intercept)" ~ "Intercept",
    term == "int_mc" ~ "Intensity_mc"
    )
    )

# Bootstrapping procedure ----

# Prepping data
boot_data <- 
  lvl1_mod %>% 
  ungroup() %>% 
  left_join(., select(ss_groups, ss, group), by = "ss")

# Bootstrapping procedure
boot_iters <- 2000 # the number of bootstrap iterations
# set.seed(14) # remember to set the seed
# initialized list to store results
boot_data_fill <- vector("list", length = boot_iters) 
loop_data <- boot_data %>% select(ss, group) %>% group_by(group)

for (i in 1:boot_iters){
  
  # boot_data_fill[[i]] <- 
  #   boot_data %>%
  #   group_by(group) %>%
  #   slice_sample(prop = 1, replace = TRUE) %>%
  #   ungroup() %>%
  #   rowwise(ss, group) %>%
  #   summarise(broom::tidy(mod))

  # boot_data_fill[[i]] <- sample(boot_data$ss, replace = TRUE)
  
  boot_data_fill[[i]] <-
    loop_data %>%
    slice_sample(prop = 1, replace = TRUE) %>%
    ungroup()
  
}

# Bootstrapped data
boots <-
  boot_data_fill %>%
  map_dfr(~as_tibble(.x), .id = "iter") %>%
  left_join(., lvl1_est, by = c("ss")) # adds the estimates 

# Summarising bootstrapped data within bootstrap interation
boots_ss <- 
  boots %>%
  group_by(iter, group, term) %>%
  summarise(m = mean(estimate), n = n()) %>%
  ungroup()

# Summarising bootstrapped data across bootstrap interation
boot_sum <- 
  boots_ss %>%
  group_by(group, term) %>%
  summarise(
    M = mean(m), 
    n = n(), 
    sd = sd(m), 
    ll = quantile(m, .025),
    ul = quantile(m, .975)
    ) %>%
  ungroup()

# Plotting bootstrapped 95% confidence intervals
ggplot(boot_sum, aes(group, M)) +
  geom_point() +
  geom_errorbar(aes(ymin = ll, ymax = ul), width = .2) +
  #theme_minimal() +
  labs(
    x = "Group", 
    y = "Regression Estimate", 
    caption = "Bootstrapped 95% CI error bars."
    ) +
  facet_wrap(~term)

###################
#                 #
# Effect of Order #
#                 #
###################

# Modeling order
behav_data_prep <-
  behav_data %>%
  mutate(
    int_mc = intensity - 3, # this mean centers intensity
    order_mc = order - 3    # this mean centers order (probably not necessary)
    )

# Level 1 mods
order_lvl1_mod <-
  behav_data_prep %>% 
  nest_by(ss) %>%
  mutate(mod = list(lm(value ~ 1 + order_mc, data = data)))

# Level 1 estimates (if you need them at some point like this)
order_lvl1_est <- 
  order_lvl1_mod %>% 
  summarise(broom::tidy(mod)) %>%
  ungroup() %>%
  mutate(term = case_when(
    term == "(Intercept)" ~ "Intercept",
    term == "order_mc" ~ "Order_mc"
  )
  )

# Level 2 mods for order
order_lvl2_mod <- 
  order_lvl1_est %>%
  nest_by(term) %>%
  mutate(mod = list(lm(estimate ~ 1, data = data)))

# This demonstrates that there is no effect of order (slope is not significant)
order_lvl2_mod %>%
  summarise(broom::tidy(mod)) %>%
  mutate(source = order_lvl2_mod$term)

#################
#               #
# Effect of Age #
#               #
#################

lvl1_est

  

