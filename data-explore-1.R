# Data exploration 1
# This script explores early versions of the behavioral data to ensure data was
# collected properly
#
# Matt Kmiecik
# Started 20 JAN 2021

# Prepares workspace ----
source("r-workspace-prep.R")

# Load data ----
load("../output/behav-data.RData") # behav data
load("../output/ss-groups.RData") # participant groups

# Exploration ----

# Summarizes ratings data
ratings_sum <- 
  behav_data %>% 
  group_by(intensity) %>%
  summarise(m = mean(value), sd = sd(value), n = n(), sem = sd/sqrt(n)) %>%
  ungroup()

# Ratings plot
ggplot(ratings_sum, aes(intensity, m)) +
  geom_point(
    data = behav_data, 
    aes(y = value), 
    alpha = 1/3, 
    position = "jitter",
    color = rdgy_pal[9],
    ) +
  geom_line(color = rdgy_pal[11]) +
  geom_point(color = rdgy_pal[11]) +
  geom_errorbar(
    aes(ymin = m-sem, ymax = m+sem), 
    width = .2, 
    color = rdgy_pal[11]
    ) +
  coord_cartesian(ylim = c(0, 20)) +
  labs(
    x = "Loudness Intensity", 
    y = "Mean Unpleasantness", 
    caption = "SEM error bars."
    ) +
  theme_minimal()

# Introducing subject groups
behav_data_groups <- 
  behav_data %>% left_join(., select(ss_groups, ss, group), by = "ss")

# ratings plot with groups
ggplot(behav_data_groups, aes(intensity, value, group = group, color = group)) +
  geom_point(position = "jitter", alpha = 1/3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Intensity", y = "Unpleasantness") +
  theme_minimal()

# summarizing by group
behav_data_groups %>% 
  group_by(intensity, group) %>%
  summarise(m = mean(value), sd = sd(value), n = n(), sem = sd/sqrt(n)) %>%
  ungroup()
