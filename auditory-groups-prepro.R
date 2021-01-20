# Participant Groups Preprocessing Script
# This script prepares and preprocesses participant group codes
# 
# Matt Kmiecik
# Started 20 JAN 2021

# Prepares workspace ----
source("r-workspace-prep.R")

# Loads data ----
ss_groups <- read_excel(path = "../data/ss-groups.xlsx", sheet = "groups")

# Saves out data ----
save(ss_groups, file = "../output/ss-groups.RData")

# Cleans up workspace ----
rm(ss_groups)