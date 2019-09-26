# NOLA UoF Inc.
# Scott Cohn
# Tidy + Clean

library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggsci)
library(ggmap)

data_path <- "data/NOPD_Use_of_Force_Incidents.csv"
uof_full <- read_csv(data_path)

# TO DO change yes/no to 0/1 etc.

uof_full %>% 
  filter(uof_full$`Subject Ethnicity` == "Black") %>% 
  summarize(
    effective_rate = mean(uof_full$`Use of Force Effective`, na.rm = T)
  )
