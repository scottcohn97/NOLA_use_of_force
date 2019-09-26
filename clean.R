# NOLA UoF Inc.
# Scott Cohn
# Tidy + Clean

library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggsci)
library(ggmap)
library(scales)

data_path <- "data/NOPD_Use_of_Force_Incidents.csv"
uof <- read_csv(data_path)

# TO DO change names of var

uof$`Use of Force Reason`[uof$`Use of Force Reason` == "room clearing"] <- "Room Clearing"
uof$`Use of Force Reason`[uof$`Use of Force Reason` == "Room clearing"] <- "Room Clearing"
uof$`Use of Force Reason`[uof$`Use of Force Reason` == "Room CLearing"] <- "Room Clearing"
uof$`Subject Ethnicity`[uof$`Subject Ethnicity` == "W"] <- "White"
uof$`Subject Ethnicity`[uof$`Subject Ethnicity` == "Race-Unknown"] <- "Uknown"
uof$`Subject Arrested` <- ifelse(uof$`Subject Arrested` == "Yes", 1, 0)
uof$`Use of Force Effective` <- ifelse(uof$`Use of Force Effective` == "Yes", 1, 0)
uof$`Subject Injured` <- ifelse(uof$`Subject Injured`  == "Yes", 1, 0)
uof$`Subject Hospitalized` <- ifelse(uof$`Subject Hospitalized`  == "Yes", 1, 0)
uof$`Officer Injured` <- ifelse(uof$`Officer Injured`  == "Yes", 1, 0)
uof$`Subject Gender` <- ifelse(uof$`Subject Gender` == "Male", 1, 2) # Male is 1, Female is 2
uof$`Officer Gender` <- ifelse(uof$`Officer Gender` == "Male", 1, 2) # Male is 1, Female is 2



# Dates, char -> date
uof$`Date Occurred` <- as.Date(uof$`Date Occurred`, "%m/%d/%Y")

# Freq over time by Reason
uof %>%
  count(year = year(`Date Occurred`), `Use of Force Reason`) %>% 
  ggplot(aes(x = year, y = n, color = `Use of Force Reason`)) +
  geom_point() +
  geom_line() +
  scale_color_d3()

# Perc of Subj Influ Factors
uof %>% 
  ggplot(aes(x = factor(`Subject Influencing Factors`))) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent) +
  scale_color_d3()

uof %>% 
  drop_na(`Subject Ethnicity`) %>%
  ggplot(aes(x = factor(`Subject Ethnicity`))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill = `Subject Ethnicity`)) + 
  scale_y_continuous(labels = percent) +
  scale_fill_d3() +
  labs(x = "Subject Ethnicity",
       y = "Percent of Total Uses of Force"
       ) +
  theme_bw()

# compute stops by race
# need numbers of people
# uof %>% 
#   count(`Subject Ethnicity`) %>% 
#   left_join(
#     population_2018,
#     by = "subject_race"
#   ) %>% 
#   mutate(stop_rate = n / num_people)

# Arrest Rate by Race
uof %>% 
  group_by(`Subject Ethnicity`) %>% 
  summarize(
    hit_rate = mean(`Subject Arrested`, na.rm = T)
  )

uof %>% 
  filter(`Subject Gender` == 2) %>% 
  group_by(`Subject Ethnicity`) %>% 
  summarize(
    hit_rate = mean(`Subject Arrested`, na.rm = T)
  )

uof %>% 
  filter(`Use of Force Effective` == 1) %>% 
  group_by(`Subject Ethnicity`) %>% 
  summarize(
    hit_rate = mean(`Subject Arrested`, na.rm = T)
  )

