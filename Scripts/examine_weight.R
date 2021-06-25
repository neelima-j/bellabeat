library(lubridate)
library(tidyverse)
library(janitor)
library(skimr)

# Examine dataframe ------------------------------------------------------------
str(weightLogInfo_merged)
# Date needs to be formatted
# Id needs to be made a factor


# Cleaning ---------------------------------------------------------------------
cleaned_weight_log <- weightLogInfo_merged %>%
  clean_names() %>% 
  mutate(date = mdy_hms(date),
         id = as.factor(id)) %>% 
  separate(date, into = c("date", "time"), sep = " ", remove = FALSE) %>% 
  select( -time)


# Exploring Weight Logs --------------------------------------------------------
skim(cleaned_weight_log)
# 8 unique Ids
# 41 manual reports, 26 auto logs
# fat not logged
# bmi complete

# Question: Who does manual reports?
manual_loggers <- cleaned_weight_log %>% 
  filter(is_manual_report == TRUE) %>% 
  distinct(id,.keep_all = FALSE)

auto_loggers <- cleaned_weight_log %>% 
  filter(is_manual_report == FALSE) %>% 
  distinct(id,.keep_all = FALSE)
# Distinct people have distinct reporting styles

# Do manual loggers have fewer reports?
cleaned_weight_log %>% 
  group_by(id, is_manual_report) %>% 
  summarise(weight = mean(weight_kg),
            count = n()) %>% 
  ggplot(aes(x=count, y= weight))+
  geom_point()+
  facet_wrap(~is_manual_report)
# Manual reports are more likely to be infrequent
# There are more manual loggers in the dataset
# Product gap: connected devices 

# Question: what is the BMI/Weight distribution in this selection of 8 loggers -
cleaned_weight_log %>% 
  group_by(id) %>% 
  summarise(bmi = mean(bmi),
  weight = mean(weight_kg)) %>% 
  ggplot()+
  geom_density(aes(bmi))+
  geom_density(aes(weight), color = "salmon")

 # Question: What is the weight loss/gain in the dataset -----------------------
 weight_summary <- cleaned_weight_log %>% 
   group_by(id, is_manual_report) %>% 
   arrange(date, .by_group = TRUE) %>% 
   summarise(start_weight = first(weight_kg),
             weight_change_kg = first(weight_kg)-last(weight_kg),
             start_bmi = first(bmi),
             bmi_change = first(bmi) - last(bmi),
             reports = n())


# Saving Cleaned Files ---------------------------------------------------------
write_csv( weight_summary, file = "~/Documents/case_study_capstone/Cleaned Data /weight_summary.csv")
write_csv( cleaned_weight_log, file = "~/Documents/case_study_capstone/Cleaned Data /cleaned_weight_log.csv")
