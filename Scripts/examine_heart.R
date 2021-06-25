# load the library
library(lubridate)
library(tidyverse)
library(skimr)
library(forcats) #reorder dataframe before plot
library(ggalt) #dumbbell plot

# Examine dataframe ------------------------------------------------------------
str(heartrate_seconds_merged)
# Date needs to be formatted
# Id to factor
# There is data for only 11 Ids



# Cleaning ---------------------------------------------------------------------
cleaned_heartrate <- heartrate_seconds_merged %>%
  mutate(time_n = mdy_hms(Time)) %>%
  separate(
    time_n,
    into = c("date", "time"),
    sep = " ",
    remove = FALSE
  ) %>%
  mutate(time = hms::as_hms(time),
         id = as.factor(Id)) %>%
  select(id, date, time, Value) %>%
  rename_with(tolower)

# Plotting all heartrates ------------------------------------------------------
p_one_person <- cleaned_heartrate %>%
  mutate(id = fct_reorder(id, value)) %>%
  ggplot(aes(id, value)) +
  geom_violin()+
  geom_jitter(alpha = 0.1)

p_one_person 

  
  ####
#id date max and min Daily
heartrate_summary <- cleaned_heartrate %>% 
  group_by(id, date) %>% 
  summarise(max = max(value), min = min(value))

#id  max and min average
heartrate_average <- heartrate_summary %>% 
  group_by(id) %>% 
  summarise(max = mean(max), min = mean(min))

p_heartrate_range <- heartrate_average %>% 
  mutate(id = fct_reorder(id, (max-min))) %>%
  ggplot(aes(x=min,xend = max, y=id))+
  geom_dumbbell()
p_heartrate_range 

cor(heartrate_average$max,heartrate_average$min)
#people with high differences in max - min may exercise more
#low min and high max = more exercise
#??


p_heartrate <- ggplot(data = heartrate_summary)+
  geom_density(aes(min, fill = "b"), color = "black", alpha = 0.7)+
  geom_density(aes(max, fill = "a"),color="black", alpha = 0.5)+
  theme_light()+
  theme(legend.position = c(.95,.95),
        panel.border = element_blank(),
        legend.justification = c("right", "top"))+
  labs(x="Heartrate", y="Density", fill=NULL) +
  scale_fill_discrete(labels = c("Maximum Heart Rate","Resting Heart Rate"))

p_heartrate

p_max_heartrate <- ggplot(data = heartrate_summary)+
  geom_density(aes(max, fill = id), alpha = 0.01)+
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank(),
        legend.justification = c("right", "top"))+
  labs(x="Heartrate", y="Density", fill=NULL) 

p_max_heartrate

p_min_heartrate <- ggplot(data = heartrate_summary)+
  geom_density(aes(min, fill = id), alpha = 0.01)+
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank(),
        legend.justification = c("right", "top"))+
  labs(x="Heartrate", y="Density", fill=NULL) 

p_min_heartrate

# these 11 people are essentially healthy, but max is different.. different exercise types?


# Saving Cleaned Files ---------------------------------------------------------
write_csv(cleaned_heartrate, file = "~/Documents/case_study_capstone/Cleaned Data /cleaned_heartrate.csv")
write_csv(heartrate_average, file = "~/Documents/case_study_capstone/Cleaned Data /heartrate_average.csv")

write_csv(heartrate_summary, file = "~/Documents/case_study_capstone/Cleaned Data /heartrate_summary.csv")

  