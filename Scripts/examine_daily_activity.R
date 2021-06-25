library(lubridate)
library(tidyverse)
library(janitor)
library(skimr)
library(ggcorrplot)
library(ggstatsplot)

# Examine dataframe ------------------------------------------------------------
str(dailyActivity_merged)
# Date needs to be formatted
# Id needs to be made a factor


# Cleaning ---------------------------------------------------------------------
cleaned_daily_activity <- dailyActivity_merged %>%
  clean_names() %>% 
  mutate(activity_date = mdy(activity_date),
         day = wday(activity_date, label = TRUE),
         id = as.factor(id))


# Exploring Daily Steps---------------------------------------------------------
skim(cleaned_daily_activity)
# 33 unique Ids

cleaned_daily_activity %>% 
  filter(total_steps > 0) %>% 
  group_by(id, day) %>% 
  summarise(steps = mean(total_steps)) %>% 
  ggplot(aes( x = day, y = steps, group = id))+
  geom_line()+
  facet_wrap(~id)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        strip.text = element_blank() )+
  labs(x="Day of the Week", y="Average Number of Steps",
       title = "Variation in Steps across Day of Week",
       subtitle = "Each line represents an individual") 
# Observation: 
# Some IDs are consistent, some vary wildly across days.
# Sunday isn't the lowest for everyone
# Can use some sort of Variance / StdDev to  find a metric of consistency 
# Consistent low activity, Erratic Mixed, Consistent high activity

# Classify Ids based on their variance in total activity minutes ---------------
activity_summary <- cleaned_daily_activity %>% 
  group_by(id) %>% 
  summarise(steps_mean = mean(total_steps),
            steps_metric = (stats::sd(total_steps)*100/mean(total_steps)),
            active_time_mean = mean(1440-sedentary_minutes),
            active_time_metric = (stats::sd(1440-sedentary_minutes)*100/mean(1440-sedentary_minutes)))
#do people with erratic steps have erratic activity minutes
cor(activity_summary$steps_metric,activity_summary$active_time_metric)
# yes , 0.86


# Observation: Lower Activity people have more erratic weekly activity
activity_summary %>% 
  ggplot(aes(x=active_time_mean, y=active_time_metric))+
  geom_point()+ 
  geom_smooth(method = "loess", span = 1)+
  theme_bw()+
  labs(x="Mean Daily Minutes of Activity", y="Percentage Standard Deviation",
       title = "Majority of Users are Consistently Active",
       subtitle = "Each point represents an individual") 
activity_summary %>% 
  ggplot(aes(x=steps_mean, y=steps_metric))+
  geom_point()+ 
  geom_smooth(method = "loess", alpha = 0.2)+
  theme_bw()+
  labs(x="Mean Daily number of Steps", y="Percentage Standard Deviation",
       title = "Majority of Users are Consistently Active",
       subtitle = "Each point represents an individual") +
  annotate("rect", xmin = 0, xmax = 4000, ymin = 75, ymax = 160,
         alpha = .2)+
  annotate("text", x =1720, y = 165, label = "Sedentary - Erratic")+
annotate("rect", xmin = 5000, xmax = 16000, ymin = 0, ymax = 74,
         alpha = .2, fill = "green")+
  annotate("text", x =12000, y = 65, label = "Active - Consistent")
# Sedentary is less than 5,000 steps per day. 
# https://www.bbc.com/future/article/20190723-10000-steps-a-day-the-right-amount
# What can be done to monetize these users
# Reward consistency in levels - the majority of users are consistent and may
# appreciate the recognition 
# Consistency based discounts to subscriptions, products. Ethics of this tracking?


# Exploring Daily Calories -----------------------------------------------------
cleaned_daily_activity %>% 
  #filter(total_steps > 0) %>% 
  group_by(id, day) %>% 
  summarise(cals = mean(calories)) %>% 
  ggplot(aes( x = day, y = cals, group = id))+
  geom_line()+
  facet_wrap(~id)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90),
        strip.text = element_blank() )+
  labs(x="Day of the Week", y="Average Number of Calories",
       title = "Variation in Calories across Day of Week",
       subtitle = "Each line represents an individual") 



# Exploring Correlations with Calories -----------------------------------------
calories_corr <-  cleaned_daily_activity %>% 
  select_if(is.numeric) %>% 
  apply(2, function(col)cor(col, cleaned_daily_activity$calories)) %>% 
  sort()

ggscatterstats(cleaned_daily_activity , tracker_distance , calories, 
               type = "bayesian")
ggscatterstats(cleaned_daily_activity , very_active_minutes , calories, 
               type = "bayesian")

# Observations:

# Grouping Type of People ------------------------------------------------------
# Ids whose cor() between distance and calories is weaker aren't walking/running
# Other exercise type logging - manual vs auto record Product Feature
# Pull survey data here? third party source

#
cor_id <- cleaned_daily_activity %>% 
  group_by(id) %>% 
  summarise(cor_activity_time = cor((1440-sedentary_minutes), calories),
            cor_dist = cor(tracker_distance, calories))


ggscatterstats(cor_id , cor_activity_time , cor_dist , 
               type = "bayesian")

#corr <- cleaned_daily_activity %>% 
#  select_if(is.numeric) %>% 
#  filter(total_distance != tracker_distance ) %>% 
#  cor()
#ggcorrplot(corr, hc.order = TRUE, type = "lower",
#           outline.col = "white", lab = TRUE,  insig = "blank")



# Saving Cleaned Files ---------------------------------------------------------
write_csv(cleaned_daily_activity, file = "~/Documents/case_study_capstone/Cleaned Data /cleaned_daily_activity.csv")
write_csv(activity_summary, file = "~/Documents/case_study_capstone/Cleaned Data /activity_summary.csv")

