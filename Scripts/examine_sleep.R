library(lubridate)
library(tidyverse)
library(skimr)
library(janitor)

# Examine dataframe ------------------------------------------------------------
str(minuteSleep_merged)
str(sleepDay_merged)
# Date needs to be formatted
# Id needs to be made a factor
# `SleepDay` to be converted to date

# Cleaning ---------------------------------------------------------------------
 cleaned_minute_sleep  <- minuteSleep_merged %>% 
   clean_names() %>% 
   mutate(date = mdy_hms(date)) %>% 
   separate(date, into = c("date", "time"), sep = " ", remove = FALSE) %>%
   mutate(id = as.factor(id),
          log_id = as.factor(log_id),
          time = hms::as_hms(time))
 
cleaned_daily_sleep <- sleepDay_merged %>% 
  mutate(Date = mdy_hms(SleepDay),
         Day = wday(Date, label = TRUE),
         Id = as.factor(Id)) %>% 
  select(-SleepDay,-TotalSleepRecords)


# Calculate bedtimes for each person-------------------------------------------- 
bedtime <- cleaned_minute_sleep %>% 
  group_by(id, log_id) %>% 
  summarise(bedtime =hms::as_hms( min(time)))

bedtime_summary <- bedtime %>% 
  group_by(id) %>% 
  summarise(mean_bedtime = hms::as_hms(mean(bedtime)),
            bedtime_metric = round(stats::sd(bedtime)*100/mean(as.numeric(bedtime)),0),
            records = n())


# Daily summary of sleep--------------------------------------------------------
sleep_summary <- cleaned_daily_sleep %>% 
  group_by(Id) %>% 
  summarise(meanSleepHours = round(mean(TotalMinutesAsleep)/60,2),
            meanAwakeMins = round(mean(TotalTimeInBed - TotalMinutesAsleep),2))


# Finding types of sleepers-----------------------------------------------------

# Plot all sleeps to find median and distribution ------------------------------
plot_sleep <-
  ggplot(data = cleaned_daily_sleep , aes(x = TotalMinutesAsleep / 60)) +
  # geom_bar(binwidth = .2, stat = "bin")+
  geom_density(alpha = .4, fill = "darksalmon") +
  geom_vline(
    aes(xintercept = median((
      TotalMinutesAsleep / 60
    ), na.rm = T)),
    # Ignore NA values for mean
    color = "orangered1",
    linetype = "dashed",
    size = 1
  ) +
  theme_light() +
  geom_label(aes(
    label = paste0 ("Median Sleep: ", round(median(
      TotalMinutesAsleep / 60
    ), 2), " hrs"),
    x = 10,
    y = 0.2
  )) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.border = element_blank(), panel.grid.minor.y = element_blank()) +
  labs(x = "Hours of Sleep", y = "Frequency", fill = NULL)


# Making Bins for sleep types---------------------------------------------------
## https://www.nhs.uk/live-well/sleep-and-tiredness/how-to-get-to-sleep/
sleep_category <- cut(sleep_summary$meanSleepHours, breaks = c(0,6,9,100),
                labels = c("Poor","Normal","Over"),
                right = FALSE)
sleep_summary <- cbind(sleep_summary, sleep_category)


#----------------------------------------------------------------
#Ignore this plot

#Sleep Latency. 0-10, 10-20, 20+

ggplot(data = cleaned_daily_sleep , aes(x=(TotalTimeInBed - TotalMinutesAsleep)))+
  #geom_bar(binwidth = 5, stat = "bin")+
  geom_density(alpha=.4, fill="darksalmon")+
  geom_vline(aes(xintercept=median((TotalTimeInBed - TotalMinutesAsleep), na.rm=T)),   # Ignore NA values for mean
             color="orangered1", linetype="dashed", size=1)+
  geom_label(aes(label = paste0 ("Median Latency: ",round(median((TotalTimeInBed - TotalMinutesAsleep)),2), " mins"),
                 x = 200,y = .01)) +
  theme_light() +
  xlim(0,290)+
  scale_y_continuous(expand = c(0,0)) +
  theme(panel.border = element_blank())+
  labs(x="Minutes in Bed Awake", y="Frequency", fill=NULL) 
# ------------------------------------------------------------------------------
latency_category <- cut(sleep_summary$meanAwakeMins, breaks = c(0,10,20,Inf),
                        labels = c("Sleepy","Normal","Insomnia"),
                        right = FALSE)

df <- data.frame(sleep_summary, latency_category)
df <- remove_missing(df, na.rm = FALSE, vars = names(df), name = "", finite = FALSE)
ggplot(data = df  , aes(x = latency_category, y= meanAwakeMins))+
  geom_jitter()
sleep_summary <- df %>% clean_names()

#
sleep_summary <- full_join(sleep_summary, bedtime_summary,
                           by = NULL, keep = FALSE)
###################
#Time taken to fall asleep vs exercise of the day
#Sleep duration vs exercise type

# Saving Cleaned Files ---------------------------------------------------------

cleaned_daily_sleep <- clean_names(cleaned_daily_sleep)
write_csv(sleep_summary, file = "~/Documents/case_study_capstone/Cleaned Data /sleep_summary.csv")
write_csv(cleaned_daily_sleep, file = "~/Documents/case_study_capstone/Cleaned Data /cleaned_daily_sleep.csv")
write_csv(cleaned_minute_sleep, file = "~/Documents/case_study_capstone/Cleaned Data /cleaned_minute_sleep.csv")
