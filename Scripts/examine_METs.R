# load the library
library(ggridges)


# Examine dataframe ------------------------------------------------------------
str(minuteMETsNarrow_merged)


# Cleaning ---------------------------------------------------------------------
cleaned_mets <- minuteMETsNarrow_merged %>% 
  rename_with(tolower) %>% 
  mutate(time_n = mdy_hms(activityminute)) %>% 
  separate(time_n, into = c("date", "time"), sep = " ", remove = FALSE) %>%
  mutate(time = hms::as_hms(time),
         id = as.factor(id)) %>%  
  select(id, date, time, mets)

cleaned_mets <- mutate(cleaned_mets, mets_n = mets/10)

cleaned_mets %>% 
  filter(mets_n>3) %>% 
    ggplot()+
  geom_density(aes(mets_n, fill = id), alpha = 0.01)+
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank())+
  labs(x="METs", y="Density", fill=NULL) 

mets_summary <- cleaned_mets %>% 
  group_by(id, date) %>% 
  summarise(max = max(mets_n))

mets_summary %>% 
  ggplot()+
  geom_density_ridges(aes(x = max, y = id), alpha = 0.4)+
  theme_light()+
  theme(legend.position = "none",
        panel.border = element_blank())+
  labs(x="METs", y="Density", fill=NULL) 

mets_types <- mets_summary %>% 
  group_by(id) %>% 
  summarise(avg_mets = mean(max))
mets_types %>% 
  mutate(id = fct_reorder(id, avg_mets)) %>%
  ggplot(aes(x=avg_mets,y=id)) +
  geom_col()
#categorise people based on this metric into "Moderate","Vigorous","Elite"?

fitness_level <- cut(mets_types$avg_mets, breaks = c(0,6,9,100),
                labels = c(0,1,2),
                right = FALSE)
mets_types <- cbind(mets_types, fitness_level)
#https://www.healthline.com/health/what-are-mets#examples


# Saving Cleaned Files ---------------------------------------------------------
write_csv(cleaned_mets, file = "~/Documents/case_study_capstone/Cleaned Data /cleaned_mets.csv")
write_csv(mets_summary, file = "~/Documents/case_study_capstone/Cleaned Data /mets_summary.csv")

write_csv(mets_types, file = "~/Documents/case_study_capstone/Cleaned Data /mets_types.csv")
