install.packages("lubridate")
install.packages("tidyverse")
install.packages("ggplot2")

library(lubridate)
library(tidyverse)
library(ggplot2)



dailySteps <- read.csv("dailySteps_merged.csv")

dailySteps$ActivityDay <- mdy(dailySteps$ActivityDay)
dailySteps <- unique(dailySteps)
dailySteps <- coalesce(dailySteps)
dailySteps$less4k <- with(dailySteps,ifelse(StepTotal < 4000, 'Less than 4k steps', 'More than 4k steps'))

glimpse(dailySteps)


numberOfUsers <- n_distinct(dailySteps$Id)
from_date <- min(dailySteps$ActivityDay)
to_date <- max(dailySteps$ActivityDay)




cat(c("Number of Users:", numberOfUsers))
cat(c("From Date:", as.Date(from_date)))
cat(c("To Date:", to_date))




dailySteps_clean <- dailySteps %>%
  #Eliminating observations with no steps and observations from 2016-05-12 due findings in 'nuevo_numPasos'
  #  that show how only half of that day was registered.
  filter(StepTotal > 0, ActivityDay != '2016-05-12') %>% 
  group_by(ActivityDay) %>% 
  summarise(mean_Steps = round(mean(StepTotal),2))

mean_Steps <- mean(dailySteps_clean$mean_Steps)

ggplot(dailySteps_clean, aes(x=ActivityDay, y=mean_Steps)) +
  geom_col( fill = "#660000", color = "white") +
  geom_smooth(method = "loess") +
  labs(
    x = "Observation Date",
    y = "Number of Steps",
    title = "How many steps does the users walk daily?",
    subtitle = "Users walk an average of 8,420 steps daily",
    caption = "Data from 2016-04-14 to 2016-05-11"
  )


less4k_df <- dailySteps %>% 
  group_by(less4k) %>% 
  summarise(Count = n(), Percentage = as.double(((Count/dailySteps_total_obs)*100)))


ggplot(less4k_df, aes(x = less4k, y=Percentage)) +
  geom_col(width = 0.5, fill = '#003300', color ='white')+
  coord_flip()+
  labs(
    x='Amount of Daily Steps',
    title="Percentage of users that walk less than 4000 steps a day",
    subtitle = "Twenty six percent of the users walk less than 4k steps daily",
    caption = "Data from the New York Times shows that walking at least 4k steps a day reduces health impact of sedentarism"
  )

