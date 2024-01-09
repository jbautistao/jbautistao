---
title: "Bellabeat_dailySteps_merged"
author: "Jorge Bautista"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction 
##### The data frame "dailySteps_merged" shows the number of steps that 33 diferent users walked daily from 12-04-2016 to 12-05-2016.

##### The main goal of this document is to show how much users walk the minimum amount of steps to reduce health impact of sedentarism.
##### Recomendations for the Bellabeat's marketing team are included at the end of this document.

```{r installPackages, include=FALSE}
#install.packages("lubridate")
#install.packages("tidyverse")
#install.packages("ggplot2")
```
## Processing dailySteps_merged
To process dailySteps_merged data frame the following libraries were implemented.
```{r libraries, results='hide', warning=FALSE, message=FALSE}
library(lubridate)
library(tidyverse)
library(ggplot2)
```

Next, we took the first glance to the .csv file.

```{r}
dailySteps <- read.csv("dailySteps_merged.csv")
glimpse(dailySteps)
```
Before extracting the first valuable information the data frame had to be cleaned, so functions like unique and coalese were run to eliminate duplicates and null observations.

ActivityDay attribute was changed from character to time type value to easier further manipulation.
A new attribute was created through an if condition to identify if the user had walked more or less than 4000 steps. According to the New York Times walking at least 4k steps a day reduces health impact of sedentarism.[1]

[1]Blum, D. (2023) No hay que dar 10.000 pasos diarios para Comenzar a obtener Beneficios de Salud, The New York Times. Available at: https://www.nytimes.com/es/2023/08/18/espanol/pasos-diarios-caminar-ejercicio.html (Accessed: 28 November 2023).

```{r}
dailySteps <- unique(dailySteps)
dailySteps <- coalesce(dailySteps)
dailySteps$ActivityDay <- mdy(dailySteps$ActivityDay)
dailySteps$less4k <- with(dailySteps,ifelse(StepTotal < 4000, 'Less than 4k steps', 'More than 4k steps'))
dailySteps_total_obs <- count(dailySteps)

glimpse(dailySteps)
```

After cleaning the data frame the first important information was extracted, the data frame contains information of 33 different users from 2016-04-12 to 2016-05-12. 

```{r}
numberOfUsers <- n_distinct(dailySteps$Id)
from_date <- min(dailySteps$ActivityDay)
to_date <- max(dailySteps$ActivityDay)
```

```{r}
print(paste0("Number of users: ", numberOfUsers))
print(paste0("From Date: ", from_date))
print(paste0("To Date: ", to_date))
```
Next, we wanted to know the mean steps by day in other to calculate the average daily steps walked by the users. We focused on the observations with StepTotal bigger than 0 and eliminated observations from 2016-05-12 after observing only half day was registered.

```{r}
dailySteps_clean <- dailySteps %>%
  filter(StepTotal > 0, ActivityDay != '2016-05-12') %>% 
  group_by(ActivityDay) %>% 
  summarise(mean_Steps = round(mean(StepTotal),2))%>%
  ungroup()

glimpse(dailySteps_clean)
```
With data frames that contains the information we want to focus in to understand customer behavior, we began to extract insights, starting with the average daily steps of our users. 
```{r}
mean_Steps <- mean(dailySteps_clean$mean_Steps)
cat(c("Average daily steps:", mean_Steps))
```

We also want to know how much people is reaching the minimum amount of steps according to the New York Times. [1] 

```{r}
less4k_df <- dailySteps %>% 
  group_by(less4k) %>% 
  summarise(Count = n(), Percentage = as.double(((Count/dailySteps_total_obs)*100)))

head(less4k_df)
```
## Presenting Findings
After cleaning and analyzing this data frame, important insights of user's daily activity were found, the following visualizations will help to highlight them.

### Viz 1:How many steps does the users walk daily?
After focusing on observations with StepTotal bigger than 0 and eliminating observations from 2016-05-12 (because only half day was registered), we can appreciate that the average amount of daily steps of the users is around 8 thousand steps

This information is backed up with the numeric analysis done previously which indicated 8,420 steps as the mean value.

```{r, message=FALSE}
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
```

### Viz 2:Percentage of users that walk less than 4000 steps a day
In the second visualization 
After grouping the information of dailySteps based on the attribute "less4k" that we created to identify how much users walk less than 4000 steps, we can observe the percentage of users that walk more or less than that.

A percentage of 26.2% is not reaching the minimum amount of daily steps and could be jeopardizing their health. 
```{r}
ggplot(less4k_df, aes(x = less4k, y=Percentage)) +
  geom_col(width = 0.5, fill = '#003300', color ='white')+
  coord_flip()+
  labs(
    x='Amount of Daily Steps',
    title="Percentage of users that walk less than 4000 steps a day",
    subtitle = "Twenty six percent of the users walk less than 4k steps daily",
    caption = "Data from the New York Times shows that walking at least 4k steps a day reduces health impact of sedentarism"
  )
```

## Marketing Recomendations
#### After knowing how much steps does the user walk daily and how much of them walk less than 4000 steps, we found out that there is an area of opportunity to improve the habits of more than 26% of the Bellabeat users, we can use this insights to present a new commercial that remarks how almost 1/3 of the population don't walk enough and the dangers of sedentary lifestyle.

## R&D Recomendations
#### To improve user's experience when interacting with our app we can use the new information about mean daily steps (8420 steps) to preset goal to 8000 steps; sparing the extra step to most of our users.

