### Load Libraries ###
library(jsonlite)
library(tidyverse)
library(dplyr)
library(ggimage)
library(ggrepel)
library(ggplot2)

## Load Data ##

df_2010draft <- read.csv("2010draft.csv")
df_2011draft <- read.csv("2011draft.csv")
df_2012draft <- read.csv("2012draft.csv")
df_2013draft <- read.csv("2013draft.csv")
df_2014draft <- read.csv("2014draft.csv")
df_2015draft <- read.csv("2015draft.csv")
df_2016draft <- read.csv("2016draft.csv")
df_2017draft <- read.csv("2017draft.csv")
df_2018draft <- read.csv("2018draft.csv")
df_2019draft <- read.csv("2019draft.csv")

## Arrange Data ##
df_2010draft <- df_2010draft%>%
  mutate(draftyear = 2010)

df_2011draft <- df_2011draft%>%
  mutate(draftyear = 2011)

df_2012draft <- df_2012draft%>%
  mutate(draftyear = 2012)

df_2013draft <- df_2013draft%>%
  mutate(draftyear = 2013)

df_2014draft <- df_2014draft%>%
  mutate(draftyear = 2014)

df_2015draft <- df_2015draft%>%
  mutate(draftyear = 2015)

df_2016draft <- df_2016draft%>%
  mutate(draftyear = 2016)

df_2017draft <- df_2017draft%>%
  mutate(draftyear = 2017)

df_2018draft <- df_2018draft%>%
  mutate(draftyear = 2018)

df_2019draft <- df_2019draft%>%
  mutate(draftyear = 2019)

df_combined_draft <- df_2010draft%>%
  full_join(df_2011draft)%>%
  full_join(df_2012draft)%>%
  full_join(df_2013draft)%>%
  full_join(df_2014draft)%>%
  full_join(df_2015draft)%>%
  full_join(df_2016draft)%>%
  full_join(df_2017draft)%>%
  full_join(df_2018draft)%>%
  full_join(df_2019draft)%>%
  mutate(AVperYear = CarAV / (To - draftyear + 1))

## Filter by position, snap count and targets, and remove weekly data##

draft_filtered_plot  <- df_combined_draft%>% 
  filter(Pos == "RB")%>%
  ggplot(aes(x = desc(Rnd), y = AVperYear )) +
  geom_point()+
  #geom_point(color = ifelse(draft_filtered_plot$Tm == "SEA", "green", "black")) +
  geom_text(aes(label=Player),check_overlap = TRUE, size=3, hjust=.5, vjust=-.75) +
  labs(x = "Draft Round",
       y = "AVperYear",
       caption = "Data from PFR",
       title = "AV per round",
       subtitle = "2010-2019") +
  theme_bw() +
  geom_hline(yintercept = mean(4.5), color = "green", linetype = "dashed") +
  #geom_vline(xintercept = 10, color = "blue", linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE)
  
draft_filtered_plot
