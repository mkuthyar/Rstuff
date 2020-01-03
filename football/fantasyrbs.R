# Load necessary libraries #
library(jsonlite)
library(tidyverse)
library(dplyr)

# Read in CSV's (design this programatically in the future)
df_rb_basic <- read_csv("RBBasicstats.csv")
df_rb_advanced <- read_csv("RBAdvancedStats.csv")
df_rb_snaps <- read_csv("RBSnapCounts.csv")
df_rb_fantasy <- read_csv("RBfantasy.csv")
df_rb_air <- fromJSON('http://airyards.com/2019/weeks')

## Compile total by adding weekly numbers and performing calculations ##
rb_air_totals <- df_rb_air %>%
  group_by(player_id) %>%
  mutate(games_played = n(), 
         targets = sum(tar),
         receptions = sum(rec),
         receiving_yards = sum(rec_yards),
         totalyac = sum(yac),
         totalairyards = sum(air_yards),
         team_attempts = sum(tm_att),
         team_airyards = sum(team_air),
         avgadot = (totalairyards/targets), 
         avgracr = (receiving_yards/totalairyards),
         potay = (totalairyards/team_airyards), #percentage of team air yards
         target_percentage = (targets/team_attempts),
         avgwopr = ((1.5 * target_percentage) + (.7 * potay))
  ) %>%
  filter(position == "RB") %>%
  select (-week,-index,-tar,-td,-rush_td,-rec,-rec_yards,-rush_yards,-yac,-air_yards,-tm_att,-team_air,-aypt,-racr,-ms_air_yards,-target_share,-wopr) %>%
  distinct()

## Merge in fantasy data ##
rb_totals <- merge(df_rb_fantasy,rb_air_totals, all=TRUE)

## Merge in snap data ##
rb_totals <- merge(df_rb_snaps,rb_totals, all=TRUE)

## Merge in advanced data ##
rb_totals <- merge(df_rb_advanced,rb_totals, all=TRUE)

## Merge in basic data ##
rb_totals <- merge(df_rb_basic,rb_totals, all=TRUE)

## Filtering and any other residual cleanup ##
rb_totals_filtered <- rb_totals %>%
  filter(snaps > 30, Att >5) %>%
  select (-player_id,-games_played,-team, -Team,-position) %>%
  rename(rushingtds = TD) %>%
  mutate(opportunities = targets + Att)

## CHARTS ##

# Opportunities vs Fantasy PPG
opportunitiesvfantasy  <- rb_totals_filtered%>% 
  filter(opportunities >50)%>%
  ggplot(aes(x = opportunities, y = fantasypoints_pg )) +
  geom_point() +
  geom_text(aes(label=full_name),check_overlap = FALSE, size=3, hjust=.5, vjust=-.75) +
  labs(x = "opportunities",
       y = "fantasypoints_pg",
       caption = "Data from airyards",
       title = "opportunities and fantasypoints_pg",
       subtitle = "2019") +
  theme_bw() +
  #geom_hline(yintercept = 0.5, color = "green", linetype = "dashed") +
  #geom_vline(xintercept = 10, color = "blue", linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE) +
  annotate(x=.3, y=17, 
           label=paste("R = ", round(cor(rb_totals_filtered$opportunities, rb_totals_filtered$fantasypoints_pg,use = "complete.obs"),2)), 
           geom="text", size=5)

opportunitiesvfantasy


