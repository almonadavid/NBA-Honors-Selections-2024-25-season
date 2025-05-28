library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(car)
library(leaps)


Per_100_Poss <- read_csv("C:/Users/almon/OneDrive - Centre College of Kentucky/Desktop/DSC 230/Final Project/NBA Stats (1947-present)/Per 100 Poss.csv")%>% 
  filter(season %in% c(2017, 2018, 2019, 2022, 2023, 2024, 2025)) %>% 
  mutate(
    seas_id = case_when(
      season == 2015 ~ 15,
      season == 2016 ~ 16,
      season == 2017 ~ 17,
      season == 2018 ~ 18,
      season == 2019 ~ 19,
      season == 2022 ~ 22,
      season == 2023 ~ 23,
      season == 2024 ~ 24,
      season == 2025 ~ 25,
      TRUE ~ seas_id
    )) %>% 
  select(-birth_year, -age, -experience)


End_of_Season_Teams <- read_csv("C:/Users/almon/OneDrive - Centre College of Kentucky/Desktop/DSC 230/Final Project/NBA Stats (1947-present)/End of Season Teams.csv")%>% 
  filter(season %in% c(2017, 2018, 2019, 2022, 2023, 2024, 2025)) %>% 
  mutate(
    seas_id = case_when(
      season == 2015 ~ 15,
      season == 2016 ~ 16,
      season == 2017 ~ 17,
      season == 2018 ~ 18,
      season == 2019 ~ 19,
      season == 2022 ~ 22,
      season == 2023 ~ 23,
      season == 2024 ~ 24,
      season == 2025 ~ 25,
      TRUE ~ seas_id
    ))

Player_Play_By_Play <- read_csv("C:/Users/almon/OneDrive - Centre College of Kentucky/Desktop/DSC 230/Final Project/NBA Stats (1947-present)/Player Play By Play.csv")%>% 
  filter(season %in% c(2017, 2018, 2019, 2022, 2023, 2024, 2025)) %>% 
  mutate(
    seas_id = case_when(
      season == 2015 ~ 15,
      season == 2016 ~ 16,
      season == 2017 ~ 17,
      season == 2018 ~ 18,
      season == 2019 ~ 19,
      season == 2022 ~ 22,
      season == 2023 ~ 23,
      season == 2024 ~ 24,
      season == 2025 ~ 25,
      TRUE ~ seas_id
    )) %>% 
  select(-season, -birth_year, -age, -g, -mp) #remove

All_Star_Selections <- read_csv("C:/Users/almon/OneDrive - Centre College of Kentucky/Desktop/DSC 230/Final Project/NBA Stats (1947-present)/All-Star Selections.csv") %>% 
  filter(season %in% c(2017, 2018, 2019, 2022, 2023, 2024, 2025)) %>% 
  mutate(
    seas_id = case_when(
      season == 2015 ~ 15,
      season == 2016 ~ 16,
      season == 2017 ~ 17,
      season == 2018 ~ 18,
      season == 2019 ~ 19,
      season == 2022 ~ 22,
      season == 2023 ~ 23,
      season == 2024 ~ 24,
      season == 2025 ~ 25,
      TRUE ~ season
    ))

Regular_Season <- read_csv("C:/Users/almon/OneDrive - Centre College of Kentucky/Desktop/DSC 230/Final Project/NBA Stats (1947-present)/Regular Season.csv") %>% 
  filter(season %in% c(2017, 2018, 2019, 2022, 2023, 2024, 2025)) %>% 
  mutate(
    seas_id = case_when(
      season == 2015 ~ 15,
      season == 2016 ~ 16,
      season == 2017 ~ 17,
      season == 2018 ~ 18,
      season == 2019 ~ 19,
      season == 2022 ~ 22,
      season == 2023 ~ 23,
      season == 2024 ~ 24,
      season == 2025 ~ 25,
      TRUE ~ season
    ))

# Summarise for traded players so each player had one column ##
aggregate_player_stats <- function(df, weight_var = "mp") {
  df <- df %>% filter(!is.na(seas_id), !is.na(player_id))
  
  traded_flag <- df %>%
    count(seas_id, player_id) %>%
    mutate(traded = as.integer(n > 1)) %>%
    select(seas_id, player_id, traded)
  
  # Save character columns to reattach
  char_info <- df %>%
    group_by(seas_id, player_id) %>%
    summarise(
      player = first(na.omit(player)),
      pos = first(na.omit(pos)),
      tm = first(na.omit(tm)),
      .groups = "drop"
    )
  
  numeric_cols <- df %>% select(where(is.numeric), -player_id, -seas_id)
  numeric_colnames <- names(numeric_cols)
  
  rate_stats <- grep("_percent|_rate|percent|per_game|per_36|_min|_rtg|_poss|pct_|ratio", 
                     numeric_colnames, ignore.case = TRUE, value = TRUE)
  count_stats <- setdiff(numeric_colnames, rate_stats)
  
  df_summary <- df %>%
    group_by(seas_id, player_id) %>%
    summarise(
      across(all_of(count_stats), ~sum(.x, na.rm = TRUE)),
      across(all_of(rate_stats), ~{
        w <- cur_data()[[weight_var]]
        x <- .x
        valid <- !is.na(x) & !is.na(w)
        if (any(valid)) weighted.mean(x[valid], w[valid], na.rm = TRUE) else NA_real_
      }),
      .groups = "drop"
    )
  
  df_summary %>%
    left_join(traded_flag, by = c("seas_id", "player_id")) %>%
    left_join(char_info, by = c("seas_id", "player_id"))
}



# Now apply the function to each dataset ##
Per_100_Poss <- aggregate_player_stats(Per_100_Poss, weight_var = "mp")
Player_Play_By_Play <- aggregate_player_stats(Player_Play_By_Play, weight_var = "mp") %>% select(-traded, -player, -pos, -tm)



# Get project data ##
project_data <- Per_100_Poss %>%
  left_join(Player_Play_By_Play, by = c("seas_id", "player_id")) %>%
  left_join(
    End_of_Season_Teams %>%
      select(seas_id, player_id, type),
    by = c("seas_id", "player_id")
  ) %>%
  left_join(
    Regular_Season %>% 
      select(season, tm, topconf, topdivision, playoffs),
    by = c("season" = "season", "tm" = "tm")
  ) %>% 
  mutate(
    all_nba = coalesce(if_else(type == "All-NBA", 1, 0),0),
    all_defense = coalesce(if_else(type == "All-Defense", 1, 0),0),
    all_rookie = coalesce(if_else(type == "All-Rookie", 1, 0),0)
  ) %>%
  select(-c(
    pg_percent, sg_percent, sf_percent, pf_percent, c_percent,
    on_court_plus_minus_per_100_poss, net_plus_minus_per_100_poss,
    fga_blocked, type, points_generated_by_assists,
    and1, o_rtg, d_rtg, traded, x2pa_per_100_poss,
    x3pa_per_100_poss, fga_per_100_poss, fta_per_100_poss
  )) %>%
  filter(season == 2025 | !if_any(everything(), is.na))
  
  

project_data <- project_data %>%
  group_by(player_id, season) %>%
  summarise(across(everything(), ~ if (is.numeric(.)) max(.) else first(.)), .groups = "drop")

write.csv(project_data, "project_data.csv", row.names = FALSE)
getwd() #check where file was saved
