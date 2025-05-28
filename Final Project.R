library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(car)
library(leaps)

#excluded advanced statistics and only included box score statistics

# Load the project data, excluding season 2025
project_data <- read_csv("C:/Users/almon/OneDrive - Centre College of Kentucky/Desktop/DSC 230/Final Project/project_data.csv") %>% 
  filter(season != 2025)

# Load current season data (2025)
current_season <- read_csv("C:/Users/almon/OneDrive - Centre College of Kentucky/Desktop/DSC 230/Final Project/project_data.csv") %>% 
  filter(season == 2025)

# Correlation matrix
cor_data <- project_data %>%
  select(where(is.numeric)) %>%
  select(where(~ sd(., na.rm = TRUE) != 0)) %>%
  select(-seas_id, -season, -player_id) %>%
  cor()
cor_data <- round(cor_data, 2)


# All-NBA & All-Defense eligibility criteria: games >= 65, minutes played >= 1260 (pre-2023 season)
# For 2023 season onward, the criteria changed to g >= 65 & mp >= 1260
# No criteria for All-Rookie



## All-NBA Model ####
null_ANBA = lm(all_nba ~ 1, 
               subset(project_data, g >= 65 & mp >= 1260))
full_ANBA = lm(all_nba ~ . -experience -all_defense -all_rookie -seas_id -player_id -season -player -tm, 
               subset(project_data, g >= 65 & mp >= 1260))

# Stepwise regression for All-NBA model
summary(step(null_ANBA, scope = list(upper = full_ANBA), direction = 'forward')) #forward
summary(step(full_ANBA, scope = list(lower = null_ANBA), direction = 'backward')) #backward
model_ANBA = step(null_ANBA, scope = list(lower = null_ANBA, upper = full_ANBA)) #both ways

summary(model_ANBA)
#crPlots(model_ANBA)
#residualPlots(model_ANBA)

## GLM() step
full_glm_ANBA = glm(all_nba ~ mp + x3p_per_100_poss + x2p_per_100_poss + x2p_percent + 
                 ft_percent +  trb_per_100_poss + ast_per_100_poss + blk_per_100_poss + 
                 shooting_foul_committed + shooting_foul_drawn + playoffs, #variables were switched in and out to find best fit
               subset = (
                 (season < 2023) | (season >= 2023 & g >= 65 & mp >= 1260)),
               data = project_data, family = 'binomial')

glm_step_ANBA <- step(full_glm, direction = "both")
summary(glm_step_ANBA)

##Testing different interactions and models for All-NBA
# summary(glm(all_nba ~ x2p_per_100_poss*x2p_percent + x3p_per_100_poss*x3p_percent,
#             subset = (
#               (season < 2023) | (season >= 2023 & g >= 65 & mp >= 1260)),
#             data = project_data, family = 'binomial'))
# test1 = glm(all_nba ~ mp + x3p_per_100_poss + x2p_per_100_poss + x2p_percent + 
#               ft_percent +  trb_per_100_poss + ast_per_100_poss + blk_per_100_poss + 
#               shooting_foul_committed + shooting_foul_drawn + playoffs,
#             subset(project_data, (season < 2023) | (season >= 2023 & g >= 65 & mp >= 1260)),
#             family = 'binomial')
# 
# test2 = glm(all_nba ~ mp + x3p_per_100_poss + x2p_per_100_poss*x2p_percent + 
#               ft_percent +  trb_per_100_poss + ast_per_100_poss + blk_per_100_poss + 
#               shooting_foul_committed + shooting_foul_drawn + playoffs,
#             subset(project_data, (season < 2023) | (season >= 2023 & g >= 65 & mp >= 1260)),
#             family = 'binomial')
# 
# 
# summary(test1)
# anova(test1, test2)

# Best Subsets Selection (Cp criterion) for All-NBA
reg_lm_ANBA <- regsubsets(
  all_nba ~ . -experience -all_defense -all_rookie -seas_id -player_id -season -player -tm -pts_per_100_poss -x2p_per_100_poss -x2p_percent -x3p_per_100_poss -x3p_percent, 
  data = project_data,
  subset = ((season < 2023) | (season >= 2023 & g >= 65 & mp >= 1260)),
  really.big = TRUE #dataset is LARGE
)
plot(reg_lm, scale = "Cp")


# Final All-NBA GLM model
final_ANBA = glm(all_nba ~ mp + x3p_per_100_poss + x2p_per_100_poss + x2p_percent + 
                   ft_percent +  trb_per_100_poss + ast_per_100_poss + blk_per_100_poss + 
                   shooting_foul_committed + shooting_foul_drawn + playoffs,
                 subset(project_data, (season < 2023) | (season >= 2023 & g >= 65 & mp >= 1260)),
                 family = 'binomial')
summary(final_ANBA)
crPlots(final_ANBA)
residualPlots(final_ANBA)




## All-Defense Model ####
# only using defensive stats

null_ADEF = lm(all_defense ~ 1, 
               subset(project_data, g >= 65 & mp >= 1260))
full_ADEF = lm(all_defense ~ (g + gs + mp + pos + playoffs + topconf + topdivision +
                 drb_per_100_poss + stl_per_100_poss + blk_per_100_poss + pf_per_100_poss +
                 shooting_foul_committed + offensive_foul_drawn)^2, 
               subset(project_data, g >= 65 & mp >= 1260))

# Stepwise regression for All-Defense model
summary(step(null_ADEF, scope = list(upper = full_ADEF), direction = 'forward')) #forward
summary(step(full_ADEF, scope = list(lower = null_ADEF), direction = 'backward')) #backward
summary(step(null_ADEF, scope = list(lower = null_ADEF, upper = full_ADEF))) #both ways
model_ADEF = glm(all_defense ~ blk_per_100_poss + mp + g + offensive_foul_drawn +
                  stl_per_100_poss + topdivision, 
                 subset(project_data, g >= 65 & mp >= 1260), family = 'binomial')
summary(model_ADEF)

##GLM() step
full_glm = glm(all_defense ~ gs + pos + playoffs + topconf + drb_per_100_poss + 
                 stl_per_100_poss + blk_per_100_poss + pf_per_100_poss +
                 shooting_foul_committed + offensive_foul_drawn + pos*drb_per_100_poss + 
                 pos*blk_per_100_poss + pos*pf_per_100_poss + pos*offensive_foul_drawn +
                 drb_per_100_poss*blk_per_100_poss + blk_per_100_poss*shooting_foul_committed +
                 blk_per_100_poss*pf_per_100_poss + blk_per_100_poss*offensive_foul_drawn, 
               subset = (
                 (season < 2023) | (season >= 2023 & g >= 65 & mp >= 1260)),
               data = project_data, family = 'binomial')

glm_step <- step(full_glm, direction = "both")

##Testing different interactions
summary(glm(all_defense ~ blk_per_100_poss*offensive_foul_drawn*pos,
            subset = (
              (season < 2023) | (season >= 2023 & g >= 65 & mp >= 1260)),
            data = project_data, family = 'binomial'))



##Testing different interactions and models for All-NBA
# These were attempts to find the best model based on summary stats and ANOVA
# test1 = glm(all_defense ~ gs + pos + playoffs + topconf + drb_per_100_poss + 
#               stl_per_100_poss + blk_per_100_poss + pf_per_100_poss +
#               shooting_foul_committed + offensive_foul_drawn, 
#             subset = (
#               (season < 2023) | (season >= 2023 & g >= 65 & mp >= 1260)),
#             data = project_data, family = 'binomial')
# test2 = glm(all_defense ~ gs + pos + playoffs + topconf + drb_per_100_poss + 
#               stl_per_100_poss + blk_per_100_poss + pf_per_100_poss +
#               shooting_foul_committed + offensive_foul_drawn + pos*drb_per_100_poss, 
#             subset = (
#               (season < 2023) | (season >= 2023 & g >= 65 & mp >= 1260)),
#             data = project_data, family = 'binomial')
# test3 = glm(all_defense ~ gs + playoffs + stl_per_100_poss + 
#               blk_per_100_poss * offensive_foul_drawn + blk_per_100_poss * pos, 
#             subset = (
#               (season < 2023) | (season >= 2023 & g >= 65 & mp >= 1260)),
#             data = project_data, family = 'binomial')
# test4 = glm(all_defense ~ gs + playoffs + stl_per_100_poss + 
#               blk_per_100_poss * offensive_foul_drawn + blk_per_100_poss * pos + offensive_foul_drawn * stl_per_100_poss, 
#             subset = (
#               (season < 2023) | (season >= 2023 & g >= 65 & mp >= 1260)),
#             data = project_data, family = 'binomial')
# test5 = glm(all_defense ~ gs + playoffs + stl_per_100_poss + 
#               blk_per_100_poss * offensive_foul_drawn + blk_per_100_poss * pos + pf_per_100_poss * pos, 
#             subset = (
#               (season < 2023) | (season >= 2023 & g >= 65 & mp >= 1260)),
#             data = project_data, family = 'binomial')
# 
# summary(test1)
# anova(test1, test2)

# Final All-Defense GLM model
final_ADEF = glm(all_defense ~ gs + pos + playoffs + drb_per_100_poss + 
                   stl_per_100_poss + blk_per_100_poss + pf_per_100_poss + shooting_foul_committed + 
                   offensive_foul_drawn + pos:drb_per_100_poss + pos:blk_per_100_poss + 
                   pos:pf_per_100_poss + pos:offensive_foul_drawn + drb_per_100_poss:blk_per_100_poss + 
                   blk_per_100_poss:shooting_foul_committed + blk_per_100_poss:offensive_foul_drawn, 
                 subset(project_data, (season < 2023) | (season >= 2023 & g >= 65 & mp >= 1260)),
                 family = 'binomial')
summary(final_ADEF)
crPlots(final_ADEF)
residualPlots(final_ADEF)




## All-Rookie Model ####
null_ARKY = lm(all_rookie ~ 1, 
               subset(project_data, experience == 1))
full_ARKY = lm(all_rookie ~ . -experience -all_nba -all_defense -seas_id -player_id -season -player, 
               subset(project_data, experience == 1))

# Stepwise regression for All-Rookie model
summary(step(null_ARKY, scope = list(upper = full_ARKY), direction = 'forward')) #forward
summary(step(full_ARKY, scope = list(lower = null_ARKY), direction = 'backward')) #backward
model_ARKY = step(null_ARKY, scope = list(lower = null_ARKY, upper = full_ARKY)) #both ways
summary(model_ARKY)

# Best Subsets Selection (Cp criterion) for All-Rookie
reg_lm <- regsubsets(
  all_rookie ~ . -experience -all_defense -all_nba -seas_id -player_id -season -player -tm,
  data = project_data,
  really.big = TRUE
)
summary(reg_lm)
plot(reg_lm, scale = "Cp")


##GLM() step
full_glm = glm(all_rookie ~ mp + fg_per_100_poss + offensive_foul_drawn +
                 playoffs + blk_per_100_poss*pf_per_100_poss, 
               subset(project_data, experience == 1), family = 'binomial')

glm_step <- step(full_glm, direction = "both")
summary(glm_step)


##Testing different interactions and models for All-NBA
# test1 = glm(all_rookie ~ mp + fg_per_100_poss +
#               playoffs + blk_per_100_poss*pf_per_100_poss,
#             subset(project_data, experience == 1), family = 'binomial')
# 
# test2 = glm(all_rookie ~ mp + fg_per_100_poss + offensive_foul_drawn +
#               playoffs + blk_per_100_poss*pf_per_100_poss,
#             subset(project_data, experience == 1), family = 'binomial')
# 
# test3 = glm(all_rookie ~ x2p_per_100_poss + x3p_per_100_poss + ft_per_100_poss, 
#             subset(project_data, experience == 1), family = 'binomial')
# 
# summary(test2)
# anova(test2, test1)


# Final All-Rookie GLM model
final_ARKY = glm(all_rookie ~ mp + fg_per_100_poss + playoffs + blk_per_100_poss*pf_per_100_poss,
                 subset(project_data, experience == 1), family = 'binomial')
summary(final_ARKY)
crPlots(final_ARKY)
residualPlots(final_ARKY)

## Predictions for Current Season (2025) ####

## All-Defense Prediction

# Filter eligible players for All-Defense (using 60 games and 1260 mp as eligibility criteria)
eligible_players_adef <- subset(current_season, g >= 60 & mp >= 1260)
eligible_players_adef$predicted_all_defense_prob <- predict(final_ADEF, eligible_players_adef, type = "response")

alldefense_predictions <- eligible_players_adef %>%
  arrange(desc(predicted_all_defense_prob)) %>%
  select(player, season, tm, pos, g, mp, predicted_all_defense_prob) %>%
  mutate(predicted_all_defense_prob = round(predicted_all_defense_prob * 100, 2))


## All-NBA Prediction

# Filter eligible players for All-Defense (using 60 games and 1260 mp as eligibility criteria)
eligible_players_anba <- subset(current_season, g >= 60 & mp >= 1260)
eligible_players_anba$predicted_all_nba_prob <- predict(final_ANBA, eligible_players_anba, type = "response")

allnba_predictions <- eligible_players_anba %>%
  arrange(desc(predicted_all_nba_prob)) %>%
  select(player, season, tm, pos, g, mp, predicted_all_nba_prob) %>%
  mutate(predicted_all_nba_prob = round(predicted_all_nba_prob * 100, 2))
  
  
## All-Rookie Prediction
eligible_players_arky <- subset(current_season, experience == 1)
eligible_players_arky$predicted_all_rookie_prob <- predict(final_ARKY, eligible_players_arky, type = "response")

# Arrange and format All-Rookie predictions
allrookie_predictions <- eligible_players_arky %>%
  arrange(desc(predicted_all_rookie_prob)) %>%
  select(player, season, tm, pos, g, mp, predicted_all_rookie_prob) %>%
  mutate(predicted_all_rookie_prob = round(predicted_all_rookie_prob * 100, 2))
