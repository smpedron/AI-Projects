## Monotony Experiment

rm(list=ls())

library(stringr)
library(lubridate)
library(janitor)
library(dplyr)
library(magrittr)
library(tidyverse)
library(sandwich)
library(lmtest)
library(MASS)
library(glmmTMB)
library(gt)

setwd("C:/Users/steph/Documents/Courses/PhD/Research Projects/AI (Kennedy)")

monotony_data <- read.csv("monotony_data.csv")

#

##### Cleanup ######

monotony_data <- monotony_data %>% 
  filter(Finished == 1) %>% 
  filter(Status != "Survey Preview") %>% 
  filter(consent == "Yes, I consent") %>% 
  filter(mobile_disqualifier == "No")

monotony_data <- monotony_data %>% 
  dplyr::select(-c(1:25, PROLIFIC_PID)) 

table(monotony_data$Q14_Click.Count) # short scenario
table(monotony_data$Q15_Click.Count) # long scenario
table(monotony_data$Q18_Click.Count) # zombie attacks simulation


monotony_data <- monotony_data %>%
  mutate(
    t_zombieattack = ifelse(t_zombieattack == "Fire Sector 4", "Success", "Failure"),
    tutorial_result = case_when(
      FL_34_DO == "Tutorial-OutcomeAIMalfunction" ~ "AI Malfunction",
      FL_34_DO == "Tutorial-OutcomeAIWorked" ~ "AI Success",
      is.na(FL_34_DO) ~ "Failure",
      TRUE ~ "Failure"),
    sim_binary = ifelse(s_zombieattack %in% c("Fire Sector 1", "Fire Sector 3"), "Sim Success", "Sim Fail"),
    scenario_type = case_when(
      FL_25_DO == "ShortScenario" ~ "short",
      FL_25_DO == "LongScenario" ~ "long",
      TRUE ~ NA_character_),
    sim_ai_result = case_when(
      FL_48_DO == "SimulationOutcome-AIMalfunction" ~ "AI Malfunction",
      FL_48_DO == "SimulationOutcome-Success" ~ "AI Success",
      TRUE ~ NA_character_))

table(monotony_data$scenario_type, monotony_data$sim_binary) # checks
table(monotony_data$t_zombieattack, monotony_data$tutorial_result)

monotony_data$sim_binary_num <- ifelse(monotony_data$sim_binary == "Sim Success", 1,
                                       ifelse(monotony_data$sim_binary == "Sim Fail", 0, NA))

monotony_data$sim_result <- ifelse(!is.na(monotony_data$sim_ai_result) & monotony_data$sim_ai_result != "", 
                                    monotony_data$sim_ai_result, monotony_data$sim_binary)
monotony_data$sim_result <- factor(monotony_data$sim_result, levels = c("Sim Fail", "Sim Success", "AI Malfunction", "AI Success"))
table(monotony_data$sim_result, useNA = "always")


## fix variables
## ai trust (higher = more trust)
likert_map <- c("Strongly disagree" = 1,
                "Disagree" = 2,
                "Somewhat disagree" = 3,
                "Neither agree nor disagree" = 4,
                "Somewhat agree" = 5,
                "Agree" = 6,
                "Strongly agree" = 7)

monotony_data <- monotony_data %>%
  mutate(across(Q40_1:Q40_4, ~ as.numeric(likert_map[.x])))

monotony_data <- monotony_data %>%
  mutate(Q40_2 = 8 - Q40_2,
         Q40_3 = 8 - Q40_3)

monotony_data$ai_trust_comp <- rowMeans(monotony_data[, c("Q40_1", "Q40_2", "Q40_3", "Q40_4")], na.rm = TRUE)

##NfCNfJ (higher = more complex thinking)
# Map Likert labels to numeric
likert_map <- c("Extremely uncharacteristic" = 1, 
                "Uncharacteristic" = 2,
                "Neither characteristic nor uncharacteristic" = 3,
                "Characteristic" = 4,
                "Extremely characteristic" = 5)

monotony_data <- monotony_data %>%
  mutate(across(NfCNfJ_1:NfCNfJ_6, ~ as.numeric(likert_map[.x])))

monotony_data <- monotony_data %>%
  mutate(NfCNfJ_2 = 6 - NfCNfJ_2, 
         NfCNfJ_4 = 6 - NfCNfJ_4) 

monotony_data <- monotony_data %>%
  mutate(NfCNfJ_comp = rowMeans(dplyr::select(., NfCNfJ_1:NfCNfJ_6), na.rm = TRUE)) # composite


## Big 5
likert_map <- c("Disagree strongly" = 1,
                "Disagree a little" = 2,
                "Neither agree nor disagree" = 3,
                "Agree a little" = 4,
                "Agree strongly" = 5)

monotony_data <- monotony_data %>%
  mutate(across(Big5_1:Big5_10, ~ as.numeric(likert_map[.x])))
monotony_data <- monotony_data %>%
  mutate(Big5_1 = 6 - Big5_1,
         Big5_3 = 6 - Big5_3,
         Big5_5 = 6 - Big5_5,
         Big5_7 = 6 - Big5_7,
         Big5_9 = 6 - Big5_9)

monotony_data <- monotony_data %>%
  mutate(Big_Openness = rowMeans(dplyr::select(., Big5_5, Big5_10), na.rm = TRUE),
         Big_Conscientiousness = rowMeans(dplyr::select(., Big5_3, Big5_8), na.rm = TRUE),
         Big_Extraversion = rowMeans(dplyr::select(., Big5_1, Big5_6), na.rm = TRUE),
         Big_Agreeableness = rowMeans(dplyr::select(., Big5_2, Big5_7), na.rm = TRUE),
         Big_Emotional_Stability = rowMeans(dplyr::select(., Big5_4, Big5_9), na.rm = TRUE))


## need for cognition (higher = more need for cognition)
likert_map <- c("Extremely uncharacteristic" = 1,
                "Somewhat uncharacteristic" = 2,
                "Uncertain" = 3,
                "Somewhat characteristic" = 4,
                "Extremely characteristic" = 5)

monotony_data <- monotony_data %>%
  mutate(across(needforcognition_1:needforcognition_4, ~ as.numeric(likert_map[.x])))

monotony_data <- monotony_data %>%
  mutate(needforcognition_4 = 6 - needforcognition_4)

monotony_data <- monotony_data %>%
  mutate(needforcognition_comp = rowMeans(dplyr::select(., needforcognition_1:needforcognition_4), na.rm = TRUE))

## trustAutomation
likert_map <- c("Strongly disagree" = 1,
                "Disagree" = 2,
                "Somewhat disagree" = 3,
                "Neither agree nor disagree" = 4,
                "Somewhat agree" = 5,
                "Agree" = 6,
                "Strongly agree" = 7)
monotony_data <- monotony_data %>%
  mutate(across(trustAutomation_1:trustAutomation_8, ~ as.numeric(likert_map[.x])))
monotony_data <- monotony_data %>%
  mutate(trustAutomation_4 = 8 - trustAutomation_4,
         trustAutomation_5 = 8 - trustAutomation_5,
         trustAutomation_8 = 8 - trustAutomation_8)

monotony_data <- monotony_data %>%
  mutate(trustAutomation_comp = rowMeans(dplyr::select(., trustAutomation_1:trustAutomation_8), na.rm = TRUE))

## making clear the info useful variables
monotony_data$info_useful_ai <- monotony_data$info_useful_1
monotony_data$info_useful_human <- monotony_data$info_useful_2
monotony_data$info_useful_sentry <- monotony_data$info_useful_3

###### Regressions #####

## Trust in AI DV
model_aitrust <- lm(ai_trust_comp ~ sim_result + scenario_type + tutorial_result + NfCNfJ_comp + 
                    trustAutomation_comp + needforcognition_comp + Big_Openness + Big_Extraversion +
                    Big_Conscientiousness + Big_Agreeableness + Big_Emotional_Stability, monotony_data)

summary(model_aitrust)
coeftest(model_aitrust, vcov = vcovHC(model_aitrust, type = "HC1")) # robust SE

## Simulation firing time DV
model_firing_time <- lm(Q18_First.Click ~ scenario_type + tutorial_result + ai_trust_comp + NfCNfJ_comp + 
                          trustAutomation_comp + needforcognition_comp + Big_Openness + Big_Extraversion +
                          Big_Conscientiousness + Big_Agreeableness + Big_Emotional_Stability, monotony_data)
summary(model_firing_time)
coeftest(model_firing_time, vcov = vcovHC(model_firing_time, type = "HC1"))


## Click counter DV (zero inflated negative binom)
monotony_data <- monotony_data %>% 
  mutate(click_counter = coalesce(Q14_Click.Count, Q15_Click.Count))
model_click_counter <- glmmTMB(click_counter ~ scenario_type,
                               ziformula = ~ scenario_type, family = nbinom2, monotony_data)
summary(model_click_counter)
plogis(-2.55)
plogis(-2.55 + 0.9266)

## conditional model - short scenario produces fewer clicks
## zero-inflated model - short scenario people about 7% to 16.5% about 2.5x more likely to never click (2.5x cause intercept)



## descriptive: firing based on human feedback or AI feedback
table(monotony_data$scenario_type, monotony_data$s_zombieattack)

monotony_data %>%
  mutate(scenario_type = recode(scenario_type,
                           "long" = "Long Scenario",
                           "short" = "Short Scenario"),
         s_zombieattack = recode(as.character(s_zombieattack),
                            "0" = "No Selection",
                            "1" = "Selection")) %>%
  count(scenario_type, s_zombieattack) %>%
  group_by(scenario_type) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup() %>%
  gt(rowname_col = "scenario_type") %>%
  tab_header(title = "Selections by Scenario Type") %>%
  fmt_percent(columns = percent, decimals = 1) %>%
  cols_label(scenario_type = "Scenario Type",
             s_zombieattack = "Selection Outcome", n = "Count", percent = "Percent") %>%
  tab_spanner(label = "Results", columns = c(n, percent)) %>%
  tab_style(style = list(cell_text(weight = "bold")), locations = cells_column_labels(everything()))


monotony_data %>%
  mutate(scenario_type = recode(scenario_type,
                           "long" = "Long Scenario",
                           "short" = "Short Scenario"),
         s_zombieattack = recode(as.character(s_zombieattack),
                            "0" = "No Selection",
                            "1" = "Selection")) %>%
  count(scenario_type, s_zombieattack) %>%
  group_by(scenario_type) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup() %>%
  gt() %>%
  tab_row_group(group = "Long Scenario", rows = scenario_type == "Long Scenario") %>%
  tab_row_group(group = "Short Scenario",rows = scenario_type == "Short Scenario") %>%
  fmt_percent(columns = percent, decimals = 1) %>%
  cols_label( s_zombieattack = "Selection Outcome", n = "Count",percent = "Percent") %>%
  cols_hide(columns = "scenario_type") %>%
  tab_header(title = "Selections by Scenario Type") %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(everything()))



## tutorial result and where they chose to fire during zombie attack
table(monotony_data$tutorial_result, monotony_data$s_zombieattack)

monotony_data %>%
  mutate(tutorial_result = recode(as.character(tutorial_result),
                                  "Failure" = "Failure",
                                  "AI Malfunction" = "AI Malfunction",
                                  "AI Success" = "AI Success"),
         s_zombieattack = recode(as.character(s_zombieattack),
                                 "0" = "No Selection",
                                 "1" = "Selection")) %>%
  count(tutorial_result, s_zombieattack) %>%
  group_by(tutorial_result) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup() %>%
  gt() %>%
  tab_row_group(group = "AI Malfunction", rows = tutorial_result == "AI Malfunction") %>%
  tab_row_group(group = "AI Success", rows = tutorial_result == "AI Success") %>%
  tab_row_group(group = "Failure", rows = tutorial_result == "Failure") %>%
  fmt_percent(columns = percent, decimals = 1) %>%
  cols_label(s_zombieattack = "Selection Outcome", n = "Count",percent = "Percent") %>%
  cols_hide(columns = "tutorial_result") %>%
  tab_header(title = "Selections by Tutorial Result") %>%
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_column_labels(everything()))




