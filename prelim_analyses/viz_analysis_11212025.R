## Viz Experiment - Clean + Analysis

## R version 4.5.1 (2025-06-13 ucrt) -- "Great Square Root"

##### Setup ######
rm(list=ls())
library(tidyverse)
library(magrittr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(psych)
library(here)
library(dplyr)
library(wesanderson)
mycols <- wes_palette("Darjeeling1", n = 5)

#viz_data <- read.csv(here("Data", "viz_data_10302025.csv"))
setwd("C:/Users/steph/Documents/Courses/PhD/Research Projects/AI (Kennedy)")
viz_data <- read.csv("viz_data_10302025.csv")
#

##### Cleanup ######

## removing those who didn't finish
viz_data <- viz_data %>% 
  filter(Finished == 1)

## removing those who finished in under 3 min
viz_data <- viz_data %>% 
  filter(Duration..in.seconds. >= 180) # alter as needed

## removing unnecessary vars
viz_data <- viz_data %>%
  dplyr::select(-c(1:24, Create.New.Field.or.Choose.From.Dropdown..., PROLIFIC_PID)) 

## replace empty strings with NA
viz_data <- viz_data %>%
  mutate(across(everything(), ~na_if(., "")))


## coalesce firing choice vars and cleaning
viz_data <- viz_data %>%
  mutate(fire_authorization = coalesce(!!!dplyr::select(., starts_with("firing_")))) %>%
  dplyr::select(-starts_with("firing_"))

viz_data <- viz_data %>%
  mutate(fire_authorization = str_replace_all(fire_authorization, "\\n", " "), 
         fire_authorization = str_squish(fire_authorization))

viz_data <- viz_data %>%
  mutate(fire_authorization_num = case_when(
    str_detect(fire_authorization, "Do not") ~ 0,
    str_detect(fire_authorization, "object A$|object B$") ~ 1,
    str_detect(fire_authorization, "objects A and B") ~ 2))

## rename randomization vars
viz_data <- viz_data %>%
  rename(dashboard = FL_19_DO) %>% 
  mutate(false_positive_flag = recode(FL_11_DO,
                                      "FalsePositive" = "False Positive",
                                      "NoFalsePositive" = "No False Positive")) %>% 
  dplyr::select(-FL_11_DO)

## recode answers to numeric
likert_map <- c("Strongly disagree" = 1,
                "Disagree" = 2,
                "Neither agree nor disagree" = 3,
                "Agree" = 4,
                "Strongly agree" = 5)

viz_data <- viz_data %>%
  mutate(across(1:40, 
                ~as.numeric(likert_map[.x]), 
                .names = "{.col}_num"))

table(viz_data$accuracy_1_num) # checking
table(viz_data$accuracy_1)

rm(likert_map)

## assign random ids
set.seed(98465) 
viz_data <- viz_data %>%
  mutate(rid = sample(1:n(), n(), replace = FALSE))

## checking
table(viz_data$dashboard)
sapply(viz_data[41:80], function(x) sum(is.na(x)))


## Create question/answer database
accuracy_answers <- viz_data %>%
  dplyr::select(rid, accuracy_1:transparency_2) %>%
  pivot_longer(!rid, names_to = "question_category", values_to = "response")

accuracy_questions <- viz_data %>%
  dplyr::select(rid, accuracy_1.1:user_interface_2.1) %>%
  pivot_longer(!rid, names_to = "question_category", values_to = "question_wording") %>%
  mutate(question_category = case_when(question_category == "accuracy_1.1" ~ "accuracy_1",
                                      question_category == "accuracy_2.1" ~ "accuracy_2",
                                      question_category == "other_1.1" ~ "other_1",
                                      question_category == "other_2.1" ~ "other_2",
                                      question_category == "behavioral_general_1" ~ "beh_gen_1",
                                      question_category == "behavioral_general_2" ~ "beh_gen_2",
                                      question_category == "cognitive_disposition_1" ~ "cog_disp_1",
                                      question_category == "cognitive_disposition_2" ~ "cog_disp_2",
                                      question_category == "confidence_general_1" ~ "conf_gen_1",
                                      question_category == "confidence_general_2" ~ "conf_gen_2",
                                      question_category == "costs_stakes_1.1" ~ "costs_stakes_1",
                                      question_category == "costs_stakes_2.1" ~ "costs_stakes_2",
                                      question_category == "embedded_expertise_training_data_1" ~ "embed_exp_1",
                                      question_category == "embedded_expertise_training_data_2" ~ "embed_exp_2",
                                      question_category == "human_like_qualities_1" ~ "human_like_1",
                                      question_category == "human_like_qualities_2" ~ "human_like_2",
                                      question_category == "level_of_autonomy_1" ~ "autonomy_1",
                                      question_category == "level_of_autonomy_2" ~ "autonomy_2",
                                      question_category == "moral_agency_1.1" ~ "moral_agency_1",
                                      question_category == "moral_agency_2.1" ~ "moral_agency_2",
                                      question_category == "perceived_beneficence_1" ~ "beneficence_1",
                                      question_category == "perceived_beneficence_2" ~ "beneficence_2",
                                      question_category == "performance_1.1" ~ "performance_1",
                                      question_category == "performance_2.1" ~ "performance_2",
                                      question_category == "personal_beliefs_1.1" ~ "personal_beliefs_1",
                                      question_category == "personal_beliefs_2.1" ~ "personal_beliefs_2",
                                      question_category == "sense_of_collaboration_1" ~ "collab_1",
                                      question_category == "sense_of_collaboration_2" ~ "collab_2",
                                      question_category == "sense_of_predictability_1" ~ "predictability_1",
                                      question_category == "sense_of_predictability_2" ~ "predictability_2",
                                      question_category == "transparency_1.1" ~ "transparency_1",
                                      question_category == "transparency_2.1" ~ "transparency_2",
                                      question_category == "trust_general_1" ~ "trust_gen_1",
                                      question_category == "trust_general_2" ~ "trust_gen_2",
                                      question_category == "user_control_1.1" ~ "user_control_1",
                                      question_category == "user_control_2.1" ~ "user_control_2",
                                      question_category == "user_expertise_1.1" ~ "user_expertise_1",
                                      question_category == "user_expertise_2.1" ~ "user_expertise_2",
                                      question_category == "user_interface_1.1" ~ "user_interface_1",
                                      question_category == "user_interface_2.1" ~ "user_interface_2"))

question_data <- accuracy_questions %>%
  group_by(question_wording) %>%
  summarize(n = n()) %>%
  rownames_to_column(var = "qid") %>%
  mutate(qid = paste0("q", qid))

full_question_data <- accuracy_questions %>%
  inner_join(accuracy_answers, by = c("rid", "question_category")) %>%
  left_join(question_data, by = c("question_wording"))  %>%
  mutate(response_num = case_when(response == "Strongly disagree" ~ 1,
                                  response == "Disagree" ~ 2,
                                  response == "Neither agree nor disagree" ~ 3,
                                  response == "Agree" ~ 4,
                                  response == "Strongly agree" ~ 5)) %>%
  dplyr::select(rid, qid, response_num) %>%
  pivot_wider(names_from = qid, values_from = response_num) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), mean(., na.rm = TRUE)))

#
###### PCA #####

# Get PCA results
pca_results <- principal(full_question_data[,-1], nfactors = 3, rotate = "varimax", scores = TRUE)

# Print PCA results
print(pca_results)

# Eigenvalues (variance explained by each component)
print(pca_results$values)

# Access specific elements of the results, for example:
# Loadings (correlations between variables and components)
print(pca_results$loadings)

# Plot a scree plot to visualize eigenvalues
plot(pca_results$values, type = "b", main = "Scree Plot",
     xlab = "Component Number", ylab = "Eigenvalue")

# Visualization - scree
pca_t <- tibble(
  component = seq_along(pca_results$values),
  values = pca_results$values
)

p1 <- pca_t %>% 
  ggplot(aes(x = component, y = values)) + 
  geom_line(color = mycols[2]) + 
  geom_point(shape = 21, 
             size = 3, 
             fill = mycols[2], 
             color = "grey70") +
  scale_x_continuous(breaks = seq(0, 350, by = 50)) +
  scale_y_continuous(breaks = seq(0, 16, by = 2)) +
  labs(
    title = "Scree Plot",
    x = "Component number",
    y = "Eigenvalue"
  ) + 
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.x = element_blank()
  )

# # Save the file
# ggsave(
#   p1, 
#   file = "scree-plot.png",
#   width = 8,
#   height = 6
# ) 

## Order loadings
loadings_matrix <- as.matrix(pca_results$loadings[, 1:3])

loadings_long <- data.frame(Question = rownames(loadings_matrix), loadings_matrix) %>%
  pivot_longer(
    cols = -Question,
    names_to = "Factor",
    values_to = "Loading")

## Rank loadings by factor
top_loadings_by_factor <- loadings_long %>%
  group_by(Factor) %>%
  arrange(desc(abs(Loading))) %>%
  ungroup()

## View top 10 per factor
top_ten <- top_loadings_by_factor %>%
  group_by(Factor) %>%
  slice_head(n = 10)

#
##### TOP 5 LOADINGS FOR REPORT #####
top_with_text <- top_loadings_by_factor %>%
  left_join(question_data, by = c("Question" = "qid")) %>%
  dplyr::select(Question, question_wording, Factor, Loading)

top_5_table <- top_with_text %>%
  group_by(Factor) %>%
  slice_max(order_by = abs(Loading), n = 5) %>%
  arrange(Factor, desc(abs(Loading)))


top_5_tibble <- top_5_table %>%
  mutate(Factor = gsub("RC", "", Factor)) %>% 
  as_tibble

gt1 <- gt(top_5_tibble) %>%
  data_color(
    columns = Factor,
    colors = mycols
  )

gtsave(gt1, filename = "pca_table.png")


##### REGRESSION #####
t1 <- cbind(full_question_data, pca_results$scores) %>% dplyr::select(rid, RC1, RC2, RC3)
t2 <- t1 %>% inner_join(viz_data, by = "rid") %>% dplyr::select(rid, RC1, RC2, RC3, dashboard, false_positive_flag)
t3 <- t2 %>% mutate(
  dashboard = dashboard %>% 
    factor(levels = c("Dash8", "Dash1", "Dash2", "Dash3", "Dash4", "Dash5", "Dash6", "Dash7")),
  false_positive_flag = false_positive_flag %>% factor(levels = c("False Positive", "No False Positive"))
) 

mod_rc1 <- lm(RC1 ~ dashboard + false_positive_flag, data = t3)
summary(mod_rc1)
mod_rc2 <- lm(RC2 ~ dashboard + false_positive_flag, data = t3)
summary(mod_rc2)
mod_rc3 <- lm(RC3 ~ dashboard + false_positive_flag, data = t3)
summary(mod_rc3)

reg_1 <- modelsummary(
  list("Factor 1" = mod_rc1, "Factor 2" = mod_rc2, "Factor 3" = mod_rc3),
  fmt = 3, # for formatting e.g. 3 decimal places
  output ="html",
  vcov = "HC1",
  statistic = "std.error",
  coef_map = c(`dashboardDash1` = "Dashboard1",
               `dashboardDash2` = "Dashboard2",
               `dashboardDash3` = "Dashboard3",
               `dashboardDash4` = "Dashboard4",
               `dashboardDash5` = "Dashboard5",
               `dashboardDash6` = "Dashboard6",
               `dashboardDash7` = "Dashboard7",
               `false_positive_flagNo False Positive` = "No False Positive"),
  stars = TRUE,
  gof_omit = "R2|AIC|BIC|Log.Lik.|F|Std.Errors",
  title = "",
  notes = "Using robust standard errors"
) %>% 
  style_tt(i = 15:16, j = 1:4, background = mycols[2], color = "white", bold = TRUE)

save_tt(reg_1, "viz_ols_colored.html")
webshot("viz_ols_colored.html", file = "viz_ols_colored.png", zoom = 3)


###### For Kylie: Export values for database #####
loadings_matrix <- as.matrix(pca_results$loadings)

max_loadings <- t(apply(loadings_matrix, 1, function(x) {
  factor_num <- which.max(abs(x))
  loading_value <- x[factor_num] 
  eigenvalue <- pca_results$values[factor_num]
  c(factor_num, loading_value, eigenvalue)
  }))

max_loadings_df <- as.data.frame(max_loadings)
colnames(max_loadings_df) <- c("Factor", "Loading", "Eigenvalue")

## Add question names
max_loadings_df <- max_loadings_df %>%
  mutate(Question = rownames(loadings_matrix),
         Factor = as.integer(Factor),
         Loading = as.numeric(Loading),
         Eigenvalue = as.numeric(Eigenvalue)) %>%
  select(Question, Factor, Loading, Eigenvalue)


## q names
max_loadings_df <- max_loadings_df %>%
  left_join(question_data %>% select(qid, question_wording),
            by = c("Question" = "qid"))


# Export to CSV
#write.csv(max_loadings_df, "question_factor_loadings.csv", row.names = FALSE)

#