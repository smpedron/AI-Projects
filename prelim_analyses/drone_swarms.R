##############################
# Analysis of drone swarm experiment
##############################

# Load libraries ----
library(tidyverse)
library(here)

# Load data ----
drone_data <- read_csv(here("Data", "drone_swarm.csv"))

# Create main variables ----
drone_data2 <- drone_data %>%
  mutate(predicta = coalesce(predict_5a_1, predict_15a_1, predict_35a_1),
         predictb = coalesce(predict_5b_1, predict_15b_1, predict_35b_1),
         predictc = coalesce(predict_5c_1, predict_15c_1, predict_35c_1),
         predictd = coalesce(predict_5d_1, predict_15d_1, predict_35d_1),
         predicte = coalesce(predict_5e_1, predict_15e_1, predict_35e_1),
         predictf = coalesce(predict_5f_1, predict_15f_1, predict_35f_1),
         trusta = coalesce(trust_5a, trust_15a, trust_35a),
         trustb = coalesce(trust_5b, trust_15b, trust_35b),
         trustc = coalesce(trust_5c, trust_15c, trust_35c),
         trustd = coalesce(trust_5d, trust_15d, trust_35d),
         truste = coalesce(trust_5e, trust_15e, trust_35e),
         trustf = coalesce(trust_5f, trust_15f, trust_35f),
         accuracya = coalesce(accuracy_5a, accuracy_15a, accuracy_35a),
         accuracyb = coalesce(accuracy_5b, accuracy_15b, accuracy_35b),
         accuracyc = coalesce(accuracy_5c, accuracy_15c, accuracy_35c),
         accuracyd = coalesce(accuracy_5d, accuracy_15d, accuracy_35d),
         accuracye = coalesce(accuracy_5e, accuracy_15e, accuracy_35e),
         accuracyf = coalesce(accuracy_5f, accuracy_15f, accuracy_35f),
         true_error = p_error * 100,
         predict_errora = predicta - true_error,
         predict_errorb = predictb - true_error,
         predict_errorc = predictc - true_error,
         predict_errord = predictd - true_error,
         predict_errore = predicte - true_error,
         predict_errorf = predictf - true_error,
         n_swarms_fac = factor(n_swarms, levels = c(5, 15, 35)),
         p_error_fac = factor(p_error, levels = c(0.01, 0.05, 0.1, 0.2)),
         trusta_num = case_when(trusta == "Not at all" ~ 0, trusta == "A little" ~ 1, trusta == "A moderate amount" ~ 2, trusta == "A lot" ~ 3, trusta == "A great deal" ~ 4),
         trustb_num = case_when(trustb == "Not at all" ~ 0, trustb == "A little" ~ 1, trustb == "A moderate amount" ~ 2, trustb == "A lot" ~ 3, trustb == "A great deal" ~ 4),
         trustc_num = case_when(trustc == "Not at all" ~ 0, trustc == "A little" ~ 1, trustc == "A moderate amount" ~ 2, trustc == "A lot" ~ 3, trustc == "A great deal" ~ 4),
         trustd_num = case_when(trustd == "Not at all" ~ 0, trustd == "A little" ~ 1, trustd == "A moderate amount" ~ 2, trustd == "A lot" ~ 3, trustd == "A great deal" ~ 4),
         truste_num = case_when(truste == "Not at all" ~ 0, truste == "A little" ~ 1, truste == "A moderate amount" ~ 2, truste == "A lot" ~ 3, truste == "A great deal" ~ 4),
         trustf_num = case_when(trustf == "Not at all" ~ 0, trustf == "A little" ~ 1, trustf == "A moderate amount" ~ 2, trustf == "A lot" ~ 3, trustf == "A great deal" ~ 4),
         acc_erra = ifelse(accuracya != "human observers", 1, ifelse(!is.na(accuracya), 0, NA)),
         acc_errb = ifelse(accuracyb != "human observers", 1, ifelse(!is.na(accuracyb), 0, NA)),
         acc_errc = ifelse(accuracyc != "human observers", 1, ifelse(!is.na(accuracyc), 0, NA)),
         acc_errd = ifelse(accuracyd != "human observers", 1, ifelse(!is.na(accuracyd), 0, NA)),
         acc_erre = ifelse(accuracye != "human observers", 1, ifelse(!is.na(accuracye), 0, NA)),
         acc_errf = ifelse(accuracyf != "human observers", 1, ifelse(!is.na(accuracyf), 0, NA)),
         acc_dronea = ifelse(accuracya == "drones themselves", 1, ifelse(!is.na(accuracya), 0, NA)),
         acc_droneb = ifelse(accuracyb == "drones themselves", 1, ifelse(!is.na(accuracyb), 0, NA)),
         acc_dronec = ifelse(accuracyc == "drones themselves", 1, ifelse(!is.na(accuracyc), 0, NA)),
         acc_droned = ifelse(accuracyd == "drones themselves", 1, ifelse(!is.na(accuracyd), 0, NA)),
         acc_dronee = ifelse(accuracye == "drones themselves", 1, ifelse(!is.na(accuracye), 0, NA)),
         acc_dronef = ifelse(accuracyf == "drones themselves", 1, ifelse(!is.na(accuracyf), 0, NA)))


library(MASS)

predict_model <- lm(predict_errorf ~ n_swarms_fac + p_error_fac, data = drone_data2)
summary(predict_model)
predict_sim <- MASS::mvrnorm(n = 1000, mu = coef(predict_model), Sigma = vcov(predict_model)) %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(names(coef(predict_model))) %>%
  mutate(draw = row_number()) %>%
  relocate(draw) %>%
  mutate(N5 = `(Intercept)`,
         P0.01 = `(Intercept)`,
         N15 = `(Intercept)` + n_swarms_fac15,
         N35 = `(Intercept)` + n_swarms_fac35,
         P0.05 = `(Intercept)` + p_error_fac0.05,
         P0.1 = `(Intercept)` + p_error_fac0.1,
         P0.2 = `(Intercept)` + p_error_fac0.2) %>%
  dplyr::select(draw, N5, N15, N35, P0.01, P0.05, P0.1, P0.2) %>%
  pivot_longer(
    cols = -draw,
    names_to  = "term",
    values_to = "value"
  ) %>%
  group_by(term) %>%
  summarize(Estimate = mean(value),
            SE = sd(value)) %>%
  mutate(EstHi = Estimate + 2*SE,
         EstLo = Estimate - 2*SE,
         term = factor(term, levels = c("N5", "N15", "N35", "P0.01", "P0.05", "P0.1", "P0.2"))) %>%
  ggplot() +
  geom_point(aes(x = term, y = Estimate)) +
  geom_errorbar(aes(x = term, ymin = EstLo, ymax = EstHi)) +
  theme_bw() + coord_flip() +
  labs(x = "Condition", y = "Estimate", title = "(A) Direct effect of error probability and number of swarms \nmonitored on final accuracy of evaluations")
predict_sim

trust_model <- lm(trustf_num ~ n_swarms_fac + p_error_fac, data = drone_data2)
summary(trust_model)
trust_sim <- MASS::mvrnorm(n = 1000, mu = coef(trust_model), Sigma = vcov(trust_model)) %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(names(coef(trust_model))) %>%
  mutate(draw = row_number()) %>%
  relocate(draw) %>%
  mutate(N5 = `(Intercept)`,
         P0.01 = `(Intercept)`,
         N15 = `(Intercept)` + n_swarms_fac15,
         N35 = `(Intercept)` + n_swarms_fac35,
         P0.05 = `(Intercept)` + p_error_fac0.05,
         P0.1 = `(Intercept)` + p_error_fac0.1,
         P0.2 = `(Intercept)` + p_error_fac0.2) %>%
  dplyr::select(draw, N5, N15, N35, P0.01, P0.05, P0.1, P0.2) %>%
  pivot_longer(
    cols = -draw,
    names_to  = "term",
    values_to = "value"
  ) %>%
  group_by(term) %>%
  summarize(Estimate = mean(value),
            SE = sd(value)) %>%
  mutate(EstHi = Estimate + 2*SE,
         EstLo = Estimate - 2*SE,
         term = factor(term, levels = c("N5", "N15", "N35", "P0.01", "P0.05", "P0.1", "P0.2"))) %>%
  ggplot() +
  geom_point(aes(x = term, y = Estimate)) +
  geom_errorbar(aes(x = term, ymin = EstLo, ymax = EstHi)) +
  theme_bw() + coord_flip() +
  labs(x = "Condition", y = "Estimate", title = "(B) Direct effect of error probability and number of swarms \nmonitored on final trust in swarms")
trust_sim

acc_err_model <- lm(acc_errf ~ n_swarms_fac + p_error_fac, data = drone_data2)
summary(acc_err_model)
accerr_sim <- MASS::mvrnorm(n = 1000, mu = coef(acc_err_model), Sigma = vcov(acc_err_model)) %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(names(coef(acc_err_model))) %>%
  mutate(draw = row_number()) %>%
  relocate(draw) %>%
  mutate(N5 = `(Intercept)`,
         P0.01 = `(Intercept)`,
         N15 = `(Intercept)` + n_swarms_fac15,
         N35 = `(Intercept)` + n_swarms_fac35,
         P0.05 = `(Intercept)` + p_error_fac0.05,
         P0.1 = `(Intercept)` + p_error_fac0.1,
         P0.2 = `(Intercept)` + p_error_fac0.2) %>%
  dplyr::select(draw, N5, N15, N35, P0.01, P0.05, P0.1, P0.2) %>%
  pivot_longer(
    cols = -draw,
    names_to  = "term",
    values_to = "value"
  ) %>%
  group_by(term) %>%
  summarize(Estimate = mean(value),
            SE = sd(value)) %>%
  mutate(EstHi = Estimate + 2*SE,
         EstLo = Estimate - 2*SE,
         term = factor(term, levels = c("N5", "N15", "N35", "P0.01", "P0.05", "P0.1", "P0.2"))) %>%
  ggplot() +
  geom_point(aes(x = term, y = Estimate)) +
  geom_errorbar(aes(x = term, ymin = EstLo, ymax = EstHi)) +
  theme_bw() + coord_flip() +
  labs(x = "Condition", y = "Estimate", title = "(C) Direct effect of error probability and number of swarms \nmonitored on final incorrect attribution")
accerr_sim

acc_drone_model <- lm(acc_dronef ~ n_swarms_fac + p_error_fac, data = drone_data2)
summary(acc_drone_model)
accdrone_sim <- MASS::mvrnorm(n = 1000, mu = coef(acc_drone_model), Sigma = vcov(acc_drone_model)) %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(names(coef(acc_drone_model))) %>%
  mutate(draw = row_number()) %>%
  relocate(draw) %>%
  mutate(N5 = `(Intercept)`,
         P0.01 = `(Intercept)`,
         N15 = `(Intercept)` + n_swarms_fac15,
         N35 = `(Intercept)` + n_swarms_fac35,
         P0.05 = `(Intercept)` + p_error_fac0.05,
         P0.1 = `(Intercept)` + p_error_fac0.1,
         P0.2 = `(Intercept)` + p_error_fac0.2) %>%
  dplyr::select(draw, N5, N15, N35, P0.01, P0.05, P0.1, P0.2) %>%
  pivot_longer(
    cols = -draw,
    names_to  = "term",
    values_to = "value"
  ) %>%
  group_by(term) %>%
  summarize(Estimate = mean(value),
            SE = sd(value)) %>%
  mutate(EstHi = Estimate + 2*SE,
         EstLo = Estimate - 2*SE,
         term = factor(term, levels = c("N5", "N15", "N35", "P0.01", "P0.05", "P0.1", "P0.2"))) %>%
  ggplot() +
  geom_point(aes(x = term, y = Estimate)) +
  geom_errorbar(aes(x = term, ymin = EstLo, ymax = EstHi)) +
  theme_bw() + coord_flip() +
  labs(x = "Condition", y = "Estimate", title = "(D) Direct effect of error probability and number of swarms \nmonitored on final incorrect attribution to drones")
accdrone_sim


predict_model_i <- lm(predict_errorf ~ n_swarms_fac * p_error_fac, data = drone_data2)
summary(predict_model_i)
predict_sim_i <- MASS::mvrnorm(n = 1000, mu = coef(predict_model_i), Sigma = vcov(predict_model_i)) %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(names(coef(predict_model_i))) %>%
  mutate(draw = row_number()) %>%
  relocate(draw) %>%
  mutate(N5xP0.01 = `(Intercept)`,
         N15xP0.01 = `(Intercept)` + n_swarms_fac15,
         N35xP0.01 = `(Intercept)` + n_swarms_fac35,
         N5xP0.05 = `(Intercept)` + p_error_fac0.05,
         N5xP0.1 = `(Intercept)` + p_error_fac0.1,
         N5xP0.2 = `(Intercept)` + p_error_fac0.2,
         N15xP0.05 = `(Intercept)` + n_swarms_fac15 + p_error_fac0.05 + `n_swarms_fac15:p_error_fac0.05`,
         N35xP0.05 = `(Intercept)` + n_swarms_fac35 + p_error_fac0.05 + `n_swarms_fac35:p_error_fac0.05`,
         N15xP0.1 = `(Intercept)` + n_swarms_fac15 + p_error_fac0.1 + `n_swarms_fac15:p_error_fac0.1`,
         N15xP0.2 = `(Intercept)` + n_swarms_fac15 + p_error_fac0.2 + `n_swarms_fac15:p_error_fac0.2`,
         N35xP0.1 = `(Intercept)` + n_swarms_fac35 + p_error_fac0.1 + `n_swarms_fac35:p_error_fac0.1`,
         N35xP0.2 = `(Intercept)` + n_swarms_fac35 + p_error_fac0.2 + `n_swarms_fac35:p_error_fac0.2`) %>%
  dplyr::select(draw, N5xP0.01, N5xP0.05, N5xP0.1, N5xP0.2, N15xP0.01, N15xP0.05, N15xP0.1, N15xP0.2,
                N35xP0.01, N35xP0.05, N35xP0.1, N35xP0.2) %>%
  pivot_longer(
    cols = -draw,
    names_to  = "term",
    values_to = "value"
  ) %>%
  group_by(term) %>%
  summarize(Estimate = mean(value),
            SE = sd(value)) %>%
  mutate(EstHi = Estimate + 2*SE,
         EstLo = Estimate - 2*SE) %>%
  separate(term, into = c("Number", "Probability"), sep = "x") %>%
  mutate(Number = factor(Number, levels = c("N5", "N15", "N35")),
         Probability = factor(Probability, levels = c("P0.01", "P0.05", "P0.1", "P0.2"))) %>%
  ggplot() +
  geom_point(aes(x = Probability, y = Estimate, color = Number)) +
  geom_errorbar(aes(x = Probability, ymin = EstLo, ymax = EstHi, color = Number)) +
  theme_bw() + coord_flip() +
  labs(x = "Error Probability", y = "Estimate", color = "N Examples", title = "(E) Interaction effect of error probability and number of swarms \nmonitored on final accuracy of evaluations")
predict_sim_i

trust_model_i <- lm(trustf_num ~ n_swarms_fac * p_error_fac, data = drone_data2)
summary(trust_model_i)
trust_sim_i <- MASS::mvrnorm(n = 1000, mu = coef(trust_model_i), Sigma = vcov(trust_model_i)) %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(names(coef(trust_model_i))) %>%
  mutate(draw = row_number()) %>%
  relocate(draw) %>%
  mutate(N5xP0.01 = `(Intercept)`,
         N15xP0.01 = `(Intercept)` + n_swarms_fac15,
         N35xP0.01 = `(Intercept)` + n_swarms_fac35,
         N5xP0.05 = `(Intercept)` + p_error_fac0.05,
         N5xP0.1 = `(Intercept)` + p_error_fac0.1,
         N5xP0.2 = `(Intercept)` + p_error_fac0.2,
         N15xP0.05 = `(Intercept)` + n_swarms_fac15 + p_error_fac0.05 + `n_swarms_fac15:p_error_fac0.05`,
         N35xP0.05 = `(Intercept)` + n_swarms_fac35 + p_error_fac0.05 + `n_swarms_fac35:p_error_fac0.05`,
         N15xP0.1 = `(Intercept)` + n_swarms_fac15 + p_error_fac0.1 + `n_swarms_fac15:p_error_fac0.1`,
         N15xP0.2 = `(Intercept)` + n_swarms_fac15 + p_error_fac0.2 + `n_swarms_fac15:p_error_fac0.2`,
         N35xP0.1 = `(Intercept)` + n_swarms_fac35 + p_error_fac0.1 + `n_swarms_fac35:p_error_fac0.1`,
         N35xP0.2 = `(Intercept)` + n_swarms_fac35 + p_error_fac0.2 + `n_swarms_fac35:p_error_fac0.2`) %>%
  dplyr::select(draw, N5xP0.01, N5xP0.05, N5xP0.1, N5xP0.2, N15xP0.01, N15xP0.05, N15xP0.1, N15xP0.2,
                N35xP0.01, N35xP0.05, N35xP0.1, N35xP0.2) %>%
  pivot_longer(
    cols = -draw,
    names_to  = "term",
    values_to = "value"
  ) %>%
  group_by(term) %>%
  summarize(Estimate = mean(value),
            SE = sd(value)) %>%
  mutate(EstHi = Estimate + 2*SE,
         EstLo = Estimate - 2*SE) %>%
  separate(term, into = c("Number", "Probability"), sep = "x") %>%
  mutate(Number = factor(Number, levels = c("N5", "N15", "N35")),
         Probability = factor(Probability, levels = c("P0.01", "P0.05", "P0.1", "P0.2"))) %>%
  ggplot() +
  geom_point(aes(x = Probability, y = Estimate, color = Number)) +
  geom_errorbar(aes(x = Probability, ymin = EstLo, ymax = EstHi, color = Number)) +
  theme_bw() + coord_flip() +
  labs(x = "Error Probability", y = "Estimate", color = "N Examples", title = "(F) Interaction effect of error probability and number of swarms \nmonitored on final trust in swarms")
trust_sim_i


acc_err_model_i <- lm(acc_errf ~ n_swarms_fac * p_error_fac, data = drone_data2)
summary(acc_err_model_i)
accerr_sim_i <- MASS::mvrnorm(n = 1000, mu = coef(acc_err_model_i), Sigma = vcov(acc_err_model_i)) %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(names(coef(acc_err_model_i))) %>%
  mutate(draw = row_number()) %>%
  relocate(draw) %>%
  mutate(N5xP0.01 = `(Intercept)`,
         N15xP0.01 = `(Intercept)` + n_swarms_fac15,
         N35xP0.01 = `(Intercept)` + n_swarms_fac35,
         N5xP0.05 = `(Intercept)` + p_error_fac0.05,
         N5xP0.1 = `(Intercept)` + p_error_fac0.1,
         N5xP0.2 = `(Intercept)` + p_error_fac0.2,
         N15xP0.05 = `(Intercept)` + n_swarms_fac15 + p_error_fac0.05 + `n_swarms_fac15:p_error_fac0.05`,
         N35xP0.05 = `(Intercept)` + n_swarms_fac35 + p_error_fac0.05 + `n_swarms_fac35:p_error_fac0.05`,
         N15xP0.1 = `(Intercept)` + n_swarms_fac15 + p_error_fac0.1 + `n_swarms_fac15:p_error_fac0.1`,
         N15xP0.2 = `(Intercept)` + n_swarms_fac15 + p_error_fac0.2 + `n_swarms_fac15:p_error_fac0.2`,
         N35xP0.1 = `(Intercept)` + n_swarms_fac35 + p_error_fac0.1 + `n_swarms_fac35:p_error_fac0.1`,
         N35xP0.2 = `(Intercept)` + n_swarms_fac35 + p_error_fac0.2 + `n_swarms_fac35:p_error_fac0.2`) %>%
  dplyr::select(draw, N5xP0.01, N5xP0.05, N5xP0.1, N5xP0.2, N15xP0.01, N15xP0.05, N15xP0.1, N15xP0.2,
                N35xP0.01, N35xP0.05, N35xP0.1, N35xP0.2) %>%
  pivot_longer(
    cols = -draw,
    names_to  = "term",
    values_to = "value"
  ) %>%
  group_by(term) %>%
  summarize(Estimate = mean(value),
            SE = sd(value)) %>%
  mutate(EstHi = Estimate + 2*SE,
         EstLo = Estimate - 2*SE) %>%
  separate(term, into = c("Number", "Probability"), sep = "x") %>%
  mutate(Number = factor(Number, levels = c("N5", "N15", "N35")),
         Probability = factor(Probability, levels = c("P0.01", "P0.05", "P0.1", "P0.2"))) %>%
  ggplot() +
  geom_point(aes(x = Probability, y = Estimate, color = Number)) +
  geom_errorbar(aes(x = Probability, ymin = EstLo, ymax = EstHi, color = Number)) +
  theme_bw() + coord_flip() +
  labs(x = "Error Probability", y = "Estimate", color = "N Examples", title = "(G) Interaction effect of error probability and number of swarms \nmonitored on final incorrect attribution")
accerr_sim_i

acc_drone_model_i <- lm(acc_dronef ~ n_swarms_fac * p_error_fac, data = drone_data2)
summary(acc_drone_model_i)
accdrone_sim_i <- MASS::mvrnorm(n = 1000, mu = coef(acc_drone_model_i), Sigma = vcov(acc_drone_model_i)) %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(names(coef(acc_drone_model_i))) %>%
  mutate(draw = row_number()) %>%
  relocate(draw) %>%
  mutate(N5xP0.01 = `(Intercept)`,
         N15xP0.01 = `(Intercept)` + n_swarms_fac15,
         N35xP0.01 = `(Intercept)` + n_swarms_fac35,
         N5xP0.05 = `(Intercept)` + p_error_fac0.05,
         N5xP0.1 = `(Intercept)` + p_error_fac0.1,
         N5xP0.2 = `(Intercept)` + p_error_fac0.2,
         N15xP0.05 = `(Intercept)` + n_swarms_fac15 + p_error_fac0.05 + `n_swarms_fac15:p_error_fac0.05`,
         N35xP0.05 = `(Intercept)` + n_swarms_fac35 + p_error_fac0.05 + `n_swarms_fac35:p_error_fac0.05`,
         N15xP0.1 = `(Intercept)` + n_swarms_fac15 + p_error_fac0.1 + `n_swarms_fac15:p_error_fac0.1`,
         N15xP0.2 = `(Intercept)` + n_swarms_fac15 + p_error_fac0.2 + `n_swarms_fac15:p_error_fac0.2`,
         N35xP0.1 = `(Intercept)` + n_swarms_fac35 + p_error_fac0.1 + `n_swarms_fac35:p_error_fac0.1`,
         N35xP0.2 = `(Intercept)` + n_swarms_fac35 + p_error_fac0.2 + `n_swarms_fac35:p_error_fac0.2`) %>%
  dplyr::select(draw, N5xP0.01, N5xP0.05, N5xP0.1, N5xP0.2, N15xP0.01, N15xP0.05, N15xP0.1, N15xP0.2,
                N35xP0.01, N35xP0.05, N35xP0.1, N35xP0.2) %>%
  pivot_longer(
    cols = -draw,
    names_to  = "term",
    values_to = "value"
  ) %>%
  group_by(term) %>%
  summarize(Estimate = mean(value),
            SE = sd(value)) %>%
  mutate(EstHi = Estimate + 2*SE,
         EstLo = Estimate - 2*SE) %>%
  separate(term, into = c("Number", "Probability"), sep = "x") %>%
  mutate(Number = factor(Number, levels = c("N5", "N15", "N35")),
         Probability = factor(Probability, levels = c("P0.01", "P0.05", "P0.1", "P0.2"))) %>%
  ggplot() +
  geom_point(aes(x = Probability, y = Estimate, color = Number)) +
  geom_errorbar(aes(x = Probability, ymin = EstLo, ymax = EstHi, color = Number)) +
  theme_bw() + coord_flip() +
  labs(x = "Error Probability", y = "Estimate", color = "N Examples", title = "(H) Interaction effect of error probability and number of swarms \nmonitored on final incorrect attribution to drones")
accdrone_sim_i

predict_model_a <- lm(predict_errora ~ n_swarms_fac + p_error_fac, data = drone_data2)
predict_sim_a <- MASS::mvrnorm(n = 1000, mu = coef(predict_model_a), Sigma = vcov(predict_model_a)) %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(names(coef(predict_model_a))) %>%
  mutate(draw = row_number()) %>%
  relocate(draw) %>%
  mutate(N5 = `(Intercept)`,
         P0.01 = `(Intercept)`,
         N15 = `(Intercept)` + n_swarms_fac15,
         N35 = `(Intercept)` + n_swarms_fac35,
         P0.05 = `(Intercept)` + p_error_fac0.05,
         P0.1 = `(Intercept)` + p_error_fac0.1,
         P0.2 = `(Intercept)` + p_error_fac0.2) %>%
  dplyr::select(draw, N5, N15, N35, P0.01, P0.05, P0.1, P0.2) %>%
  pivot_longer(
    cols = -draw,
    names_to  = "term",
    values_to = "value"
  ) %>%
  group_by(term) %>%
  summarize(Estimate = mean(value),
            SE = sd(value)) %>%
  mutate(EstHi = Estimate + 2*SE,
         EstLo = Estimate - 2*SE,
         term = factor(term, levels = c("N5", "N15", "N35", "P0.01", "P0.05", "P0.1", "P0.2")),
         trial = "Trial 1")

predict_model_b <- lm(predict_errorb ~ n_swarms_fac + p_error_fac, data = drone_data2)
predict_sim_b <- MASS::mvrnorm(n = 1000, mu = coef(predict_model_b), Sigma = vcov(predict_model_b)) %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(names(coef(predict_model_b))) %>%
  mutate(draw = row_number()) %>%
  relocate(draw) %>%
  mutate(N5 = `(Intercept)`,
         P0.01 = `(Intercept)`,
         N15 = `(Intercept)` + n_swarms_fac15,
         N35 = `(Intercept)` + n_swarms_fac35,
         P0.05 = `(Intercept)` + p_error_fac0.05,
         P0.1 = `(Intercept)` + p_error_fac0.1,
         P0.2 = `(Intercept)` + p_error_fac0.2) %>%
  dplyr::select(draw, N5, N15, N35, P0.01, P0.05, P0.1, P0.2) %>%
  pivot_longer(
    cols = -draw,
    names_to  = "term",
    values_to = "value"
  ) %>%
  group_by(term) %>%
  summarize(Estimate = mean(value),
            SE = sd(value)) %>%
  mutate(EstHi = Estimate + 2*SE,
         EstLo = Estimate - 2*SE,
         term = factor(term, levels = c("N5", "N15", "N35", "P0.01", "P0.05", "P0.1", "P0.2")),
         trial = "Trial 2")

predict_model_c <- lm(predict_errorc ~ n_swarms_fac + p_error_fac, data = drone_data2)
predict_sim_c <- MASS::mvrnorm(n = 1000, mu = coef(predict_model_c), Sigma = vcov(predict_model_c)) %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(names(coef(predict_model_c))) %>%
  mutate(draw = row_number()) %>%
  relocate(draw) %>%
  mutate(N5 = `(Intercept)`,
         P0.01 = `(Intercept)`,
         N15 = `(Intercept)` + n_swarms_fac15,
         N35 = `(Intercept)` + n_swarms_fac35,
         P0.05 = `(Intercept)` + p_error_fac0.05,
         P0.1 = `(Intercept)` + p_error_fac0.1,
         P0.2 = `(Intercept)` + p_error_fac0.2) %>%
  dplyr::select(draw, N5, N15, N35, P0.01, P0.05, P0.1, P0.2) %>%
  pivot_longer(
    cols = -draw,
    names_to  = "term",
    values_to = "value"
  ) %>%
  group_by(term) %>%
  summarize(Estimate = mean(value),
            SE = sd(value)) %>%
  mutate(EstHi = Estimate + 2*SE,
         EstLo = Estimate - 2*SE,
         term = factor(term, levels = c("N5", "N15", "N35", "P0.01", "P0.05", "P0.1", "P0.2")),
         trial = "Trial 3")

predict_model_d <- lm(predict_errorb ~ n_swarms_fac + p_error_fac, data = drone_data2)
predict_sim_d <- MASS::mvrnorm(n = 1000, mu = coef(predict_model_d), Sigma = vcov(predict_model_d)) %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(names(coef(predict_model_d))) %>%
  mutate(draw = row_number()) %>%
  relocate(draw) %>%
  mutate(N5 = `(Intercept)`,
         P0.01 = `(Intercept)`,
         N15 = `(Intercept)` + n_swarms_fac15,
         N35 = `(Intercept)` + n_swarms_fac35,
         P0.05 = `(Intercept)` + p_error_fac0.05,
         P0.1 = `(Intercept)` + p_error_fac0.1,
         P0.2 = `(Intercept)` + p_error_fac0.2) %>%
  dplyr::select(draw, N5, N15, N35, P0.01, P0.05, P0.1, P0.2) %>%
  pivot_longer(
    cols = -draw,
    names_to  = "term",
    values_to = "value"
  ) %>%
  group_by(term) %>%
  summarize(Estimate = mean(value),
            SE = sd(value)) %>%
  mutate(EstHi = Estimate + 2*SE,
         EstLo = Estimate - 2*SE,
         term = factor(term, levels = c("N5", "N15", "N35", "P0.01", "P0.05", "P0.1", "P0.2")),
         trial = "Trial 4")

predict_model_e <- lm(predict_errore ~ n_swarms_fac + p_error_fac, data = drone_data2)
predict_sim_e <- MASS::mvrnorm(n = 1000, mu = coef(predict_model_e), Sigma = vcov(predict_model_e)) %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(names(coef(predict_model_e))) %>%
  mutate(draw = row_number()) %>%
  relocate(draw) %>%
  mutate(N5 = `(Intercept)`,
         P0.01 = `(Intercept)`,
         N15 = `(Intercept)` + n_swarms_fac15,
         N35 = `(Intercept)` + n_swarms_fac35,
         P0.05 = `(Intercept)` + p_error_fac0.05,
         P0.1 = `(Intercept)` + p_error_fac0.1,
         P0.2 = `(Intercept)` + p_error_fac0.2) %>%
  dplyr::select(draw, N5, N15, N35, P0.01, P0.05, P0.1, P0.2) %>%
  pivot_longer(
    cols = -draw,
    names_to  = "term",
    values_to = "value"
  ) %>%
  group_by(term) %>%
  summarize(Estimate = mean(value),
            SE = sd(value)) %>%
  mutate(EstHi = Estimate + 2*SE,
         EstLo = Estimate - 2*SE,
         term = factor(term, levels = c("N5", "N15", "N35", "P0.01", "P0.05", "P0.1", "P0.2")),
         trial = "Trial 5")

predict_model_f <- lm(predict_errorf ~ n_swarms_fac + p_error_fac, data = drone_data2)
predict_sim_f <- MASS::mvrnorm(n = 1000, mu = coef(predict_model_f), Sigma = vcov(predict_model_f)) %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(names(coef(predict_model_f))) %>%
  mutate(draw = row_number()) %>%
  relocate(draw) %>%
  mutate(N5 = `(Intercept)`,
         P0.01 = `(Intercept)`,
         N15 = `(Intercept)` + n_swarms_fac15,
         N35 = `(Intercept)` + n_swarms_fac35,
         P0.05 = `(Intercept)` + p_error_fac0.05,
         P0.1 = `(Intercept)` + p_error_fac0.1,
         P0.2 = `(Intercept)` + p_error_fac0.2) %>%
  dplyr::select(draw, N5, N15, N35, P0.01, P0.05, P0.1, P0.2) %>%
  pivot_longer(
    cols = -draw,
    names_to  = "term",
    values_to = "value"
  ) %>%
  group_by(term) %>%
  summarize(Estimate = mean(value),
            SE = sd(value)) %>%
  mutate(EstHi = Estimate + 2*SE,
         EstLo = Estimate - 2*SE,
         term = factor(term, levels = c("N5", "N15", "N35", "P0.01", "P0.05", "P0.1", "P0.2")),
         trial = "Trial 6")

predict_ts <- bind_rows(predict_sim_a, predict_sim_b, predict_sim_c, predict_sim_d,
                        predict_sim_e, predict_sim_f)

ggplot(predict_ts) +
  geom_point(aes(x = trial, y = Estimate)) +
  geom_errorbar(aes(x = trial, ymin = EstLo, ymax = EstHi)) +
  theme_bw() + facet_wrap(~term) +
  labs(x = "Trial", y = "Expected error", title = "(I) Change in estimate accuracy across trials")
