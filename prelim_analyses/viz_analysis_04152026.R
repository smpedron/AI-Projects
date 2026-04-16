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

setwd("C:/Users/steph/Documents/Courses/PhD/Research Projects/AI (Kennedy)")
viz_data <- read.csv("viz_data_04152026.csv")
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
  dplyr::select(-c(1:18, 20:24, PROLIFIC_PID, Q_RecaptchaStatus, Q_RecaptchaError)) 

## replace empty strings with NA
# viz_data <- viz_data %>%
#   mutate(across(everything(), ~na_if(., "")))
viz_data <- viz_data %>%
  mutate(across(where(is.character), ~na_if(., "")))

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

viz_data %>% colnames

## recode answers to numeric
likert_map <- c("Strongly disagree" = 1,
                "Disagree" = 2,
                "Neither agree nor disagree" = 3,
                "Agree" = 4,
                "Strongly agree" = 5)

viz_data <- viz_data %>% 
  mutate(across(2:41,
                ~as.numeric(dplyr::recode(.x, !!!likert_map)), .names = "{.col}_num"))


table(viz_data$question_1_num) # checking
table(viz_data$question_1)

rm(likert_map)

## assign random ids
set.seed(98465) 
viz_data <- viz_data %>%
  mutate(rid = sample(1:n(), n(), replace = FALSE))

## checking
table(viz_data$dashboard)
sapply(viz_data[42:81], function(x) sum(is.na(x)))

#
##### Create question/answer database #####

## fixing apostrophe unicode
viz_data <- viz_data %>%
  mutate(across(
    matches("^question_\\d+\\.1$"),
    ~ stringr::str_replace_all(.x, "â€™", "'")))

## categorical assingment
category_lookup <- bind_rows(
  
  tibble(category = "accuracy", question_wording = c(
    "The system provided me with the accurate assistance to complete the given task correctly.",
    "I believe the system shows real data.",
    "I trust the information provided by the system is accurate.",
    "The information presented was believable.",
    "The information presented was accurate.",
    "The information presented was biased."
  )),
  
  tibble(category = "other", question_wording = c(
    "The system can adapt its behavior based on prior events.",
    "The outcome of the system is certain.",
    "Carefully watching the system takes time away from more important or interesting things.",
    "I am familiar with the system."
  )),
  
  tibble(category = "beh_gen", question_wording = c(
    "I like using the system for decision making.",
    "I have a personal preference for making decisions with the system.",
    "I like working with the system.",
    "I wish the system weren't around.",
    "I dislike the system.",
    "I'm glad I have the option of using the system.",
    "Overall, I feel positively toward the system.",
    "It would be good to use the system in work, even if it is not compulsory.",
    "I would be voluntarily using the system.",
    "I will use the system with pleasure.",
    "If life were busy, I would let an automated system handle some tasks for me.",
    "I intend to be a heavy user of the system.",
    "If I had a challenging problem, I would want to use the system again.",
    "I would confidently act on the advice I was given by the system.",
    "I will use this system again.",
    "I will use this system frequently.",
    "I will tell my friends about this system.",
    "I am very likely to provide the system with the information it needs to better serve my needs.",
    "I would not want to speak with the system.",
    "I would recommend the system to others.",
    "I would quickly abandon using this system.",
    "The factors considered in the system's decision were important."
  )),
  
  tibble(category = "cog_disp", question_wording = c(
    "Even if I have no reason to expect the system will be able to solve a difficult problem, I still feel certain that it will.",
    "I know when I should trust the system.",
    "One should be careful with unfamiliar automated systems.",
    "I trust the country's legal system.",
    "I trust the country's police.",
    "I trust the country's political parties.",
    "I trust the country's politicians.",
    "I trust the federal government in Washington to do what is right.",
    "I trust people until they give me a reason not to trust them.",
    "I generally trust new acquaintances until they prove that they should not be trusted.",
    "It is easy for me to trust others.",
    "Even if I am uncertain, I will generally give others the benefit of the doubt.",
    "I tend to trust others even if I have little knowledge of them.",
    "Trusting another person is not difficult for me.",
    "My typical approach is to trust new acquaintances until they prove I should not trust them.",
    "I am seldom wary of others.",
    "I don't mind giving up control to others over matters which are essential to my future plans.",
    "I believe that people usually keep their promises.",
    "My tendency to trust others is high.",
    "Even when I have a lot to do, I am likely to watch automation carefully for errors.",
    "I believe that there could be negative consequences when using the system.",
    "I feel I must be cautious when using the system."
  )),
  
  tibble(category = "conf_gen", question_wording = c(
    "If I am not sure about a decision, I have faith that the system will provide the best solution.",
    "When the system gives unusual advice I am confident that the advice is correct.",
    "How much faith did you have in the system.",
    "I have confidence in the advice given by the system.",
    "I feel apprehensive about using the system.",
    "I am confident in the system.",
    "I would be comfortable giving the system complete responsibility for the completion of a project.",
    "I am confident in the system's decisions.",
    "I would feel confident using the information to make a decision.",
    "I am confident in the output generated by the system.",
    "I am confident in the system's capability."
  )),
  
  tibble(category = "costs_stakes", question_wording = c(
    "The system's actions will have a harmful or injurious outcome.",
    "The system is risky.",
    "It is risky to interact with the system."
  )),
  
  tibble(category = "embed_exp", question_wording = c(
    "The system has sound knowledge about this type of problem built into it.",
    "The system makes use of all the knowledge and information available to it to produce its solution to the problem.",
    "The system is skilled.",
    "The system is competent.",
    "Professionals are knowledgeable in their chosen field.",
    "The system is aware of the physical world (e.g., its user, its location, etc.).",
    "The system is aware of the virtual world (e.g., other applications, the Internet, data, etc.).",
    "The system has nothing to gain by not being knowledgeable when helping me.",
    "I agree with the system's decision."
  )),
  
  tibble(category = "human_like", question_wording = c(
    "The system is sincere.",
    "The system is genuine.",
    "The system is respectable.",
    "The system is candid.",
    "The system is authentic.",
    "The system is meticulous.",
    "I would characterize the system as honest.",
    "The system is able to speak like a human.",
    "The system can be happy.",
    "The system can feel love.",
    "The system can get upset at times.",
    "The system can get frustrated at times.",
    "The system can be friendly.",
    "The system can be respectful.",
    "The system can be funny.",
    "The system can be caring.",
    "I think the system respects people.",
    "I think the system is very responsible."
  )),
  
  tibble(category = "autonomy", question_wording = c(
    "The system is able to operate without my intervention.",
    "The system is able to set and pursue tasks by itself in anticipation of future user needs."
  )),
  
  tibble(category = "moral_agency", question_wording = c(
    "The system is principled.",
    "The system has integrity.",
    "The system is deceptive.",
    "The system behaves in an underhanded manner.",
    "The system is truthful.",
    "I think the system is fair.",
    "I would trust the system's decision more than a human's decision.",
    "Given the system's track record, I have no reservations about acting on it's output.",
    "The system is ethical.",
    "I think the system has a strong sense of justice."
  )),
  
  tibble(category = "beneficence", question_wording = c(
    "The developers take my well-being seriously.",
    "I believe that the system would act in my best interest.",
    "If I needed help, the system would do its best ot help me.",
    "The system is interested in my well-being, not just its own.",
    "I believe that the system is interested in understanding my needs and preferences.",
    "The system has nothing to gain by not caring about me.",
    "The system will not cause harm to its users.",
    "The system has nothing to gain by being dishonest in its interactions with me."
  )),
  
  tibble(category = "performance", question_wording = c(
    "The system always provides the advice I require to make my decision.",
    "The system performs reliably.",
    "The system responds the same way under the same conditions at different times.",
    "I can rely on the system to function properly.",
    "The system analyzes problems consistently.",
    "The system uses appropriate methods to reach decisions.",
    "The advice the system produces is as good as that which a highly competent person could produce.",
    "The system correctly uses the information I enter.",
    "How dependable was the system.",
    "The system helps me achieve my goals.",
    "The system performs consistently.",
    "The system performs as it should.",
    "I feel comfortable relying on the information provided by the system.",
    "I wish the system gave me more information.",
    "I think I could do a better job than the system.",
    "I am concerned the system is vulnerable to hacking.",
    "I believe the system is a competent performer.",
    "I can depend on the system.",
    "I can rely on the system to behave in consistent ways.",
    "I can rely on the system to do its best every time I take its advice.",
    "The system was simple to use.",
    "The system is reliable.",
    "The system is capable.",
    "The system is consistent.",
    "I would find the system useful in my job.",
    "Using the system enables me to accomplish tasks more quickly.",
    "Using the system increases my productivity.",
    "If I use the system I would have more chances for career advancement.",
    "I would find the system easy to use.",
    "A specific person (or group) is available for assistance with system difficulties.",
    "Inappropriate exploitation of system could lead to huge information loss.",
    "The system is capable of interpreting situations correctly.",
    "A system malfunction is likely.",
    "The system is capable of taking over complicated tasks.",
    "The system might make sporadic errors.",
    "Automated systems generally work well.",
    "The system provides security.",
    "When I have a lot to do, it makes sense to delegate a task to automation.",
    "Automation should be used to ease people's workload.",
    "Even if an automated aid can help me with a task, I should pay attention to its performance.",
    "Distractions and interruptions are less of a problem for me when I have an automated system to cover some of the work.",
    "I feel confident finding information in the system.",
    "I believe I would be able to rely on the system not to make my job more difficult by careless work.",
    "I believe that the system has all the functionalities I would expect from similar systems.",
    "If I use the system, I think I would be able to depend on it completely.",
    "The system can complete tasks quickly.",
    "The system can understand my commands.",
    "The system can find and process the necessary information for completing the tasks.",
    "The system is able to provide me with a useful answer.",
    "When an important issue arises, I would feel comfortable depending on the information provided by the system.",
    "I can always rely on the system in a tough situation.",
    "I would be willing to provide my social security number to the system if needed.",
    "Faced with a difficult situation, I would be willing to pay to access information from the system.",
    "I would be willing to provide credit card information to the system if needed.",
    "Overall, the system worked very well technically.",
    "The system is very reliable. I can count on it to be correct all the time.",
    "I feel safe that when I rely on the system I will get the right answers.",
    "The system is efficient in that it works very quickly.",
    "The system can perform tasks better than a novice human user.",
    "The system is flexible to interact with.",
    "The system understands privacy boundaries.",
    "I am comfortable sharing sensitive information with the system.",
    "I would rely on the facts in this system.",
    "I am sure that this system is maintaining a secure environment.",
    "I am confident that my anonymity is protected by this system.",
    "I think the system can complete its tasks qualifiedly.",
    "I believe that decisions made by AI systems are reliable.",
    "I can rely on the system to undertake a thorough analysis of the situation before providing me with information.",
    "The system is safe."
  )),
  
  tibble(category = "personal_beliefs", question_wording = c(
    "People who are important to me believe that I should use the system.",
    "Using the system is a good idea.",
    "People really care about the well-being of others.",
    "I think people generally try to back up their words with their actions.",
    "I believe that professional people do a good job at their work.",
    "People are sincerely concerned about the problems of others.",
    "People generally keep their promises.",
    "If automation is available to help me with something, it makes sense for me to pay more attention to my other tasks.",
    "Constantly monitoring an automated system's performance is a waste of time.",
    "I like using the system based on the similarity of my values and the societal values underlying its use.",
    "Most people are honest in describing their experience and abilities.",
    "Most people tell the truth about the limits of their knowledge.",
    "In general, I am worried about my privacy.",
    "I think that technological devices can help solve daily issues.",
    "It makes sense to use different email addresses for different situations.",
    "The probability of personal data (like credit card number, email address, online account information) misuse on the Internet is very high.",
    "Consumers have lost all control over how personal information is collected and used by companies.",
    "Most businesses handle the personal information they collect about consumers in a proper and confidential way.",
    "Existing laws and organizational practices provide a reasonable level of protection for consumer privacy today."
  )),
  
  tibble(category = "collab", question_wording = c(
    "I would feel a sense of loss if the system was unavailable and I could no longer use it.",
    "I feel a sense of attachment to using the system.",
    "I find the system suitable to my style of decision making."
  )),
  
  tibble(category = "predictability", question_wording = c(
    "I know what will happen the next time I use the system because I understand how it behaves.",
    "I am rarely surprised by how the system behaves.",
    "The system is predictable.",
    "The system reacts unpredictably.",
    "It's difficult to identify what the system will do next.",
    "The outcome of the system is unpredictable."
  )),
  
  tibble(category = "transparency", question_wording = c(
    "I understand how the system will assist me with decisions I have to make.",
    "It is easy to follow what the system does.",
    "I understand what the system should do.",
    "I understand how the system executes tasks.",
    "I understand the limitations of the system.",
    "I understand the capabilities of the system.",
    "The system state was always clear to me.",
    "I was able to understand why things happened.",
    "The system appropriately disclosed infromation.",
    "I feel like I understood the system's confidence.",
    "I understood the intentions of the system.",
    "I understood the actions of the system."
  )),
  
  tibble(category = "trust_gen", question_wording = c(
    "I believe advice from the system even when I don't know for certain that it is correct.",
    "When I am uncertain about a decision I believe the system rather than myself.",
    "I trust this system as a whole to carry out its task.",
    "I trust the system.",
    "To what extent do you believe you can trust the decisions the system will make?.",
    "How much did you trust the decisions made by the system?.",
    "The system is someone you can trust.",
    "I trust the system's ability to respond accurately.",
    "The developers are trustworthy.",
    "I'd rather trust the system than mistrust it.",
    "I am suspicious of the system's intent, action, or outputs.",
    "I am wary of the system.",
    "I would have no problem allowing the system to complete a task that I am fully capable of completing myself.",
    "I can trust the information presented to me by the system.",
    "I trust the system as a source of recommendations.",
    "I trust that this survey guarantees anonymity.",
    "I trust automated machine learning (Auto ML).",
    "I trust the AI in the scenario presented.",
    "At this moment, I trust the system to act appropriately.",
    "I trust the system's predictions for this case.",
    "I would trust this system in a real scenario.",
    "The information presented was trustworthy.",
    "I believe the information that the system provides me.",
    "I trust that the system makes correct decisions.",
    "I trust the decisions made by the system.",
    "Decisions made by AI systems are trustworthy.",
    "I trust this system's ability to obey norms in a principled way."
  )),
  
  tibble(category = "user_control", question_wording = c(
    "I was confident I could control the system.",
    "I wish I had more control over how the system executes tasks.",
    "I hesitate to use the system for fear of making mistakes I cannot correct.",
    "It's not usually necessary to pay much attention to automation when it is running.",
    "I would be willing to provide information like my name, address, and phone number to the system if needed.",
    "I would not share information with the system.",
    "I always change my browser settings to protect information about myself."
  )),
  
  tibble(category = "user_expertise", question_wording = c(
    "Although I may not know exactly how the system works, I know how to use it to make decisions about the problem.",
    "I recognize what I should do to get the advice I need from the system the next time I use it.",
    "It would be easy for me to become a skilful system exploitator.",
    "Learning to operate the system will be easy for me.",
    "People who influence my behaviour think that it would be easy for me to use the system.",
    "I have the reasons and resources necessary to use the system.",
    "I have the knowledge necessary to use the system.",
    "The system is somewhat intimidating to me.",
    "I already know similar systems.",
    "I have already used similar systems.",
    "I have the necessary skills for using the system.",
    "I am familiar with the topic or data this system presents.",
    "I can explain the difference between generative AI and predictive AI.",
    "I can explain the difference between supervised, unsupervised, and reinforcement learning.",
    "I am confident in explaining how to train deep learning algorithms.",
    "I can explain how dimensionality reduction (e.g., PCA or t-SNE) help in pattern recognition.",
    "I can clearly explain the challenges of overfitting in pattern recognition."
  )),
  
  tibble(category = "user_interface", question_wording = c(
    "My interaction with the system is clear and understandable.",
    "I trust the system's display.",
    "The system can communicate with me in an understandable manner.",
    "The system facilitates answering questions about the data.",
    "The system provides a new or better understanding of the data.",
    "The system provides opportunities for serendipitous discoveries.",
    "The system affords rapid parallel comprehension for efficient browsing.",
    "The system provides mechanisms for quickly seeking specific information.",
    "The system provides a big picture perspective of the data.",
    "The system provides an understanding of the data beyond individual data cases.",
    "The system helps avoid making incorrect inferences.",
    "The system facilitates learning more broadly about the domain of the data.",
    "The system helps understand data quality.",
    "The system provided the right amount of information.",
    "I understand what this system is trying to tell me.",
    "The information presented was complete.",
    "I believe that the system is a competent performer."
  )))

## Pivot question wording columns long (question1.1, question2.1, ...)
question_wording_long <- viz_data %>%
  select(rid, matches("^question_\\d+\\.1$")) %>%
  pivot_longer(cols = -rid,
               names_to  = "q_col",
               values_to = "question_wording") %>%
  mutate(q_index = as.integer(str_extract(q_col, "\\d+")))

## Pivot numeric response columns long (question_1_num, question_2_num, ...)
question_response_long <- viz_data %>%
  select(rid, matches("^question_\\d+_num$")) %>%
  pivot_longer(cols = -rid,
               names_to  = "q_col",
               values_to = "question_response") %>%
  mutate(q_index = as.integer(str_extract(q_col, "\\d+")))

## Join wording + responses, then map category by matching question text
full_question_data <- question_wording_long %>%
  inner_join(question_response_long, by = c("rid", "q_index")) %>%
  left_join(category_lookup, by = "question_wording") %>%
  mutate(qid = paste0("q", q_index))

## CHECK: flag any questions that didn't match
unmatched <- full_question_data %>%
  filter(is.na(category)) %>%
  distinct(q_index, question_wording)

if (nrow(unmatched) > 0) {
  message("WARNING: ", nrow(unmatched), " question(s) did not match the category lookup.")
  message("These may have minor differences in whitespace, punctuation, or encoding.")
  message("Compare carefully and fix in category_lookup above.")
  print(unmatched)
} else {
  message("All questions matched successfully.")
}

## CHECK: inspecting which questions landed in each category
check_category_questions <- function(cat_name) {
  full_question_data %>%
    filter(category == cat_name) %>%
    distinct(q_index, question_wording) %>%
    arrange(q_index)
}

check_category_questions("accuracy")
check_category_questions("trust_gen")
check_category_questions("transparency")

#
##### PARALLEL ANALYSIS #####

## collapsing to mean scores per category per respondent
category_scores <- full_question_data %>%
  filter(!is.na(question_response)) %>%
  group_by(rid, category) %>%
  summarise(score = mean(question_response, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = category, values_from = score)

## checking how many respondents are missing each category entirely
category_scores %>% 
  select(-rid) %>% 
  summarise(across(everything(), ~sum(is.na(.)))) %>% 
  pivot_longer(everything(), names_to = "category", values_to = "n_missing") %>%
  arrange(desc(n_missing))

## Parallel Analysis for number of components
fa.parallel(select(category_scores, -rid), fa = "pc")

## why's it only 2? Checking eigenvalues
ev <- eigen(cor(select(category_scores, -rid), use = "pairwise.complete.obs"))$values
ev # single strong latent structure, 2-3 weaker


#

##### PCA + SCREE #####

n_components <- 3  # should update based on parallel analysis output

## STEPH NOTE: I also ran 2, and 3 is more interpretable

pca_results <- principal(r = select(category_scores, -rid), nfactors = n_components, rotate = "varimax")

print(pca_results)
print(pca_results$values)
print(pca_results$loadings)


t1 <- category_scores %>%
  select(rid) %>%
  bind_cols(as.data.frame(pca_results$scores))

## STEPH NOTES:
# 1 - system trust and perforamnce
# 2 - general personal dispositions and beliefs
# 3 - user agency and control

## Scree plot
mycols <- c("#2C5F8A", "#D05538", "#3B8B4A")

pca_scree <- tibble(component = seq_along(pca_results$values),
                    eigenvalue = pca_results$values)

p_scree <- pca_scree %>%
  ggplot(aes(x = component, y = eigenvalue)) +
  geom_line(color = mycols[1]) +
  geom_point(shape = 21, size = 3, fill = mycols[1], color = "grey70") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey60") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(title = "Scree Plot", x = "Component number", y = "Eigenvalue") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5))

p_scree

#

##### Extracting loadings and throwing in a table #####

## extract loadings
loadings_df <- as.data.frame(unclass(pca_results$loadings)) %>%
  rownames_to_column("category")

## reshaping for table
loadings_long <- loadings_df %>%
  pivot_longer(cols      = -category,
               names_to  = "Factor",
               values_to = "Loading") %>%
  mutate(Factor = gsub("RC", "Factor ", Factor))

## TOP 3 CATEGORIES per factor — what is each factor capturing? (Alter number as needed)
top_3_table <- loadings_long %>%
  group_by(Factor) %>%
  slice_max(order_by = abs(Loading), # rank by absolute loading (captures neg too)
            n = 3, with_ties = FALSE) %>%
  arrange(Factor, desc(abs(Loading))) %>%
  mutate(Loading = round(Loading, 3)) %>%
  as_tibble()

top_3_table

## put together in a table
library(gt)

factor_loadings_table <- gt(top_3_table, groupname_col = "Factor") %>%
  tab_header(title = "Top 3 Loadings by Factor",
             subtitle = "Based on category-mean scores collapsed across respondents") %>%
  cols_label(category = "Category", Loading  = "Loading") %>%
  fmt_number(columns = Loading, decimals = 3) %>%
  data_color(columns = Loading,
             palette = c("#D05538", "white", "lightblue"),  # red = negative, blue = positive
             domain  = c(-1, 1)) %>%
  tab_options(row_group.font.weight = "bold")

factor_loadings_table


## ITEM SPECIFIC VERSION
get_top_items_per_factor <- function(factor_num, categories, n_top = 5) {
  
  ## Get item-level wide matrix for these categories
  cat_wide <- full_question_data %>%
    filter(category %in% categories, 
           !is.na(question_response), 
           !is.na(question_wording)) %>%
    select(rid, question_wording, question_response) %>%
    ## If a respondent saw multiple questions mapping to same wording, average them
    group_by(rid, question_wording) %>%
    summarise(question_response = mean(question_response, na.rm = TRUE), 
              .groups = "drop") %>%
    pivot_wider(names_from  = question_wording,
                values_from = question_response)
  
  cat_matrix <- cat_wide %>%
    select(-rid) %>%
    mutate(across(everything(), ~ replace(., is.na(.), mean(., na.rm = TRUE))))
  
  ## Single factor PCA — extracts the dominant dimension across these items
  pca <- principal(cat_matrix, nfactors = 1, rotate = "none")
  
  ## Extract loadings, join back to category info
  as.data.frame(unclass(pca$loadings)) %>%
    rownames_to_column("question_wording") %>%
    rename(Loading = PC1) %>%
    left_join(distinct(full_question_data, question_wording, category), 
              by = "question_wording") %>%
    mutate(Factor  = factor_num,
           Loading = round(Loading, 3)) %>%
    arrange(desc(abs(Loading))) %>%
    slice_head(n = n_top)
}

## Run for each factor
rc1_categories <- c("accuracy", "autonomy", "beh_gen", "beneficience", "conf_gen", "costs_stakes", "embed_exp", "moral_agency",
                    "performance", "trust_gen", "user_interface")
rc2_categories <- c("cog_disp", "collab", "personal_beliefs",  "user_expertise", "transparency", "human_like", "other")
rc3_categories <- c("user_control", "predictability", "autonomy")

top_items <- bind_rows(get_top_items_per_factor(1, rc1_categories),
                       get_top_items_per_factor(2, rc2_categories),
                       get_top_items_per_factor(3, rc3_categories))

## GT table 
gt_items <- top_items %>%
  select(question_wording, category, Factor, Loading) %>%
  mutate(Factor = as.character(Factor)) %>%
  gt() %>%
  tab_header(title = "Top 5 Item Loadings by Factor") %>%
  cols_label(question_wording = "Question Wording",
             category  = "Category",
             Factor = "Factor",
             Loading = "Loading") %>%
  fmt_number(columns = Loading, decimals = 3) %>%
  data_color(columns = Factor,
             palette = c("#D05538", "#2C8A6E", "#E8A020"),  # red=1, teal=2, gold=3
             domain  = c("1", "2", "3")) %>%
  data_color(columns = Loading,
             palette = c("#D05538", "white", "#2C5F8A"),
             domain  = c(-1, 1)) %>%
  tab_options(row_group.font.weight = "bold") %>%
  cols_width(Factor ~ px(60),
             Loading~ px(100),
             category ~ px(120),
    question_wording ~ px(400))

gt_items

#

##### REGRESSION #####

## STEPH NOTE:
## We have structurally missing data. I'm imputing mean since this respondent doesn't have information about this
## THIS DOES NOT ALTER PCA RESULTS (eigenvalues and loadings are the same), but this step is necessary for binding back scores for regression
cat_matrix <- category_scores %>% 
  select(-rid) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
sum(is.na(cat_matrix))  # confirm no NA
pca_results <- principal(r  = cat_matrix , nfactors = 3, rotate = "varimax", scores = TRUE) # add scores argument

## attaching pca scores
t1 <- category_scores %>%
  select(rid) %>%
  bind_cols(as.data.frame(pca_results$scores))

## join back to viz for experimental variables
t2 <- t1 %>%
  inner_join(viz_data, by = "rid") %>%
  dplyr::select(rid, RC1, RC2, RC3, dashboard, false_positive_flag)

## factoring experimental vars
t3 <- t2 %>%
  mutate(dashboard = factor(dashboard, levels = c("Dash8", "Dash1", "Dash2", "Dash3", "Dash4", "Dash5", "Dash6", "Dash7")),
    false_positive_flag = factor(false_positive_flag, levels = c("False Positive", "No False Positive")))

summary(t3$RC1)
summary(t3$RC2)
summary(t3$RC3)

## ols
mod_rc1 <- lm(RC1 ~ dashboard + false_positive_flag, data = t3)
mod_rc2 <- lm(RC2 ~ dashboard + false_positive_flag, data = t3)
mod_rc3 <- lm(RC3 ~ dashboard + false_positive_flag, data = t3)

summary(mod_rc1) # expected that dash 8 produces lowest system trust
summary(mod_rc2) # expected. general trust disposition is a stable individual difference. probably a good validation check honestly
summary(mod_rc3) # same story. dashboard design and false positives don't affect perceived user control. Sense of agency might be less about visualizatoin type and more individual differences or task

## CONC: experimental manipulations affect RC1 and nothing else. Good in the sense that RC2 and RC3 are moer stable constructs that I wouldn't expect to move anyway

## making a table
library(modelsummary)
library(tinytable)

regs <- modelsummary(
  list("Factor 1" = mod_rc1, "Factor 2" = mod_rc2, "Factor 3" = mod_rc3),
  fmt = 3,
  output = "html",
  vcov = "HC1",
  statistic = "std.error",
  coef_map  = c(`dashboardDash1` = "Dashboard 1",
                `dashboardDash2` = "Dashboard 2",
                `dashboardDash3` = "Dashboard 3",
                `dashboardDash4`= "Dashboard 4",
                `dashboardDash5` = "Dashboard 5",
                `dashboardDash6` = "Dashboard 6",
                `dashboardDash7` = "Dashboard 7",
                `false_positive_flagNo False Positive` = "No False Positive"),
  stars = TRUE,
  gof_omit = "R2|AIC|BIC|Log.Lik.|F|Std.Errors",
  title = "",
  notes = "All models use robust standard errors.") %>%
  style_tt(i = 15:16, j = 1:4, background = mycols[2], color = "white", bold = TRUE)

regs


#
