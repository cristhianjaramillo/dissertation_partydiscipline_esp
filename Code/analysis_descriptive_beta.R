#### Packages to use ####

library(readxl)
library(gamlss)
library(pscl)
library(tidyverse)
library(margins)
library(broom)
library(ggpubr)
library(betareg)
library(plyr)

#### Data cleaning ####

base <- read_excel("../Final data/Final_dataset.xlsx", 
                   sheet = "FINAL_edited")

base <- base[,c(6,7,8,11,12,13,14,15,17,18)] #These are the columns needed for the regression

base <- as.data.frame(base)
names(base)

# Editing some of the variables

base$gender <-  factor(base$gender, levels = c(0,1), labels = c("Female", "Male"))
base$role <-  factor(base$role, levels = c(0,1), labels = c("No", "Yes"))
base$re_elected <-  factor(base$re_elected, levels = c(0,1), labels = c("No", "Yes"))
base$rule <-  factor(base$rule, levels = c(0,1), labels = c("No", "Yes"))

# This is only for the density plot below

base$party_affiliation_plot <-  factor(base$party_affiliation, levels = c(1,0), labels = c("Yes", "No"))
base$party_affiliation <-  factor(base$party_affiliation, levels = c(0,1), labels = c("No", "Yes"))

# Standarization

base_b <- base %>%
  mutate(pol_experience_std = as.numeric(scale(pol_experience)),
         number_parties_std = as.numeric(scale(number_parties)),
         age_std = as.numeric(scale(age)),
         prop_votes_std = as.numeric(scale(prop_votes)))

# Handling exact 1 values by subtracting a small constant

base_b$party_discipline_adjusted <- ifelse(base_b$party_discipline == 1, 1 - 0.001, base_b$party_discipline)

summary(base_b$party_discipline_adjusted)

#### Descriptive analysis ####

mu <- ddply(base_b, "party_affiliation_plot", summarise, grp.mean=mean(party_discipline))

# Creating ypos for the location of the points
base_b$ypos <- NA
base_b$ypos[base_b$party_affiliation== "No"] <- 0.03 
base_b$ypos[base_b$party_affiliation== "Yes"] <- 0.06

density_plot <- ggplot(base_b, aes(x=party_discipline, color=party_affiliation_plot)) +
  geom_density() +
  geom_point(data=base_b, aes(x= party_discipline, y= ypos, colour= party_affiliation_plot), alpha=0.5) +
  labs(
    x = "\nParty discipline",
    y = "\nDensity",
    color = "Party affiliation"
  ) +
  scale_x_continuous(expand = c(0,0), limits = c(0,1.01)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=party_affiliation_plot),
             linetype="dashed") +
  scale_color_brewer(palette="Set2") +
  theme(legend.position = "none") +
  theme_minimal()

ggsave(filename = "../Figures/density_plot.jpg",
       plot = density_plot,
       width = 10, height = 6, units = "in")

#### Fitting regression 1 ####

beta_model1 <- betareg(party_discipline_adjusted ~ 
                       party_affiliation, 
                       data = base_b)

summary(beta_model1)

beta_model1 <- tidy(beta_model1)

coefs_1 <- beta_model1 %>%
  filter(term != "(Intercept)") %>%
  filter(term != "(phi)") %>%
  mutate(lwr = estimate - (1.96 * std.error),
         upr = estimate + (1.96 * std.error)) %>%
  mutate(term = dplyr::recode(term,
                              `party_affiliationYes` = "Party affiliation (Yes)")) %>% 
  arrange(estimate) %>% 
  mutate(term = factor(term,levels = unique(term))) %>% 
  mutate(same_sign = ifelse(sign(lwr) == sign(upr), 1, 0)) %>% 
  mutate(model = "Model 1")

#### Fitting regression 2 ####

beta_model2 <- betareg(party_discipline_adjusted ~ 
                      party_affiliation +
                      gender, 
                      data = base_b)

summary(beta_model2)

beta_model2 <- tidy(beta_model2)

coefs_2 <- beta_model2 %>%
  filter(term != "(Intercept)") %>%
  filter(term != "(phi)") %>%
  mutate(lwr = estimate - (1.96 * std.error),
         upr = estimate + (1.96 * std.error)) %>%
  mutate(term = dplyr::recode(term,
                              `party_affiliationYes` = "Party affiliation (Yes)",
                              `genderMale` = "Gender (Male)")) %>% 
  arrange(estimate) %>% 
  mutate(term = factor(term,levels = unique(term))) %>% 
  mutate(same_sign = ifelse(sign(lwr) == sign(upr), 1, 0)) %>% 
  mutate(model = "Model 2")

#### Fitting regression 3 ####

beta_model3 <- betareg(party_discipline_adjusted ~ 
                         party_affiliation +
                         gender + 
                         re_elected, 
                       data = base_b)

summary(beta_model3)

beta_model3 <- tidy(beta_model3)

coefs_3 <- beta_model3 %>%
  filter(term != "(Intercept)") %>%
  filter(term != "(phi)") %>%
  mutate(lwr = estimate - (1.96 * std.error),
         upr = estimate + (1.96 * std.error)) %>%
  mutate(term = dplyr::recode(term,
                              `party_affiliationYes` = "Party affiliation (Yes)",
                              `genderMale` = "Gender (Male)",
                              `re_electedYes` = "Re-elected (Yes)")) %>% 
  arrange(estimate) %>% 
  mutate(term = factor(term,levels = unique(term))) %>% 
  mutate(same_sign = ifelse(sign(lwr) == sign(upr), 1, 0)) %>% 
  mutate(model = "Model 3")

#### Fitting regression 4 ####

beta_model4 <- betareg(party_discipline_adjusted ~ 
                         party_affiliation +
                         gender +
                         re_elected +
                         role, 
                       data = base_b)

summary(beta_model4)

beta_model4 <- tidy(beta_model4)

coefs_4 <- beta_model4 %>%
  filter(term != "(Intercept)") %>%
  filter(term != "(phi)") %>%
  mutate(lwr = estimate - (1.96 * std.error),
         upr = estimate + (1.96 * std.error)) %>%
  mutate(term = dplyr::recode(term,
                              `party_affiliationYes` = "Party affiliation (Yes)",
                              `genderMale` = "Gender (Male)",
                              `roleYes` = "Role (Yes)",
                              `re_electedYes` = "Re-elected (Yes)")) %>% 
  arrange(estimate) %>% 
  mutate(term = factor(term,levels = unique(term))) %>% 
  mutate(same_sign = ifelse(sign(lwr) == sign(upr), 1, 0)) %>% 
  mutate(model = "Model 4")

#### Fitting regression 5 ####

beta_model5 <- betareg(party_discipline_adjusted ~ 
                         party_affiliation +
                         gender + 
                         re_elected +
                         role +
                         pol_experience_std, 
                       data = base_b)

summary(beta_model5)

beta_model5 <- tidy(beta_model5)

coefs_5 <- beta_model5 %>%
  filter(term != "(Intercept)") %>%
  filter(term != "(phi)") %>%
  mutate(lwr = estimate - (1.96 * std.error),
         upr = estimate + (1.96 * std.error)) %>%
  mutate(term = dplyr::recode(term,
                              `party_affiliationYes` = "Party affiliation (Yes)",
                              `genderMale` = "Gender (Male)",
                              `roleYes` = "Role (Yes)",
                              `re_electedYes` = "Re-elected (Yes)",
                              `pol_experience_std` = "Political experience (Std)")) %>% 
  arrange(estimate) %>% 
  mutate(term = factor(term,levels = unique(term))) %>% 
  mutate(same_sign = ifelse(sign(lwr) == sign(upr), 1, 0)) %>% 
  mutate(model = "Model 5")

#### Fitting regression 6 ####

beta_model6 <- betareg(party_discipline_adjusted ~ 
                         party_affiliation +
                         gender + 
                         re_elected +
                         role +
                         pol_experience_std +
                         age_std, 
                       data = base_b)


summary(beta_model6)

beta_model6 <- tidy(beta_model6)

coefs_6 <- beta_model6 %>%
  filter(term != "(Intercept)") %>%
  filter(term != "(phi)") %>%
  mutate(lwr = estimate - (1.96 * std.error),
         upr = estimate + (1.96 * std.error)) %>%
  mutate(term = dplyr::recode(term,
                              `party_affiliationYes` = "Party affiliation (Yes)",
                              `genderMale` = "Gender (Male)",
                              `roleYes` = "Role (Yes)",
                              `re_electedYes` = "Re-elected (Yes)",
                              `pol_experience_std` = "Political experience (Std)",
                              `age_std` = "Age (Std)",)) %>% 
  arrange(estimate) %>% 
  mutate(term = factor(term,levels = unique(term))) %>% 
  mutate(same_sign = ifelse(sign(lwr) == sign(upr), 1, 0)) %>% 
  mutate(model = "Model 6")


#### Fitting regression 7 ####

beta_model7 <- betareg(party_discipline_adjusted ~ 
                         party_affiliation +
                         gender + 
                         re_elected +
                         role +
                         pol_experience_std +
                         age_std + 
                         prop_votes_std, 
                       data = base_b)

summary(beta_model7)

beta_model7 <- tidy(beta_model7)

coefs_7 <- beta_model7 %>%
  filter(term != "(Intercept)") %>%
  filter(term != "(phi)") %>%
  mutate(lwr = estimate - (1.96 * std.error),
         upr = estimate + (1.96 * std.error)) %>%
  mutate(term = dplyr::recode(term,
                              `party_affiliationYes` = "Party affiliation (Yes)",
                              `genderMale` = "Gender (Male)",
                              `roleYes` = "Role (Yes)",
                              `re_electedYes` = "Re-elected (Yes)",
                              `age_std` = "Age (Std)",
                              `prop_votes_std` = "Proportion of votes (Std)",
                              `pol_experience_std` = "Political experience (Std)")) %>% 
  arrange(estimate) %>% 
  mutate(term = factor(term,levels = unique(term))) %>% 
  mutate(same_sign = ifelse(sign(lwr) == sign(upr), 1, 0)) %>% 
  mutate(model = "Model 7")


#### Fitting regression 8 ####

beta_model8 <- betareg(party_discipline_adjusted ~ 
                         party_affiliation +
                         gender + 
                         re_elected +
                         role +
                         pol_experience_std +
                         age_std + 
                         prop_votes_std +
                         rule, 
                       data = base_b)

summary(beta_model8)

beta_model8 <- tidy(beta_model8)

coefs_8 <- beta_model8 %>%
  filter(term != "(Intercept)") %>%
  filter(term != "(phi)") %>%
  mutate(lwr = estimate - (1.96 * std.error),
         upr = estimate + (1.96 * std.error)) %>%
  mutate(term = dplyr::recode(term,
                              `party_affiliationYes` = "Party affiliation (Yes)",
                              `genderMale` = "Gender (Male)",
                              `roleYes` = "Role (Yes)",
                              `re_electedYes` = "Re-elected (Yes)",
                              `age_std` = "Age (Std)",
                              `prop_votes_std` = "Proportion of votes (Std)",
                              `ruleYes` = "Ruling party (Yes)",
                              `pol_experience_std` = "Political experience (Std)")) %>% 
  arrange(estimate) %>% 
  mutate(term = factor(term,levels = unique(term))) %>% 
  mutate(same_sign = ifelse(sign(lwr) == sign(upr), 1, 0)) %>% 
  mutate(model = "Model 8")

#### Graph model 8 ####

# The graph 8 has been selected given the AIC and BIC values in the "evaluation.R" file

coefs_8$term <- as.character(coefs_8$term)

desired_order <- c("Ruling party (Yes)",
                   "Proportion of votes (Std)",
                   "Age (Std)",
                   "Political experience (Std)",
                   "Role (Yes)",
                   "Re-elected (Yes)",
                   "Gender (Male)",
                   "Party affiliation (Yes)")

coefs_8$term <- factor(coefs_8$term, levels = desired_order)

plot_8 <- ggplot(coefs_8, 
       aes(x = term, y = estimate, ymin = lwr, ymax = upr)) +
  geom_pointrange(aes(alpha = same_sign, color = factor(same_sign)), 
                  size = 0.5, show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "red") +
  geom_text(aes(x = as.numeric(as.factor(term)) + 0.25, y = estimate, 
                label = paste0(round(estimate, 2), ifelse(same_sign == 1, "*", ""))),
            color = "black", size = 3, show.legend = FALSE) +
  facet_wrap(~ model, nrow = 1) +
  coord_flip() +
  scale_y_continuous("\nEstimates of Predictors on Adjusted Party Discipline") +
  scale_x_discrete("") +
  scale_alpha_continuous(range = c(0.3, 1)) +
  scale_color_manual(values = c("black", "black")) +
  theme(panel.background = element_blank(),
        # panel.grid.major = element_line(color = "gray", linetype = "dotted"),
        panel.border = element_rect(color = "black", fill = NA),
        axis.line = element_line(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(face = c(rep("plain", 7), rep("bold", 1))))

# Saving the plot as a JPG file
ggsave(filename = "../Figures/model8.jpg",
       plot = plot_8,
       width = 6, height = 5, units = "in")

#### Complete graph ####

# Creating a dataset for all variables

total <- rbind(coefs_1, coefs_2, coefs_3, coefs_4, coefs_5, coefs_6, coefs_7, coefs_8)

total$term <- as.character(total$term)

desired_order <- c("Ruling party (Yes)",
                   "Proportion of votes (Std)",
                   "Age (Std)",
                   "Political experience (Std)",
                   "Role (Yes)",
                   "Re-elected (Yes)",
                   "Gender (Male)",
                   "Party affiliation (Yes)")

total$term <- factor(total$term, levels = desired_order)

plot_full <- ggplot(total, 
       aes(x = term, y = estimate, ymin = lwr, ymax = upr)) +
  geom_pointrange(aes(alpha = same_sign, color = factor(same_sign)), 
                  size = 0.5, show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "red") +
  geom_text(aes(x = as.numeric(as.factor(term)) + 0.4, y = estimate, 
                label = paste0(round(estimate, 2), ifelse(same_sign == 1, "*", ""))),
            color = "black", size = 3, show.legend = FALSE) +
  facet_wrap(~ model, nrow = 2) +
  coord_flip() +
  scale_y_continuous("\nEstimates of Predictors on Adjusted Party Discipline") +
  scale_x_discrete("") +
  scale_alpha_continuous(range = c(0.3, 1)) +
  scale_color_manual(values = c("black", "black")) +
  theme(panel.background = element_blank(),
        # panel.grid.major = element_line(color = "gray", linetype = "dotted"),
        panel.border = element_rect(color = "black", fill = NA),
        axis.line = element_line(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_text(face = c(rep("plain", 7), rep("bold", 1))))

# Saving the plot as a JPG file
ggsave(filename = "../Figures/model_full.jpg",
       plot = plot_full,
       width = 9, height = 8, units = "in")
