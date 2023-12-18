# Load packages
library(tidyverse)
library(dagitty)
library(ggdag)
library(ggthemr)
library(dplyr)
ggthemr("flat")
rm(list = ls())


# Load data
data <- readRDS("~/GitHub/cdsba-lucaspuebla93/Causal_Data_Science_Data/hospdd.rds")
print(data)

summary(data)



# Filter data for treated and control groups before and after treatment
treated_before <- data %>%
  filter(hospital <= 18, procedure == 0) %>%  # Before procedure for treated
  summarise(mean_satisfaction_before = mean(satis))

treated_after <- data %>%
  filter(hospital <= 18, procedure == 1) %>%  # After procedure for treated
  summarise(mean_satisfaction_after = mean(satis))

control_before <- data %>%
  filter(hospital > 18, procedure == 0) %>%  # Before procedure for control
  summarise(mean_satisfaction_before = mean(satis))

# Display the computed means
treated_before
treated_after
control_before



# Perform difference-in-differences analysis with fixed effects
model <- lm(satis ~ as.factor(procedure) * as.factor(hospital) + as.factor(month), data = data)

# View the summary of the regression results
summary(model)



