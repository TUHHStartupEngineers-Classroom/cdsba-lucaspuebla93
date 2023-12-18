# Load packages
library(tidyverse)
library(dagitty)
library(ggdag)
library(ggthemr)
ggthemr("flat")
rm(list = ls())


# Load data
data <- readRDS("~/GitHub/cdsba-lucaspuebla93/Causal_Data_Science_Data/membership.rds")
print(data)

summary(data)
cor_matrix <- cor(data)
print(cor_matrix)


# Define the DAG model with chosen relations and custom positions
custom_dag_model <- 'dag {
  "pre_avg_purch" [pos="0.25,0.75"]
  "age" [pos="0.75,0.75"]
  "card" [pos="0.75,0.25"]
  "avg_purch" [pos="0.25,0.25"]
  "pre_avg_purch" -> "avg_purch"
  "age" -> "avg_purch"
  "card" -> "avg_purch"
  "age" -> "pre_avg_purch"
  "age" -> "card"
}'

# Plot the updated custom DAG using ggdag
ggdag(custom_dag_model) +
  geom_dag_point(color = "blue") +
  geom_dag_text(color = "black") +
  geom_dag_edges(edge_color = "black")


########################################################################
# Compute the average outcome for the treatment group (premium membership)
avg_outcome_treatment <- colMeans(data[data$card == 1, "avg_purch"], na.rm = TRUE)
print(avg_outcome_treatment)

# Compute the average outcome for the control group (non-premium membership)
avg_outcome_control <- colMeans(data[data$card == 0, "avg_purch"], na.rm = TRUE)
print(avg_outcome_control)

# Compute the naive estimate of the Average Treatment Effect (ATE)
naive_ate <- avg_outcome_treatment - avg_outcome_control
print(naive_ate)
#########################################






# Coarsened Exact Matching
cem_model <- matchit(card ~ age + sex + pre_avg_purch, data = data, method = "cem")

# Estimate treatment effect using coarsened exact matching
cem_estimate <- summary(cem_model, method = "ATT")
print(cem_estimate)



# Nearest-Neighbor Matching
nn_model <- matchit(card ~ age + sex + pre_avg_purch, data = data, method = "nearest")

# Estimate treatment effect using nearest-neighbor matching
nn_estimate <- summary(nn_model, method = "ATT")
print(nn_estimate)




# Fit a logistic regression model to estimate the propensity scores
logistic_model <- glm(card ~ age + sex + pre_avg_purch, data = data, family = "binomial")

# Calculate propensity scores
data$propensity_score <- predict(logistic_model, type = "response")

# Inverse Probability Weighting
data$ipw_weights <- ifelse(data$card == 1, 1 / data$propensity_score, 1 / (1 - data$propensity_score))

# Estimate treatment effect using inverse probability weighting
weighted_model <- lm(avg_purch ~ card, data = data, weights = ipw_weights)
ipw_estimate <- summary(weighted_model)
print(ipw_estimate)










