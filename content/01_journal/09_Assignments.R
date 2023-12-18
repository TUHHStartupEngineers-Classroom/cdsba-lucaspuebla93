# Load packages
library(tidyverse)
library(dagitty)
library(ggdag)
library(ggthemr)
library(AER)
ggthemr("flat")
rm(list = ls())


# Load data
data <- readRDS("~/GitHub/cdsba-lucaspuebla93/Causal_Data_Science_Data/rand_enc.rds")
print(data)

summary(data)
cor_matrix <- cor(data)
print(cor_matrix)


# Define the DAG model with chosen relations and custom positions
custom_dag_model <- 'dag {
  "unobservable" [pos="0.5,0.75"]
  "rand_enc" [pos="0,0.5"]
  "used_ftr" [pos="0.25,0.5"]
  "time_spent" [pos="0.75,0.5"]
  "unobservable" -> "used_ftr"
  "unobservable" -> "time_spent"
  "rand_enc" -> "used_ftr"
  "used_ftr" -> "time_spent"
}'

# Plot the updated custom DAG using ggdag
ggdag(custom_dag_model) +
  geom_dag_point(color = "blue") +
  geom_dag_text(color = "black") +
  geom_dag_edges(edge_color = "black")


########################################################################
# Calculate the naive, biased estimate
naive_biased_estimate <- data %>%
  filter(used_ftr == 1) %>%
  group_by(rand_enc) %>%
  summarise(mean_time_spent = mean(time_spent))

naive_biased_estimate
########################################################################

# Compute correlations
correlation_matrix <- data %>%
  select(rand_enc, used_ftr, time_spent) %>%
  cor()

correlation_matrix
########################################################################
# Perform 2SLS regression
iv_model <- ivreg(time_spent ~ used_ftr | rand_enc, data = data)

# Summary of the IV regression
summary(iv_model)









