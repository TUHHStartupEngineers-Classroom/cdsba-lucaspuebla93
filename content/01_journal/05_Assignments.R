# Load packages
library(tidyverse)
library(dagitty)
library(ggdag)
library(ggthemr)
ggthemr("flat")
rm(list = ls())

## Collider
#collider <- dagify(
#  Sales ~ Parking,
#  Sales ~ Location,
#  coords = list(x = c(Location = 3, Sales = 2, Parking = 1),
#                y = c(Location = 1, Sales = 0, Parking = 1))
#)
## Plot DAG
#ggdag(collider) +
#  geom_dag_point(color = ggthemr::swatch()[2]) +
#  geom_dag_text(color = "white") +
#  geom_dag_edges(edge_color = "white")



# Load data
data <- readRDS("~/GitHub/cdsba-lucaspuebla93/Causal_Data_Science_Data/customer_sat.rds")
# print(data)

# Run linear regression: satisfaction on follow_ups
model_1 <- lm(satisfaction ~ follow_ups, data = data)

# Summarize the regression results
summary(model_1)


# Run linear regression: satisfaction on follow_ups and account for subscription
model_2 <- lm(satisfaction ~ follow_ups + subscription, data = data)

# Summarize the regression results
summary(model_2)



# Not conditioning on subscription
not_cond_plot <- ggplot(data, aes(x = follow_ups, y = satisfaction)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)

# Conditioning on subscription
cond_plot <- ggplot(data, aes(x = follow_ups, y = satisfaction,
                              color = subscription, 
                              alpha = subscription)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("Premium" = "blue", "Premium+" = "red", "Elite" = "green")) +
  scale_alpha_manual(values = c("Premium" = 0.5, "Premium+" = 0.6, "Elite" = 0.3)) +
  theme(legend.position = "top")

# Plot both
not_cond_plot
cond_plot

