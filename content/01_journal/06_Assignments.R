# Load packages
library(tidyverse)
library(dagitty)
library(ggdag)
library(ggthemr)
ggthemr("flat")
rm(list = ls())


# Load data
data <- readRDS("~/GitHub/cdsba-lucaspuebla93/Causal_Data_Science_Data/abtest_online.rds")
print(data)



# Calculate summary statistics for mobile_device by group
mobile_summary <- data %>%
  group_by(chatbot) %>%
  summarise(
    prop_mobile_device = mean(mobile_device)
  )

# Calculate summary statistics for previous_visits by group
visits_summary <- data %>%
  group_by(chatbot) %>%
  summarise(
    mean_visits = mean(previous_visit),
    median_visits = median(previous_visit),
    sd_visits = sd(previous_visit)
  )

# Display the calculated summary statistics
mobile_summary
visits_summary

# Check covariate balance for categorical variable (mobile_device)
ggplot(data, aes(x = as.factor(chatbot), fill = as.factor(mobile_device))) +
  geom_bar(position = "fill") +
  labs(x = "Group", y = "Proportion", fill = "Mobile Device", title = "Comparison of Mobile Device Usage by Group")

# Check covariate balance for another variable (previous_visits)
ggplot(data, aes(x = as.factor(chatbot), y = previous_visit)) +
  geom_boxplot() +
  labs(x = "Group", y = "Previous Visits", title = "Comparison of Previous Visits by Group")
#######################################################################


# Fit a linear regression model
model_1 <- lm(purchase_amount ~ chatbot, data = data)
# Display the summary of the regression model
summary(model_1)

# Fit a linear regression model
model_2 <- lm(purchase ~ chatbot, data = data)
# Display the summary of the regression model
summary(model_2)


#######################################################################
# Fit a linear regression model with interaction for purchase_amount and chatbot by mobile_device
model_interaction <- lm(purchase_amount ~ chatbot * mobile_device, data = data)

# Display the summary of the regression model with interaction
summary(model_interaction)

#######################################################################
# Fit a logistic regression model for purchase and chatbot
model_purchase <- glm(purchase ~ chatbot, data = data, family = "binomial")

# Display the summary of the logistic regression model for purchase
summary(model_purchase)


