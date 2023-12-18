# Load packages
library(tidyverse)
library(dagitty)
library(ggdag)
library(ggthemr)
library(AER)
library(rdd)
ggthemr("flat")
rm(list = ls())


# Load data
data <- readRDS("~/GitHub/cdsba-lucaspuebla93/Causal_Data_Science_Data/coupon.rds")
print(data)



# Specify bandwidth range around the cutoff point
cutoff_point <- 60  
bandwidth <- cutoff_point + c(-5, 5)
bandwidth_half <- cutoff_point + c(-2.5, 2.5)
bandwidth_double <- cutoff_point + c(-10, 10)

# Create subsets based on different bandwidths
data_subset <- subset(data, abs(days_since_last_centered) <= bandwidth)
data_half_bw <- subset(data, abs(days_since_last_centered) <= bandwidth_half)
data_double_bw <- subset(data, abs(days_since_last_centered) <= bandwidth_double)

# Perform RDD analysis without squared term
rdd_model <- lm(purchase_after ~ days_since_last_centered + coupon, data = data_subset)
summary(rdd_model)

# Perform RDD analysis for half the bandwidth
rdd_half_bw <- lm(purchase_after ~ days_since_last_centered + coupon, data = data_half_bw)
summary(rdd_half_bw)

# Perform RDD analysis for double the bandwidth
rdd_double_bw <- lm(purchase_after ~ days_since_last_centered + coupon, data = data_double_bw)
summary(rdd_double_bw)

################################################################################


# Load data
data <- readRDS("~/GitHub/cdsba-lucaspuebla93/Causal_Data_Science_Data/shipping.rds")
print(data)

# Plotting a density plot to visualize the distribution around the 30€ cutoff
library(ggplot2)  # Loading the ggplot2 package for visualization

# Create a density plot for purchase_amount
ggplot(data, aes(x = purchase_amount)) +
  geom_density(fill = "skyblue", color = "black") +
  geom_vline(xintercept = 30, linetype = "dashed", color = "red") +  # Adding a vertical line at 30€ cutoff
  labs(x = "Purchase Amount", y = "Density", title = "Density Plot of Purchase Amount with Cutoff at 30€")


