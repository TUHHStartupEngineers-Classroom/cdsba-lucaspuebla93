# Load tidyerse package
library(tidyverse)
library(ggthemr)
library(ggplot2)
ggthemr("fresh") 


df <- readRDS("~/GitHub/cdsba-lucaspuebla93/Causal_Data_Science_Data/car_prices.rds")
# View(df)

# Check the dimensions of the dataframe
dimensions <- dim(df) # Retrieve the dimensions (rows and columns) of the dataframe

# Print the number of rows and columns
cat("Number of rows:", dimensions[1], "\n")
cat("Number of columns:", dimensions[2], "\n")


head(df) # View the first few rows of the dataframe
summary(df) # Summary statistics for numerical columns in the dataframe
str(df) # Structure of the dataframe, including data types




# Select only numeric columns from the dataframe
numeric_df <- df[sapply(df, is.numeric)]

# Calculate the correlation matrix for numeric columns
correlation_matrix <- cor(numeric_df)

# Find correlations with the 'price' variable
price_correlations <- correlation_matrix['price', ]
price_correlations <- price_correlations[!names(price_correlations) %in% "price"]  # Exclude 'price'

sorted_correlations <- sort(price_correlations, decreasing = TRUE)
print(sorted_correlations)

# Select variables with higher correlations
threshold <- 0.5 # Adjustable threshold
relevant_variables <- names(sorted_correlations[abs(sorted_correlations) > threshold])

# Filter the dataframe to include only relevant variables
df_relevant <- df[, c("price", relevant_variables)]

# Run linear regression using only the relevant variables
model_relevant <- lm(price ~ ., data = df_relevant)

# Summary of the linear regression model
summary(model_relevant)

#----------------------------------------------------------------------

# Perform linear regression between price and enginesize
model_enginesize <- lm(price ~ enginesize, data = df)

# Create scatter plot of price vs enginesize with regression line
ggplot(df, aes(x = enginesize, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
  labs(x = "Engine size", y = "Price") +
  ggtitle("Linear Regression: Price vs Engine size")

#----------------------------------------------------------------------
# Add 'seat_heating' variable with value TRUE for all observations
df_with_seat_heating <- df %>%
  mutate(seat_heating = 1)

# Run linear regression with the added 'seat_heating' variable
model_with_seat_heating <- lm(price ~ ., data = df_with_seat_heating)

# Summary of the linear regression model
summary(model_with_seat_heating)



