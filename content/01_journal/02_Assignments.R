random_vars <- readRDS("~/GitHub/cdsba-lucaspuebla93/Causal_Data_Science_Data/random_vars.rds")
# View(random_vars)

# Calculate statistics for the 'age' variable
age_expected_value <- mean(random_vars$age)
age_variance <- var(random_vars$age)
age_standard_deviation <- sd(random_vars$age)

# Calculate statistics for the 'income' variable
income_expected_value <- mean(random_vars$income)
income_variance <- var(random_vars$income)
income_standard_deviation <- sd(random_vars$income)

# Display the computed statistics
cat("For the 'age' variable:\n")
cat("Expected Value:", age_expected_value, "\n")
cat("Variance:", age_variance, "\n")
cat("Standard Deviation:", age_standard_deviation, "\n\n")

cat("For the 'income' variable:\n")
cat("Expected Value:", income_expected_value, "\n")
cat("Variance:", income_variance, "\n")
cat("Standard Deviation:", income_standard_deviation, "\n")

# Calculate covariance between 'age' and 'income'
covariance <- cov(random_vars$age, random_vars$income)

# Calculate correlation between 'age' and 'income'
correlation <- cor(random_vars$age, random_vars$income)

# Display the computed covariance and correlation
cat("Covariance between 'age' and 'income':", covariance, "\n")
cat("Correlation between 'age' and 'income':", correlation, "\n")


# Compute conditional expected value of income for different age groups
income_conditional_exp <- c()

# For age <= 18
income_conditional_exp[1] <- mean(subset(random_vars$income, random_vars$age <= 18))

# For 18 < age < 65
income_conditional_exp[2] <- mean(subset(random_vars$income, random_vars$age > 18 & random_vars$age < 65))

# For age >= 65
income_conditional_exp[3] <- mean(subset(random_vars$income, random_vars$age >= 65))

# Display the conditional expected values of income for each age group
cat("Conditional Expected Value of Income for different age groups:\n")
cat("Age <= 18:", income_conditional_exp[1], "\n")
cat("18 < Age < 65:", income_conditional_exp[2], "\n")
cat("Age >= 65:", income_conditional_exp[3], "\n")





