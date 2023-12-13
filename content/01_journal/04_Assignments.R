# Load required libraries
library(readxl)
library(ggplot2)
rm(list = ls())

# Load data
data <- read_excel("~/GitHub/cdsba-lucaspuebla93/content/01_journal/04_data.xlsx")

# Assuming the first column is the X-axis and the rest are Y-axis variables
# Change the column names accordingly if needed
x_axis <- names(data)[1]
y1_axis <- names(data)[2]
y2_axis <- names(data)[3]

# Determine scaling factors for secondary axis
scaling_factor <- 65 / 3.95
print(data[3])
data[3] <- data[3] * scaling_factor
print(scaling_factor)
print(data[3])

# Define intervals as pairs (tuples)
interval_y1 <- c(0, 65)
interval_y2 <- c(0, 3.95)

# Reshaping the data to long format for ggplot
data_long <- tidyr::gather(data, key = "variable", value = "value", -{{x_axis}})

print("up here all good")
# Plotting code with specified y-axis ranges
ggplot(data_long, aes_string(x = x_axis, y = "value", color = "variable")) +
  geom_point() +
  scale_color_manual(values = c("blue", "red"), labels = c(y1_axis, y2_axis)) +
  theme_minimal() +
  labs(x = x_axis, y = y1_axis) +
  geom_line(aes(y = value * scaling_factor), color = "red") +
  scale_y_continuous(sec.axis = sec_axis(~./scaling_factor, name = y2_axis)) +
  coord_cartesian(ylim = interval_y1) +  # Set y-axis limits based on interval_y1
  theme(legend.position = "top")

print("up here all good too")