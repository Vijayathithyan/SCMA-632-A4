# Load libraries
library(dplyr)
#install.packages('lm.beta')
library(lm.beta)
library(ggplot2)

# Load data (replace 'pizza_data.csv' with your actual file path)
setwd('D:\\MDA\\Course\\Boot Camp\\SCMA 632\\Assignments\\A4')
df <- read.csv("pizza_data.csv")

dim(df)

# Define formula with categorical variables
model_formula <- formula(ranking ~ 
                           C(brand, Sum) + 
                           C(price, Sum) + 
                           C(weight, Sum) + 
                           C(crust, Sum) + 
                           C(cheese, Sum) + 
                           C(size, Sum) + 
                           C(toppings, Sum) + 
                           C(spicy, Sum))

# Fit the model
model_fit <- lm.beta(model_formula, data = df)

# Print model summary
summary(model_fit)
