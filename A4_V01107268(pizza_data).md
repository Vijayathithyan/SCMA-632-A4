# Load necessary libraries
library(readr)
library(dplyr)
library(stats)
library(ggplot2)

# Set working directory
setwd('D:/MDA/Course/Boot Camp/SCMA 632/Assignments/A4')

# Read the data
df <- read_csv('pizza_data.csv')
df

# Define the model
model <- as.formula("ranking ~ factor(brand) + factor(price) + factor(weight) + factor(crust) + factor(cheese) + factor(size) + factor(toppings) + factor(spicy)")

# Fit the model
model_fit <- lm(model, data = df)
summary(model_fit)

conjoint_attributes <- c('brand', 'price', 'weight', 'crust', 'cheese', 'size', 'toppings', 'spicy')

level_name <- list()
part_worth <- list()
part_worth_range <- numeric(length(conjoint_attributes))
important_levels <- list()

start <- 2  # Initialize index for coefficient in coefficients

for (item in conjoint_attributes) {
  nlevels <- length(unique(df[[item]]))
  level_name[[item]] <- unique(df[[item]])
  
  begin <- start
  end <- begin + nlevels - 1
  
  new_part_worth <- coef(model_fit)[begin:end]
  new_part_worth <- c(new_part_worth, -sum(new_part_worth))
  
  important_levels[[item]] <- which.max(new_part_worth)
  part_worth[[item]] <- new_part_worth
  part_worth_range[match(item, conjoint_attributes)] <- max(new_part_worth) - min(new_part_worth)
  
  # Update start for the next iteration
  start <- end + 1
}

print("level name:")
print(level_name)
print("npw with sum element:")
print(new_part_worth)
print("imp level:")
print(important_levels)
print("part worth:")
print(part_worth)
print("part_worth_range:")
print(part_worth_range)
print(length(part_worth))
print("important levels:")
print(important_levels)

attribute_importance <- sapply(part_worth_range, function(x) round(100 * (x / sum(part_worth_range)), 2))
print(attribute_importance)

part_worth_dict <- list()
attrib_level <- list()
for (item in conjoint_attributes) {
  cat("Attribute :", item, "\n")
  cat("    Relative importance of attribute ", attribute_importance[match(item, conjoint_attributes)], "\n")
  cat("    Level wise part worths: \n")
  for (j in seq_along(level_name[[item]])) {
    cat("          ", level_name[[item]][j], ":", part_worth[[item]][j], "\n")
    part_worth_dict[[level_name[[item]][j]]] <- part_worth[[item]][j]
    attrib_level[[item]] <- level_name[[item]]
  }
}
part_worth_dict

# Plot the relative importance of attributes
df_importance <- data.frame(Attribute = conjoint_attributes, Importance = attribute_importance)
ggplot(df_importance, aes(x = Attribute, y = Importance)) +
  geom_bar(stat = "identity") +
  ggtitle('Relative importance of attributes') +
  xlab('Attributes') +
  ylab('Importance')

utility <- numeric(nrow(df))
for (i in 1:nrow(df)) {
  score <- sum(sapply(conjoint_attributes, function(attr) part_worth_dict[[df[[attr]][i]]]))
  utility[i] <- score
}

df$utility <- utility

cat("The profile that has the highest utility score :", '\n')
print(df[which.max(utility),])

for (i in conjoint_attributes) {
  cat("Preferred level in", i, "is ::", level_name[[i]][important_levels[[i]]], "\n")
}

