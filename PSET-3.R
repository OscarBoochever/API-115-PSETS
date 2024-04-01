# Load necessary packages
library(tidyverse)
library(haven)
library(lmtest)
library(sandwich)
library(rddensity)
library(knitr)
library(kableExtra)
library(broom)
library(AER)
library(rdrobust)

# Load the dataset
data <- read_dta("data/ozier_jhr_rep_data_v1_1.dta")


# Filter observations with valid KCPE score
filtered_data <- data %>%
  filter(has_score_2016 == 1)

# Apply the 80-point bandwidth restriction to ravens_plus_vocab_standardized
outcome_filtered_data <- filtered_data %>%
  filter(abs(rkcpe) < 0.8)

# Calculate summary statistics for kcpe_self_or_matched_recent without bandwidth restriction
summary_stats1 <- filtered_data %>%
  summarise(
    Variable = "kcpe_self_or_matched_recent",
    Min = round(min(kcpe_self_or_matched_recent, na.rm = TRUE), 2),
    Quartile_1 = round(quantile(kcpe_self_or_matched_recent, 0.25, na.rm = TRUE), 2),
    Median = round(median(kcpe_self_or_matched_recent, na.rm = TRUE), 2),
    Mean = round(mean(kcpe_self_or_matched_recent, na.rm = TRUE), 2),
    Quartile_3 = round(quantile(kcpe_self_or_matched_recent, 0.75, na.rm = TRUE), 2),
    Max = round(max(kcpe_self_or_matched_recent, na.rm = TRUE), 2),
    SD = round(sd(kcpe_self_or_matched_recent, na.rm = TRUE), 2)
  )

# Calculate summary statistics for finishsecondary without bandwidth restriction
summary_stats2 <- filtered_data %>%
  summarise(
    Variable = "finishsecondary",
    Min = round(min(finishsecondary, na.rm = TRUE), 2),
    Quartile_1 = round(quantile(finishsecondary, 0.25, na.rm = TRUE), 2),
    Median = round(median(finishsecondary, na.rm = TRUE), 2),
    Mean = round(mean(finishsecondary, na.rm = TRUE), 2),
    Quartile_3 = round(quantile(finishsecondary, 0.75, na.rm = TRUE), 2),
    Max = round(max(finishsecondary, na.rm = TRUE), 2),
    SD = round(sd(finishsecondary, na.rm = TRUE), 2)
  )

# Calculate summary statistics for ravens_plus_vocab_standardized with bandwidth restriction
summary_stats3 <- outcome_filtered_data %>%
  summarise(
    Variable = "ravens_plus_vocab_standardized",
    Min = round(min(ravens_plus_vocab_standardized, na.rm = TRUE), 2),
    Quartile_1 = round(quantile(ravens_plus_vocab_standardized, 0.25, na.rm = TRUE), 2),
    Median = round(median(ravens_plus_vocab_standardized, na.rm = TRUE), 2),
    Mean = round(mean(ravens_plus_vocab_standardized, na.rm = TRUE), 2),
    Quartile_3 = round(quantile(ravens_plus_vocab_standardized, 0.75, na.rm = TRUE), 2),
    Max = round(max(ravens_plus_vocab_standardized, na.rm = TRUE), 2),
    SD = round(sd(ravens_plus_vocab_standardized, na.rm = TRUE), 2)
  )

# Combine summary statistics
summary_stats <- rbind(summary_stats1, summary_stats2, summary_stats3)

# Print summary statistics
print(summary_stats)


# Question 13: First-stage effect of test score cutoff on secondary school completion
# Pooled sample with triangular kernel
# Pooled sample
model_pooled <- lm(finishsecondary ~ passrkcpe + rkcpe + passrkcpe:rkcpe, 
                   data = data_bandwidth)

summary_pooled <- coeftest(model_pooled, vcov. = sandwich::vcovCL, cluster = ~ rkcpe)
summary_pooled

# Male sample
model_male <- lm(finishsecondary ~ passrkcpe + rkcpe + passrkcpe:rkcpe,
                 data = subset(data_bandwidth, female == 0))

summary_male <- coeftest(model_male, vcov. = sandwich::vcovCL, cluster = ~ rkcpe)
summary_male

# Female sample
model_female <- lm(finishsecondary ~ passrkcpe + rkcpe + passrkcpe:rkcpe, data = subset(data_bandwidth, female == 1))
summary_female <- coeftest(model_female, vcov. = sandwich::vcovCL, cluster = ~ rkcpe)
summary_female

# Convert tidy outputs to data frames
tidy_pooled <- tidy(summary_pooled)
tidy_male <- tidy(summary_male)
tidy_female <- tidy(summary_female)

# Merge data frames by 'term'
merged_results <- merge(tidy_pooled, tidy_male, by = "term", suffixes = c("_pooled", "_male"))
merged_results <- merge(merged_results, tidy_female, by = "term")

# Round coefficients, standard errors, and p-values
merged_results <- merged_results %>%
  mutate(across(c(estimate_pooled, estimate_male, estimate), ~ round(., 2)),
         across(c(std.error_pooled, std.error_male, std.error), ~ round(., 2)),
         across(c(p.value_pooled, p.value_male, p.value), ~ round(., 3)))

# Rename columns
names(merged_results) <- c("term", "estimate_pooled", "std.error_pooled", "statistic_pooled", "p.value_pooled",
                           "estimate_male", "std.error_male", "statistic_male", "p.value_male",
                           "estimate_female", "std.error_female", "statistic_female", "p.value_female")

# Remove the statistic columns
merged_results <- merged_results[, !(names(merged_results) %in% c("statistic_pooled", "statistic_male", "statistic_female"))]

# Define the desired order of terms
desired_order <- c("passrkcpe", "rkcpe", "passrkcpe:rkcpe", "(Intercept)")

# Reorder rows based on the desired order of terms
merged_results <- merged_results[match(desired_order, merged_results$term), ]



# Question 14: Effect of completing secondary school on cognitive tests

# Fit the instrumental variable regression model
iv_model <- ivreg(ravens_plus_vocab_standardized ~ finishsecondary + rkcpe + passrkcpe:rkcpe + female | passrkcpe + rkcpe + passrkcpe:rkcpe + female,
                  data = data_bandwidth)

# Extract coefficients and standard errors
coefficients <- coef(iv_model)
# standard_errors <- sqrt(diag(vcovHC(iv_model, type = "HC1")))  # Robust standard errors

# Calculate clustered standard errors
clustered_se <- sandwich::vcovCL(iv_model, cluster = ~ rkcpe)
standard_errors <- sqrt(diag(clustered_se))

# Combine coefficients and standard errors into a dataframe
table_data <- data.frame(
  Coefficient = round(coefficients, 3),
  `Standard Error` = round(standard_errors, 3)
)

# Define the desired order of coefficients
desired_order <- c("finishsecondary", "rkcpe", "female", "passrkcpe:rkcpe", "(Intercept)")

# Reorder the coefficients and standard errors according to the desired order
table_data <- table_data[match(desired_order, rownames(table_data)), ]

# Print the reordered and rounded table
print(table_data)



# Question 15: RD plot
# Estimate Bandwidth
out <- rdrobust(data_bandwidth$ravens_plus_vocab_standardized, 
              data_bandwidth$rkcpe, 
              covs = data_bandwidth$female,
              h = 0.8,
              p = 1,
              cluster = data_bandwidth$rkcpe,
              kernel = "uniform")

summary(out)

# Create RD Plot
out <- rdplot(data_bandwidth$ravens_plus_vocab_standardized, 
              data_bandwidth$rkcpe, 
              h = 0.8,
              nbins = 8,
              p = 1)

# Summary of Plot
summary(out)



# # Define the bandwidth
# bandwidth <- 0.8
# 
# # Filter data within bandwidth
# data_bandwidth <- data_bandwidth[abs(data_bandwidth$rkcpe) < bandwidth, ]
# 
# # Create a scatterplot
# ggplot(data_bandwidth, aes(x = rkcpe, y = ravens_plus_vocab_standardized)) +
#   geom_point(alpha = 0.5) +  # Add points
#   geom_smooth(method = "lm", se = TRUE, color = "red") +  # Add regression line
#   labs(x = "KCPE Score", y = "Cognitive Score") +  # Axis labels
#   ggtitle("Regression Discontinuity Plot") +  # Title
#   theme_minimal()  # Minimal theme




# Question 16: Manipulation test
# Step 1: Run the manipulation test
out <- rddensity(data_bandwidth$rkcpe, c = 0)

# Step 2: Plot the density plot
rdplotdensity(out, X = data_bandwidth$rkcpe)

