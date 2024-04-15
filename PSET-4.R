library(tidyverse)
library(haven)
library(lmtest)
library(sandwich)
library(broom)
library(lfe)
library(bacondecomp)


data <- read_dta("data/stevenson_wolfers_210.dta")

## Q13 ####
# Subset data for states that passed unilateral divorce laws in 1973 and those that never passed
data <- data %>% 
  mutate(is_post = year >= 1973,
         treated_group = unilateral,
         treat = is_post * treated_group)

# data_subset <- subset(data, divyear == 1973 | divyear == 2000)

women <- data %>% 
  filter(sex == 2)

# Run the simple 2x2 DID regression a few ways
felm(log(suiciderate_jag) ~ is_post + treated_group + treat | 0 | 0| st , data = women) %>% tidy()

# Check the distribution of treated_group within each level of is_post
table(data$is_post, data$treated_group)



# # Cluster standard errors at the state level
# robust_se <- coeftest(model, vcov = vcovHC(model, type = "HC1", cluster = "group"))
# 
# # Extract the estimated effect and standard error for the 'unilateral' coefficient
# est_did_effect <- robust_se["unilateral", "Estimate"]
# se_did_effect <- robust_se["unilateral", "Std. Error"]
# 
# # Create a table to display the estimated DID effect and standard error
# did_results <- data.frame(
#   Estimate = round(est_did_effect, 3),
#   `Std_Error` = round(se_did_effect, 3)
# )
# 
# # Print the table
# print(did_results)
# 
# summary(model)

## Q14 ####
# Define the year variable relative to 1973
data <- mutate(data, year_rel1973 = year - 1973)

# Restrict the data to relative year -5 to 5
event_study <- filter(data, year_rel1973 >= -5 & year_rel1973 <= 5)

event_study$year_rel1973 <- as.factor(event_study$year_rel1973)

# Change the reference level of year_rel1973 to -1
event_study$year_rel1973 <- relevel(event_study$year_rel1973, ref = 5)

event_study_model <- felm(log(suiciderate_jag) ~ year_rel1973 + unilateral | st | 0 | st, data = event_study) 

summary(event_study_model)

# Extract coefficients and standard errors
coefficients <- coef(event_study_model)
se <- sqrt(diag(vcov(event_study_model)))



# Create a data frame for plotting
plot_data <- data.frame(year = -5:5, coefficient = coefficients[-12], se = se[-12])

# Add a fake variable for year_rel1973-1 with coefficient and se of 0
plot_data <- rbind(plot_data, c(-1, 0, 0, 0))

# Calculate confidence intervals
plot_data$lower <- plot_data$coefficient - 1.96 * plot_data$se
plot_data$upper <- plot_data$coefficient + 1.96 * plot_data$se

plot_data <- plot_data %>% 
  filter(year != 5) %>% 
  mutate(year = ifelse(year >= -1, year + 1, year),
         year = ifelse(coefficient == 0, -1, year))

# Sort the data frame by year
plot_data <- plot_data[order(plot_data$year), ]

# Plot the event study
ggplot(plot_data, aes(x = year, y = coefficient)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Years Relative to Implementation", y = "Coefficient Estimate") +
  scale_x_continuous(breaks = seq(-5, 5, 1)) +
  ggtitle("Event Study of the Effect of Unilateral Divorce Laws on Suicide Rates") +
  theme_minimal()


## Q15 ###
twfe <- felm(log(suiciderate_jag) ~ unilateral | year + st | 0 | st, data = women) 
twfe %>% tidy

## Q16 ###
# Decompose the estimate
decomp <- bacon(twfe, data = women, id_var = st, time_var = year)

# Plot the estimates against the weights
ggplot(data = decomp, aes(x = estimate, y = weight)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Decomposition of Two-way Fixed-effects Estimate",
       x = "Estimate",
       y = "Weight")

