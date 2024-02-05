# Title: PSET 1 Script
# Author: Oscar Boochever
# Date: Feb 5, 2024

# Load necessary libraries ####
library(haven)
library(broom)
library(lfe)
library(tidyverse)
library(forcats)
library(glue)
library(ggplot2)

# Read data ####
data <- read_dta('ReplicationDataGhanaJDE.dta')

# 17. Descriptive statistics table ####

## Filter data for Wave 2 ====
data_wave2 <- data %>%
  filter(wave2 == 1)

## Calculate descriptive statistics ====
descriptive_stats <- data_wave2 %>%
  summarise(
    'Number of Households' = n(),
    'Number of Units of Randomization' = n(),
    'Real Final Profit Sample Mean (Treatment)' = mean(realfinalprofit[assigntreat == 1], na.rm = TRUE),
    'Real Final Profit Sample SD (Treatment)' = sd(realfinalprofit[assigntreat == 1], na.rm = TRUE),
    'Real Final Profit Sample Mean (Control)' = mean(realfinalprofit[assigntreat == 0], na.rm = TRUE),
    'Real Final Profit Sample SD (Control)' = sd(realfinalprofit[assigntreat == 0], na.rm = TRUE)
  )

descriptive_stats <- descriptive_stats %>%
  mutate_if(is.numeric, round, digits = 2)

descriptive_stats <- cbind(Country = "Ghana", descriptive_stats)


## Perform test for statistical significance ====
treatment_data <- data_wave2$realfinalprofit[data_wave2$assigntreat == 1]
control_data <- data_wave2$realfinalprofit[data_wave2$assigntreat == 0]

t_test_result <- t.test(x = treatment_data, y = control_data, alternative = "two.sided")
print(t_test_result)


# 18. Reproduce Estimates ####
table3_model <- felm(realfinalprofit ~ atreatcash + atreatequip | groupnum + wave | 0 | sheno, data) 

## Create trimmed data ====
trimmed <- data %>% filter(is.na(trimgroup))

## Create column two model ====
table3_model_trimmed <- felm(realfinalprofit ~ atreatcash + atreatequip | groupnum + wave | 0 | sheno, trimmed) 

## Print results ====
tidy(table3_model)
tidy(table3_model_trimmed)

#trimgroup
# quad group and wave fixed effects

data <- data %>% mutate(wave_female = wave*female)

# 19. Column 5 Row 3 onward ####
table3_model_col5 <- felm(realfinalprofit ~ atreatcashfemale + atreatequipfemale + atreatcashmale + atreatequipmale | groupnum + wave + wave_female| 0 | sheno, data)
coef_table <- tidy(table3_model_col5)

# Define a vector of new names
new_names <- c("Cash Treatment Female", "Equipment Treatment Female", "Cash Treatment Male", "Equipment Treatment Male")

# Rename the terms in coef_table
coef_table <- coef_table %>%
  mutate(term = new_names)

# Create the coefficient plot
coefficient_plot <- ggplot(coef_table, aes(x = estimate, y = term, shape = term, color = term)) +
  geom_point(size = 2.2) +
  geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error), height = 0) +
  geom_text(aes(label = paste(round(estimate, 2), " (", round(std.error, 2), ")", sep = "")), vjust = -1.2) + # Add text labels
  scale_shape_manual(values = c("Cash Treatment Female" = 17, "Equipment Treatment Female" = 19, "Cash Treatment Male" = 17, "Equipment Treatment Male" = 19)) +
  scale_color_manual(values = c("Cash Treatment Female" = "blue", "Equipment Treatment Female" = "blue", "Cash Treatment Male" = "darkorange", "Equipment Treatment Male" = "darkorange")) +
  labs(x = "Coefficient", y = NULL, title = "Treatment Effects by Gender and Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) + # Center the title
  guides(shape = FALSE, color = FALSE)  # Remove legend


# Print the plot
print(coefficient_plot)


# redo 18 with firm level fixed effects

#use ggplot2 



