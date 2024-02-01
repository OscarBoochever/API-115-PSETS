# Title: PSET 1 Script
# Author: Oscar Boochever
# Date: Feb 5, 2024

# Load necessary libraries
library(haven)
library(broom)
library(lfe)
library(tidyverse)
library(forcats)
library(glue)
library(ggplot2)

# Read data
data <- read_dta('ReplicationDataGhanaJDE.dta')

# 17. Descriptive statistics table 

# Filter data for Wave 2
data_wave2 <- data %>%
  filter(wave2 == 1)

# Calculate descriptive statistics
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

# Print the table
print(descriptive_stats)