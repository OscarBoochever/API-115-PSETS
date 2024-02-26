# Load Libraries
library(tidyverse)
library(haven)
library(zoo)
library(lmtest)
library(sandwich)
library(broom)
library(lfe)

# Import Data 
data <- read_dta('data/AK91_1930_39.dta')


# Question 14 ####
## Create dummy variables for year of birth and quarter of birth
data$year_dummies <- as.factor(data$YOB)
data$quarter_dummies <- as.factor(data$QOB)

## Expand dummy variables for each year and each quarter
for (year in unique(data$YOB)) {
  data[paste0("year_", year)] <- ifelse(data$YOB == year, 1, 0)
}

for (quarter in unique(data$QOB)) {
  data[paste0("quarter_", quarter)] <- ifelse(data$QOB == quarter, 1, 0)
}

## Create dataset for figure 1 plot
figure_1 <- data %>%
  filter(YOB >= 30, YOB < 40) %>% 
  group_by(YOB, QOB) %>% 
  summarise(mean_educ = mean(EDUC)) %>% 
  mutate(quarter = QOB/4,
         year_quarter = YOB + quarter - 0.25)

## Create the plot with points labeled by QOB
ggplot(figure_1, aes(x = year_quarter, y = mean_educ)) +
  geom_line() + # Plotting one line for mean education
  geom_point() + # Adding points
  geom_text(aes(label = as.character(QOB)), size = 3, vjust = -0.5, hjust = 0.5) + # Adding labels to points
  labs(x = "Year of Birth", y = "Mean Completed Years of Education") +
  ggtitle("Mean Completed Years of Education by Year of Birth") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(30, 40, by = 2)) # Set x-axis scale with breaks every 2 years

## Create a better, colored plot
ggplot(figure_1, aes(x = year_quarter, y = mean_educ)) +
  geom_line(color = "black") + # Plotting one line for mean education
  geom_point(aes(color = factor(QOB)), size = 2) + # Adding points, color by QOB
  geom_text(aes(label = as.character(QOB)),
            size = 3 , vjust = -.8, hjust = 0.5) + # Adding labels to points
  labs(x = "Year of Birth", y = "Years of Education") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(30, 40, by = 1)) + # Set x-axis scale with breaks every 2 years
  scale_color_manual(values = c("orange", "grey", "grey", "blue"),
                     name = "Quarter of Birth", labels = c("1", "2", "3", "4")) + # Set colors for QOB
  scale_y_continuous(limits = c(12.2, max(figure_1$mean_educ) + 0.2),
                     breaks = seq(12.2, max(figure_1$mean_educ) + 0.2, by = 0.2)) + # Set y-axis scale
  theme(panel.grid.minor = element_blank(), # Remove minor grid lines
        panel.grid.major.x = element_line(color = "grey", linetype = "dashed")) # Modify major grid lines




# Question 15 ####
## Create a mean education variable by year and quartertabe
moving_average <- data %>% group_by(quarter_1, quarter_2, quarter_3, quarter_4, YOB, AGEQ) %>%
  summarise(mean_ed = mean(EDUC)) %>% 
  ungroup()

## Store the lag and lead two periods around each row 
moving_average <- moving_average %>% 
  mutate(
    lag2 = lag(mean_ed, n = 2, order_by = AGEQ),
    lag1 = lag(mean_ed, n = 1, order_by = AGEQ), 
    lead1 = lead(mean_ed, n = 1, order_by = AGEQ), 
    lead2 = lead(mean_ed, n = 2, order_by = AGEQ)
  )

## Compute moving average of lags and lead 
moving_average <- moving_average %>% 
  mutate(
    moving_avg = rowMeans(select(moving_average, c(lag2, lag1, lead1, lead2)), na.rm = FALSE),
    educ_detrend = mean_ed - moving_avg
  ) %>% 
  select(AGEQ, moving_avg, educ_detrend)

## Merge the moving average, detrended educ back into the main dataset
data <- merge(moving_average, data, by = "AGEQ")

## Filter to the first cohort and remove the first two quarters of 1930
data_table_1 <- data %>% 
  filter(YOB >= 30, 
         YOB <= 39,
         !(YOB == 30 & (quarter_1 | quarter_2)))


## Estimate coefficients of detrended quarter-of-birth dummies on detrended total years of education
education_model_detrended <- lm(educ_detrend ~ quarter_1 + quarter_2 + quarter_3, data = data_table_1)

## View the coefficients
table_1_coeffs <- tidy(education_model_detrended) %>% mutate(estimate = round(estimate, 3))




# Question 16 ####
reduced_model <- felm(LWKLYWGE ~ quarter_2 + quarter_3 + quarter_4 | YOB, data = data)
summary(reduced_model)



# Question 17 ####
## OLS
cohort_one <- data %>% 
  filter(YOB >= 30, 
         YOB <= 39)

ols_return2ed <- lm(LWKLYWGE ~ EDUC, data = cohort_one)
summary(ols_return2ed)

wald_return2ed <- felm(LWKLYWGE ~ 1 | 0 | (EDUC ~ quarter_1) | 0, data = cohort_one)
summary(wald_return2ed)


# Question 18 ####
# Create the year-quarter interaction factor variable
cohort_one <- cohort_one %>%
  mutate(year_quarter = as.factor(YOB + (QOB / 4) - 0.25))

tsls_simple <- felm(LWKLYWGE ~ 1 | YOB | (EDUC ~ year_quarter), data = cohort_one)
tsls_simple <- felm(LWKLYWGE ~ 1 | YOB | (EDUC ~ year_dummies*quarter_dummies), data = cohort_one) #equivalent
summary(tsls_simple)
tsls_simple_table <- tidy(tsls_simple) %>%
  mutate_at(vars(-term), ~ round(., 4))

  
tsls_full <- felm(LWKLYWGE ~ RACE + SMSA + MARRIED | 
                                 YOB + 
                                 NEWENG +
                                 MIDATL +
                                 ENOCENT +
                                 WNOCENT +
                                 SOATL +
                                 ESOCENT +
                                 WSOCENT +
                                 MT |
                    (EDUC ~ year_dummies*quarter_dummies), data = cohort_one) 
                    

summary(tsls_full)
tsls_full_table <- tidy(tsls_full) %>%
  mutate_at(vars(-term), ~ round(., 4))



# Question 19 ####

# Step 1: Regress education on the instruments
education_model_19 <- lm(EDUC ~ year_dummies*quarter_dummies + factor(YOB), data = cohort_one)

# Step 2: Predict education using the model
cohort_one$predicted_educ <- predict(education_model_19)

# Step 3: Run the TSLS regression using predicted education
tsls_regression <- lm(LWKLYWGE ~ predicted_educ + factor(YOB), data = cohort_one)

# Step 4: View the results
summary(tsls_regression)




# Question 20 ####
cohort_one <- cohort_one %>% 
  mutate(treatment = EDUC >= 12,
         instrument = ifelse(quarter_4 == 1, 1, 0))

## a. 
first_stage <- lm(treatment ~ instrument, cohort_one)
summary(first_stage)

### alternate method
table("instrument" = cohort_one$instrument, "treatment" = cohort_one$treatment)
prop.table(table("instrument" = cohort_one$instrument, "treatment" = cohort_one$treatment), 1)

d_z_1 <- mean(cohort_one$treatment[cohort_one$instrument == 1])
d_z_0 <- mean(cohort_one$treatment[cohort_one$instrument == 0])
proportion_compliers <- d_z_1 - d_z_0
proportion_compliers

## b.
mean(cohort_one$LWKLYWGE[cohort_one$treatment == FALSE & cohort_one$instrument == 1])


## c. 
mean(cohort_one$LWKLYWGE[cohort_one$treatment == TRUE & cohort_one$instrument == 0])


## d. comparing balance of treatment groups

# Create a summary table with mean values, standard deviation, and p-values
summary_stats <- cohort_one %>% 
  group_by(treatment) %>% 
  summarise(
    mean_wage = mean(LWKLYWGE, na.rm = TRUE),
    mean_race = mean(RACE, na.rm = TRUE),
    mean_urban = mean(SMSA, na.rm = TRUE))

# Print the summary table
print(summary_stats)


# Conduct t-tests for continuous variables
t.test(LWKLYWGE ~ treatment, data = cohort_one)
t.test(RACE ~ treatment, data = cohort_one)
t.test(SMSA ~ treatment, data = cohort_one)

t.test(LWKLYWGE ~ instrument, data = cohort_one)
t.test(RACE ~ instrument, data = cohort_one)
t.test(SMSA ~ instrument, data = cohort_one)



# Extract p-values from the tests
p_value_wage <- t_test_wage$p.value
p_value_race <- t_test_race$p.value
p_value_urban <- t_test_urban$p.value

# Create a vector of p-values
p_values <- c("test", p_value_wage, p_value_race, p_value_urban)

# Bind the p-values row to the summary_stats table
summary_stats <- rbind(summary_stats, p_values)

# Add row names for p-values
rownames(summary_stats)[3, ] <- c("t_test_p_value_wage", "t_test_p_value_race", "t_test_p_value_urban", "chi_sq_p_value_married")

# Print the updated summary table
print(summary_stats)

