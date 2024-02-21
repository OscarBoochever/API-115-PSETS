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

gsummary(ols_return2ed)

wald_return2ed <- felm(LWKLYWGE ~ 1 | 0 | EDUC ~ quarter_1, data = cohort_one) # this isn't working properly
summary(wald_return2ed)


# Question 18 #### 
