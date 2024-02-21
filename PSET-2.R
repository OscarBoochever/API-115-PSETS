# Load Libraries
library(tidyverse)
library(haven)

# Import Data 
data <- read_dta('data/AK91_1930_39.dta')

# Question 14

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


