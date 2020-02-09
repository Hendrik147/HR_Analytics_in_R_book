## ----pay-gap-analysis, include=FALSE------------------------------------------
chap <- 13
lc <- 0
rq <- 0
# **`r paste0("(LC", chap, ".", (lc <- lc + 1), ")")`**
# **`r paste0("(RQ", chap, ".", (rq <- rq + 1), ")")`**

knitr::opts_chunk$set(
  tidy = FALSE, 
  out.width = '\\textwidth', 
  fig.height = 4,
  warning = FALSE
  )

options(scipen = 99, digits = 3)

# Set random number generator see value for replicable pseudorandomness. Why 76?
# https://www.youtube.com/watch?v=xjJ7FheCkCU
set.seed(76)


## ----packages-----------------------------------------------------------------
# Load R libraries.
library(tidyverse)
library(tidymodels)
library(devtools)
install_github("Hendrik147/HR_Analytics_in_R_book/HRAnalytics")
library(HRAnalytics) # Helper package

#Turn off scientific notation. 
options(scipen = 999)


## ----load data----------------------------------------------------------------
# Load data. 
gd_data = read_csv("https://glassdoor.box.com/shared/static/beukjzgrsu35fqe59f7502hruribd5tt.csv")
# N = 1000 total observations.


## ----Summary Statistics-------------------------------------------------------
gd_data %>%
  # Age brackets
  mutate(age_bin = cut(age,
                  breaks = c(0, 25, 35, 45, 55, Inf),
                  right = FALSE)) %>%
  # Total compensation
  mutate(total_pay = basePay + bonus) %>%
  # Log of compensation
  mutate(log_base = log(basePay, base = exp(1)),
         log_total = log(total_pay, base = exp(1)),
         # Adds 1 to allow for log of 0 bonus values.
         log_bonus = log(bonus + 1, base = exp(1))) %>%
  # Make flags
  mutate_if(is_character, fct_infreq) %>%
  mutate(age_bin = fct_infreq(age_bin)) ->
  gd_data_clean


## ----gd_summary_gender_base---------------------------------------------------
# Base pay summary stats.
gd_data_clean %>%
  # Exclude stuff with missing info
  filter(!is.na(basePay)) %>%
  # Analyse by gender
  group_by(gender) %>%
  # Retrieve summary statistics
  summarise(
    mean_base = mean(basePay),
    median_base = median(basePay),
    count = n()
  ) ->
  gd_summary_gender_base

gd_summary_gender_base


## ----gd_summary_gender_total--------------------------------------------------
# Total pay summary stats.
gd_data_clean %>%
  # Use a helper function to save typing
  HRAnalytics::gap_summary(gender, total_pay) ->
  gd_summary_gender_total

gd_summary_gender_total


## ----gd_summary_gender_bonus--------------------------------------------------
# Bonus summary stats. 
gd_data_clean %>%
  # Use a helper function to save typing
  HRAnalytics::gap_summary(gender, bonus) ->
  gd_summary_gender_bonus

gd_summary_gender_bonus


## ----gd_summary_gender_perf---------------------------------------------------
# Performance evaluations summary stats. 
gd_data_clean %>%
  # Use a helper function to save typing
  HRAnalytics::gap_summary(gender, perfEval)  ->
  gd_summary_gender_perf

gd_summary_gender_perf


## ----gd_summary_dept_gender_total---------------------------------------------
# Performance evaluations summary stats. 
gd_data_clean %>%
  filter(!is.na(total_pay)) %>%
  # Create a summary by department and gender
  group_by(dept, gender) %>%
  summarise(
    mean_perf = mean(total_pay),
    median_perf = median(total_pay),
    count = n()
  ) %>%
  # "Unpivot" the data
  gather(measure, value, mean_perf:count) %>%
  # Combine the gender with measure
  unite(combo, measure, gender) %>%
  # "Pivot" the data to see all the measures split by gender
  spread(combo, value) ->
  gd_summary_dept_gender_total

gd_summary_dept_gender_total


## ----gd_summary_job_gender_total----------------------------------------------
# Performance evaluations summary stats. 
gd_data_clean %>%
  filter(!is.na(total_pay)) %>%
  # Create a summary by job and gender
  group_by(jobTitle, gender) %>%
  summarise(
    mean_perf = mean(total_pay),
    median_perf = median(total_pay),
    count = n()
  ) %>%
  # "Unpivot" the data
  gather(measure, value, mean_perf:count) %>%
  # Combine the gender with measure
  unite(combo, measure, gender) %>%
  # "Pivot" the data to see all the measures split by gender
  spread(combo, value) ->
  gd_summary_job_gender_total

gd_summary_job_gender_total


## ----Linear models------------------------------------------------------------
# No controls. ("unadjusted" pay gap.)
lm_gender <- lm(log_base ~ gender, data = gd_data_clean)

# Adding "human capital" controls (performance evals, age and education).
lm_humancapital <- lm(log_base ~ gender + perfEval + age_bin + edu, data = gd_data_clean)

# Adding all controls. ("adjusted" pay gap.)
lm_allcontrols <- lm(log_base ~ gender + perfEval + age_bin + edu + dept + seniority + jobTitle, data = gd_data_clean)


## ----lm_gender----------------------------------------------------------------
lm_gender %>%
  summary()


## ----lm_gender viz------------------------------------------------------------
lm_gender %>%
  # Get the predicted values
  augment() %>%
  # Rename columns to more friendly names
  rename(actual = log_base, predicted = .fitted) %>%
  # Build a chart
  ggplot() +
  # Add columns to it
  aes(x=actual, y=predicted) +
  # Choose chart type
  geom_point() +
  # Add a diagonal line representing perfect predictions
  geom_abline(colour="blue", slope=1, intercept=0)+
  # Split by gender
  facet_wrap(~gender) +
  # Add some labels
  labs(title="Actual vs predicted", 
       subtitle="Values predicted using a linear model containing gender")


## ----lm_humancapital----------------------------------------------------------
lm_humancapital %>% 
  summary()


## ----lm_humancapital viz------------------------------------------------------
lm_humancapital %>% 
  # Get the predicted values
  augment() %>% 
  # Rename columns to more friendly names
  rename(actual = log_base, predicted = .fitted) %>% 
  HRAnalytics::pred_vs_actuals() +
  # Add some labels
  labs(title="Actual vs predicted", 
       subtitle="Values predicted using a linear model containing human capital measures")


## ----lm_allcontrols-----------------------------------------------------------
lm_allcontrols %>% 
  summary()


## ----lm_allcontrols viz-------------------------------------------------------
lm_allcontrols %>% 
  # Get the predicted values
  augment() %>% 
  # Rename columns to more friendly names
  rename(actual = log_base, predicted = .fitted) %>% 
  HRAnalytics::pred_vs_actuals() +
  # Add some labels
  labs(title="Actual vs predicted", 
       subtitle="Values predicted using a linear model all controls")


## ----coefficients-------------------------------------------------------------
#  Gather up all the models
list(lm_gender, lm_humancapital, lm_allcontrols) %>% 
  # Extract coefficients for all models at once and combine into a single table
  map_df(tidy, .id = "model") %>% 
  # Let's look at the impact of gender
  filter(term=="genderFemale") %>%
  # P values less than 0.05 are usually taken to mean an estimate is reliable
  select(model, log_gap=estimate, p.value)


## ----lm_allcontrols_dept------------------------------------------------------
# All controls with department interaction terms. 
lm_allcontrols_dept <- lm(log_base ~ gender*dept + perfEval + age_bin + edu + seniority + jobTitle, data = gd_data_clean)

tidy(lm_allcontrols_dept)

lm_allcontrols_dept %>% 
  # Get the predicted values
  augment() %>% 
  # Rename columns to more friendly names
  rename(actual = log_base, predicted = .fitted) %>% 
  HRAnalytics::pred_vs_actuals() +
  # Add some labels
  labs(title="Actual vs predicted", 
       subtitle="Values predicted using a linear model all controls & department interaction")


## ----lm_allcontrols_job-------------------------------------------------------
# All controls with department interaction terms. 
lm_allcontrols_job <- lm(log_base ~ gender*jobTitle + perfEval + age_bin + edu + seniority + dept, data = gd_data_clean)

tidy(lm_allcontrols_job)

lm_allcontrols_job %>% 
  # Get the predicted values
  augment() %>% 
  # Rename columns to more friendly names
  rename(actual = log_base, predicted = .fitted) %>% 
  # Build a chart
  HRAnalytics::pred_vs_actuals() +
  # Add some labels
  labs(title="Actual vs predicted", 
       subtitle="Values predicted using a linear model all controls & job interaction")

