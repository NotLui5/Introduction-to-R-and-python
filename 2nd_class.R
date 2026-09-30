## ============================================================
## R for Data Science - Chapters 2 & 3 + R intro - Simple manipulations.
## Clinical dataset: medicaldata::covid_testing
## ============================================================

library(tidyverse) 
## filter, distinct, arrange
## mutate, rename, select
## group_by, (..., .by)
library(medicaldata)

## ------------------------------------------------------------
## STEP 1 - Warm-up (Chapter 2, assignment)
## ------------------------------------------------------------
my_variable <- 10
my_varıable

libary(tidyverse)

ggplot(dTA = mpg) +
  geom_point(maping = aes(x = displ, y = hwy)) +
  geom_smooth(method = "lm")

## ------------------------------------------------------------
## STEP 2 - Meet covid_testing dataset
## ------------------------------------------------------------

data(covid_testing)
covid_testing <- as_tibble(covid_testing)

# ?covid_testing opens the help page with full variable definitions.
# Key variables for this class:
# Which columns look categorical vs. numeric?
#   age             - patient age in years (num)
#   fake_first_name - an auto-generated fake first name (num)
#   fake_last_name  - an auto-generated fake fake last name ()
#   gender          - patient gender ()
#   demo_group      - patient/staff/other ()
#   payor_group     - insurance payor category ()
#   clinic_name     - testing clinic ()
#   result          - test result: positive/negative/invalid ()
#   pan_day         - day of the pandemic the test was run on ()
#   col_rec_tat     - turnaround time in HOURS from collection to result () 
covid_testing |> 
  select(age, fake_first_name, fake_last_name, gender, demo_group, payor_group, 
         clinic_name, result, pan_day, col_rec_tat)

# glimpse(covid_testing)

## >>> TRY: How many people, and patients, are in this dataset? How many tests? 
##          How many columns? 

covid_testing  |>
  distinct(fake_first_name, fake_last_name, .keep_all = TRUE) |>
  group_by(demo_group) |>
  summarize(n()) #count function!!!!!!

## ------------------------------------------------------------
## STEP 3 - Live-code filter, arrange, mutate 
## ------------------------------------------------------------

# 3a. filter() - keep only positive results
covid_testing |>
  filter(result == "positive")

# 3b. filter() with two conditions - positive AND pediatric (age < 18)
df_covid_testing  |>
  filter(result == "positive" & age < 18)

# --- TRIGGER THIS ON PURPOSE ---------------------
covid_testing |>
  # filter(age %in% c(65, 1))
  filter(age == 65 | age == 1)

## >>> TRY: Write a filter() that keeps tests from patients (not
##          staff) that came back positive. (Hint: check the levels
##          of demo_group with unique(covid_testing$demo_group))

# 3c. mutate() - turnaround time is already in hours (col_rec_tat);
# create a version in DAYS to make the point about deriving new
# variables from existing ones.
covid_testing |>
  mutate(tat_days = col_rec_tat / 24, .after = col_rec_tat)

# 3d. arrange() - find the slowest turnaround times
covid_testing |>
  arrange(desc(col_rec_tat))

## >>> TRY: Does it matter whether you filter() before or after
##          arrange()? Try both orders below and compare - does the
##          final result differ? Which order does less work?

## mas eficiente:
covid_testing |> 
  filter(result == "positive") |> 
  arrange(desc(col_rec_tat))

covid_testing |>
  arrange(desc(col_rec_tat)) |> 
  filter(result == "positive")


## ------------------------------------------------------------
## STEP 4 - Live-code select, rename, and the pipe (5 min)
## ------------------------------------------------------------

# Combine everything so far into ONE pipeline: find the pediatric
# positive tests with the slowest turnaround, showing only the
# columns that matter.

covid_testing |>
  filter(result == "positive" & age < 18) |>
  mutate(tat_days = col_rec_tat / 24) |>
  select(age, gender, clinic_name, tat_days) |>
  rename(turnaround_days = tat_days) |>
  relocate(turnaround_days, .after = gender) |>
  arrange(desc(turnaround_days))

## >>> TRY: Read the pipeline above out loud as a sentence


## ------------------------------------------------------------
## STEP 5 - Live-code group_by + summarize
## ------------------------------------------------------------

# Average turnaround time by clinic
covid_testing |>
  group_by(clinic_name) |>
  summarize(
    avg_tat_hours = mean(col_rec_tat),
    n = n()
  )

# --- TRIGGER THIS ON PURPOSE: missing values give NA -------------
# If col_rec_tat has any missing values, fix it:
covid_testing |>
  group_by(clinic_name) |>
  summarize(
    avg_tat_hours = mean(col_rec_tat, na.rm=TRUE),
    n = n()
  )

## >>> TRY: Group by demo_group instead of clinic_name and calculate
##          the positivity rate (hint: result == "positive" inside
##          mean() gives you a proportion, e.g.
##          mean(result == "positive", na.rm = TRUE))

# covid_testing |>
#   group_by(demo_group) |>
#   summarize(
#     positivity_rate = mean(result == "positive", na.rm = TRUE),
#     n = n()
#   )

## ------------------------------------------------------------
## STEP 6 - Debrief 
## ------------------------------------------------------------
## Discuss as a group:
## - Which dplyr verb do you think you'll use most in your own work?
## - Preview: next class (Chapter 4) is about making these pipelines
##   easier to read and format consistently.
