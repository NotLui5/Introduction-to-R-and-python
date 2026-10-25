## ============================================================
## R for Data Science - Chapters 4-8
## (4 Workflow: code style | 5 Data tidying | 6 Workflow: scripts
##  and projects | 7 Data import | 8 Workflow: getting help)
## ============================================================
##
## REPOSITORY LAYOUT
## This script expects two small data files in the SAME folder as
## the script (or edit the paths below to match your repo structure):
##   wide_vitals.csv   - synthetic EHR-style wide vitals export
##   messy_labs.csv    - synthetic messy lab results export

library(tidyverse)
library(janitor)

## ------------------------------------------------------------
## STEP 1 - Workflow habits lightning round (5 min)
## ------------------------------------------------------------

# --- Chapter 4: code style - BEFORE -----------------------------
covidtesting%>%filter(result=="positive")%>%group_by(clinic_name)%>%summarize(n=n(),avgtat=mean(col_rec_tat,na.rm=TRUE))

## >>> TRY: What's hard to read in the line above? List 3 problems
##          before looking at the fixed version.

# ---------------- ANSWER ----------------------------------------
# Problems: no spaces around operators/commas, everything crammed on
# one line, non-descriptive variable names (n, avgtat), no snake_case
# consistency, pipe steps not broken out for readability.
#
# covid_testing |>
#   filter(result == "positive") |>
#   group_by(clinic_name) |>
#   summarize(
#     n = n(),
#     avg_tat = mean(col_rec_tat, na.rm = TRUE)
#   )

## ------------------------------------------------------------
## STEP 2 - Tidy data concept (5 min)
## ------------------------------------------------------------

# The 3 rules of tidy data:






#  1. Each variable is a column
#  2. Each observation is a row
#  3. Each value is a cell

# Load our wide, EHR-style export (one row per patient, repeated
# visit measurements spread across columns - a VERY common shape
# for real clinical data exports).
wide_vitals <- read_csv("wide_vitals.csv", show_col_types = FALSE)
wide_vitals

## >>> TRY: Looking at wide_vitals, what IS the observation here -
##          a patient, or a patient-visit? Is this data tidy by the
##          3 rules above?

# ---------------- ANSWER ----------------------------------------
# rule:
# ------------------------------------------------------------------

## ------------------------------------------------------------
## STEP 3 - pivot_longer() practice (10 min)
## ------------------------------------------------------------

# Step 1: pivot all 6 visit columns into long format
vitals_long <- wide_vitals |>
  pivot_longer(
    cols = starts_with("visit"),
    names_to = "visit_measure",
    values_to = "value"
  )
vitals_long

## >>> TRY: The visit_measure column now contains things like
##          "visit1_sbp" and "visit2_dbp" - both the visit NUMBER and
##          the measurement TYPE are stuck together. How would you
##          split these into two separate columns?
##          (Hint: look up the `names_sep` argument of pivot_longer())

# ---------------- ANSWER ----------------------------------------
vitals_tidy <- wide_vitals |>
  pivot_longer(
    cols = starts_with("visit"),
    names_to = c("visit", "measure"),
    names_sep = "_",
    values_to = "value"
  )
vitals_tidy

# Now each row is one patient-visit-measurement - truly tidy. Notice
# visit is still text ("visit1"); you could clean it further with
# mutate(visit = parse_number(visit)) if you want it numeric.
# ------------------------------------------------------------------

## >>> TRY: Using vitals_tidy, make a quick plot: boxplot of value,
##          faceted by measure (sbp vs dbp), colored by visit. Does
##          blood pressure trend down across visits in this sample?

# ---------------- ANSWER ----------------------------------------
vitals_tidy |>
  ggplot(aes(x = visit, y = value, fill = visit)) +
  geom_boxplot() +
  facet_wrap(~measure, scales = "free_y") +
  labs(title = "Blood pressure across visits") +
  theme_classic()

# ------------------------------------------------------------------


## ------------------------------------------------------------
## STEP 4 - pivot_wider() practice (5 min)
## ------------------------------------------------------------

## >>> TRY: Starting from vitals_tidy, use pivot_wider() to build a
##          summary table with one row per patient and separate
##          columns for average sbp and average dbp (across visits).

# ---------------- ANSWER ----------------------------------------
vitals_summary <- vitals_tidy |>
  group_by(patient_id, measure) |>
  summarize(avg_value = mean(value), .groups = "drop") |>
  pivot_wider(
    names_from = measure,
    values_from = avg_value
  )
vitals_summary

## ------------------------------------------------------------
## STEP 5 - Data import and cleanup (10 min)
## ------------------------------------------------------------

# Read the messy lab export AS-IS first, and look at the column
# specification message readr prints.
messy_labs_raw <- read_csv("messy_labs.csv")
messy_labs_raw

## >>> TRY: Look at the printed column types. What went wrong with
##          `Patient ID`? What went wrong with `WBC Count`? (Hint:
##          check problems(messy_labs_raw) too)

# ---------------- ANSWER ----------------------------------------
problems(messy_labs_raw)
# `Patient ID` was guessed as a NUMBER (readr strips the leading
# zeros: "001" becomes 1) - but this is an identifier, not a
# quantity, so it should stay a character/string.
#
# `WBC Count` has "." used as a missing-value code, which readr
# doesn't know about by default, so the WHOLE column gets guessed as
# character instead of double, because "." isn't a valid number.
# ------------------------------------------------------------------

## >>> TRY: Fix the import by (1) telling read_csv() that "." means
##          missing, and (2) forcing `Patient ID` to stay a character
##          column. (Hint: use the `na` argument and `col_types`)

# ---------------- ANSWER ----------------------------------------
messy_labs_clean <- read_csv(
  "messy_labs.csv",
  na = ".",
  col_types = cols(
    `Patient ID` = col_character(),
    .default = col_guess()
  )
) |>
  clean_names()   # janitor::clean_names() 

messy_labs_clean

## >>> TRY: How many rows have a missing wbc_count after cleaning?
##          Write one line of dplyr to count them.

# ---------------- ANSWER ----------------------------------------
messy_labs_clean |>
  summarize(n_missing_wbc = sum(is.na(wbc_count)))
# ------------------------------------------------------------------


## ------------------------------------------------------------
## DEBRIEF (last couple minutes)
## ------------------------------------------------------------
## Discuss as a group:
## - Which was more useful for your own work: pivot_longer or
##   pivot_wider? When would you reach for each one?
## - Has anyone's own REDCap/EHR/Excel export looked like
##   wide_vitals.csv or messy_labs.csv? What did you do about it
##   before today?
## - Preview: once data is tidy and imported cleanly, Chapters 9-11
##   go deeper into layered visualization and exploratory analysis.
