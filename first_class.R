## ============================================================
## R for Data Science - Chapter 1 (Data Visualization) practice
## Clinical dataset: MASS::birthwt (Risk Factors for Low Birth Weight)
## ============================================================

library(tidyverse)
library(MASS)  # ships with base R, no install needed
library(ggplot2)

## ------------------------------------------------------------
## STEP 1 - Orient to the data
## ------------------------------------------------------------

data(birthwt)
birthwt <- as_tibble(birthwt)

# How many patients (rows)? How many variables (columns)?
glimpse(birthwt)
# What means each variable? 
# ?birthwt 

# Is `smoke` numeric or categorical? Does R currently treat
# it as a number? What problem might that cause when we
# try to color a plot by it?

# Recode the 0/1 flags as readable factors - a good discussion point
# about "tidy" categorical data before plotting.
birthwt <- birthwt %>%
  mutate(
    smoke_f = factor(x = smoke, labels = c("Non-smoker", "Smoker")),
    race_f  = factor(race, levels =c(1,2,3), c("White", "Black", "Other")),
    ht_f    = factor(ht,    labels = c("No hypertension", "Hypertension"))
  )


## ------------------------------------------------------------
## STEP 2 - Build the target plot together, layer by layer 
## ------------------------------------------------------------
# TARGET PLOT: birth weight vs. mother's age, colored by smoking
# status, with a linear trend line per group.
# Build this on the projector ONE layer at a time, pausing after
# each "+" to ask what changed and why.

# 2a. Empty canvas
ggplot(data = birthwt)

# 2b. Map the axes
ggplot(data = birthwt, mapping = aes(x = age, y = bwt))

# 2c. Add points
ggplot(data = birthwt, mapping = aes(x = age, y = bwt)) +
  geom_point()

# 2d. Color by smoking status (ask: aesthetic or geom? -> aesthetic!)
ggplot(data = birthwt, mapping = aes(x = age, y = bwt, color = smoke)) +
  geom_point()

# 2e. Add a linear trend line per group
ggplot(data = birthwt, mapping = aes(x = age, y = bwt, color = smoke_f)) +
  geom_point() +
  geom_smooth(method = "lm")

# 2f. Finish with labels (the "ultimate goal" polish step)
ggplot(data = birthwt, mapping = aes(x = age, y = bwt, color = smoke_f)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Birth weight by mother's age and smoking status",
    x = "Mother's age (years)",
    y = "Birth weight (g)",
    color = "Smoking status"
  )

## >>> TRY: What happens if you move `color = smoke_f` out of the
##          global aes() and into geom_point() only? What changes
##          about the geom_smooth() lines?


## ------------------------------------------------------------
## STEP 3 - Debug this!
## ------------------------------------------------------------

# --- Bug 1 -----------------------
ggplot(data = birthwt, aes(x = age, y = bwt)) +
 geom_point()

# --- Bug 2 ------------------------
ggplot(birthwt, aes(x = age, y = bwt)) +
  geom_point()

# --- Bug 3 ---------------------
ggplot(birthwt, aes(x = age, y = bwt)) +
  geom_point()


# --- Bug 4 --------------------------
ggplot(birthwt, aes(x = smoke_f, y = bwt)) +
  geom_boxplot() +
  labs(title = "Birth weight by smoking status")


## ------------------------------------------------------------
## STEP 4 - Debrief
## ------------------------------------------------------------
## Discuss as a group:
## - Which error message was the most confusing? Why?
## - What's the first thing to check when a ggplot2 plot doesn't
##   render (hint: unmatched (), unmatched "", + in the wrong place)?
## - Remind everyone: googling the exact error message is a
##   completely normal and expected part of writing R code.