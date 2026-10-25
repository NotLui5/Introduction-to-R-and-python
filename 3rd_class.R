## ============================================================
## Correlation: Pearson, Spearman, and Kendall's tau
## Clinical dataset: medicaldata::covid_testing
## ============================================================

library(tidyverse)
library(medicaldata)

data(covid_testing)
covid_testing <- as_tibble(covid_testing)


# set.seed(97)
# # Generate data 
# x <- sample(1:100, 10)        
# y <- sample(1:100, 10)
# # Create a data frame
# df <- data.frame(x, y)
# df$rank_x <- rank(df$x)
# df$rank_y <- rank(df$y)
# ### Kendall C-D
# # 1. To rank / ranked_data
# # 8, 65 =  1, 2
# # 25, 35 = 2, 1
# # 100, 88 = 3, 3
# # (8,65) (25,35) -> C  = (1,2) (2,1) -> D
# # (25,35) (100,88) -> D = (2,1) (3,3) -> D
# # (100,88) (8,65) -> D = (3,3) (1,2) -> D
# # r = C-D / C+D === # r = C-D / C+D 
# # r = 1 - 2 / 1 + 2 === 0 - 3 / 0 + 3
# # r = 0.3333 === -1 


## ------------------------------------------------------------
## STEP 1 - Concept warm-up 
## ------------------------------------------------------------
set.seed(97)
n <- 100

# Panel A: linear relationship -> Pearson is appropriate
xA <- rnorm(n, mean = 50, sd = 10)
yA <- 0.8 * xA + rnorm(n, sd = 8)

# Panel B: monotonic but curved (exponential-ish) -> Spearman fits better
xB <- seq(1, 100, length.out = n)
yB <- exp(xB / 30) + rnorm(n, sd = 5)

# Panel C: non-monotonic (U-shaped / quadratic) -> ____ fits well
xC <- seq(-3, 3, length.out = n)
yC <- xC^2 + rnorm(n, sd = 1)

# Panel D: linear relationship with one extreme outlier -> Pearson is
# distorted by the outlier, Spearman is more robust
xD <- rnorm(n, mean = 50, sd = 10)
yD <- 0.8 * xD + rnorm(n, sd = 8)
xD[n + 1] <- 95; yD[n + 1] <- 5  # inject an extreme outlier

warmup_df <- bind_rows(
  tibble(x = xA, y = yA, panel = "A"),
  tibble(x = xB, y = yB, panel = "B"),
  tibble(x = xC, y = yC, panel = "C"),
  tibble(x = xD, y = yD, panel = "D")
)

ggplot(warmup_df, aes(x, y)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  facet_wrap(~panel, scales = "free") +
  labs(title = "Which coefficient fits each panel?") +
  theme_classic()

## >>> TRY: For each panel (A-D), guess: Pearson, Spearman, or
##          neither fits well? 

# ---------------- ANSWER ----------------------------------------
# A: Linear, no major outliers -> PEARSON is appropriate.
# B: Monotonic but clearly curved, not a straight line -> SPEARMAN
#    captures this better than Pearson.
# C: Non-monotonic (goes down then up) -> NEITHER captures this well;
#    both will give a coefficient near 0 despite a strong relationship.
# D: Linear but with one extreme outlier -> Pearson will be distorted
#    by the single point; SPEARMAN is more robust here.
cor(xA, yA, method = "pearson")
cor(xB, yB, method = "pearson"); cor(xB, yB, method = "spearman")
cor(xC, yC, method = "pearson"); cor(xC, yC, method = "spearman")
cor(xD, yD, method = "pearson"); cor(xD, yD, method = "spearman")

## ------------------------------------------------------------
## STEP 2 - Pearson: assumptions and checks 
## ------------------------------------------------------------
covid_testing <- covid_testing |>
  filter(col_rec_tat < 10)
# Variable pair: age vs. col_rec_tat (turnaround time, hours)
covid_testing |>
  ggplot(aes(x = age, y = col_rec_tat)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  labs(title = "Age vs. turnaround time", x = "Age", y = "Turnaround time (hrs)") +
  theme_classic()

# Check the distribution of col_rec_tat - is it roughly normal?
covid_testing |>
  ggplot(aes(x = col_rec_tat)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(title = "Distribution of turnaround time") +
  theme_classic()

# Q-Q plot - do points follow the diagonal line?
qqnorm(covid_testing$col_rec_tat, main = "Q-Q plot: turnaround time")
qqline(covid_testing$col_rec_tat, col = "red")

# Written check: Shapiro-Wilk test (shapiro.test() only accepts up to
# 5000 values, so we sample if the dataset is larger)
set.seed(97)
shapiro.test(sample(na.omit(covid_testing$col_rec_tat), 5000))

## >>> TRY: Based on the histogram, Q-Q plot, and Shapiro-Wilk result,
##          does col_rec_tat look normally distributed? Does that
##          mean Pearson is automatically the wrong choice?

# ---------------- ANSWER ----------------------------------------
# col_rec_tat is right-skewed (a long tail of slow results), the Q-Q
# plot curves away from the diagonal at the upper end, and the
# Shapiro-Wilk test will almost certainly return p < 0.05 (rejecting
# normality) given the sample size.
#
# This does NOT automatically rule out Pearson - the article notes
# there's genuine debate: Pearson's assumptions are about LINEARITY
# and no extreme outliers, not strictly normality, and some research
# finds Pearson fairly robust to non-normal data. But given the clear
# skew here, Spearman is the safer, more defensible choice to report.

cor(covid_testing$age, covid_testing$col_rec_tat,
    method = "pearson", use = "complete.obs")
# ------------------------------------------------------------------


## ------------------------------------------------------------
## STEP 3 - Spearman: ranks and monotonicity
## ------------------------------------------------------------

covid_df <- covid_testing |>
  filter(!is.na(age), !is.na(col_rec_tat)) |>
  mutate(
    rank_age = rank(age),
    rank_tat = rank(col_rec_tat)
  )

# Plot ranked values against the 1:1 line
covid_df |>
  ggplot(aes(x = rank_age, y = rank_tat)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Ranked age vs. ranked turnaround time") +
  theme_classic()

## >>> TRY: Compute Spearman's rho with cor() and compare it to the
##          Pearson value from Step 2. Are they very different? What
##          does that tell you about this relationship?

# ---------------- ANSWER ----------------------------------------
cor(covid_df$age, covid_df$col_rec_tat, method = "spearman")
cor(covid_df$age, covid_df$col_rec_tat, method = "pearson")
# In this dataset age and turnaround time are only weakly related, so
# expect both coefficients to be small and fairly close to each other
# (neither close to -1 or 1). If they diverge noticeably, that's a
# sign of non-linearity or skew distorting the Pearson estimate -
# exactly what the Step 2 histogram/Q-Q plot flagged.
# ------------------------------------------------------------------


## ------------------------------------------------------------
## STEP 4 - Kendall's tau for ordinal data 
## ------------------------------------------------------------

# Create an ordinal variable: age bucketed into ordered groups
covid_df <- covid_df |>
  mutate(
    age_group = cut(
      age,
      breaks = c(-Inf, 5, 12, 18, 40, 65, Inf),
      labels = c("0-5", "6-12", "13-18", "19-40", "41-65", "65+"),
      ordered_result = TRUE
    )
  )

## >>> TRY: Compute Kendall's tau between age_group (as a number) and
##          col_rec_tat using cor(..., method = "kendall").

# ---------------- ANSWER ----------------------------------------
cor(as.numeric(covid_df$age_group), covid_df$col_rec_tat, method = "kendall")
# ------------------------------------------------------------------

# Toy example mirroring the article's concordant/discordant walkthrough
# (3 patients, ranked age and ranked turnaround time)
toy <- tibble(
  patient = c("P1", "P2", "P3"),
  rank_age = c(2, 3, 10),
  rank_tat = c(7, 5, 8)
)
toy

## >>> TRY: By hand, is the pair (P1, P2) concordant or discordant?
##          What about (P1, P3)? (P2, P3)?

# ---------------- ANSWER ----------------------------------------
# (P1, P2): age rank goes 2 -> 3 (increase), tat rank goes 7 -> 5
#   (decrease). One up, one down -> DISCORDANT.
# (P1, P3): age rank goes 2 -> 10 (increase), tat rank goes 7 -> 8
#   (increase). Both up -> CONCORDANT.
# (P2, P3): age rank goes 3 -> 10 (increase), tat rank goes 5 -> 8
#   (increase). Both up -> CONCORDANT.
# 2 concordant, 1 discordant -> tau_a = (2 - 1) / (2 + 1) = 0.33
(2 - 1) / (2 + 1)
cor(toy$rank_age, toy$rank_tat, method = "kendall")
# ------------------------------------------------------------------


## ------------------------------------------------------------
## STEP 5 - Independent practice (8 min)
## Pick ONE pairing, justify your choice of coefficient with a quick
## plot, compute it, and write one sentence interpreting it.
## ------------------------------------------------------------

## >>> TRY A: pan_day (day of the pandemic) vs. col_rec_tat - has
##            turnaround time improved or worsened over the course
##            of the pandemic?

# ---------------- ANSWER ----------------------------------------
covid_testing |>
  ggplot(aes(x = pan_day, y = col_rec_tat)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  theme_classic()
# Trend is unlikely to be a clean straight line (testing systems
# changed a lot over 2020), so Spearman is the safer default here.
cor(covid_testing$pan_day, covid_testing$col_rec_tat,
    method = "spearman", use = "complete.obs")
# ------------------------------------------------------------------

## >>> TRY B: within only "positive" results, is age correlated with
##            turnaround time? Does restricting to one group change
##            the coefficient much?

# ---------------- ANSWER ----------------------------------------
covid_testing |>
  filter(result == "positive") |>
  summarize(
    pearson  = cor(age, col_rec_tat, method = "pearson",  use = "complete.obs"),
    spearman = cor(age, col_rec_tat, method = "spearman", use = "complete.obs")
  )
# Compare this to the full-dataset values from Step 2/3 - restricting
# to a subgroup can change (even reverse) a correlation, a reminder
# that correlation coefficients describe the SPECIFIC sample you
# calculated them on, not a universal truth about the variables.
# ------------------------------------------------------------------

## >>> TRY C: payor_group (treat as ordinal if it has a natural order
##            in your data) vs. age_group - compute Kendall's tau.

# ---------------- ANSWER ----------------------------------------
# This one depends on whether payor_group has a meaningful order in
# your version of the dataset - check with:
unique(covid_testing$payor_group)
# If there's no natural order, this is actually a trick question:
# Kendall's tau requires ORDINAL (ranked) data. Nominal categories
# (no inherent order) aren't appropriate for tau - that's a good
# discussion point about checking assumptions BEFORE computing.
# ------------------------------------------------------------------


## ------------------------------------------------------------
## STEP 6 - Debrief: correlation is not slope (5 min)
## ------------------------------------------------------------

set.seed(97)
x <- 1:100
y1 <- x + rnorm(100, mean = 0, sd = 25)       # shallower slope
y2 <- 5 * x + rnorm(100, mean = 0, sd = 130)  # steeper slope, similar r

cor1 <- cor(x, y1); cor2 <- cor(x, y2)
slope1 <- coef(lm(y1 ~ x))[2]
slope2 <- coef(lm(y2 ~ x))[2]

cat("y1: r =", round(cor1, 2), " slope =", round(slope1, 2), "\n")
cat("y2: r =", round(cor2, 2), " slope =", round(slope2, 2), "\n")

## >>> TRY: If a paper reports "r = 0.6" for a clinical association,
##          what do you NOT yet know about the relationship?

# ---------------- ANSWER ----------------------------------------
# You don't know HOW MUCH y changes per unit of x (the slope/effect
# size), whether the relationship is linear or just monotonic, or
# whether it's clinically meaningful - r tells you about consistency
# of direction and strength of association, not magnitude of effect.
# You also don't know anything about CAUSATION - the classic example
# is ice cream sales and drownings both rising with warmer weather,
# despite no causal link between the two.
# ------------------------------------------------------------------
