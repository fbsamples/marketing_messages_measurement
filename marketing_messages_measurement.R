# Power analysis

### Scenario 1: INCREMENTALITY ###
# this power calculation is straightforward, as it requires a minimum effect size between two proportions
# In this type of research, the cost is only applied to the treatment cell, so we can calculate the cost per incremental
# conversion metric. We only need to power this experiment to be able to detect a minimum difference between conv rates
# at 80% power, 5% significance level and a minimum lift of 10%

control_cvr = 0.05 # Benchmark from previous WA campigns. Conversion rate = 5%
mde = 0.1 #minimum effect size

#Power calculation for proportions
power.prop.test(
  p1 = control_cvr,
  p2 = control_cvr *(1 + mde),
  sig.level = 0.05,
  power = 0.8, # could be 90, if we want to be more conservative
  alternative = "two.sided"
)

# --- Output: Two-sample comparison of proportions power calculation

## BENCHMARK OF WA VS LEGACY CHANNELS

# For this design, the power analysis is more complex because the difference in conversion rates doesn't
# take into account the difference in cost between the alternatives.
# In this case, we need to estimate a baseline of conversion rates for the control group that equalizes for
# the difference in costs. By doing so, we will have a well-powered experiment 
# to be able to detect a specific difference in CPA.

# Eg: Whatsapp vs SMS, and the cost to send a msg in each channel is USD 0.05 and USD 0.02 respectively.
# from historical data, we know SMS's convertion rate is 1.5%

# cost per message ratio between WA and SMS: 0.05 / 0.02 = 2.5

# The next step is to use the multiplier times the conversion rate of SMS to define a new baseline:
# 2.5 * 0.015 = 0.0375

# then we proceed with the power calculation for 2 different proportions. The baseline for control is 
# the number calculated in the step above (0.0375), and the test cell will be the baseline * the minimum
# expeted lift (let's assume 10%)
# Baseline (control): 0.0375
# Test cell: 0.0375 * 1.10 (10% improvement) = 0.04125
# Min amount of conversions = 75 is a result of previous experiences at Meta on minimum requirements for power calculation

sms_cost = 0.02
wa_cost = 0.05
sms_cvr = 0.015
wa_cvr = (wa_cost/sms_cost)*sms_cvr
min_conversions = 75 # minimum amount of conversions
sms_n = min_conversions / sms_cvr # Min sample size for SMS for power calc
wa_n = min_conversions / wa_cvr # min sample size for WA for power calc

library(pwr)

pwr.2p2n.test(
  h = ES.h(p1 = wa_cvr, p2 = wa_cvr*(1 + mde)), #Note that we have baseline CVR here vs baseline*10%
  n1 = sms_n,
  n2 = wa_n,
  sig.level = 0.05,
  alternative = "two.side"
)

# -- Output: difference of proportion power calculation for binomial distribution (arcsine transformation)
# h = 0.01928697
# n1 = 5000
# n2 = 2000
# sig.level = 0.05
# power = 0.1127482
# alternative = two.sided
# NOTE: Different sample sizes

# Note that in the example above, we have to estimate initial sample sizes for each group (n1 = 5000, n2 = 2000)
# This was calculated based on the minimum amount of impressions needed given the conversion rates
# in each group to get at least 75 conversions. This number 75 is a result of previous experiences
# at Meta on minimum requirements for power calculation. The estimated power for this given scenario 
# is 11.3%, which is insufficient (we should target 80%).
# So, the recommendation here is to increase the minimum sample sizes (n1 and n2) at the
# same rates until we get power = 0.80. After some trial and error, we were able to find that 15X
# more audience will give us 80% power:

pwr.2p2n.test(
  h = ES.h(p1 = wa_cvr, p2 = wa_cvr * (1 + mde)), # Note that we have baseline CVR here vs baseline*10%
  n1 = sms_n * 15, # 15x audience multiplier
  n2 = wa_n * 15, # 15x audience multiplier
  sig.level = 0.05,
  alternative = "two.side"
)

# -- Output: difference of proportion power calculation for binomial distribution (arcsine transformation)
# h = 0.01928697
# n1 = 75000
# n2 = 30000
# sig.level = 0.05
# power = 0.806031
# alternative = two.sided
# NOTE: Different sample sizes

# That means, to be able to detect a 10% improvement in cost per action (conversion), we need
# SMS treatment: 75000 people reached
# WA treatment: 30000 people reached

##### RANDOMIZATION #######
# In this step, you will divide your customers into the treatment groups. If you’re running an
# incrementality analysis (type A), you will have the control (holdout) group and the test group
# (will receive WhatsApp messages). If you’re running the benchmark analysis (type B), you will
# have to randomize the audience in the same way, but in the respective treatment groups

# to test if the randomization was well executed, you should run some comparison tests for several 
# metrics for historical behavior:
# Number of WA messages received in the last 28 days
# Number of SMS messages received in the last 28 days
# Number of purchases in the last 28 days
# age, gender, location

## checking randomization using package tableone
library(tableone) # this package helps scaling t-tests for all metrics from your table
library(dplyr)
base_data <- read.csv('base_data.csv', header = TRUE, sep = ";")

table1 = CreateTableOne(
  vars = c("wa_messages_28d", "email_messages_28d", "purchases_28d"), # all vars from the table
  strata = "group", # var that indicates the stratification assigment
  data = base_data, #data input - from the client
  test = T
)
summary(table1)

# --- Output: summary of continuous variables
# compare the p-value for each variable: we want pvalues > 0.05 for all vars - if pvalue < 0.05, that means 
# the randomization is skewed or biased in that variable, and we need to re-run it

# ------ ########## DATA ANALYSIS FOR SCENARIO 1: INCREMENTALITY OF WA ########
# Assuming we run the experiment with the proper power mentioned earlier
n = 32000 # sample size in each group
holdout_conversions = 1430 # provided by client / partner
test_conversions = 1705 # provided by client / partner
wa_cost = 0.05

# Test for differences in proportions
diff = prop.test(
  x = c(holdout_conversions, test_conversions),
  n = c(n, n)
)

holdout_ptest = prop.test(holdout_conversions, n)
wa_ptest = prop.test(test_conversions, n)

# Showing results from the tests / confidence intervals
diff
holdout_ptest
wa_ptest

# Calculating cost per incremental conversion
incremental_conversions = test_conversions - holdout_conversions
cost_per_incremental_conversions = (wa_cost * n) / incremental_conversions
cost_per_incremental_conversions

# ---- Output: 2-sample test for equality of proportions with continuity correction

# ------ ########## DATA ANALYSIS FOR SCENARIO 2: BENCHMARK ANALYSIS ########
# Assuming the power calculation scenario described above
# We sent 75k messages through SMS and 35k through WA
# cost per message sent is 0.02 for SMS and 0.05 for WA
# conversions: 1490 for SMS and 1678 for WA

# Setting parameters
sms_n = 75000 # SMS messages sent
wa_n = 30000 # WA msgs sent
sms_conversions = 1490 # conversions from SMS group
wa_conversions = 1678 # conversions from WA group
sms_cost = 0.02 # cost per message sent on SMS
wa_cost = 0.05  # cost per message sent on WA

# Simulating conversions given experimental data
sms_sim = rbinom(10000, sms_n, sms_conversions/sms_n)
wa_sim = rbinom(10000, wa_n, wa_conversions/wa_n)

# Calculating CPAs per treatment group
sms_sim_cpa = (sms_n * sms_cost) / sms_sim
wa_sim_cpa = (wa_n * wa_cost) / wa_sim

# Confidence interval for each treatment
summaryTable = data.frame(
  group = "sms",
  avg_cpa = mean(sms_sim_cpa),
  lwr = quantile(sms_sim_cpa, 0.025),
  upr = quantile(sms_sim_cpa, 0.975)
) %>%
  bind_rows(
    data.frame(
      group = "wa",
      avg_cpa = mean(wa_sim_cpa),
      lwr = quantile(wa_sim_cpa, 0.025),
      upr = quantile(wa_sim_cpa, 0.975)
    )
  )

rownames(summaryTable) = c()
summaryTable

# Confidence Intervals for Delta
delta = wa_sim_cpa / sms_sim_cpa -1

deltaSummary = data.frame(
  avg_delta = mean(delta),
  p.value = mean(delta > 0),
  lwr = quantile(delta, 0.025),
  upr = quantile(delta, 0.975)
)

rownames(deltaSummary) = c()
deltaSummary

# when the confidence intervals do not contain zeros, and pvalue < 0.05: Difference is stat sig