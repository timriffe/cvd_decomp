source("code/00_setup.R")
source("code/01_functions.R")

IN2 <- read_excel("data/TP_2016_2020.xlsx") |> 
  fselect(1:10)

# decompositions between like-education groups, i.e.
# basic vs basic, etc includes initial health conditions.
dec2 <- 
  IN2 |> 
  group_by(educ) |> 
  group_modify(~do_dec(data = .x)) |> 
  ungroup()

# To know how to weight these, we need all cvd-free life
# expectancies, for use in a Kitagawa decomposition.
expectancies <-
  IN2 |> 
  group_by(educ, sex) |> 
  summarize(HLE = calc_expectancy(ptibble=.data)) |> 
  pivot_wider(names_from = sex, values_from = HLE, names_prefix = "HLE_")

# An expectancy calculated based on the total education group
# is not stationary insofar as the underlying education composition
# would not be stationary. Hence, we call it the non-stationary
# expectancy because we compare it with a blended education one.
total_non_stationary <- expectancies |> 
  filter(educ == "total") |> 
  mutate(variant = "non-stationary", .before = 1)

# Only need educ-specific expectancies for kitagawa
le_for_kit <- expectancies |> 
  filter(educ != "total")

# Prevalence values hard coded. These come from exact age 40; we may replace
# these prevalence averages from ages 38-42 or so. 
prev_for_kit4 <- tibble(educ = c("basic","intermediate","tertiary"),
                       prev_m = c(0,.469,.375),
                       prev_f = c(0,.354,.567)) |> 
  mutate(prev_m = prev_m / sum(prev_m),
         prev_f = prev_f / sum(prev_f))

# Perform Kitagawa decomposition. Structure effects must be summed;
# rate effects we use to reweight group0-wise decompositions.
kit4 <- left_join(le_for_kit, prev_for_kit4, by = join_by(educ)) |> 
  mutate(HLE_d = HLE_f - HLE_m,
         HLE_avg = (HLE_f + HLE_m) / 2,
         prev_avg = (prev_f + prev_m) / 2,
         prev_d = prev_f - prev_m,
         structure_effect = prev_d * HLE_avg,
         rate_effect = HLE_d * prev_avg)

# This is used to weight educ-wide decompositions...
dec_weights4 <-
  kit4 |> 
  select(educ, rate_effect)

# produce stationary total expectancy, blending edu according
# to radix composition.
total_stationary4 <-
  kit4 |> 
  ungroup() |> 
  summarize(educ = "total",
            HLE_f = sum(HLE_f * prev_f),
            HLE_m = sum(HLE_m * prev_m)) |> 
  mutate(variant = "stationary", .before = 1)

# reweight decompositions:
dec_total4 <-
  dec2 |> 
  filter(educ != "total") |> 
  left_join(dec_weights4, by = join_by(educ)) |> 
  group_by(educ) |> 
  mutate(cc_total = (cc / sum(cc)) * rate_effect)

# Our complete decomposition, check sums
dec_total4 |> 
  pull(cc_total) |> 
  sum() + sum(kit4$structure_effect)

# compare with stationary difference:
total_stationary4$HLE_f - total_stationary4$HLE_m 

total_stationary
total_stationary4





