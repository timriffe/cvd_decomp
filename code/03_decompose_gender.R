source("code/00_setup.R")
source("code/01_functions.R")

IN2 <- read_excel("data/TP_2016_2020.xlsx") |> 
  fselect(1:10)

# sex decompositions between like-education groups, i.e.
# basic vs basic, etc includes initial health conditions.
dec2 <- 
  IN2 |> 
  group_by(educ) |> 
  group_modify(~do_dec_gender(data = .x)) |> 
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
prev_for_kit <- tibble(educ = c("basic","intermediate","tertiary"),
                       prev_m = c(.155,.469,.375),
                       prev_f = c(.079,.354,.567)) |> 
  mutate(prev_m = prev_m / sum(prev_m),
         prev_f = prev_f / sum(prev_f))

# Perform Kitagawa decomposition. Structure effects must be summed;
# rate effects we use to reweight group0-wise decompositions.
kit <- left_join(le_for_kit, prev_for_kit, by = join_by(educ)) |> 
  mutate(HLE_d = HLE_f - HLE_m,
         HLE_avg = (HLE_f + HLE_m) / 2,
         prev_avg = (prev_f + prev_m) / 2,
         prev_d = prev_f - prev_m,
         structure_effect = prev_d * HLE_avg,
         rate_effect = HLE_d * prev_avg)

# This is used to weight educ-wide decompositions...
dec_weights <-
  kit |> 
  select(educ, rate_effect)

# produce stationary total expectancy, blending edu according
# to radix composition.
total_stationary <-
  kit |> 
  ungroup() |> 
  summarize(educ = "total",
            HLE_f = sum(HLE_f * prev_f),
            HLE_m = sum(HLE_m * prev_m)) |> 
  mutate(variant = "stationary", .before = 1)


# compare total cvd-free life expectancy (blended versus observed)
total_hle <- 
  total_stationary |> 
  bind_rows(total_non_stationary)

# reweight decompositions:
dec_total <-
  dec2 |> 
  filter(educ != "total") |> 
  left_join(dec_weights, by = join_by(educ)) |> 
  group_by(educ) |> 
  mutate(cc_total = (cc / sum(cc)) * rate_effect)

# Our complete decomposition, check sums
dec_total |> 
  pull(cc_total) |> 
  sum() + sum(kit$structure_effect)

# compare with stationary difference:
total_stationary$HLE_f - total_stationary$HLE_m 

# This is necessarily exact because (1) Kitagawa is exact,
# and (2) the edu-specific contributions derive from
# Kitagawa parameters, i.e. they were rescaled.

write_csv(dec_total, file = "data/dec_total.csv")
write_csv(kit, file = "data/kitagawa.csv")


