source("code/00_setup.R")
source("code/01_functions.R")

IN <- read_csv("data/TP_final.csv.gz", show_col_types = FALSE)  |> 
  filter(version == "ps_fit_constrained") |> 
  select(period, gender, educ, transition, age, p) |> 
  pivot_wider(names_from = transition, values_from = p) |> 
  mutate(HH = if_else(is.na(HH),1 - HU - HD, HH),
         UH = 0,
         UU = if_else(is.na(UU), 1 - UD - UH, UU),
         time_mid = if_else(period == "2016-2020",2018,2002)) |> 
  arrange(period, educ, gender, age)

# here down copied from other one, needs to be re-set to time decomp,
# and consider role of mort now that it's split
IN2 <- 
  IN |> 
  mutate(HH = if_else(!is.na(HU),HH + HU / 2,HH),
         HU = HU / 2)

# decompositions between like-education groups, i.e.
# basic vs basic, etc includes initial health conditions.
dec2 <- 
  IN2 |> 
  group_by(educ) |> 
  group_modify(~do_dec_time(data = .x)) |> 
  ungroup()

# To know how to weight these, we need all cvd-free life
# expectancies, for use in a Kitagawa decomposition.
expectancies2 <-
  IN2 |> 
  group_by(educ, sex) |> 
  summarize(HLE = calc_expectancy(ptibble=.data)) |> 
  pivot_wider(names_from = sex, 
              values_from = HLE, 
              names_prefix = "HLE_")

# An expectancy calculated based on the total education group
# is not stationary insofar as the underlying education composition
# would not be stationary. Hence, we call it the non-stationary
# expectancy because we compare it with a blended education one.
total_non_stationary3 <- expectancies3 |> 
  filter(educ == "total") |> 
  mutate(variant = "non-stationary", .before = 1)

# Only need educ-specific expectancies for kitagawa
le_for_kit3 <- expectancies3 |> 
  filter(educ != "total")

# Prevalence values hard coded. These come from exact age 40; we may replace
# these prevalence averages from ages 38-42 or so. 
prev_for_kit3 <- tibble(educ = c("basic","intermediate","tertiary"),
                        prev_m = c(.155,.469,.375),
                        prev_f = c(.079,.354,.567)) |> 
  mutate(prev_m = prev_m / sum(prev_m),
         prev_f = prev_f / sum(prev_f))

# Perform Kitagawa decomposition. Structure effects must be summed;
# rate effects we use to reweight group0-wise decompositions.
kit3 <- left_join(le_for_kit3, prev_for_kit3, by = join_by(educ)) |> 
  mutate(HLE_d = HLE_f - HLE_m,
         HLE_avg = (HLE_f + HLE_m) / 2,
         prev_avg = (prev_f + prev_m) / 2,
         prev_d = prev_f - prev_m,
         structure_effect = prev_d * HLE_avg,
         rate_effect = HLE_d * prev_avg)

# This is used to weight educ-wide decompositions...
dec_weights3 <-
  kit3 |> 
  select(educ, rate_effect)

# produce stationary total expectancy, blending edu according
# to radix composition.
total_stationary3 <-
  kit3 |> 
  ungroup() |> 
  summarize(educ = "total",
            HLE_f = sum(HLE_f * prev_f),
            HLE_m = sum(HLE_m * prev_m)) |> 
  mutate(variant = "stationary", .before = 1)


# compare total vcd=free life expectancy (blended versus observed)
total_hle3 <- 
  total_stationary3 |> 
  bind_rows(total_non_stationary)

# reweight decompositions:
dec_total3 <-
  dec3 |> 
  filter(educ != "total") |> 
  left_join(dec_weights3, by = join_by(educ)) |> 
  group_by(educ) |> 
  mutate(cc_total = (cc / sum(cc)) * rate_effect)

# Our complete decomposition, check sums
dec_total3 |> 
  pull(cc_total) |> 
  sum() + sum(kit3$structure_effect)

# compare with stationary difference:
total_stationary3$HLE_f - total_stationary3$HLE_m 
6.312354 - 5.746101
# gender gap would reduce by 0.566 years

# CVDFLE would increase by ca 4 years
as.matrix(expectancies3[,2:3])-
  as.matrix(expectancies[,2:3])
  
