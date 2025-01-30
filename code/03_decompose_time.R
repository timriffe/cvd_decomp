source("code/00_setup.R")
source("code/01_functions.R")
IN1 <- read_excel("data/TP_2000_2004.xlsx") |> 
  fselect(1:10)
IN2 <- read_excel("data/TP_2016_2020.xlsx") |> 
  fselect(1:10)

IN1 <-
  IN1 |> 
  mutate(period = "2000-2004",
         time_mid = 2002)
IN2 <-
  IN2 |> 
  mutate(period = "2016-2020",
         time_mid = 2018)

IN <-
  bind_rows(IN1, IN2)

dec <-
  IN |> 
  group_by(educ, sex)

# this is 8 different decompositions, so it takes
# slightly longer than before
dec <- IN |> 
  group_by(educ, sex) |> 
  group_modify(~do_dec_time(data = .x)) |> 
  ungroup()

# To know how to weight these, we need all cvd-free life
# expectancies, for use in a Kitagawa decomposition.
expectancies <-
  IN |> 
  group_by(educ, sex, period) |> 
  summarize(HLE = calc_expectancy(ptibble=.data,expectancy = "h"),
            .groups = "drop") |> 
  pivot_wider(names_from = period, values_from = HLE, names_prefix = "HLE_")

# An expectancy calculated based on the total education group
# is not stationary insofar as the underlying gender & education composition
# would not be stationary. Hence, we call it the non-stationary
# expectancy because we compare it with a blended education one.
total_non_stationary <- 
  expectancies |> 
  filter(educ == "total") |> 
  mutate(variant = "non-stationary", .before = 1)

# Only need educ-specific expectancies for kitagawa
le_for_kit <- expectancies |> 
  filter(educ != "total") |> 
  arrange(sex, educ) |> 
  rename(HLE_1 = `HLE_2000-2004`,
         HLE_2 = `HLE_2016-2020`)

# age 40 prevalence emailed 16 dec 2024
# 2000-04: 
#   Men: Basic-0.1685, Secondary-0.5057, Tertiary-0.3258
# Women: Basic-0.0961, Secondary-0.4350, Tertiary-0.4689
# 
# 2016-20: 
#   Men: Basic-0.1554, Secondary-0.4693, Tertiary-0.3753
# Women: Basic-0.0787, Secondary-0.3544, Tertiary-0.5669

# Prevalence values hard coded. These come from exact age 40; we may replace
# these prevalence averages from ages 38-42 or so. 
prev_for_kit <- tibble(educ = c("basic","intermediate","tertiary",
                                "basic","intermediate","tertiary"),
                       sex = c(rep("m",3),rep("f",3)),
                       prev_1 = c(0.1685,0.5057,0.3258,0.0961,0.4350,0.4689),
                       prev_2 = c(0.1554,0.4693,0.3753,0.0787,0.3544,0.5669)) |> 
  group_by(sex) |> 
  mutate(prev_1 = prev_1 / sum(prev_1),
         prev_2 = prev_2 / sum(prev_2))

# Perform Kitagawa decomposition. Structure effects must be summed;
# rate effects we use to reweight group0-wise decompositions. This is two decomps,
# but no need for group_by() here.
kit <- 
  left_join(le_for_kit, prev_for_kit, by = join_by(sex,educ)) |> 
  mutate(HLE_d = HLE_2 - HLE_1,
         HLE_avg = (HLE_2 + HLE_1) / 2,
         prev_avg = (prev_2 + prev_1) / 2,
         prev_d = prev_2 - prev_1,
         structure_effect = prev_d * HLE_avg,
         rate_effect = HLE_d * prev_avg)

# This is used to weight educ-wide decompositions...
dec_weights <-
  kit |> 
  select(sex, educ, rate_effect)


# produce stationary total expectancy, blending edu according
# to radix composition.
total_stationary <-
  kit |> 
  group_by(sex) |> 
  summarize(educ = "total",
            `HLE_2000-2004` = sum(HLE_1 * prev_1),
            `HLE_2016-2020` = sum(HLE_2 * prev_2)) |> 
  mutate(variant = "stationary", .before = 1)


# compare total cvd-free life expectancy (blended versus observed)
total_hle <- 
  total_stationary |> 
  bind_rows(total_non_stationary)


# reweight decompositions:
dec_total <-
  dec |> 
  filter(educ != "total") |> 
  left_join(dec_weights, by = join_by(educ,sex)) |> 
  group_by(sex,educ) |> 
  mutate(cc_total = (cc / sum(cc)) * rate_effect)

# Our complete decomposition, to check sums
dt <- 
  dec_total |> 
  group_by(sex) |> 
  summarize(cc_rates = sum(cc_total))

kit |> 
  group_by(sex) |> 
  summarize(cc_structure = sum(structure_effect)) |> 
  left_join(dt, by = join_by(sex)) |> 
  mutate(Delta = cc_rates + cc_structure)
# check above
# 1.7 increase in CVDFLE for females, 2.35 for males
# compare with below
total_stationary |> 
  mutate(Delta = `HLE_2016-2020` - `HLE_2000-2004`)
