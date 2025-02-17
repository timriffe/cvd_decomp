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

# perform decomposition for each subgroup
dec <-
  IN |> 
  group_by(educ, gender) |> 
  group_modify(~do_dec_time(data = .x)) |> 
  ungroup()

# Now the mortality effect can split into cause-specific effects.
# Check it out!
HD_sen <- dec |> 
  filter(transition == "HD") |> 
  select(-transition, -p, -delta)

HD_deltas <-
  IN |> 
  select(period, gender, educ, age, HD1, HD2, HD3) |> 
  filter(age > 40) |> 
  mutate(age = age - 0.25) |> 
  pivot_longer(HD1:HD3, names_to = "transition", values_to = "p") |> 
  pivot_wider(names_from = period, values_from = p) |> 
  mutate(delta = `2016-2020` - `2000-2004`,
         p = (`2016-2020` + `2000-2004`)/2) |> 
  select(gender, educ, age, transition, delta, p)

HD_cc <- HD_deltas |> 
  left_join(HD_sen, by = join_by(gender, educ, age)) |> 
  mutate(cc = delta * effect)

# check equal
HD_cc |> 
  group_by(gender, educ, age) |> 
  summarize(cc_check = sum(cc),
            .groups = "drop") |> 
  left_join(HD_sen,
            by = join_by(gender, educ, age)) |> 
  mutate(cc_resid = cc_check - cc) 
  

# To know how to weight these, we need all cvd-free life
# expectancies, for use in a Kitagawa decomposition.
expectancies <-
  IN |> 
  group_by(educ, gender, period) |> 
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
  arrange(gender, educ) |> 
  rename(HLE_1 = `HLE_2000-2004`,
         HLE_2 = `HLE_2016-2020`)

prev_for_kit <- read_csv("data/prev_edu.csv", show_col_types = FALSE) |> 
  pivot_wider(names_from = period, values_from = init, names_prefix = "prev_") |> 
  rename(prev_1 = `prev_2000-2004`, prev_2 = `prev_2016-2020`)

# check:
prev_for_kit |> 
  group_by(gender) |> 
  summarize(check1 = sum(prev_1),
            check2 = sum(prev_2))

# Perform Kitagawa decomposition. Structure effects must be summed;
# rate effects we use to reweight group0-wise decompositions. This is two decomps,
# but no need for group_by() here.
kit <- 
  left_join(le_for_kit, prev_for_kit, by = join_by(gender,educ)) |> 
  mutate(HLE_d = HLE_2 - HLE_1,
         HLE_avg = (HLE_2 + HLE_1) / 2,
         prev_avg = (prev_2 + prev_1) / 2,
         prev_d = prev_2 - prev_1,
         structure_effect = prev_d * HLE_avg,
         rate_effect = HLE_d * prev_avg)

# This is used to weight educ-wide decompositions...
dec_weights <-
  kit |> 
  select(gender, educ, rate_effect)


# produce stationary total expectancy, blending edu according
# to radix composition.
total_stationary <-
  kit |> 
  group_by(gender) |> 
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
  ungroup() |> 
  bind_rows(HD_cc) |> 
  filter(educ != "total",
         transition != "HD") |>
  left_join(dec_weights, by = join_by(educ,gender)) |> 
  group_by(gender,educ) |> 
  mutate(cc_total = (cc / sum(cc)) * rate_effect) |> 
  ungroup() |> 
  arrange(gender, educ, transition, age)


# Our complete decomposition, to check sums
dt <- 
  dec_total |> 
  group_by(gender) |> 
  summarize(cc_rates = sum(cc_total))

kit |> 
  group_by(gender) |> 
  summarize(cc_structure = sum(structure_effect)) |> 
  left_join(dt, by = join_by(gender)) |> 
  mutate(Delta = cc_rates + cc_structure)
# check above
# 1.7 increase in CVDFLE for females, 2.35 for males
# compare with below
total_stationary |> 
  mutate(Delta = `HLE_2016-2020` - `HLE_2000-2004`)

write_csv(dec_total,"data/dec_total_time.csv.gz")
write_csv(kit,"data/kitagawa_time.csv")


dec_total |> 
  group_by(gender, transition) |> 
  summarize(cc = sum(cc_total)) |> 
  pivot_wider(names_from = gender, values_from =cc)
