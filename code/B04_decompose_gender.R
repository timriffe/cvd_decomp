source("code/00_setup.R")
source("code/01_functions.R")

IN <- read_csv("data/TP_final.csv.gz",show_col_types = FALSE) |> 
  filter(version == "ps_fit_constrained") |> 
  select(period, gender, educ, transition, age, p) |> 
  pivot_wider(names_from = transition, values_from = p) |> 
  mutate(HH = if_else(is.na(HH),1 - HU - HD, HH),
         UH = 0,
         UU = if_else(is.na(UU), 1 - UD - UH, UU),
         time_mid = if_else(period == "2016-2020",2018,2002)) |> 
  arrange(period, educ, gender, age)

prev_edu <- read_csv("data/prev_edu.csv",show_col_types = FALSE)

# sex decompositions between like-education groups, i.e.
# basic vs basic, etc includes initial health conditions.
dec <- 
  IN |> 
  group_by(educ, period) |> 
  group_modify(~do_dec_gender(data = .x)) |> 
  ungroup()

# Now for COD
HD_sen <- dec |> 
  filter(transition == "HD") |> 
  select(-transition, -p, -delta)

HD_deltas <-
  IN |> 
  select(period, gender, educ, age, HD1, HD2, HD3) |> 
  filter(age > 40) |> 
  mutate(age = age - 0.25) |> 
  pivot_longer(HD1:HD3, names_to = "transition", values_to = "p") |> 
  pivot_wider(names_from = gender, values_from = p) |> 
  mutate(delta = women - men,
         p = (women + men) / 2) |> 
  select(period, educ, age, transition, delta, p)

HD_cc <- HD_deltas |> 
  left_join(HD_sen, by = join_by(period, educ, age)) |> 
  mutate(cc = delta * effect)

# check equal
HD_cc |> 
  group_by(period, educ, age) |> 
  summarize(cc_check = sum(cc),
            .groups = "drop") |> 
  left_join(HD_sen,
            by = join_by(period, educ, age)) |> 
  mutate(cc_resid = cc_check - cc) 


# To know how to weight these, we need all cvd-free life
# expectancies, for use in a Kitagawa decomposition.
expectancies <-
  IN |> 
  group_by(educ, gender, period) |> 
  summarize(HLE = calc_expectancy(ptibble=.data),.groups = "drop") |> 
  pivot_wider(names_from = gender, values_from = HLE, names_prefix = "HLE_")

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

prev_edu |> group_by(period, gender) |> summarize(check = sum(init))

prev_for_kit <-
  prev_edu |> 
  rename(prev = init) |> 
  pivot_wider(names_from = gender, values_from = prev, names_prefix = "prev_")
# Perform Kitagawa decomposition. Structure effects must be summed;
# rate effects we use to reweight group0-wise decompositions.
kit <- left_join(le_for_kit, prev_for_kit, by = join_by(educ, period)) |> 
  mutate(HLE_d = HLE_women - HLE_men,
         HLE_avg = (HLE_women + HLE_men) / 2,
         prev_avg = (prev_women + prev_men) / 2,
         prev_d = prev_women - prev_men,
         structure_effect = prev_d * HLE_avg,
         rate_effect = HLE_d * prev_avg)

# This is used to weight educ-wide decompositions...
dec_weights <-
  kit |> 
  select(period, educ, rate_effect)

# produce stationary total expectancy, blending edu according
# to radix composition.
total_stationary <-
  kit |> 
  ungroup() |> 
  group_by(period) |> 
  summarize(educ = "total",
            HLE_women = sum(HLE_women * prev_women),
            HLE_men = sum(HLE_men * prev_men),
            .groups = "drop") |> 
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
  left_join(dec_weights, by = join_by(educ, period)) |> 
  group_by(educ, period) |> 
  mutate(cc_total = (cc / sum(cc)) * rate_effect)

# Our complete decomposition, check sums
Delta_rates <- 
  dec_total |> 
  group_by(period) |> 
  summarize(Delta_rates = sum(cc_total)) 

Delta_structure <-
  kit |> 
  group_by(period) |> 
  summarize(Delta_structure = sum(structure_effect))

Delta_check <- left_join(Delta_rates, Delta_structure, by = join_by(period)) |> 
  mutate(Delta_check = Delta_rates + Delta_structure)
# compare with stationary difference:
total_stationary$HLE_women - total_stationary$HLE_men

# This is necessarily exact because (1) Kitagawa is exact,
# and (2) the edu-specific contributions derive from
# Kitagawa parameters, i.e. they were rescaled.

write_csv(dec_total, file = "data/dec_total_gender.csv.gz")
write_csv(kit, file = "data/kitagawa_gender.csv")

