source("code/00_setup.R")
source("code/01_functions.R")

# effect on total life expectancy in most recent period of halving onset.

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


# To know how to weight these, we need all cvd-free life
# expectancies, for use in a Kitagawa decomposition.
expectancies1 <-
  IN |> 
  filter(time_mid == max(time_mid)) |> 
  group_by(educ, gender) |> 
  summarize(LE = calc_expectancy(ptibble=.data, expectancy = "t")) |> 
  pivot_wider(names_from = gender, 
              values_from = LE, 
              names_prefix = "LE_")

expectancies2 <-
  IN |> 
  mutate(HH = if_else(!is.na(HU),HH + HU / 2,HH),
         HU = HU / 2) |> 
  filter(time_mid == max(time_mid)) |> 
  group_by(educ, gender) |> 
  summarize(LE = calc_expectancy(ptibble=.data, expectancy = "t")) |> 
  pivot_wider(names_from = gender, 
              values_from = LE, 
              names_prefix = "LE_")

# what if we also halve HD1 and UD1??
expectancies3 <-
  IN |> 
  mutate(HU = HU / 2,
         HD = HD - HD1 / 2,
         UD = UD - UD1 / 2,
         HH = if_else(age>40, 1 - HD - HU, HH),
         UU = if_else(age>40,1 - UD, UU))|> 
  filter(time_mid == max(time_mid)) |> 
  group_by(educ, gender) |> 
  summarize(LE = calc_expectancy(ptibble=.data, expectancy = "t")) |> 
  pivot_wider(names_from = gender, 
              values_from = LE, 
              names_prefix = "LE_")
  
# now also halve HD2 and HD3
expectancies4 <-
  IN |> 
  mutate(HU = HU / 2,
         HD = HD - HD1 / 2 - HD2 / 2,
         UD = UD - UD1 / 2 - UD2 / 2,
         HH = if_else(age>40, 1 - HD - HU, HH),
         UU = if_else(age>40,1 - UD, UU))|> 
  filter(time_mid == max(time_mid)) |> 
  group_by(educ, gender) |> 
  summarize(LE = calc_expectancy(ptibble=.data, expectancy = "t")) |> 
  pivot_wider(names_from = gender, 
              values_from = LE, 
              names_prefix = "LE_")

# unaltered HU
expectancies5 <-
  IN |> 
  mutate(HD = HD - HD1 / 2 - HD2 / 2,
         UD = UD - UD1 / 2 - UD2 / 2,
         HH = if_else(age>40, 1 - HD - HU, HH),
         UU = if_else(age>40,1 - UD, UU))|> 
  filter(time_mid == max(time_mid)) |> 
  group_by(educ, gender) |> 
  summarize(LE = calc_expectancy(ptibble=.data, expectancy = "t")) |> 
  pivot_wider(names_from = gender, 
              values_from = LE, 
              names_prefix = "LE_")


# unaltered HU
expectancies6 <-
  IN |> 
  mutate(HU = HU / 2,
         HD = HD - HD1 / 2 - HD2 / 2,
         HH = if_else(age>40, 1 - HD - HU, HH),
         UU = if_else(age>40,1 - UD, UU))|> 
  filter(time_mid == max(time_mid)) |> 
  group_by(educ, gender) |> 
  summarize(LE = calc_expectancy(ptibble=.data, expectancy = "t")) |> 
  pivot_wider(names_from = gender, 
              values_from = LE, 
              names_prefix = "LE_")


# probably the statistic to cite is 6 vs 1

# ---------------------------------------------------- #
# repeat for CVDFLE
expectancies6h <-
  IN |> 
  mutate(HU = HU / 2,
         HD = HD - HD1 / 2 - HD2 / 2,
         HH = if_else(age>40, 1 - HD - HU, HH),
         UU = if_else(age>40,1 - UD, UU))|> 
  filter(time_mid == max(time_mid)) |> 
  group_by(educ, gender) |> 
  summarize(LE = calc_expectancy(ptibble=.data, expectancy = "h")) |> 
  pivot_wider(names_from = gender, 
              values_from = LE, 
              names_prefix = "LE_")

expectancies1h <-
  IN |> 

  filter(time_mid == max(time_mid)) |> 
  group_by(educ, gender) |> 
  summarize(LE = calc_expectancy(ptibble=.data, expectancy = "h")) |> 
  pivot_wider(names_from = gender, 
              values_from = LE, 
              names_prefix = "LE_")

