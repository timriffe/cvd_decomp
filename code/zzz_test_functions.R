
# source the sensitivity functions from github

source("https://raw.githubusercontent.com/timriffe/ms_sensitivity/master/R/00_functions_classic.R")
source("https://raw.githubusercontent.com/timriffe/ms_sensitivity/master/R/00_sensitivity_functions.R")
library(readxl)

library(tidyverse)

IN2 <- read_excel("data/TP_2016_2020.xlsx") |> 
  select(1:10)
IN2$educ |> unique()



dec2 <- 
IN2 |> 
  group_by(educ) |> 
  group_modify(~do_dec(data = .x)) |> 
  ungroup()

dec2
prev <- tibble()


expectancies <-
  IN2 |> 
  group_by(educ, sex) |> 
  summarize(HLE = calc_expectancy(ptibble=.data)) |> 
  pivot_wider(names_from = sex, values_from = HLE, names_prefix = "HLE_")

total_non_stationary <- expectancies |> 
  filter(educ == "total") |> 
  mutate(variant = "non-stationary", .before = 1)

le_for_kit <- expectancies |> 
  filter(educ != "total")

prev_for_kit <- tibble(educ = c("basic","intermediate","tertiary"),
                       prev_m = c(.155,.469,.375),
                       prev_f = c(.079,.354,.567)) |> 
  mutate(prev_m = prev_m / sum(prev_m),
         prev_f = prev_f / sum(prev_f))

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

total_stationary <-
  kit |> 
  ungroup() |> 
  summarize(educ = "total",
            HLE_f = sum(HLE_f * prev_f),
            HLE_m = sum(HLE_m * prev_m)) |> 
  mutate(variant = "stationary", .before = 1)

# compare total HLE (blended versus observed)
total_hle <- 
total_stationary |> 
  bind_rows(total_non_stationary)

total_hle

# reweight decompositions:
dec_total <-
  dec2 |> 
  filter(educ != "total") |> 
  left_join(dec_weights, by = join_by(educ)) |> 
  group_by(educ) |> 
  mutate(cc_total = (cc / sum(cc)) * rate_effect)

# Our complete decomposition:
dec_total |> 
  pull(cc_total) |> 
  sum() + sum(kit$structure_effect)

# compare with stationary difference:
total_stationary$HLE_f - total_stationary$HLE_m 


dec_total |> 
  group_by(educ, transition) |> 
  summarize(cc = sum(cc_total)) |> 
  pivot_wider(names_from = educ, values_from = cc) |> 
  pivot_longer(-transition, names_to = "educ", values_to = "cc") |> 
  group_by(transition) |> 
  summarize(cc = sum(cc))

write_csv(dec_total, file = "data/dec_total.csv")
write_csv(kit, file = "data/kitagawa.csv")
dec2 |> 
  filter(transition != "init") |> 
  mutate(age = age) |>
  ggplot(aes(x = age, y = p, color = transition, linetype = educ)) +
  geom_line() +
  scale_y_log10()



