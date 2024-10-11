
# source the sensitivity functions from github

source("https://raw.githubusercontent.com/timriffe/ms_sensitivity/master/R/00_functions_classic.R")
source("https://raw.githubusercontent.com/timriffe/ms_sensitivity/master/R/00_sensitivity_functions.R")
library(readxl)

library(tidyverse)

IN2 <- read_excel("data/TP_2016_2020.xlsx") |> 
  select(1:10)
IN2$educ |> unique()

do_dec <- function(data){
  prev <- data |> 
    filter(age == 40)
  trans <- data |> 
    filter(age > 40) 
  
  init_pars <- 
    prev |> 
    select(sex, H = HH, U = UU) |> 
    pivot_longer(H:U, names_to = "state", values_to = "p") |> 
    pivot_wider(names_from = sex, values_from = p) |> 
    mutate(delta = f - m,
           p = (f + m) / 2)
  
  init <- init_pars$p
  names(init) <- c("H","U")
  
  decomp_data <- 
    trans |> 
    pivot_longer(HH:UD, names_to = "transition", values_to = "p") |> 
    pivot_wider(names_from = sex, values_from = p) |> 
    mutate(delta = f - m,
           p = (f + m) / 2) |> 
    select(-m, -f, -Age) |> 
    arrange(transition, age) |> 
    mutate(age = age - min(age))
  
  
  sen <-
    decomp_data |> 
    s2t(init = init, expectancy = "h", interval = .25) |> 
    mutate(effect = effect)
  
  
  dec <- 
    init_pars |> 
    filter(state == "H") |> 
    select(-state,-m,-f) |> 
    mutate(transition = "init", age = 0) |> 
    bind_rows(decomp_data) |> 
    filter(! transition %in% c("UU","HH","UH","UD")) |> 
    left_join(sen, by = join_by("transition", "age")) |> 
    mutate(cc = delta * effect) |> 
    mutate(age = age + 40)
  
  dec
}


dec2 <- 
IN2 |> 
  group_by(educ) |> 
  group_modify(~do_dec(data = .x)) |> 
  ungroup()

dec2
prev <- tibble()

calc_expectancy <- function(ptibble,
         expectancy = "h",
         interval = .25){
  init <- c(H = ptibble$HH[1], U = ptibble$UU[1])
  init <- init * 1/sum(init)

  
  f1(hh = ptibble$HH[-1], 
     hu = ptibble$HU[-1], 
     uu = ptibble$UU[-1], 
     uh = ptibble$UH[-1], 
     init = init, 
     expectancy = expectancy,
     interval = interval)
}
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

dec$cc |> sum()

m <-
trans |> 
  filter(educ == "total", sex == "m") |> 
  pivot_longer(HH:UD, names_to = 'transition',values_to = "p")  
initm <- init_pars$m
names(initm) <- c("H","U")
f <-
  trans |> 
  filter(educ == "total", sex == "f") |> 
  pivot_longer(HH:UD, names_to = 'transition',values_to = "p") 
initf <- init_pars$f
names(initf) <- c("H","U")
f2t(f, init = initf, expectancy = "h", interval = .25)- 
  f2t(m, init = initm, expectancy = "h", interval = .25)

