
# source the sensitivity functions from github

source("https://raw.githubusercontent.com/timriffe/ms_sensitivity/master/R/00_functions_classic.R")
source("https://raw.githubusercontent.com/timriffe/ms_sensitivity/master/R/00_sensitivity_functions.R")


library(tidyverse)
IN <- read_csv("data/transitions.csv")

IN |> head()

prev <- IN |> 
  filter(age == 40)
trans <- IN |> 
  filter(age > 40) 

init_pars <- 
  prev |> 
  filter(educ == "total") |> 
  select(sex, H = HH, U = UU) |> 
  pivot_longer(H:U, names_to = "state", values_to = "p") |> 
  pivot_wider(names_from = sex, values_from = p) |> 
  mutate(delta = f - m,
         p = (f + m) / 2)

init <- init_pars$p
names(init) <- c("H","U")

decomp_data <- 
  trans |> 
  filter(educ == "total") |> 
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
  mutate(transition = "init", educ = "total", age = 0) |> 
  bind_rows(decomp_data) |> 
  filter(! transition %in% c("UU","HH","UH","UD")) |> 
  left_join(sen, by = join_by("transition", "age")) |> 
  mutate(cc = delta * effect) |> 
  mutate(age = age+ 40)


dec |> 
  filter(transition != "init") |> 
  mutate(age = age) |>
  ggplot(aes(x = age, y = cc, color = transition)) +
  geom_line() 

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

