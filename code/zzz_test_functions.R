
# source the sensitivity functions from github

source("https://raw.githubusercontent.com/timriffe/ms_sensitivity/master/R/00_functions_classic.R")
source("https://raw.githubusercontent.com/timriffe/ms_sensitivity/master/R/00_sensitivity_functions.R")
library(readxl)

library(tidyverse)
IN <- read_csv("data/transitions.csv")
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
  group_modify(~do_dec(data = .x))






dec2 |> 
  filter(transition == "init")

dec2 |> 
  filter(transition != "init") |> 
  mutate(age = age) |>
  ggplot(aes(x = age, y = cc, color = transition, linetype = educ)) +
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

