
library(tidyverse)
library(readxl)
library(mgcv)
library(scam)
# Code for wrangling data out of Excel spreadsheets,
# this is optional: we may be able to share the csv produced.
# Script can begin after this if ()
run_this <- FALSE
if (run_this){
  file_names <- c("TP_2000_04.xlsx",
                  "TP_2016_20.xlsx")
  dat_out <- structure(list(period = character(0), gender = character(0), 
                            educ = character(0), age = numeric(0), transition = character(0), 
                            state_from = character(0), EMP = numeric(0), MOD = numeric(0), 
                            denom = numeric(0)), class = c("tbl_df", "tbl", "data.frame"
                            ), row.names = integer(0))
  for (f in file_names){
    
    period = substr(f,4,10)
    for (s in excel_sheets(file.path("data",f))){
      name_parts <- str_split(s, pattern = " ")[[1]]
      educ   <- name_parts[1]
      educ   <- ifelse(educ == "Overall","Total", educ)
      gender <- name_parts[2]
      ini    <- read_excel(file.path("data",f), sheet = s) |> 
        rename(age = 1) |> 
        select(!contains("...")) |> 
        mutate(age = (age - (age %% 12))/12 + ( (age %% 12) / 12)) |> 
        pivot_longer(-age, names_to = c("variant","transition"), names_sep = " ", values_to = "p") |> 
        mutate(period = period, gender = gender, educ = educ,.before = 1) |> 
        mutate(state_from = substr(transition,1,1))
      
      denom <- ini |> 
        filter(variant == "DENOM") |> 
        select(denom = p,state_from, age)
      
      ini2 <- 
        ini |> 
        filter(variant != "DENOM") |> 
        pivot_wider(names_from = variant, values_from = p) |> 
        left_join(denom, by = join_by(state_from, age))
      
      dat_out <- bind_rows(dat_out, ini2)
    }
  }
  
  dat_out <- 
    dat_out |> 
    mutate(gender = tolower(gender),
           educ = tolower(educ)) |> 
    filter(!is.na(age))
  
  write_csv(dat_out, file = "data/TP_emp.csv.gz")
}

# ------------------------------ #
# start here
# just skip the above and read in results
# ------------------------------ #

dat_out <- read_csv("data/TP_emp.csv.gz",
                    show_col_types = FALSE)


mono_pspline_scam_chunk <- function(chunk, k = 6){
  newdata <- tibble(age = seq(40,100,by=.25), denom = 1)
  chunk |> 
    mutate(y = round(EMP * denom)) |> 
    filter(!is.na(y),
           !is.nan(y),
           denom > 0) %>% 
    scam(y ~ s(age, bs = "mpi", k = k) + offset(log(denom)),
         data = .,
         family = poisson) |> 
    predict(newdata = newdata) |> 
    as_tibble() |> 
    rename(logp = value) |> 
    bind_cols(newdata) |> 
    mutate(mpi_fit = exp(logp)) |> 
    select(-logp,-denom) 
  
}
pspline_gam_chunk <- function(chunk, k = 6){
  newdata <- tibble(age = seq(40,100,by=.25), denom = 1)
  chunk |> 
    mutate(y = round(EMP * denom)) |> 
    filter(!is.na(y),
           !is.nan(y),
           denom > 0) %>% 
    gam(y ~ s(age, bs = "ps", k = k) + offset(log(denom)),
         data = .,
         family = poisson) |> 
    predict(newdata = newdata) |> 
    as_tibble() |> 
    rename(logp = value) |> 
    bind_cols(newdata) |> 
    mutate(ps_fit = exp(logp)) |> 
    select(-logp,-denom) 
}

ps_results <- 
  dat_out |> 
  group_by(period, gender, educ, transition) |> 
  group_modify(~ pspline_gam_chunk(chunk = .x, k = 6))

mpi_results <-
  dat_out |> 
  group_by(period, gender, educ, transition) |> 
  group_modify(~ mono_pspline_scam_chunk(chunk = .x, k = 6))

logistic_weights <- function(x, scale, pivot_age){
  1 / (1+exp(-scale* (x - pivot_age)))
}

# view weighting function
# tibble(age = seq(40,100,by = .25)) |> 
#   mutate(w = logistic_weights(x = age, scale = .5, pivot_age = 75)) |> 
#   ggplot(aes(x=age,y=w)) +
#   theme_minimal() +
#   geom_line()


all_compare <- 
  mpi_results |> 
  left_join(dat_out, by = join_by(period, gender, educ, transition, age)) |> 
  relocate(mpi_fit, .after = MOD) |> 
  rename(p_emp = EMP,
         p_dtms = MOD,
         p_mpi = mpi_fit) |> 
  left_join(ps_results, by = join_by(period, gender, educ, transition, age)) |> 
  rename(p_ps = ps_fit) |> 
  group_by(period, gender, educ, transition) |> 
  mutate(w = logistic_weights(x = age, scale = .5, pivot_age = 75),
         p_hybrid = p_mpi * w + p_ps * (1 - w)) |> 
  ungroup() |> 
  select(-w, -denom) |> 
  pivot_longer(p_emp:p_hybrid, names_to = "version", values_to = "p") |> 
  mutate(period = if_else(period == "2000_04","2000-2004","2016-2020"))


# get initial conditions coded same as before
# under HH and UU
versions <- all_compare |> 
  ungroup() |> 
  select(version) |> 
  distinct()

init <-
  dat_out |> 
  mutate(state_from = substr(transition,1,1)) |> 
  select(period, gender, educ, transition, state_from, age, denom) |> 
  filter(age < 45) |> 
  group_by(period, gender, educ, age, state_from) |> 
  slice(1) |> 
  group_by(period,  gender, educ, state_from) |> 
  summarize(init = sum(denom), .groups = "drop") |> 
  group_by(period,  gender, educ) |> 
  mutate(init = init / sum(init)) |> 
  ungroup() |> 
  mutate(transition = paste0(state_from, state_from),
         period = if_else(period == "2000_04","2000-2004","2016-2020")) |> 
  cross_join(versions) |> 
  mutate(age = 40) |> 
  rename(p = init) 

# join together in our standard way
TP_final <-
  all_compare |> 
  filter(age > 40) |> 
  bind_rows(init) |> 
  arrange(version, period, gender, educ, transition, age)

write_csv(TP_final, "data/TP_final.csv.gz")
# check: we're still constrained overall
# all_compare |> 
#   group_by(state_from, educ, gender, period, age) |> 
#   summarize(p = sum(p_final)) |> 
#   filter(p > 1)

run_this <- FALSE
if (run_this){
  # for comparing fits with empirical, dtms, ps, and mpi 
  all_compare |> 
  filter(educ == "total",     #"tertiary" "basic" "secondary" "total"
         gender == "women") |>   # "men" "women"
  ggplot(aes(x = age, y = mpi_fit)) +
  geom_line(mapping = aes(y = p_final), linewidth = 1, color = "green") +
  geom_line(linewidth = 1, color = "blue", linetype = 3) +
  scale_y_log10() +
  geom_point(mapping = aes(y = p_emp), alpha = .1) +
  # geom_line(mapping = aes(y = p_dtms), color = "red") +
  geom_line(mapping = aes(y = ps_fit), color = "red", linetype = 2,linewidth = 1) +
  facet_wrap(period~transition) +
  theme_minimal()
}
