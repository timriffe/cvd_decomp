

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

IN |> 
  select(period,    gender,educ, age, HD) |> 
  pivot_wider(names_from = period, values_from = HD) |> 
  mutate(ratio = `2016-2020` / `2000-2004`,
         incidence_future = if_else(ratio < 1, ratio * `2016-2020`, `2016-2020`)) |> 
  ggplot(aes(x= age, y = incidence_future)) +
  geom_line() +
  geom_line(aes(y=`2016-2020`), color = "red") +
  geom_line(aes(y=`2000-2004`), color = "blue") +
  facet_wrap(gender~educ) +
  scale_y_log10()

# do same perturbation to HU and HD, then recalculate CVDFLE









