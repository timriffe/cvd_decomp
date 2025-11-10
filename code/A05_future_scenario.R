

source("code/00_setup.R")
source("code/01_functions.R")
# Project 1:
#   Please find below the educational composition at age 25-29 in 2016-2020:
#   
#   Men:        basic (17) secondary (58) tertiary (25)
# Women: basic (13) secondary (48) tertiary (39)

# time_mid = if_else(period == "2016-2020",2018,2002)

IN <- read_csv("data/TP_final.csv.gz", show_col_types = FALSE)  |> 
  filter(version == "ps_fit_constrained") |> 
  select(period, gender, educ, transition, age, p) |> 
  pivot_wider(names_from = transition, values_from = p) |> 
  mutate(UH = 0,
         UU = if_else(is.na(UU), 1 - UD - UH, UU)) |> 
  arrange(period, educ, gender, age) 

Hprojected <-
  IN |> 
  select(period,    gender,educ, age, HD, HU,HH) |> 
  pivot_wider(names_from = period, values_from = c(HU,HD,HH)) |> 
  mutate(HU_ratio = `HU_2016-2020` / `HU_2000-2004`,
         `HU_2032-2036` = if_else(HU_ratio < 1, 
                                       HU_ratio * `HU_2016-2020`, 
                                       `HU_2016-2020`),
         HD_ratio = `HD_2016-2020` / `HD_2000-2004`,
         `HD_2032-2036` = if_else(HD_ratio < 1, 
                                       HD_ratio * `HD_2016-2020`, 
                                       `HD_2016-2020`)) |> 
  select(-ends_with("ratio")) |> 
  pivot_longer(-c(gender,educ,age), 
               names_to = c("transition","period"), 
               values_to = "p", 
               names_sep = "_") |> 
    pivot_wider(names_from = transition, values_from = p) |> 
    mutate(HH = if_else(age == 40, HH, 1 - HU - HD)) |> 
    pivot_longer(c(HH,HU,HD), names_to = "transition", values_to = "p") |> 
    mutate(p = if_else(is.na(p),0,p))
 
projected_part <-
  IN |> 
  select(period, gender, educ, age, HH, HD, HU, UU, UH, UD) |> 
  pivot_longer(HH:UD, names_to = "transition",values_to = "p") |> 
  filter(transition %in% c("UU","UH","UD"))|> 
  mutate(p = if_else(is.na(p),0,p)) |> 
  bind_rows(Hprojected)
  #HH = if_else(is.na(HH),1 - HU - HD, HH),
# do same perturbation to HU and HD, then recalculate CVDFLE
projected_p <-
  projected_part |> 
  filter(period == "2016-2020",
         transition %in% c("UU","UH","UD")) |> 
  mutate(period = "2032-2036") |> 
  bind_rows(projected_part) |> 
  arrange(period, gender, educ, transition, age) |> 
  pivot_wider(names_from = transition, values_from = p) |> 
  mutate(HH = if_else(age==40 & period == "2032-2036",1-UU,HH)) 

# projected_p |> 
# 
#   filter(age==40) |> View()
future_cvdfle <- 
  projected_p |> 
  mutate(HH = if_else(age==40 & period == "2032-2036",1-UU,HH)) |> 
  group_by(period, gender, educ) |> 
  summarize(HLE = calc_expectancy(ptibble=.data)) |> 
  pivot_wider(names_from = period, values_from= HLE) 
future_cvdfle
future_cvdfle |> 
write_csv(file="data/future_cvdfle.csv")

# the edu gap would grow:
future_cvdfle |> 
  pivot_longer(3:5, names_to = "period", values_to = "CVDFLE") |> 
  pivot_wider(names_from = educ, values_from = CVDFLE) |> 
  mutate(gap = tertiary - basic)

# the gender gap would shrink:
future_cvdfle |> 
  pivot_longer(3:5, names_to = "period", values_to = "CVDFLE") |> 
  pivot_wider(names_from = gender, values_from = CVDFLE) |> 
  mutate(gap = women - men) |> 
  select(educ, period, gap) |> 
  pivot_wider(names_from = period, values_from = gap)


# Weighted total CVDFLE:
# Period 2032-2036 uses Wittgenstein Human Capital Explorer Data
# https://dataexplorer.wittgensteincentre.org/wcde-v3/
# We average educ prevalence in ages 35-39 and 40-44 to center on age 40.
prev_2035 <-
  read_csv("data/wicdf.csv", skip = 8) |> 
  filter(Education != "Total") |> 
  mutate(educ = case_when(Education %in% 
                            c("Under 15","No Education",
                              "Incomplete Primary","Primary","Lower Secondary") ~ "basic",
                          Education %in% c("Upper Secondary") ~ "secondary",
                          # tertiary includes Short post secondary, 
                          # post secondary, bachelor, and master and higher.
                          TRUE ~ "tertiary")) |> 
  group_by(Age,Sex, educ) |> 
  summarize(pop = sum(Population), .groups = "drop") |> 
  group_by(Age,Sex) |> 
  mutate(prev = pop / sum(pop)) |> 
  ungroup() |> 
  select(-pop) |> 
  group_by(Sex, educ) |> 
  summarize(init = mean(prev), .groups= "drop") |> 
  mutate(gender = if_else(Sex == "Male","men","women")) |> 
  select(-Sex) |> 
  mutate(period = "2032-2036")


prev_educ <- read_csv("data/prev_edu.csv", show_col_types = FALSE) |> 
  bind_rows(prev_2035)

# weighted total:
future_total_cvdfle <-
  future_cvdfle |> 
  pivot_longer(-c(gender, educ),
               names_to = "period",
               values_to = "CVDFLE") |> 
  right_join(prev_educ,by = join_by(gender, educ, period)) |> 
  group_by(gender, period) |> 
  summarize(CVDFLE = sum(CVDFLE * init)) 

future_total_cvdfle |> 
  ggplot(aes(x = parse_number(period) +2, 
             y = CVDFLE, 
             color = gender)) + geom_point() + geom_line()

  
