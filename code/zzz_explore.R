library(readxl)
library(tidyverse)
IN <- read_csv("data/transitions.csv")
head(IN)
options(scipen = 5)
IN |> 
  filter(age > 40,
         age < 100) |> 
  pivot_longer(HH:UD, names_to = "transition", values_to = "prob") |> 
  filter(!transition %in% c("UU","HH","UH")) |> 
  ggplot(aes(x = age, y = prob, color = transition, linetype = sex)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~educ) +
  scale_y_log10()

# Rate ratios:
# mortality penalty
IN |> 
  filter(age > 40,
         age < 100) |> 
  mutate(ratio = UD / HD) |> 
  ggplot(aes(x = age, y = ratio, linetype = sex)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~educ) +
  scale_y_log10()

# sex ratios:
IN |> 
  filter(age > 40,
         age < 100)  |> 
  pivot_longer(HH:UD, names_to = "transition", values_to = "prob") |> 
  pivot_wider(names_from = sex, values_from = prob) |> 
  filter(! transition %in% c("HH","UU","UH")) |> 
  mutate(SR = m / f) |> 
  ggplot(aes(x = age, y = SR, color = transition)) +
  geom_line() +
  scale_y_log10() +
  theme_minimal() +
  facet_wrap(~educ)

# ALR 
IN |> 
  filter(age > 40,
         age < 100) |> 
  mutate(HD2 = log(HD / HH),
         HU2 = log(HU / HH),
         UD2 = log(UD / UU)) |> 
  select(educ,sex,age,ends_with("2")) |> 
  pivot_longer(ends_with("2"),names_to ="transition",values_to="alr") |> 
  ggplot(aes(x = age, y = alr, color = transition, linetype = sex)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~educ)


