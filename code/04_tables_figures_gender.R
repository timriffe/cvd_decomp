source("code/02_decompose.R")
# For example, some margin tables


# educ-specific gender decomposition margins
dec_total |> 
  group_by(educ, transition) |> 
  summarize(cc = sum(cc)) |> 
  pivot_wider(names_from = educ, values_from = cc) |> 
  pivot_longer(-transition, names_to = "educ", values_to = "cc") |> 
  pivot_wider(names_from = transition, values_from = cc)

dec_total |> 
  group_by(educ) |> 
  summarize(cc = sum(cc))
# and their respective contributions to the 
# total gender gap
dec_total |> 
  group_by(educ, transition) |> 
  summarize(cc = sum(cc_total)) |> 
  pivot_wider(names_from = educ, values_from = cc) |> 
  pivot_longer(-transition, names_to = "educ", values_to = "cc") |> 
  pivot_wider(names_from = transition, values_from = cc)

# I suppose there is a clever way to compose a table with all this info,
# plus the Kitagawa parameters.

p1 <- 
dec_total |> 
  filter(transition != "init") |> 
  mutate(age = age) |>
  ggplot(aes(x = age, y = cc, color = transition, linetype = educ)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Contribution to education-specific gender gap in CVDFLE")

p2 <-
dec_total |> 
  filter(transition != "init") |> 
  mutate(age = age) |>
  ggplot(aes(x = age, y = cc_total, color = transition, linetype = educ)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Contribution to total gender gap in CVDFLE")

p1
p2
