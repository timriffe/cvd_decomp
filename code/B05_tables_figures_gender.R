<<<<<<< HEAD
source("code/03_decompose_gender.R")
=======
# source("code/03_decompose_gender.R")
>>>>>>> a9367306a8c2d92005e73de3b296b7ab174b9325
# For example, some margin tables
source("code/00_setup.R")
dec_total <- read_csv("data/dec_total_gender.csv.gz", show_col_types = FALSE)

# educ-specific gender decomposition margins
dec_total |> 
  group_by(educ, transition, period) |> 
  summarize(cc = sum(cc)) |> 
  pivot_wider(names_from = educ, values_from = cc) |> 
  pivot_longer(c(basic,secondary,tertiary), names_to = "educ", values_to = "cc") |> 
  pivot_wider(names_from = transition, values_from = cc)

dec_total |> 
  group_by(educ, period) |> 
  summarize(cc = sum(cc))
# and their respective contributions to the 
# total gender gap
dec_total |> 
  group_by(educ, transition, period) |> 
  summarize(cc = sum(cc_total)) |> 
  pivot_wider(names_from = educ, values_from = cc) |> 
  pivot_longer(c(basic,secondary,tertiary), names_to = "educ", values_to = "cc") |> 
  pivot_wider(names_from = transition, values_from = cc)

# I suppose there is a clever way to compose a table with all this info,
# plus the Kitagawa parameters.

p1 <- 
dec_total |> 
  filter(transition != "init") |> 
  mutate(age = age) |>
  ggplot(aes(x = age, y = cc, color = transition, linetype = educ)) +
  geom_line(linewidth=1) +
  theme_minimal() +
  facet_wrap(~period) +
  labs(title = "Contribution to education-specific gender gap in CVDFLE")

p2 <-
dec_total |> 
  filter(transition != "init") |> 
  mutate(age = age) |>
  ggplot(aes(x = age, y = cc_total, color = transition, linetype = educ)) +
  geom_line(linewidth=1) +
  theme_minimal()  +
  facet_wrap(~period) +
  labs(title = "Contribution to total gender gap in CVDFLE")

p1
p2
