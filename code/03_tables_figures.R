source("code/02_decompose.R")
# For example, some margin tables


# educ-specific decomposition margins
dec_total |> 
  group_by(educ, transition) |> 
  summarize(cc = sum(cc)) |> 
  pivot_wider(names_from = educ, values_from = cc) |> 
  pivot_longer(-transition, names_to = "educ", values_to = "cc") |> 
  pivot_wider(names_from = transition, values_from = cc)

# and their respective contributions to the 
# total gender gap
dec_total |> 
  group_by(educ, transition) |> 
  summarize(cc = sum(cc_total)) |> 
  pivot_wider(names_from = educ, values_from = cc) |> 
  pivot_longer(-transition, names_to = "educ", values_to = "cc") |> 
  pivot_wider(names_from = transition, values_from = cc)
