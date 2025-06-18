source("code/00_setup.R")
source("code/01_functions.R")


dat_emp <- read_csv("data/emp_probs_cod.csv.gz",
                    show_col_types = FALSE)
denoms <- read_csv("data/denoms.csv.gz",
                   show_col_types = FALSE)

ps_results <- 
  dat_emp |> 
  pivot_wider(names_from = transition, values_from = p) |> 
  # mutate(HD3 = HD3 + HD4,
  #        UD3 = UD3 + UD4) |> 
  # select(-HD4, -UD4) |> 
  pivot_longer(HU:UD, names_to = "transition",values_to = "p") |> 
  mutate(state_from = substr(transition,1,1)) |> 
  left_join(denoms, by = join_by(period, gender, educ, age, state_from)) |> 
  rename(EMP = p) |> 
  group_by(period, gender, educ, transition) |> 
  group_modify(~ pspline_gam_chunk(chunk = .x, k = 5))
# ps_results |> 
#   pivot_wider(names_from = transition, values_from = ps_fit) |> 
#   mutate(HD_resid = HD1 + HD2 + HD3 - HD,
#          UD_resid = UD1 + UD2 + UD3 - UD) |> 
#   ggplot(aes(x = age, y = HD_resid, color = educ)) +
#   geom_line() +
#   theme_minimal() +
#   facet_wrap(gender~period)

ps_results_constrained <-
  ps_results |> 
  pivot_wider(names_from = transition, values_from = ps_fit) |> 
  mutate(HD_sum = HD1 + HD2 + HD3,
         UD_sum = UD1 + UD2 + UD3,
         HD1 = HD * (HD1 / HD_sum),
         HD2 = HD * (HD2 / HD_sum),
         HD3 = HD * (HD3 / HD_sum),
         UD1 = UD * (UD1 / UD_sum),
         UD2 = UD * (UD2 / UD_sum),
         UD3 = UD * (UD3 / UD_sum)) |> 
  select(-HD_sum,-UD_sum) |> 
  pivot_longer(-c(period, gender, educ, age), 
               names_to = "transition", 
               values_to = "ps_fit_constrained")

p_all <-
  full_join(ps_results, 
            ps_results_constrained,
            by = join_by(period, gender, educ, transition, age)) |> 
  left_join(dat_emp, 
            by = join_by(period, gender, educ, transition, age)) |> 
  rename(p_emp = p) |> 
  pivot_longer(c(ps_fit,ps_fit_constrained,p_emp),
               names_to = "version",
               values_to = "p")

# write_csv(p_all, file = "data/TP_final.csv.gz")



# p_all |> 
#   filter(transition == "HD3") |> 
#   ggplot(aes(x = age, y = ps_fit_constrained, color = gender)) +
#   geom_line() +
#   theme_minimal() +
#   facet_wrap(educ~period) +
#   scale_y_log10() +
#   geom_point(mapping = aes(x = age, y = p_emp),alpha=.2)

# ps_both |> 
#   filter(transition %in% c("HD2","UD2")) |> 
#   select(-ps_fit) |> 
#   pivot_wider(names_from = transition, values_from  = ps_fit_constrained) |> 
#   mutate(D2_ratio = UD2/ HD2) |> 
#   ggplot(aes(x = age, y = D2_ratio, color = educ)) +
#   geom_line() +
#   theme_minimal() +
#   facet_wrap(gender~period) +
#   geom_hline(yintercept = 1) +
#   scale_y_log10()

# ps_results <- 
#   dat_out |> 
#   group_by(period, gender, educ, transition) |> 
#   group_modify(~ pspline_gam_chunk(chunk = .x, k = 6))

# mpi_results <-
#   dat_out |> 
#   group_by(period, gender, educ, transition) |> 
#   group_modify(~ mono_pspline_scam_chunk(chunk = .x, k = 6))



# view weighting function
# tibble(age = seq(40,100,by = .25)) |> 
#   mutate(w = logistic_weights(x = age, scale = .5, pivot_age = 75)) |> 
#   ggplot(aes(x=age,y=w)) +
#   theme_minimal() +
#   geom_line()
# all_compare <- 
#   mpi_results |> 
#   left_join(dat_out, by = join_by(period, gender, educ, transition, age)) |> 
#   relocate(mpi_fit, .after = MOD) |> 
#   rename(p_emp = EMP,
#          p_dtms = MOD,
#          p_mpi = mpi_fit) |> 
#   left_join(ps_results, by = join_by(period, gender, educ, transition, age)) |> 
#   rename(p_ps = ps_fit) |> 
#   group_by(period, gender, educ, transition) |> 
#   mutate(w = logistic_weights(x = age, scale = .5, pivot_age = 75),
#          p_hybrid = p_mpi * w + p_ps * (1 - w)) |> 
#   ungroup() |> 
#   select(-w, -denom) |> 
#   pivot_longer(p_emp:p_hybrid, names_to = "version", values_to = "p") |> 
#   mutate(period = if_else(period == "2000_04","2000-2004","2016-2020"))


# all_compare <- 
#   ps_both |> 
#   left_join(dat_emp, by = join_by(period, gender, educ, transition, age)) |> 
#   rename(p_emp = p,
#          p_ps = ps_fit,
#          p_psc = ps_fit_constrained) |> 
#   pivot_longer(c(p_emp,p_ps,p_psc), names_to = "version", values_to = "p")


# get initial conditions coded same as before
# under HH and UU
versions <- p_all |> 
  ungroup() |> 
  select(version) |> 
  distinct()

init <-
  denoms |> 
  filter(age < 45) |> 
  group_by(period,  gender, educ, state_from) |> 
  summarize(init = sum(denom), .groups = "drop") |> 
  group_by(period,  gender, educ) |> 
  mutate(init = init / sum(init)) |> 
  ungroup() |> 
  mutate(transition = paste0(state_from, state_from)) |> 
  cross_join(versions) |> 
  mutate(age = 40) |> 
  rename(p = init) |> 
  select(all_of(colnames(p_all)))

# join together in our standard way
TP_final <-
  p_all |> 
  filter(age > 40) |> 
  bind_rows(init) |> 
  arrange(version, period, gender, educ, transition, age)

write_csv(TP_final, "data/TP_final.csv.gz")
# check: we're still constrained overall
# all_compare |> 
#   group_by(state_from, educ, gender, period, age) |> 
#   summarize(p = sum(p_final)) |> 
#   filter(p > 1)

# OLD
# run_this <- FALSE
# if (run_this){
#   # for comparing fits with empirical, dtms, ps, and mpi 
#   all_compare |> 
#   filter(educ == "total",     #"tertiary" "basic" "secondary" "total"
#          gender == "men") |>   # "men" "women"
#     pivot_wider(names_from = version, values_from = p) |> 
#   ggplot(aes(x = age, y = p_mpi)) +
#   geom_line(mapping = aes(y = p_hybrid), linewidth = 1, color = "green") +
#   geom_line(linewidth = 1, color = "blue", linetype = 3) +
#   scale_y_log10() +
#   geom_point(mapping = aes(y = p_emp), alpha = .1) +
#   # geom_line(mapping = aes(y = p_dtms), color = "red") +
#   geom_line(mapping = aes(y = p_ps), color = "red", linetype = 2,linewidth = 1) +
#   facet_wrap(period~transition) +
#   theme_minimal()
# }



# create prev_edu datafile 
prev_edu <-
  denoms |> 
  filter(age < 45,
         educ != "total") |> 
  select(period, gender, educ, state_from, age, denom) |> 
  group_by(period, gender, educ, age, state_from) |> 
  slice(1) |> 
  group_by(period,  gender, educ) |> 
  summarize(init = sum(denom), .groups = "drop") |> 
  group_by(period,  gender) |> 
  mutate(init = init / sum(init)) |> 
  ungroup() 

write_csv(prev_edu, "data/prev_edu.csv")
