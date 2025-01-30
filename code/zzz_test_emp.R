
library(tidyverse)
library(readxl)

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

# just skip the above and read in results
dat_out <- read_csv("data/TP_emp.csv.gz",
                    show_col_types = FALSE)

# custom chunk smoother function, very very simplified.
smooth.spline.chunk <- function(chunk, 
                                age_out =  seq(40,99.75,by=.25)){
  chunk <- chunk |> filter(!is.na(EMP))
  mod <- smooth.spline(x = chunk$age, y = log(chunk$EMP), w = sqrt(chunk$denom), df= 5)
  tibble(age = age_out,
         sm_sp = predict(mod, x = age_out)$y |> exp())
}

sm_compare <- 
  dat_out |> 
  group_by(period, gender, educ, transition) |> 
  group_modify(~ smooth.spline.chunk(chunk = .x)) |> 
  left_join(dat_out, by = join_by(period, gender, educ, transition,age))

sm_compare |> 
  filter(educ == "total",     #"tertiary" "basic" "secondary" "total"
         gender == "women") |>   # "men" "women"
  ggplot(aes(x = age, y = sm_sp)) +
  geom_line(linewidth = 2) +
  scale_y_log10() +
  geom_point(mapping = aes(y = EMP), alpha = .25) +
  geom_line(mapping = aes(y = MOD), color = "red") +
  facet_wrap(period~transition)
  

# I might rather replace this spline approach with a P-spline or similar, maybe via mgcv::gam(),
# just because it'll be easier to write down the formula in the manuscript, but the result ought 
# to be basically the same
library(mgcv)
library(scam)
# newdata <- tibble(age = seq(40,100,by=.25), denom = 1)

# chunk <- dat_out |> 
#   filter(gender == "men",
#          educ == "basic",
#          period == "2016_20",
#          transition == "UD")

mono_psline_scam_chunk <- function(chunk, k = 6){
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
    mutate(p_fit = exp(logp)) |> 
    select(-logp,-denom) 
  
}

mpi_compare <- 
  dat_out |> 
  group_by(period, gender, educ, transition) |> 
  group_modify(~ mono_psline_scam_chunk(chunk = .x, k = 6)) |> 
  left_join(dat_out, by = join_by(period, gender, educ, transition,age))

mpi_compare |> 
  filter(educ == "total",     #"tertiary" "basic" "secondary" "total"
         gender == "women") |>   # "men" "women"
  ggplot(aes(x = age, y = p_fit)) +
  geom_line(linewidth = 1) +
  scale_y_log10() +
  geom_point(mapping = aes(y = EMP), alpha = .25) +
  geom_line(mapping = aes(y = MOD), color = "red") +
  facet_wrap(period~transition)
