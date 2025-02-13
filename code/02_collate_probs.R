
library(tidyverse)
library(readxl)

emp_probs <- structure(list(period = character(0), 
                            educ = character(0), 
                            gender = character(0), 
                            age = numeric(0), 
                            transition = character(0), 
                            p = numeric(0)), 
                       row.names = integer(0), 
                       class = c("tbl_df","tbl", "data.frame"))

for (per in c("2000_04","2016_20")){
  this_file <- paste0("data/Cause_Emp_",per,".xlsx")
  shts      <- excel_sheets(this_file)
  period <- ifelse(per == "2000_04",
                   "2000-2004",
                   "2016-2020")
  for (sh in shts){
    educ_gender <- 
      str_split(sh,pattern = " ")[[1]] |> 
      tolower()
    educ_gender[educ_gender=="overall"] <-"total"
    this_subset <- read_excel(this_file, sheet = sh) |> 
      select(!contains("...")) |> 
      pivot_longer(-age, 
                   names_to = "transition",
                   values_to = "p") |> 
      mutate(p = replace_na(p,0),
             age = age / 12) |> 
      mutate(period = period, .before = 1) |> 
      mutate(educ = educ_gender[1], 
             gender = educ_gender[2],
             .after = period)
    emp_probs <- bind_rows(emp_probs, this_subset)
    
    }
}

# Group UNK (4) with Other (3)
emp_probs <- 
  emp_probs |> 
  select(!contains("...")) |> 
  pivot_wider(names_from = transition, values_from = p) |> 
  mutate(HD3 = HD3 + HD4,
         UD3 = UD3 + UD4) |> 
  select(-HD4, -UD4) |> 
  pivot_longer(-c(period, educ, gender, age), 
               names_to = "transition", 
               values_to = "p")

# save
write_csv(emp_probs,"data/emp_probs_cod.csv.gz")

# repeat for files without COD because these have denominators!
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
emp <- read_csv("data/TP_emp.csv.gz")
denoms <- 
  emp |> 
  mutate(period = if_else(period == "2000_04","2000-2004","2016-2020")) |> 
  group_by(period, gender, educ, age, state_from) |> 
  summarize(denom = denom[1], .groups = "drop")
write_csv(denoms, "data/denoms.csv.gz")
