
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

