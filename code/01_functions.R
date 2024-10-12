

# a function to combine the goodies of ms_sensitivity into a handy
# decomp wrapper. It's hard-coded to the data characteristics in this
# particular study: ages 40+, quarter-year time intervals, etc
do_dec <- function(data){
  prev <- data |> 
    filter(age == 40)
  trans <- data |> 
    filter(age > 40) 
  
  init_pars <- 
    prev |> 
    fselect(sex, H = HH, U = UU) |> 
    dt_pivot_longer(H:U, names_to = "state", values_to = "p") |> 
    dt_pivot_wider(names_from = sex, values_from = p) |> 
    fmutate(delta = f - m,
            p = (f + m) / 2)
  
  init <- init_pars$p
  names(init) <- c("H","U")
  
  decomp_data <- 
    trans |> 
    dt_pivot_longer(HH:UD, names_to = "transition", values_to = "p") |> 
    dt_pivot_wider(names_from = sex, values_from = p) |> 
    fmutate(delta = f - m,
            p = (f + m) / 2) |> 
    fselect(-m, -f, -Age) |> 
    arrange(transition, age) |> 
    fmutate(age = age - min(age))
  
  
  sen <-
    decomp_data |> 
    s2t(init = init, expectancy = "h", interval = .25) |> 
    fmutate(effect = effect)
  
  
  dec <- 
    init_pars |> 
    filter(state == "H") |> 
    fselect(-state,-m,-f) |> 
    fmutate(transition = "init", age = 0) |> 
    bind_rows(decomp_data) |> 
    filter(! transition %in% c("UU","HH","UH","UD")) |> 
    left_join(sen, by = join_by("transition", "age")) |> 
    fmutate(cc = delta * effect) |> 
    fmutate(age = age + 40)
  
  dec
}

# an expectancy wrapper, also for our data
# characteristics
calc_expectancy <- function(ptibble,
                            expectancy = "h",
                            interval = .25){
  init <- c(H = ptibble$HH[1], U = ptibble$UU[1])
  init <- init * 1/sum(init)
  
  # we could have used f2() or f3() and
  # got identical results (assuming these got
  # interval args integrated)
  f1(hh = ptibble$HH[-1], 
     hu = ptibble$HU[-1], 
     uu = ptibble$UU[-1], 
     uh = ptibble$UH[-1], 
     init = init, 
     expectancy = expectancy,
     interval = interval)
}

